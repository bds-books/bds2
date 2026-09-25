#######################################################################################
# December 2025
#
# The following R code allows to replicate all the analyses and examples
# in the book "Advanced Basketball Data Science - With Applications in R"
# (by P. Zuccolotto, M. Manisera and M. Sandri), CRC Press.
#
# All the R codes run assume that the reader has downloaded all the supplementary
# material (data, functions, code, and input objects), which is organized with a
# rigorous folder structure within a main folder called Rcode.
# The individual script files are stored in the subfolder of the chapter they
# refer to, which must be set as the working directory.
# In addition, the pacman package must be installed beforehand.
#
# The supplementary material can be downloaded from the book website
# http://bdsports.unibs.it/bds2
#####################################################################################

# The code has been tested on Windows and macOS 
# using R version 4.5.2 and RStudio version 2025.09.2+418, 
# and on Ubuntu 25.10 (64-bit, Linux kernel 6.17) 
# using R version 4.5.1 and RStudio version 2025.09.2+418.

####################################

# The package {\tt pacman} has to be installed
# install.packages("pacman")

# set Chapter4 as working directory
# setwd("....")


############################################
############################################
# PART 2                                   #
# Analyzing and comparing game splits      #                            
############################################
############################################

##############################################
# CHAPTER 4                                  #
# The race to the finish: exploring the      #
# relationship between season segments and   #
# final rankings                             #
##############################################

######################
# 4.1 Teams' dynamics in two acts
######################
rm(list=ls())
graphics.off()

pacman::p_load(BasketballAnalyzeR,operators,readr,lubridate,scales,dplyr)
load("./Data/LBAPbP.Rdata")

Poss <- PbP %>%
  filter(event_type!="start of period" & event_type!="sub" &
           event_type!="timeout" & event_type!="end of period") %>% 
  group_by(game_id, period) %>% 
  mutate(change_poss=ifelse(team!=lead(team, default="end"),1,0)) %>%
  ungroup() %>%
  group_by(game_id, team) %>%
  summarize(nposs=sum(change_poss)) %>%
  ungroup() %>%
  as.data.frame()

games_info <- PbP %>%
  filter(data_set=="2022-2023 Regular Season") %>%
  group_by(game_id) %>%
  dplyr::summarize(
    game_date=last(date), 
    home_score=last(home_score), 
    away_score=last(away_score), 
    home_team=last(hometeam),
    away_team=unique(team)[!(unique(team) %in% hometeam) & unique(team)!=""]
  ) %>%
  arrange(game_id) %>%
  as.data.frame()

games_info <- games_info %>%
  left_join(Poss, by=c("home_team"="team","game_id"="game_id")) %>%
  rename("home_poss"="nposs") %>%
  left_join(Poss, by=c("away_team"="team","game_id"="game_id")) %>%
  rename("away_poss"="nposs") %>%
  as.data.frame()    

games_info <- games_info %>%
  group_by(game_id) %>%
  mutate(pair = paste0(sort(c(home_team, away_team)), collapse=" - ") ) %>%
  group_by(pair) %>%
  mutate(round = ifelse(game_date==min(game_date), 1, 2) ) %>%
  ungroup() %>%
  select(-pair) %>%
  as.data.frame()

ranking <- games_info %>%
  mutate(winner=ifelse(home_score>away_score, home_team, away_team)) %>%
  group_by(winner) %>%
  summarize(pts=n()*2) %>%
  ungroup() %>%
  arrange(-pts)

source("additional_functions.r")
ARtg1 <- 100*AdjRtg(games_info %>% filter(round==1))
ARtg2 <- 100*AdjRtg(games_info %>% filter(round==2))

ARtg <- bind_cols(ARtg1, ARtg2) %>%
  rename(AORtg1=AORtg...1, ADRtg1=ADRtg...2, AORtg2=AORtg...3, ADRtg2=ADRtg...4) %>%
  mutate(team=rownames(ARtg1)) %>%
  left_join(ranking, by=c("team"="winner"))


# Converts numerical variables into factors, arranging the levels in numeric order.
create_labs <- function(x, add_str=NULL) {
  xchar <- sprintf("%.1f", x)
  x <- factor(xchar)
  idx <- order(as.numeric(levels(x)), decreasing=T)
  x <- factor(xchar, levels=levels(x)[idx], labels=paste0(levels(x)[idx],add_str))
  return(x)
}

ARtg_alluv <- ARtg %>%
  mutate(AORtg1 = create_labs(AORtg1),
         ADRtg1 = create_labs(ADRtg1),
         AORtg2 = create_labs(AORtg2, add_str=" "),
         ADRtg2 = create_labs(ADRtg2, add_str=" "),
         pts = factor(pts, levels=sort(unique(ARtg$pts), decreasing=T)),
         team = factor(team, levels=team[order(pts)]))

# Define flow colors
fill_cols <- colorRampPalette(c("red","blue"))(nlevels(ARtg_alluv$pts))

# Define the widths of the rectangles for each stratum
wdtsO <- with(ARtg_alluv, c(rep(1/3,nlevels(AORtg1)), rep(1/3,nlevels(AORtg2)), 
                            rep(1/6,nlevels(pts)), rep(6/5,nlevels(team))) )

wdtsD <- with(ARtg_alluv, c(rep(1/3,nlevels(ADRtg1)), rep(1/3,nlevels(ADRtg2)), 
                            rep(1/6,nlevels(pts)), rep(6/5,nlevels(team))) )

### Alluvial plot
pacman::p_load(ggalluvial)

AORtg12 <- ggplot(data = ARtg_alluv,
                  aes(axis1 = AORtg1, axis2 = AORtg2, axis3=pts, axis4=team)) +
  geom_alluvium(aes(fill = pts), show.legend=F) +
  geom_stratum(width=wdtsO) +
  geom_text(stat="stratum", aes(label=after_stat(stratum))) +
  scale_x_discrete(limits=c("100*AORtg1", "100*AORtg2", "Points", "Team"), expand=c(0.15, 0.05)) +
  scale_fill_manual(values = fill_cols) +
  theme_bw() %+replace% theme(panel.border=element_blank(), axis.text.x=element_text(size=12),
                              axis.text.y=element_blank(), axis.ticks.y=element_blank())
print(AORtg12)

ADRtg12 <- ggplot(data = ARtg_alluv,
                  aes(axis1 = ADRtg1, axis2 = ADRtg2, axis3=pts, axis4=team)) +
  geom_alluvium(aes(fill = pts), show.legend=F) +
  geom_stratum(width=wdtsD) +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits=c("100*ADRtg1", "100*ADRtg2", "Points", "Team"), expand=c(0.15, 0.05)) +
  scale_fill_manual(values = fill_cols) +
  theme_bw() %+replace% theme(panel.border=element_blank(), axis.text.x=element_text(size=12),
                              axis.text.y=element_blank(), axis.ticks.y=element_blank())

print(ADRtg12)

####################### save figures
dir.create("./Figures", showWarnings=FALSE)

pdf(file="./Figures/alluvialAORtg.pdf", width=10, height=6, paper="special")
print(AORtg12)
dev.off()

pdf(file="./Figures/alluvialADRtg.pdf", width=10, height=6, paper="special")
print(ADRtg12)
dev.off()
#################################################################

### CART
pacman::p_load(rpart,rattle)

treefit <- rpart(pts~AORtg1+AORtg2+ADRtg1+ADRtg2, data=ARtg, 
                 control = rpart.control(cp = 0.05, minbucket=3))
fancyRpartPlot(treefit,sub="")

####################### save figures
pdf(file="./Figures/tree_AORtg_ADRtg.pdf", width=6, height=6, paper="special")
fancyRpartPlot(treefit,sub="")
dev.off()
#################################################################

ARtg_alluv <- ARtg %>%
  mutate(AORtg1 = factor(ifelse(AORtg1 < -1.9, "<-1.9", ">=-1.9"), levels=c(">=-1.9","<-1.9")),
         ADRtg1 = factor(ifelse(ADRtg1 <  0.6, "<0.6", ">=0.6"), levels=c(">=0.6","<0.6")), 
         pts = factor(pts, levels=sort(unique(ARtg$pts), decreasing=T)),
         team = factor(team, levels=team[order(pts)])) 

wdtsOD <- with(ARtg_alluv, c(rep(1/3,nlevels(AORtg1)), rep(1/3,nlevels(ADRtg1)), 
                             rep(1/6,nlevels(pts)), rep(6/5,nlevels(team))) )

AODRtg1 <- ggplot(data = ARtg_alluv,
                  aes(axis1 = AORtg1, axis2 = ADRtg1, axis3=pts, axis4=team)) +
  geom_alluvium(aes(fill = pts), show.legend=F) +
  geom_stratum(width=wdtsOD) +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits=c("100*AORtg1", "100*ADRtg1", "Points", "Team"), expand=c(0.15, 0.05)) +
  scale_fill_manual(values = fill_cols) +
  theme_bw() %+replace% theme(panel.border=element_blank(), axis.text.x=element_text(size=12),
                              axis.text.y=element_blank(), axis.ticks.y=element_blank())

print(AODRtg1)

####################### save figures
pdf(file="./Figures/alluvialAODRtg.pdf", width=10, height=6, paper="special")
print(AODRtg1)
dev.off()
#################################################################

######################
# 4.2 Teams’ evolving performance from start to finish
######################

ming <- 50
nr <- nrow(games_info)
out <- lapply(ming:nr, 
              function(k, data) {
                datak <- data %>% slice(1:k)
                ARtgk <- AdjRtg(datak)
                ARtgk$time <- k
                ARtgk$team <- rownames(ARtgk)
                tmp <- datak %>% 
                  group_by(home_team) %>% 
                  summarize(pm=sum(home_score, na.rm=T)-sum(away_score, na.rm=T))
                ARtgk <- ARtgk %>% left_join(tmp, by=c("team"="home_team"))
                ARtgk <- ARtg %>% select(team, pts) %>% left_join(ARtgk, by="team")
                return(ARtgk)
              }, data=games_info)

labteams <- c("SAS","TOR","PES","TRN","MIL","BRE","NAP","SCA","BRI","TVB",
              "VAR","TRI","VER","VEN","REM","BOL")

out <- do.call("rbind", out) %>%
  mutate(team=factor(team), 
         team=factor(team,labels=labteams))

cls <- colorRampPalette(c("blue","white","red"))(5)
vals <- rescale(c(-90,-45,0,86,172))


p_static <- ggplot(data=out, aes(x=AORtg, y=ADRtg)) +
  geom_hline(yintercept=0,color="grey") + 
  geom_vline(xintercept=0,color="grey") +
  geom_point(aes(fill=pm, size=pts),shape=21,color="grey75") +
  geom_text(aes(label=team),vjust=-1.2) +
  scale_fill_gradientn(colors=cls,values=vals) +
  scale_size(breaks=seq(15,50,5), range=c(2,10), guide=guide_legend(
    override.aes =list(colour = "black",fill="black"))) +
  labs(title=paste0("Number of games = ","{closest_state}"),
       x="AORtg", y="ADRtg", fill="Plus\nMinus", size="Points") +
  theme_bw()

pacman::p_load(gganimate,gifski)

dir.create("./AnimatedPlots", showWarnings=FALSE)

p_anim <-  p_static + 
  transition_states(states=time, transition_length=1, state_length=1)

ntime <- length(unique(out$time))

anim_save("./AnimatedPlots/animated_bubbleplot.gif", animation=p_anim, fps=5, nframes=ntime, width=1000, height=1000, res=150, renderer = gifski_renderer())

pacman::p_load(av)
anim_save("./AnimatedPlots/animated_bubbleplot.mp4", animation=p_anim, fps=5, nframes=ntime, width=1000, height=1000, res=150, renderer = av_renderer())


####################### code for plotting Figure 4.4

single_frame <- ggplot(data=out %>% filter(time==100), aes(x=AORtg, y=ADRtg)) +
  geom_hline(yintercept=0,color="grey") + 
  geom_vline(xintercept=0,color="grey") +
  geom_point(aes(fill=pm, size=pts),shape=21,color="grey75") + 
  geom_text(aes(label=team),vjust=-1.2) +
  scale_fill_gradient2(low = "blue", mid="white", high = "red") +
  scale_size(breaks=seq(15,50,5), range=c(2,10), guide=guide_legend(
    override.aes =list(colour = "black",fill="black"))) +
  labs(x="AORtg", y="ADRtg", fill="Plus\nMinus", size="Points") +
  theme_bw()

plot(single_frame)

####################### save figures
pdf(file="./Figures/bubblesingleframe.pdf", width=8, height=6, paper="special")
print(single_frame)
dev.off()
#################################################################
