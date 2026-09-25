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

# set Chapter2 as working directory
# setwd("....")


############################################
############################################
# PART 1                                   #
# Analyzing and comparing game splits      #                            
############################################
############################################

############################################
# CHAPTER 2                                #
# Beyond individual skills                 #                            
############################################


######################
# 2.1 Lineups analysis
######################
rm(list=ls())
graphics.off()

pacman::p_load(BasketballAnalyzeR,operators,pbapply,combinat,gridExtra,dplyr)
load("./Data/NBAPbP_BDB.Rdata")
PbP.GSW.rs <- PbPmanipulation(PbP.BDB.GSW.rs)

nplayers <- 5
name_cols <- paste0(c("a","h"), rep(1:5,each=2))
teamplayers <- c("Andrew Wiggins","Draymond Green","Gary Payton II",
                 "Jonathan Kuminga","Jordan Poole","Kevon Looney",
                 "Klay Thompson","Otto Porter Jr.","Moses Moody",
                 "Stephen Curry")

Team <- "GSW"
min_time_l <- 48

# number of k-players combinations
cat("No. of combinations =", nCm(length(teamplayers), nplayers), "\n")

# Table of all possible lineups
source("additional_functions.r")
ktuples <- lineups(PbP.GSW.rs, teamplayers, k=nplayers)

# Table of all possible lineups (function with parallel computing features)
pacman::p_load(parallel)
ktuples <- parlineups(PbP.GSW.rs, teamplayers, k=nplayers)

lineup_set <- ktuples %>% filter(MIN>=min_time_l)

################
# Preparing data for subsequent analyses
# impute NA in shot_distance

PbP.GSW.rs <- PbP.GSW.rs %>%
  mutate(
    shot_distance = ifelse(
      event_type=='shot' & is.na(shot_distance) & ShotType=="2P",
      mean(shot_distance[event_type=='shot' & ShotType=="2P"], na.rm=TRUE),
      shot_distance
    )
  )

PbP.GSW.rs <- PbP.GSW.rs %>%
  mutate(
    shot_distance = ifelse(
      event_type=='shot' & is.na(shot_distance) & ShotType=="3P",
      mean(shot_distance[event_type=='shot' & ShotType=="3P"], na.rm = TRUE),
      shot_distance
    )
  )

##############################################################
# generate dataset of section 2.1.1
# to use TOPboxes

TOPPbP.GSW.rs <- PbP.GSW.rs %>%
  mutate(across(c(game_id, ShotType, result, team, assist, block,
                  steal, player, h1:h5, a1:a5, hometeam, type, event_type),
                as.character)) %>%
  mutate(oreb = type %~% "rebound offensive",
         dreb = type %~% "rebound defensive",
         turnover = event_type=="turnover",
         PF = (event_type == "foul") & !(type %~% "technical"))

##############################################################

pacman::p_load(network,ggnetwork)
nlineups <- nrow(lineup_set)
out_list <- vector(nlineups, mode="list")

for (k in 1:nlineups) {
  lineupk <- lineup_set[k, ]
  filtk <- apply(TOPPbP.GSW.rs[, name_cols], 1, function(x) {
    all(lineupk[1:5] %in% x) 
  })
  subdatak <- TOPPbP.GSW.rs %>% filter(filtk) %>% as.data.frame()
  subdatak.GSW <- subdatak %>% filter(team==Team)
  
  
  ##### assistnet
  
  out <- assistnet(subdatak.GSW)
  p1 <- plot(out, layout="circle", edge.thr=0,
             node.col="ASTPTS", node.size="FGPTS_ASTp")
  
  
  ##### shotchart
  
  subdatak.GSW$xx <- subdatak.GSW$original_x/10
  subdatak.GSW$yy <- subdatak.GSW$original_y/10-41.75
  subdatak.GSW$result <- as.factor(subdatak.GSW$result)  
  p2 <- shotchart(data=subdatak.GSW, x="xx", y="yy", type="density-polygons") + 
    labs(title=lineupk$label) +
    coord_fixed(xlim = c(-22.75, 22.75), ylim = c(-44.8, 0) ) + 
    theme(plot.margin = margin(13.5, 0, 0, 0))
  p3 <- shotchart(data=subdatak.GSW, x="xx", y="yy", z="result", type=NULL, scatter=TRUE) + 
    labs(title=lineupk$label)
  
  
  ##### densityplot
  
  p4 <- densityplot(data=subdatak.GSW, var="shot_distance", best.scorer=TRUE) + 
    labs(title=lineupk$label)
  
  
  ##### Tbox, Obox, Pbox
  
  TOP <- TOPboxes(subdatak, Team)
  out_list[[k]] <- list("assistnet"=p1, "shotchart1"=p2, "shotchart2"=p3, "densitylot"=p4, "TOP"=TOP)
}


##### assist-shot networks
assistnet_plots <- lapply(out_list, "[[", 1)
grid.arrange(grobs=assistnet_plots[1:4], nrow=2)
grid.arrange(grobs=assistnet_plots[5:8], nrow=2)


##### shot charts with densities
shotchart1_plots <- lapply(out_list, "[[", 2)
grid.arrange(grobs=shotchart1_plots, nrow=3)


##### shot charts with scatters
shotchart2_plots <- lapply(out_list, "[[", 3)
grid.arrange(grobs=shotchart2_plots, nrow=3)


##### density plots
density_plots <- lapply(out_list, "[[", 4)
grid.arrange(grobs=density_plots[1:4], nrow=2)
grid.arrange(grobs=density_plots[5:8], nrow=2)


####################### save figures
dir.create("./Figures", showWarnings=FALSE)

# Save assistnets
assisnet_plots <- lapply(out_list, "[[", 1)
pdf(file=paste("./Figures/assistnet_plots_team",Team,"1.pdf",sep=""),width=7,height=12,paper="special")
gridExtra::grid.arrange(grobs=assistnet_plots[1:2], nrow=2)
dev.off()
pdf(file=paste("./Figures/assistnet_plots_team",Team,"2.pdf",sep=""),width=7,height=12,paper="special")
gridExtra::grid.arrange(grobs=assistnet_plots[3:4], nrow=2)
dev.off()
pdf(file=paste("./Figures/assistnet_plots_team",Team,"3.pdf",sep=""),width=7,height=12,paper="special")
gridExtra::grid.arrange(grobs=assistnet_plots[5:6], nrow=2)
dev.off()
pdf(file=paste("./Figures/assistnet_plots_team",Team,"4.pdf",sep=""),width=7,height=12,paper="special")
gridExtra::grid.arrange(grobs=assistnet_plots[7:8], nrow=2)
dev.off()

# Save shotcharts with densities
shotchart1_plots <- lapply(out_list, "[[", 2)
pdf(file=paste("./Figures/shotcharts_with_density_team",Team,"1.pdf",sep=""),width=8,height=8,paper="special")
gridExtra::grid.arrange(grobs=shotchart1_plots[1:4], nrow=2)
dev.off()
pdf(file=paste("./Figures/shotcharts_with_density_team",Team,"2.pdf",sep=""),width=8,height=8,paper="special")
gridExtra::grid.arrange(grobs=shotchart1_plots[5:8], nrow=2)
dev.off()

# Save shotcharts with scatters
shotchart2_plots <- lapply(out_list, "[[", 3)
pdf(file=paste("./Figures/shotcharts_with_scatter_team",Team,"1.pdf",sep=""),width=10,height=8,paper="special")
gridExtra::grid.arrange(grobs=shotchart2_plots[1:4], nrow=2)
dev.off()
pdf(file=paste("./Figures/shotcharts_with_scatter_team",Team,"2.pdf",sep=""),width=10,height=8,paper="special")
gridExtra::grid.arrange(grobs=shotchart2_plots[5:8], nrow=2)
dev.off()

# Save density plots
density_plots <- lapply(out_list, "[[", 4)
pdf(file=paste("./Figures/density_plots_team",Team,"1.pdf",sep=""),width=7,height=12,paper="special")
gridExtra::grid.arrange(grobs=density_plots[1:2], nrow=2)
dev.off()
pdf(file=paste("./Figures/density_plots_team",Team,"2.pdf",sep=""),width=7,height=12,paper="special")
gridExtra::grid.arrange(grobs=density_plots[3:4], nrow=2)
dev.off()
pdf(file=paste("./Figures/density_plots_team",Team,"3.pdf",sep=""),width=7,height=12,paper="special")
gridExtra::grid.arrange(grobs=density_plots[5:6], nrow=2)
dev.off()
pdf(file=paste("./Figures/density_plots_team",Team,"4.pdf",sep=""),width=7,height=12,paper="special")
gridExtra::grid.arrange(grobs=density_plots[7:8], nrow=2)
dev.off()
#################################################################


# TOP boxes
TOP_list <- lapply(out_list, "[[", 5)

Tboxes <- lapply(TOP_list, "[[", 1)
LINTbox <- data.frame(Team=lineup_set$label, do.call(rbind, Tboxes))
Oboxes <- lapply(TOP_list, "[[", 2)
LINObox <- data.frame(Team=lineup_set$label, do.call(rbind, Oboxes))
Pboxes <- lapply(TOP_list, "[[", 3)
LINPbox <- data.frame(idlineup=rep(1:nlineups,each=5),Team=rep(lineup_set$label, each=nplayers), do.call(rbind, Pboxes))

##### Pace, Offensive/Defensive Ratings and Four Factors
FF <- fourfactors(LINTbox,LINObox)
plot(FF)

####################### save figures
listPlots <- plot(FF)
pdf(file="./Figures/FF1_GSW.pdf",width=5,height=8,paper="special")
gridExtra::grid.arrange(grobs=listPlots[1:2], nrow=2)
dev.off()
pdf(file="./Figures/FF2_GSW.pdf",width=7,height=9,paper="special")
gridExtra::grid.arrange(grobs=listPlots[3:4], nrow=2)
dev.off()
#################################################################


##### radial plots
X <- LINTbox %>%
  mutate(REB=OREB+DREB) %>%
  select(P2M, P3M, FTM, REB, AST, STL, BLK, MIN) %>%
  mutate_all(~./MIN) %>% select(-MIN)

radialprofile(data=X, title=LINTbox$Team, std=TRUE) 
radialprofile(data=X, title=LINTbox$Team, std=FALSE)


####################### save figures
pdf(file="./Figures/radial_std.pdf",width=13,height=13,paper="special")
radialprofile(data=X, title=LINTbox$Team, std=TRUE, label.size=4)
dev.off()

pdf(file="./Figures/radial_nostd.pdf",width=13,height=13,paper="special")
radialprofile(data=X, title=LINTbox$Team, std=FALSE, label.size=4)
dev.off()
#################################################################


##### bubble plot
X <- LINTbox %>%
  mutate(PMp=PM/PTS, Lineup=Team) %>%
  select(Lineup, P2p, P3p, FTp, PMp, MIN)

labs <- c("2-point shots (% made)", "3-point shots (% made)",
          "Plus-Minus/Scored points",  "Minutes played",
          "free throws (% made)")
p  <- bubbleplot(X, id="Lineup", x="P2p", y="P3p", col="PMp", mcol=0,text.col="FTp",
                 size="MIN", scale.size = FALSE, text.size = 4.5) + 
  labs(x = labs[1], y = labs[2], fill = labs[3], size = labs[4], color = labs[5]) +
  theme_minimal()
print(p)


####################### save figures
pdf(file="./Figures/bubbleplot.pdf",width=8,height=6,paper="special")
print(p)+  theme_minimal() %+replace% theme(text=element_text(size=20))
dev.off()
#################################################################


######################
# 2.1.1 Impact of the lineup on a single player
######################

plyr <- "Stephen Curry"
LINPbox.plyr <- LINPbox %>% filter(player==!!plyr)

##### radial plots

X <- LINPbox.plyr %>%
  mutate(REB=OREB+DREB) %>%
  select(P2M, P3M, FTM, REB, AST, STL, BLK, MIN) %>%
  mutate_all(~./MIN) %>% select(-MIN)

radialprofile(data=X, title=LINPbox.plyr$Team, std=TRUE)


####################### save figures
pdf(file=paste0("./Figures/radial_std_", plyr,".pdf"),width=13,height=13,paper="special")
radialprofile(data=X, title=LINPbox.plyr$Team, std=TRUE, label.size=4)
dev.off()
#################################################################

##### bubble plot

X <- LINPbox.plyr %>% 
  mutate(PMp=PM/PTS, Lineup=Team) %>%
  select(Lineup, P2p, P3p, FTp, PMp, MIN)

labs <- c("2-point shots (% made)", "3-point shots (% made)",
          "Plus-Minus/Scored points", "Minutes played",
          "free throws (% made)")
p <- bubbleplot(X, id="Lineup", x="P2p", y="P3p", col="PMp", mcol=0, text.col="FTp",
                size="MIN", scale.size=FALSE, text.size=4.5) + 
  labs(x = labs[1], y = labs[2], fill = labs[3], size = labs[4], color = labs[5])+
  theme_minimal()

print(p)


####################### save figures
pdf(file=paste0("./Figures/bubbleplot_", plyr,".pdf"), width=8, height=6, paper="special")
print(p)+  theme_minimal() %+replace% theme(text=element_text(size=20))

dev.off()
#################################################################


######################
# 2.2 Splash Brothers
######################
nplayers <- 2
min_time_c <- 400

cat("No. of combinations =", nCm(length(teamplayers), nplayers), "\n")
ktuples <- lineups(PbP.GSW.rs, teamplayers, k=nplayers) %>%
  arrange(-MIN)

couple_set <- ktuples %>%  filter(MIN>=min_time_c)

couplek <- c("Klay Thompson", "Stephen Curry")

filts <- apply(PbP.GSW.rs[, name_cols], 1, function(x) {
  data.frame(
    filt1=all(couplek %in% x),
    filt2=(couplek[1] %in% x) & !(couplek[2] %in% x),
    filt3=!(couplek[1] %in% x) & (couplek[2] %in% x),
    filt4=!(couplek[1] %in% x) & !(couplek[2] %in% x)
  )
})
filts <- do.call(rbind, filts)

# both on the court (KT+SC)
subdata12 <- TOPPbP.GSW.rs %>%  filter(filts[,1]) %>% as.data.frame()
subdata12.GSW <- PbP.GSW.rs %>%  filter(filts[,1] & team==!!Team) %>% as.data.frame()

# only Klay Thompson on the court (KT)
subdata1  <- TOPPbP.GSW.rs %>%  filter(filts[,2]) %>% as.data.frame()
subdata1.GSW  <- PbP.GSW.rs %>%  filter(filts[,2] & team==!!Team) %>% as.data.frame()

# only Stephen Curry on the court (SC)
subdata2  <- TOPPbP.GSW.rs %>%  filter(filts[,3]) %>% as.data.frame()
subdata2.GSW  <- PbP.GSW.rs %>%  filter(filts[,3] & team==!!Team) %>% as.data.frame()

# neither one nor the other on the court (none)
subdata00 <- TOPPbP.GSW.rs %>%  filter(filts[,4]) %>% as.data.frame()
subdata00.GSW <- PbP.GSW.rs %>%  filter(filts[,4] & team==!!Team) %>% as.data.frame()


##### assistnet

out12 <- assistnet(subdata12.GSW, normalize=T, time.thr=100)
p12 <- plot(out12, layout="circle",node.col="ASTPTS",
            node.size="FGPTS_ASTp",edge.col.lab="Number of assists per 48 minutes")

out1 <- assistnet(subdata1.GSW, normalize=T, time.thr=100)
p1 <- plot(out1, layout="circle", node.col="ASTPTS",
           node.size="FGPTS_ASTp",edge.col.lab="Number of assists per 48 minutes")

out2 <- assistnet(subdata2.GSW, normalize=T, time.thr=100)
p2 <- plot(out2, layout="circle", node.col="ASTPTS",
           node.size="FGPTS_ASTp",edge.col.lab="Number of assists per 48 minutes")

grid.arrange(p12, p1, p2)

####################### save figures
pdf(file=paste0("./Figures/assistnet_couple_", paste0(gsub(" ", "", couplek), collapse="_"),".pdf"), 
    width=9, height=21, paper="special")
gridExtra::grid.arrange(p12, p1, p2)
dev.off()
#################################################################

##### Expected Points
p12 <- expectedpts(data=subdata12.GSW, x.range = c(0,30))$data %>% mutate(oncourt="KT+SC")
p1 <- expectedpts(data=subdata1.GSW, x.range = c(0,30))$data %>% mutate(oncourt="KT")
p2 <- expectedpts(data=subdata2.GSW, x.range = c(0,30))$data %>% mutate(oncourt="SC")
p00 <- expectedpts(data=subdata00.GSW, x.range = c(0,30))$data %>% mutate(oncourt="none")

exppts <- do.call(rbind,list(p12,p1,p2,p00))
exppts <- exppts %>% mutate(oncourt=factor(oncourt,levels=c("KT+SC","KT","SC","none")))

p <- ggplot(data=exppts, aes(x=x, y=y, color=oncourt, linewidth=oncourt, group=oncourt))+
  geom_line() +
  scale_linewidth_manual(values = c(2, 0.5, 0.5, 2)) +
  guides(linewidth = guide_legend(override.aes = list(size = 3))) +
  labs(x="Shot distance",y="Expected Points",color="On court",linewidth="On court") +
  theme_bw()

print(p)

####################### save figures
pdf(file=paste0("./Figures/expectedpts_couple_", paste0(gsub(" ", "", couplek), collapse="_"),".pdf"), 
    width=6, height=6, paper="special")
print(p)+  theme_bw() %+replace% theme(text=element_text(size=20))
dev.off()
#################################################################

##### Scoring Probability
p12 <- scoringprob(data=subdata12.GSW, var="shot_distance",shot.type='field',x.range = c(0,30))$data %>% mutate(oncourt="KT+SC")
p1 <- scoringprob(data=subdata1.GSW, var="shot_distance",shot.type='field',x.range = c(0,30))$data %>% mutate(oncourt="KT")
p2 <- scoringprob(data=subdata2.GSW, var="shot_distance",shot.type='field',x.range = c(0,30))$data %>% mutate(oncourt="SC")
p00 <- scoringprob(data=subdata00.GSW, var="shot_distance",shot.type='field',x.range = c(0,30))$data %>% mutate(oncourt="none")

scorprob <- do.call(rbind,list(p12,p1,p2,p00))
scorprob <- scorprob %>% mutate(oncourt=factor(oncourt,levels=c("KT+SC","KT","SC","none")))

p <- ggplot(data=scorprob , aes(x=x, y=y, color=oncourt, linewidth=oncourt, group=oncourt))+
  geom_line() +
  scale_linewidth_manual(values = c(2, 0.5, 0.5, 2)) +
  guides(linewidth = guide_legend(override.aes = list(size = 3))) +
  labs(x="Shot distance",y="Scoring Probability",color="On court",linewidth="On court") +
  theme_bw()

print(p)

####################### save figures
pdf(file=paste0("./Figures/scoringprob_couple_", paste0(gsub(" ", "", couplek), collapse="_"),".pdf"), 
    width=6, height=6, paper="special")
print(p)+  theme_bw() %+replace% theme(text=element_text(size=20))
dev.off()
#################################################################



##### Pace, Offensive/Defensive Ratings and Four Factors

TOPcouple <- lapply(list(subdata12, subdata1, subdata2, subdata00), 
                    function(dts) TOPboxes(dts, Team))
CTbox <- lapply(TOPcouple, "[[", 1)
CTbox <- data.frame(Team=c("KT+SC","KT","SC","none"),do.call(rbind, CTbox))

CObox <- lapply(TOPcouple, "[[", 2)
CObox <- data.frame(Team=c("KT+SC","KT","SC","none"),do.call(rbind, CObox))

FF <- fourfactors(CTbox, CObox)

plot(FF)

####################### save figures
listPlots <- plot(FF)
pdf(file="./Figures/FF1_GSW_KTSC.pdf", width=5, height=8, paper="special")
gridExtra::grid.arrange(grobs=listPlots[1:2], nrow=2)
dev.off()
pdf(file="./Figures/FF2_GSW_KTSC.pdf", width=7, height=9, paper="special")
gridExtra::grid.arrange(grobs=listPlots[3:4], nrow=2)
dev.off()
#################################################################




