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

# set Chapter3 as working directory
# setwd("....")

############################################
############################################
# PART 2                                   #
# Analyzing and comparing game splits      #                            
############################################
############################################

##############################################
# CHAPTER 3                                  #
# Drilling down on clutch splits:            #
# measuring performance when it matters most #                            
##############################################

######################
# 3.1 Boston Celtics in the clutch
######################
rm(list=ls())
graphics.off()

pacman::p_load(BasketballAnalyzeR,operators,gridExtra,dplyr)

totalTime_c <- 2580  # last 5 minutes and overtime
scorediff_c <- 5
AS_min <- 20
Team <- "BOS"

load("./Data/NBAPbP_BDB.Rdata")
PbP.BOS.rs <- PbPmanipulation(PbP.BDB.BOS.rs)
PbP.BOS.po <- PbPmanipulation(PbP.BDB.BOS.po)
PbP.BOS <- rbind(PbP.BOS.rs,PbP.BOS.po)



##############################################################
# generate dataset as shown in section 2.1.1
# to use TOPboxes

TOPPbP.BOS <- PbP.BOS %>%
  mutate(across(c(game_id, ShotType, result, team, assist, block,
                  steal, player, h1:h5, a1:a5, hometeam, type, event_type),
                as.character)) %>%
  mutate(oreb = type %~% "rebound offensive",
         dreb = type %~% "rebound defensive",
         turnover = event_type=="turnover",
         PF = (event_type == "foul") & !(type %~% "technical"))

##############################################################


##### create variable scorediff and filter clutch situation 
TOPPbP.BOSc <- TOPPbP.BOS %>%
  mutate(scorediff=abs(home_score-away_score)) %>%
  filter(scorediff <= scorediff_c & totalTime > totalTime_c)

TOP.BOS <- TOPboxes(TOPPbP.BOS, team=Team)
Tbox.BOS <- TOP.BOS$Tbox
Obox.BOS <- TOP.BOS$Obox
Pbox.BOS <- TOP.BOS$Pbox

TOP.BOSc <- TOPboxes(TOPPbP.BOSc, team=Team)
Tbox.BOSc <- TOP.BOSc$Tbox
Obox.BOSc <- TOP.BOSc$Obox
Pbox.BOSc <- TOP.BOSc$Pbox

Tboxes.BOS <- data.frame(Team=c("Clutch","General"),rbind(Tbox.BOSc,Tbox.BOS))
Oboxes.BOS <- data.frame(Team=c("Clutch","General"),rbind(Obox.BOSc,Obox.BOS))

##### Pace, Offensive/Defensive Ratings and Four Factors

FF <- fourfactors(Tboxes.BOS,Oboxes.BOS)
listPlots <- plot(FF)
listPlots$FFOplot <- listPlots$FFOplot + labs(fill="Split")
listPlots$FFDplot <- listPlots$FFDplot + labs(fill="Split")
grid.arrange(grobs=listPlots, nrow=2)


####################### save figures
dir.create("./Figures", showWarnings=FALSE)

pdf(file="./Figures/FF1_c.pdf", width=5, height=8, paper="special")
gridExtra::grid.arrange(grobs=listPlots[1:2], nrow=2)
dev.off()
pdf(file="./Figures/FF2_c.pdf", width=7, height=9, paper="special")
gridExtra::grid.arrange(grobs=listPlots[3:4], nrow=2)
dev.off()
#################################################################

ineqc <- inequality(Pbox.BOSc$PTS, nplayers=10)
ineq <- inequality(Pbox.BOS$PTS, nplayers=10)
p1 <- plot(ineqc, title="Clutch")
p2 <- plot(ineq, title="General")
grid.arrange(p1, p2, nrow=1)

####################### save figures
pdf(file="./Figures/inequality_c.pdf", width=10, height=6, paper="special")
grid.arrange(p1, p2, nrow=1)
dev.off()
#################################################################

# impute NaN with 0
vars <- c("P2p","P3p","FTp")
Pbox.BOSc[vars] <- lapply(Pbox.BOSc[vars], function(x) replace(x, is.na(x), 0))
Pbox.BOS[vars] <- lapply(Pbox.BOS[vars], function(x) replace(x, is.na(x), 0))

##### Players' shooting performance (ZukSanMan, 2017)
##### team scoring percentages
P2p.BOS <- with(Tbox.BOS, 100*P2M/P2A)
P3p.BOS <- with(Tbox.BOS, 100*P3M/P3A)
FTp.BOS <- with(Tbox.BOS, 100*FTM/FTA)

# create variable SP and filter players with at least AS_min attempted shots
Pbox.BOSc <- Pbox.BOSc %>%
  mutate(AS=(P2A+P3A+FTA)) %>%
  mutate(SP=((P2p-P2p.BOS)*P2A + (P3p-P3p.BOS)*P3A + (FTp-FTp.BOS)*FTA)/AS) %>%
  filter(AS >= AS_min)


Pbox.BOS <- Pbox.BOS %>%
  mutate(AS=(P2A+P3A+FTA)) %>%
  mutate(SP=((P2p-P2p.BOS)*P2A + (P3p-P3p.BOS)*P3A + (FTp-FTp.BOS)*FTA)/AS) 


plyrs <- Pbox.BOSc$player

cindex <- bind_cols(player=plyrs,
                    Pbox.BOSc %>% filter(player %in% plyrs) %>% select(MIN, AS, SP) %>% rename_all(~paste0(., "c")),
                    Pbox.BOS %>% filter(player %in% plyrs) %>% select(MIN, AS, SP))


##### HP performance indicators (ZukSanMan, 2017)
cindex <- cindex %>%
  mutate(Diff = SPc-SP) %>%
  mutate(Prc = 100*((ASc/AS)/(MINc/MIN)-1)) %>%
  mutate(MINcp = 100*MINc/Tbox.BOSc$MIN)


X <- cindex %>% select(player, MINcp, SPc, Diff, Prc)

labs <- c("Clutch shooting performance (SPc)", "Difference SPc-SP",
          "Propensity to shoot",  "Clutch minutes played (%)")
p  <- bubbleplot(X, id="player", x="SPc", y="Diff", col="Prc", mx=0,my=0,mcol=0,
                 size="MINcp", scale.size = FALSE, text.size = 4.5,labels=labs) +
  scale_size(limits=c(43,86), range=c(2,10), guide=guide_legend(override.aes = list(colour = "black", fill="black"))) +
  theme_minimal()
print(p)


####################### save figures
pdf(file="./Figures/bubbleplot_c.pdf", width=8, height=6, paper="special")
print(p)+  theme_minimal() %+replace% theme(text=element_text(size=20))
dev.off()
#################################################################

######################
# 3.2 Golden State Warriors in the clutch
######################

Team <- "GSW"

PbP.GSW.rs <- PbPmanipulation(PbP.BDB.GSW.rs)
PbP.GSW.po <- PbPmanipulation(PbP.BDB.GSW.po)
PbP.GSW <- rbind(PbP.GSW.rs,PbP.GSW.po)



##############################################################
# generate dataset as shown in section 2.1.1
# to use TOPboxes

TOPPbP.GSW <- PbP.GSW %>%
  mutate(across(c(game_id, ShotType, result, team, assist, block,
                  steal, player, h1:h5, a1:a5, hometeam, type, event_type),
                as.character)) %>%
  mutate(oreb = type %~% "rebound offensive",
         dreb = type %~% "rebound defensive",
         turnover = event_type=="turnover",
         PF = (event_type == "foul") & !(type %~% "technical"))

##############################################################


##### create variable scorediff and filter clutch situation 
TOPPbP.GSWc <- TOPPbP.GSW %>%
  mutate(scorediff=abs(home_score-away_score)) %>%
  filter(scorediff <= scorediff_c & totalTime > totalTime_c)

TOP.GSW <- TOPboxes(TOPPbP.GSW, team=Team)
Tbox.GSW <- TOP.GSW$Tbox
Obox.GSW <- TOP.GSW$Obox
Pbox.GSW <- TOP.GSW$Pbox

TOP.GSWc <- TOPboxes(TOPPbP.GSWc, team=Team)
Tbox.GSWc <- TOP.GSWc$Tbox
Obox.GSWc <- TOP.GSWc$Obox
Pbox.GSWc <- TOP.GSWc$Pbox

Tboxes.GSW <- data.frame(Team=c("Clutch","General"),rbind(Tbox.GSWc,Tbox.GSW))
Oboxes.GSW <- data.frame(Team=c("Clutch","General"),rbind(Obox.GSWc,Obox.GSW))

##### Pace, Offensive/Defensive Ratings and Four Factors

FF <- fourfactors(Tboxes.GSW,Oboxes.GSW)
listPlots <- plot(FF)
listPlots$FFOplot <- listPlots$FFOplot + labs(fill="Split")
listPlots$FFDplot <- listPlots$FFDplot + labs(fill="Split")
grid.arrange(grobs=listPlots, nrow=2)


####################### save figures
pdf(file="./Figures/FF1_c1.pdf", width=5, height=8, paper="special")
gridExtra::grid.arrange(grobs=listPlots[1:2], nrow=2)
dev.off()
pdf(file="./Figures/FF2_c1.pdf", width=7, height=9, paper="special")
gridExtra::grid.arrange(grobs=listPlots[3:4], nrow=2)
dev.off()
#################################################################

ineqc <- inequality(Pbox.GSWc$PTS, nplayers=10)
ineq <- inequality(Pbox.GSW$PTS, nplayers=10)
p1 <- plot(ineqc, title="Clutch")
p2 <- plot(ineq, title="General")
grid.arrange(p1, p2, nrow=1)

####################### save figures
pdf(file="./Figures/inequality_c1.pdf", width=10, height=6, paper="special")
grid.arrange(p1, p2, nrow=1)
dev.off()
#################################################################

# impute NaN with 0
vars <- c("P2p","P3p","FTp")
Pbox.GSWc[vars] <- lapply(Pbox.GSWc[vars], function(x) replace(x, is.na(x), 0))
Pbox.GSW[vars] <- lapply(Pbox.GSW[vars], function(x) replace(x, is.na(x), 0))

##### Players' shooting performance (ZukSanMan, 2017)
##### team scoring percentages
P2p.GSW <- with(Tbox.GSW, 100*P2M/P2A)
P3p.GSW <- with(Tbox.GSW, 100*P3M/P3A)
FTp.GSW <- with(Tbox.GSW, 100*FTM/FTA)

# create variable SP and filter players with at least AS_min attempted shots
Pbox.GSWc <- Pbox.GSWc %>%
  mutate(AS=(P2A+P3A+FTA)) %>%
  mutate(SP=((P2p-P2p.GSW)*P2A + (P3p-P3p.GSW)*P3A + (FTp-FTp.GSW)*FTA)/AS) %>%
  filter(AS >= AS_min)


Pbox.GSW <- Pbox.GSW %>%
  mutate(AS=(P2A+P3A+FTA)) %>%
  mutate(SP=((P2p-P2p.GSW)*P2A + (P3p-P3p.GSW)*P3A + (FTp-FTp.GSW)*FTA)/AS) 


plyrs <- Pbox.GSWc$player

cindex <- bind_cols(player=plyrs,
                    Pbox.GSWc %>% filter(player %in% plyrs) %>% select(MIN, AS, SP) %>% rename_all(~paste0(., "c")),
                    Pbox.GSW %>% filter(player %in% plyrs) %>% select(MIN, AS, SP))


##### HP performance indicators (ZukSanMan, 2017)
cindex <- cindex %>%
  mutate(Diff = SPc-SP) %>%
  mutate(Prc = 100*((ASc/AS)/(MINc/MIN)-1)) %>%
  mutate(MINcp = 100*MINc/Tbox.GSWc$MIN)


X <- cindex %>% select(player, MINcp, SPc, Diff, Prc)

labs <- c("Clutch shooting performance (SPc)", "Difference SPc-SP",
          "Propensity to shoot",  "Clutch minutes played (%)")
p  <- bubbleplot(X, id="player", x="SPc", y="Diff", col="Prc", mx=0,my=0,mcol=0,
                 size="MINcp", scale.size = FALSE, text.size = 4.5,labels=labs) +
  scale_size(limits=c(43,86), range=c(2,10), guide=guide_legend(override.aes = list(colour = "black", fill="black"))) +
  theme_minimal()
print(p)


####################### save figures
pdf(file="./Figures/bubbleplot_c1.pdf", width=8, height=6, paper="special")
print(p)+  theme_minimal() %+replace% theme(text=element_text(size=20))
dev.off()
#################################################################




