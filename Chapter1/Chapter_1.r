#####################################################################################
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

# set Chapter1 as working directory
# setwd("....")

############################################
# CHAPTER 1                                #
# Book description and data                #                            
############################################

######################
# 1.2 Datasets
######################

######################
# 1.2.1.1 NBA play-by-play data
######################
rm(list=ls())
graphics.off()

pacman::p_load(BasketballAnalyzeR)
load("./Data/NBAPbP_BDB.Rdata")
PbP.GSW.rs <- PbPmanipulation(PbP.BDB.GSW.rs)
PbP.GSW.po <- PbPmanipulation(PbP.BDB.GSW.po)
PbP.BOS.rs <- PbPmanipulation(PbP.BDB.BOS.rs)
PbP.BOS.po <- PbPmanipulation(PbP.BDB.BOS.po)


######################
# 1.2.2 Player tracking data
######################
rm(list=ls())
graphics.off()

load("./Data/tracking_data.Rdata")

tdata_long <- with(tdata_wide,data.frame(
  time=rep(time, 12),
  game=rep(game, 12),
  quarter=rep(quarter, 12),
  game_clock=rep(game_clock, 12),
  x = c(a1_x, a2_x, a3_x, a4_x, a5_x,
        h1_x, h2_x, h3_x, h4_x, h5_x,
        bcarr_x, b_x),
  y = c(a1_y, a2_y, a3_y, a4_y, a5_y,
        h1_y, h2_y, h3_y, h4_y, h5_y,
        bcarr_y, b_y),
  name = c(a1_name, a2_name, a3_name, a4_name, a5_name,
           h1_name, h2_name, h3_name, h4_name, h5_name,
           rep("Ballcarrier", nrow(tdata_wide)), rep("Ball", nrow(tdata_wide))),
  role  = c(a1_role, a2_role, a3_role, a4_role, a5_role,
            h1_role, h2_role, h3_role, h4_role, h5_role,
            bcarr_role, rep("Ball", nrow(tdata_wide))),
  event= c(a1_event, a2_event, a3_event, a4_event, a5_event,
           h1_event, h2_event, h3_event, h4_event, h5_event,
           rep(NA, 2*nrow(tdata_wide))),
  off_team = rep(off_team, 12),
  nposs = rep(nposs, 12),
  rcs = rep(rcs, 12),
  group1=rep(c("a1","a2","a3","a4","a5","h1","h2","h3","h4","h5","bcarr","ball"), each=nrow(tdata_wide)),
  group2=rep(c("away","away","away","away","away","home","home","home","home","home","bcarr","ball"), each=nrow(tdata_wide))
))


######################
# 1.3 Computing box scores from play-by-play data
######################

######################
# 1.3.1 NBA data
######################
rm(list=ls())
graphics.off()

## step 1
pacman::p_load(BasketballAnalyzeR)
load("./Data/NBAPbP_BDB.Rdata")
Team <- "GSW"
PbP.GSW.rs <- PbPmanipulation(PbP.BDB.GSW.rs)

## steps 2/3
pacman::p_load(operators,dplyr)
TOPPbP.GSW.rs <- PbP.GSW.rs %>%
  mutate(across(c(game_id, ShotType, result, team, assist, block,
                  steal, player, h1:h5, a1:a5, hometeam, type, event_type),
                as.character)) %>%
  mutate(oreb = type %~% "rebound offensive",
         dreb = type %~% "rebound defensive",
         turnover = event_type=="turnover",
         PF = (event_type == "foul") & !(type %~% "technical"))

## step 4
TOP <- TOPboxes(TOPPbP.GSW.rs, team=Team)
Tbox.GSW.rs <- TOP$Tbox
Obox.GSW.rs <- TOP$Obox
Pbox.GSW.rs <- TOP$Pbox

############## alternative way for steps 2/3
############## to avoid counting "team rebounds"

TOPPbP.GSW.rs <- PbP.GSW.rs %>%
  mutate(across(c(game_id, ShotType, result, team, assist, block,
                  steal, player, h1:h5, a1:a5, hometeam, type, event_type),
                as.character)) %>%
  mutate(oreb = type %~% "rebound offensive" & !(player==""),
         dreb = type %~% "rebound defensive" & !(player==""),
         turnover = event_type=="turnover",
         PF = (event_type == "foul") & !(type %~% "technical"))

######################
# 1.3.2 Italian National League data
######################
rm(list=ls())
graphics.off()

## step 1
pacman::p_load(BasketballAnalyzeR,operators,readr,lubridate,dplyr)
load("./Data/LBAPbP.Rdata")
Team <- "EA7 Emporio Armani Milano"

## steps 2/3
TOPPbP <- PbP %>%
  mutate(across(c(game_id,ShotType,result,team,assist,block,
                  steal,player,h1:h5,a1:a5,hometeam,type,event_type),
                as.character)) %>%
  mutate(oreb = type=="rebound offensive" | 
           (type=="team rebound" & description=="Rimbalzi offensivi di squadra"),
         dreb = type=="rebound defensive" |
           (type=="team rebound" & description=="Rimbalzi difensivi di squadra"),
         turnover = event_type=="turnover",
         PF = (type %~% "foul") & !(type=="t.foul def. 3 sec") & 
           !(type=="offensive foul turnover") &
           !(type=="foul") & !(type=="t.foul delay of game")) 


## filter analysed team data
TOPPbP.EA7 <- TOPPbP %>%
  mutate(filt=(team==Team)) %>% 
  filter(game_id %in% unique(game_id[filt])) %>%
  select(-filt)


## step 4
TOP <- TOPboxes(TOPPbP.EA7, team=Team)
Tbox.EA7 <- TOP$Tbox
Obox.EA7 <- TOP$Obox
Pbox.EA7 <- TOP$Pbox

############## alternative way for steps 2/3
############## to avoid counting "team rebounds"
TOPPbP <- PbP %>%
  mutate(across(c(game_id, ShotType, result, team, assist, block,
                  steal, player, h1:h5, a1:a5, hometeam, type, event_type),
                as.character)) %>%
  mutate(oreb = type=="rebound offensive" | 
           (type=="team rebound" & description=="Rimbalzi offensivi di squadra") & !(player==""),
         dreb = type=="rebound defensive" |
           (type=="team rebound" & description=="Rimbalzi difensivi di squadra") & !(player==""),
         turnover = event_type=="turnover") 
