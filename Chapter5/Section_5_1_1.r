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

#####################################################################################

# The package {\tt pacman} has to be installed
# install.packages("pacman")

# set Chapter5 as working directory
# setwd("....")


############################################
############################################
# PART 2                                   #
# Decoding motion                          #                            
############################################
############################################

##############################################
# CHAPTER 5                                  #
# Understanding players' spatial dynamics    #
##############################################

######################
# 5.1 Animated plots of the players' movements on the court
######################

######################
# 5.1.1 Animated plots with Voronoi tessellation
######################
rm(list=ls())
graphics.off()

pacman::p_load(deldir, transformr, sf)

load("./Data/Section_5_1.Rdata")
pacman::p_load(ggplot2, gganimate, gifski, av, dplyr)
source("additional_functions.r")

dts_vor <- lapply(tm, function(t) {
  dtsk <- dts %>% filter(time==t & name!="Ballcarrier" & name!="Ball")
  vor_tessk <- deldir(x=dtsk$x, y=dtsk$y, rw=c(-47,47,-25,25))
  vor_tilesk <- tile.list(vor_tessk) 
  dfk <- do.call(rbind, lapply(seq_along(vor_tilesk), function(i) {
    tile <- vor_tilesk[[i]]
    data.frame(x=c(tile$x, tile$x[1]), y=c(tile$y, tile$y[1]), group2=dtsk$group2[i], id=i)
  }))
  dfk$time <- t
  return(dfk)
})	
dts_vor <- do.call(rbind, dts_vor)

##### add tessellation to the static graph p1
p2 <- p1 + 
  geom_polygon(data=dts_vor, aes(x=x, y=y, group=id, fill=group2), 
               color="black", alpha=0.2, show.legend=F) +
  scale_fill_manual(values=c("away"="blue","home"="red"))

# Animate p2     
p2_anim <-  p2 + transition_states(states=time)

# dir.create("./AnimatedPlots", showWarnings=FALSE)

anim_save("./AnimatedPlots/animated_tracking_voronoi.gif", animation=p2_anim, fps=20, 
          nframes=ntime, width=1800, height=900, res=150, renderer=gifski_renderer())
anim_save("./AnimatedPlots/animated_tracking_voronoi.mp4", animation=p2_anim, fps=20, 
          nframes=ntime, width=1800, height=900, res=150, render=av_renderer())

####################### code for plotting Figure 5.2
dts_vor_sf <- dts_vor %>% filter(time==106037)

single_frame_voronoi <- single_frame +
  geom_polygon(data=dts_vor_sf, aes(x=x, y=y, group=id, fill=group2), 
               color="black", alpha=0.2, show.legend=F) +
  scale_fill_manual(values=c("away"="blue","home"="red"))

plot(single_frame_voronoi)

####################### save figures
# dir.create("./Figures", showWarnings=FALSE)

pdf(file="./Figures/trackingvoronoisingleframe.pdf", width=12, height=6, paper="special")
print(single_frame_voronoi)
dev.off()
#################################################################

######################
# function which, given the coordinates of a polygon's vertices
# determines if a point is inside the polygon.
######################
is_within <- function(x, y, pt) {
  x <- c(x, x[1]); y <- c(y, y[1])
  xy <- as.matrix(cbind(x,y))
  pt <- st_point(pt)
  polyg <- st_polygon(list(xy))
  area <- st_area(polyg)
  chk <- length(st_within(pt, polyg)[[1]])
  return(list(chk=chk, area=area))
}


load("./Data/tracking_data.Rdata")

##### repeat for quarter==2, quarter==3, quarter==4
dts_mp <- tdata_long %>%
  filter(quarter==1 & !is.na(x) & !is.na(y)) %>%
  mutate(pl_num=ifelse(group1=="ball" | group1=="bcarr", NA, 
                       as.numeric(gsub("[^0-9]", "", group1))))     

tm_mp <- unique(dts_mp$time)

dts_vor_mp <- lapply(tm_mp, function(t) {
  dtsk <- dts_mp %>% filter(time==t & name!="Ballcarrier" & name!="Ball")
  vor_tessk <- deldir(x=dtsk$x, y=dtsk$y, rw=c(-47,47,-25,25))
  vor_tilesk <- tile.list(vor_tessk) 
  dfk <- do.call(rbind, lapply(seq_along(vor_tilesk), function(i) {
    tile <- vor_tilesk[[i]]
    data.frame(x=c(tile$x, tile$x[1]), y=c(tile$y, tile$y[1]), 
               group2=dtsk$group2[i], id=i, off_team=dtsk$off_team[i],
               quarter=dtsk$quarter[i], event=dtsk$event[i])
  }))
  dfk$time <- t
  return(dfk)
})
dts_vor_mp <- do.call(rbind, dts_vor_mp)

######################
# function to determine the coordinates of the basket
######################
bsk_x <- function(offteam, qrt) {
  bsk_x <- ifelse(offteam=="home", ifelse(qrt==1 | qrt==2, -41.75,  41.75), 
                  ifelse(qrt==1 | qrt==2,  41.75, -41.75))
  return(bsk_x)
}

dts_vor_within <- dts_vor_mp %>%
  group_by(time, id) %>%
  summarize(bsk_x = first(bsk_x(off_team, quarter)),
            chk=is_within(x, y, pt=c(bsk_x,0))$chk, 
            area=is_within(x, y, pt=c(bsk_x,0))$area, 
            group2=first(group2), event=first(event), 
            off_team=first(off_team), .groups="drop") %>%
  as.data.frame()  

calc_area <- function(area,  chk) {
  area <- area[chk==1]
  if (length(area)==0) area=NA
  return(area)
}

dts_vor_within_ha <- dts_vor_within %>%
  group_by(time, group2, off_team) %>%
  summarize(chk2 = any(chk==1), area=calc_area(area, chk), 
            shot_made=ifelse(any(!is.na(event) & event==3), 1, 0), 
            shot_miss=ifelse(any(!is.na(event) & event==4), 1, 0),
            .groups="drop") %>%
  as.data.frame()


tbl <- table(dts_vor_within_ha %>% 
               filter(group2==off_team) %>% 
               select(chk2, off_team))
prop.table(tbl, 2)

df <- dts_vor_within_ha %>% 
  filter(group2==off_team) %>%
  mutate(x=seq_along(time), y=0)

p1 <- ggplot(data=df %>% filter(off_team=="home")) +
  geom_tile(aes(x=x, y=y, fill=chk2), show.legend=F) +
  geom_point(data=df %>% filter(shot_miss==1 & off_team=="home"), 
             aes(x=x, y=y), color="green", shape=17, size=2) +
  geom_point(data=df %>% filter(shot_made==1 & off_team=="home"), 
             aes(x=x, y=y), color="yellow", size=2) +
  scale_x_continuous(expand = c(0, 0), limits=c(0, max(df$x))) + 
  labs(y="Home") +
  scale_fill_manual(values=c("FALSE"="blue", "TRUE"="red")) +
  theme_void() +
  theme(axis.title.y = element_text(size=18))

p2 <- ggplot(data=df %>% filter(off_team=="away")) +
  geom_tile(aes(x=x, y=y, fill=chk2), show.legend=F) +
  geom_point(data=df %>% filter(shot_miss==1 & off_team=="away"), 
             aes(x=x, y=y), color="green", shape=17, size=2) +
  geom_point(data=df %>% filter(shot_made==1 & off_team=="away"), 
             aes(x=x, y=y), color="yellow", size=2) +
  scale_x_continuous(expand = c(0, 0), limits=c(0, max(df$x))) + 
  labs(y="Away") +
  scale_fill_manual(values=c("FALSE"="red", "TRUE"="blue")) +
  theme_void() +
  theme(axis.title.y=element_text(size=18))

pacman::p_load(patchwork)
wrap_plots(list(p1,p2), ncol=1)

####################### save figures
pdf(file="./Figures/time_series_voronoi1.pdf", width=15, height=3, paper="special")
wrap_plots(list(p1,p2), ncol=1)
dev.off()







