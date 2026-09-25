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
# 5.1.2 Animated plots with convex hulls
######################
rm(list=ls())
graphics.off()

load("./Data/Section_5_1.Rdata")
pacman::p_load(ggplot2, gganimate, gifski, av, dplyr)
source("additional_functions.r")

pacman::p_load(sf)
subdts <- dts %>% filter(name!="Ballcarrier" & name!="Ball")
dts_chull <- lapply(tm, function(t) {
  df_h <- subdts %>% filter(time==t & group2=="home") %>% select(x,y)
  chull_h <- chull(df_h)
  chull_h <- c(chull_h, chull_h[1]) 
  poly_h <- st_polygon(list(as.matrix(df_h[chull_h,])))
  df_h$area <- st_area(poly_h)
  df_h$group2 <- "home"
  df_a <- subdts %>% filter(time==t & group2=="away") %>% select(x,y)
  chull_a <- chull(df_a)
  chull_a <- c(chull_a, chull_a[1])
  poly_a <- st_polygon(list(as.matrix(df_a[chull_a,])))
  df_a$area <- st_area(poly_a)
  df_a$group2 <- "away"
  df_ha <- rbind(df_h[chull_h, ], df_a[chull_a, ])
  inters <- st_intersection(st_sfc(poly_h), st_sfc(poly_a))
  df_ha$inters_area <- st_area(inters)
  df_ha$time <- t
  return(df_ha)
})

dts_chull <- do.call(rbind, dts_chull)
dts_chull$group2 <- factor(dts_chull$group2, levels=c("home","away"))

####################### alternative quick way to detemine dts_chull
load("./Data/tracking_data.Rdata")
out <- chulls(tdata_long %>% filter(nposs==2))
dts_chull <- out$dts_chull
#######################

p3 <- p1 + 
  geom_polygon(data=dts_chull, aes(x=x, y=y, fill=group2), 
               color="black", alpha=0.2, show.legend=F) +
  geom_text(aes(x=-47, y=Inf, label=paste("Away - Chull area:", format(area, nsmall=2))), 
            data=dts_chull %>% filter(group2=="away"), hjust=0, vjust=1, size=5) +
  geom_text(aes(x=+47, y=Inf, label=paste("Home - Chull area:", format(area, nsmall=2))), 
            data=dts_chull %>% filter(group2=="home"), hjust=1, vjust=1, size=5) +
  scale_fill_manual(values=c("away"="blue","home"="red"))

# Animate p3     
p3_anim <-  p3 + transition_states(states=time) 

# dir.create("./AnimatedPlots", showWarnings=FALSE)
anim_save("./AnimatedPlots/animated_tracking_chull.gif", animation=p3_anim, fps=20, 
          nframes=ntime, width=1800, height=900, res=150, renderer=gifski_renderer())
anim_save("./AnimatedPlots/animated_tracking_chull.mp4", animation=p3_anim, fps=20, 
          nframes=ntime, width=1800, height=900, res=150, render=av_renderer())

####################### code for plotting Figure 5.4
dts_chull_sf <- dts_chull %>% filter(time==106037)

single_frame_chull <- single_frame +
  geom_polygon(data=dts_chull_sf, aes(x=x, y=y, fill=group2), 
               color="black", alpha=0.2, show.legend=F) +
  geom_text(aes(x=-47, y=Inf, label=paste("Away - Chull area:", format(area, nsmall=2))), 
            data=dts_chull_sf %>% filter(group2=="away"), hjust=0, vjust=1, size=5) +
  geom_text(aes(x=+47, y=Inf, label=paste("Home - Chull area:", format(area, nsmall=2))), 
            data=dts_chull_sf %>% filter(group2=="home"), hjust=1, vjust=1, size=5) +
  scale_fill_manual(values=c("away"="blue","home"="red"))


dev.new(width=12, height=6)
plot(single_frame_chull)


####################### save figures
# dir.create("./Figures", showWarnings=FALSE)

pdf(file="./Figures/trackingchullsingleframe.pdf", width=12, height=6, paper="special")
print(single_frame_chull)
dev.off()
#################################################################

# warning: time-consuming command
# It took 12.93582 minutes to run on an Apple iMac with Chip Apple M3:
# 8‑core CPU with 4 performance cores and 4 efficiency cores,
# 10‑core GPU and 16‑core Neural Engine, 1TB SSD storage, 24GB unified memory

# stime <- Sys.time()
out <- chulls(tdata_long)
# etime <- Sys.time()
# etime-stime

df_area <- out$area_chull_od
df_area <- df_area %>% mutate(R_d=inters_area/d_area,R_o=inters_area/o_area)

Rd <- ggplot(df_area, aes(x = d_group2, y = R_d, fill=d_group2)) +
  geom_boxplot(alpha=0.7) +
  scale_fill_manual(values=c("away"="blue", "home"="red")) +
  labs(x = "Defending team", y = "Rd", title ="% of defensive convex hull covering the offensive area") +
  theme_minimal() +
  theme(legend.position = "none")

Ro <- ggplot(df_area, aes(x = d_group2, y = R_o, fill=d_group2)) +	
  geom_boxplot(alpha=0.7) + 
  scale_fill_manual(values=c("away"="blue", "home"="red")) +
  labs(x = "Defending team", y = "Ro", title ="% of covered offensive convex hull") + 
  theme_minimal() +
  theme(legend.position = "none")

pacman::p_load(gridExtra)
grid.arrange(Rd,Ro, nrow=1)


####################### save figures
pdf(file="./Figures/chullboxplots.pdf", width=10, height=5, paper="special")
grid.arrange(Rd,Ro, nrow=1)
dev.off()



