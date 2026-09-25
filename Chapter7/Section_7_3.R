########################################################################################
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

########################################################################


# The package {\tt pacman} has to be installed
# install.packages("pacman")

# set Chapter7 as working directory
# setwd("....")


############################################
############################################
# PART 2                                   #
# Decoding motion                          #                            
############################################
############################################

##############################################
# CHAPTER 7                                  #
# Tracking and analyzing ball trajectories   #
##############################################

######################
# 7.3 Using data to improve shooting ability
######################

rm(list=ls())
graphics.off()

pacman::p_load(plotly,ggplot2,dplyr)
source("additional_functions.r")
load(file="./Data/ball_trajectories.Rdata")
load(file="./Data/list_traj.Rdata")
load(file="./Data/traj_stats.Rdata")

#### select example trajectory
#### missed shot 
#### with R2>0.9
#### taken from an average height
#### with angle_dir < 0.671

dts <- lapply(traj_fit,
              function(x) {
                data.frame(id=x$df_pred$id[1],
                           made_miss=x$df_pred$made_miss[1],
                           R2=summary(x$models$mod_z)$r.squared,
                           z0=x$df_pred$z[1],
                           ad=x$directions$angle_dir)}) %>%
  do.call(rbind, .)

IDs <- unique(dts %>% dplyr::filter(made_miss==0 & R2>=0.9 & z0 > -0.6 & z0 < -0.4 & ad < 0.671) %>% dplyr::select(id))


k <- 212
idk <- IDs$id[k]
df <- dtset %>% dplyr::filter(id==idk) %>% dplyr::select(x, y, z, time, made_miss, id)

df_stats <- traj_stats %>% dplyr::filter(id==idk)
actual_distVM <- df_stats$distVM
actual_isp_feet <- df_stats$isp.feet
actual_rel_angle <- df_stats$angle
actual_distVM
actual_isp_feet
actual_rel_angle


### simulated trajectory 1
### initial speed must be decreased (between 27.7 and 28.3)
### release angle > 49.2

x0 <- df$x[1]
y0 <- df$y[1] 
z0 <- df$z[1]

isp_feet <- 28
rel_angle <- 49.3

df_simul1 <- simul_traj(x0, y0, z0, isp_feet, rel_angle)
df_simul1 <- data.frame(df_simul1,time=seq(0,max(df$time),length.out=nrow(df_simul1)))
descr1 <- trajectory_analysis(df_simul1)
simul1_distVM <- descr1$distV$dist_VvsM* descr1$distV$sign_V_pos
simul1_distVM


simtraj1 <- plot_single_traj(df) %>%
  add_trace(x=~x, y=~y, z=~z, data=df_simul1, 
            type="scatter3d", mode="lines", showlegend=FALSE,
            line=list(width=4, color="green", dash="dash"), 
            inherit=FALSE) %>% 
  layout(scene = list(
    aspectmode = "data",
    aspectratio = list(x = 1, y = 1, z = 1),
    camera = list(eye=list(x = -1, y = 1.9, z = 0)),
    margin = list(l = 5, r = 5, t = 5, b = 5)))

print(simtraj1)

####################### save figure
# you may need to run:

# pacman::p_load(reticulate)
# py_config()
# reticulate::py_install("kaleido==0.2.1")
# reticulate::py_install("plotly")

# or

# pacman::p_load(reticulate)
# py_config()
# reticulate::py_require("kaleido==0.2.1")
# reticulate::py_require("plotly")

# dir.create("./Figures", showWarnings=FALSE)

save_image(simtraj1, file="./Figures/improveshot1.png", width=1200,height=800)
#################################################################


### simulated trajectory 2
### same release angle
### initial speed much decreased (between 26.6 and 27.7)


x0 <- df$x[1]
y0 <- df$y[1] 
z0 <- df$z[1]

isp_feet <- 27.6
rel_angle <- actual_rel_angle

df_simul2 <- simul_traj(x0, y0, z0, isp_feet, rel_angle)
df_simul2 <- data.frame(df_simul2,time=seq(0,max(df$time),length.out=nrow(df_simul2)))
descr2 <- trajectory_analysis(df_simul2)
simul2_distVM <- descr2$distV$dist_VvsM* descr2$distV$sign_V_pos
simul2_distVM

simtraj2 <- plot_single_traj(df) %>%
  add_trace(x=~x, y=~y, z=~z, data=df_simul2, 
            type="scatter3d", mode="lines", showlegend=FALSE,
            line=list(width=4, color="green", dash="dash"), 
            inherit=FALSE) %>% 
  layout(scene = list(
    aspectmode = "data",
    aspectratio = list(x = 1, y = 1, z = 1),
    camera = list(eye=list(x = -1, y = 1.9, z = 0)),
    margin = list(l = 5, r = 5, t = 5, b = 5)))

print(simtraj2)



####################### save figure

save_image(simtraj2, file="./Figures/improveshot2.png", width=1200,height=800)
#################################################################

