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

# set Chapter6 as working directory
# setwd("....")

############################################
############################################
# PART 2                                   #
# Decoding motion                          #                            
############################################
############################################

##############################################
# CHAPTER 6                                  #
# Athletic motion kinematics analysis        #
##############################################

######################
# 6.5 Pose detection data analysis
######################

######################
# 6.5.1 Crossover
######################
rm(list=ls())
graphics.off()

pacman::p_load(ggplot2,grid,magick,av,gganimate,stringr,gifski,dplyr)

load(file = "./Data/posedt_crossover.Rdata")

keypoints <- all_keypoints[[1]]
edges <- all_edges[[1]]
nframes <- length(keypoints)

video_name <- "posedt_crossover1"

# dir.create("./AnimatedPlots", showWarnings=FALSE)
source("additional_functions.r")
animation_keypoints_edges(keypoints, edges, out_filename=video_name, 
                          out_dir="./AnimatedPlots/", out_format="gif")


###
# Elbows path
###
# Retain elbow coordinates and
# add the time coordinate
elbows <- lapply(1:nframes, function(k) {
  data.frame(keypoints[[k]] %>% filter(str_detect(labs, "elbow")), 
             time=c(k,k), 
             elbow=c("L","R"))
})
elbows <- do.call(rbind, elbows)
p_elbows <- ggplot() +
  geom_path(aes(x=x, y=y, group=elbow, color=elbow),
            data=elbows, linewidth=1,
            show.legend=F) +
  geom_text(aes(x=0.5, y=0.9, label="Elbows"), color="white", size=14) +
  xlim(c(0,1)) + ylim(c(0,1)) + 
  theme_void() +
  theme(legend.position="inside",
        legend.position.inside=c(0.9,0.8),
        legend.text = element_text(color = "white"),
        legend.title = element_text(color = "white"),
        plot.background = element_rect(fill="black"))

elbows_path_anim <- p_elbows + transition_reveal(time)
ntime <- length(unique(elbows$time))
anim_save("./AnimatedPlots/elbows_path.gif", animation=elbows_path_anim,
          fps=20, nframes=ntime, res=150, width=2000, height=2000, renderer=gifski_renderer())


###
# Knees path
###
# Retain knee coordinates and
# add the time coordinate
knees <- lapply(1:nframes, function(k) {
  data.frame(keypoints[[k]] %>% filter(str_detect(labs, "knee")), 
             time=c(k,k), 
             knee=c("L","R"))
})
knees <- do.call(rbind, knees)
p_knees <- ggplot() +
  geom_path(aes(x=x, y=y, group=knee, color=knee),
            data=knees, linewidth=1,
            show.legend=F) +
  geom_text(aes(x=0.5, y=0.9, label="Knees"), color="white", size=14) +
  xlim(c(0,1)) + ylim(c(0,1)) + 
  theme_void() +
  theme(legend.position="inside",
        legend.position.inside=c(0.9,0.8),
        legend.text = element_text(color = "white"),
        legend.title = element_text(color = "white"),
        plot.background = element_rect(fill="black"))

knees_path_anim <- p_knees + transition_reveal(time)
ntime <- length(unique(knees$time))
anim_save("./AnimatedPlots/knees_path.gif", animation=knees_path_anim,
          fps=20, nframes=ntime, res=150, width=2000, height=2000, renderer=gifski_renderer())


######
# Combine plots
######
img1 <- image_read("./AnimatedPlots/elbows_path.gif")
img2 <- image_read(paste0("./AnimatedPlots/",video_name,".gif"))
img3 <- image_read("./AnimatedPlots/knees_path.gif")

nframes <- min(length(img1), length(img2), length(img3))
img_app <- image_append(c(img1[1],img2[1],img3[1]), stack=F)
for (k in 2:nframes) {
  img_app <- c(img_app,
               image_append(c(img1[k],img2[k],img3[k]), stack=F))
}

image_write_gif(image=img_app, loop=TRUE, delay=1/10,
                path = paste0("./AnimatedPlots/", video_name,"_combined.gif"))

image_write_video(image=img_app, framerate=10,
                  path = paste0("./AnimatedPlots/", video_name,"_combined.mp4"))


####################### save figures
# dir.create("./Figures", showWarnings=FALSE)

pdf(file="./Figures/image8.pdf", width=24, height=10, paper="special")
plot(img_app[length(img1)])
dev.off()
#################################################################

######
# analysis
######

# example for knee_L
pacman::p_load(amt)

kneeL_traj <- lapply(keypoints, function(x) {
  x %>% dplyr::filter(labs=="knee_L") %>% dplyr::select(x, y)
}) %>% do.call(rbind, .) 

kneeL_traj$time <- 1:nrow(kneeL_traj)
xytrk <- make_track(tbl=kneeL_traj, .x=x, .y=y, .t=time)

xytrk <- xytrk %>%
  mutate(tmpcol = (x_ == lag(x_)) & (y_ == lag(y_))) %>%
  filter(!tmpcol | is.na(tmpcol)) %>%  
  select(-tmpcol)

# Total distance
TD <- tot_dist(xytrk)

# Cumulative distance
CD <- cum_dist(xytrk)

# Straightness (tot_dist/cum_dist)
SI <- straightness(xytrk)

# Mean squared displacement
MSD <- msd(xytrk)

# Mean turn angle correlation
TAC <- tac(xytrk)

# Intensity of use 
IU <- intensity_use(xytrk)

# Home range (by kernel density estimation)
HR <- hr_kde(xytrk, levels=seq(0.5,0.95,by=0.15))
plot(HR)


########
# Summary table
########
elbow_knee <- c("elbow_L","elbow_R","knee_L","knee_R")
df <- lapply(1:length(all_keypoints), function(k) {
  lapply(elbow_knee, function(el_kn) {
    traj <- lapply(all_keypoints[[k]], function(x) {
      x %>% dplyr::filter(labs==el_kn) %>% dplyr::select(x, y)
    }) %>%
      do.call(rbind, .)
    traj$time <- 1:nrow(traj)
    xytrk <- make_track(tbl=traj, .x=x, .y=y, .t=time)    
    xytrk <- xytrk %>%
      mutate(tmpcol = (x_ == lag(x_)) & (y_ == lag(y_))) %>%
      filter(!tmpcol | is.na(tmpcol)) %>%  
      select(-tmpcol) 
    dfk <- data.frame(Crossover=paste0("Crossover",k),
                      Keypoint=el_kn,
                      TD=tot_dist(xytrk),
                      CD=cum_dist(xytrk),
                      SI=straightness(xytrk),
                      MSD=msd(xytrk),
                      TAC=tac(xytrk),
                      IU=intensity_use(xytrk))
    HR <- hr_kde(xytrk, levels=seq(0.5,0.95,by=0.15))	
    pdf(file=paste0("./Figures/HR_Crossover",k,"_",el_kn,".pdf"), width=10, height=10, paper="special")
    plot(HR, main=paste0("Crossover",k," - ",el_kn), cex.main=4)
    dev.off()	
    return(dfk)
  }) %>% do.call(rbind, .)
}) %>% do.call(rbind, .)

pacman::p_load(gt)
tbl <- gt(df, groupname_col="Crossover") %>%
  cols_align(columns=-all_of("Keypoint"), align="right") %>%
  fmt_number(columns="MSD", decimals=5) %>%
  fmt_number(columns=-all_of("MSD"), decimals=3)
print(tbl)


tbl %>% gtsave("Table.tex")


