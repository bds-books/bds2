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

#####################################################################################

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
# 6.5.2 Shot
######################
rm(list=ls())
graphics.off()

# function to compute the angle between two vectors
angle_between_vectors <- function(A, B, C) {
  v1 <- A - B
  v2 <- C - B
  dot_product <- sum(v1 * v2)  # Scalar product
  mag_v1 <- sqrt(sum(v1^2))   # norm of v1
  mag_v2 <- sqrt(sum(v2^2))   # norm of v2
  cos_theta <- dot_product / (mag_v1 * mag_v2)  # cosine of the angle
  theta <- acos(cos_theta)  # angle in radians
  theta_deg <- theta * (180 / pi)  # conversion to degrees
  return(theta_deg)
}

pacman::p_load(ggplot2,gganimate,grid,tensorflow,magick,av,dplyr)
load(file = "./Data/posedt_shot.Rdata")

keypoints <- all_keypoints[[1]]
edges <- all_edges[[1]]
video_name <- "posedt_shot1"
nframes <- length(keypoints)

# dir.create("./AnimatedPlots", showWarnings=FALSE)
source("additional_functions.r")
animation_keypoints_edges(keypoints, edges, out_filename=video_name, 
                          out_dir="./AnimatedPlots/", out_format="gif")


# time series of the elbow angle
df <- data.frame(time=1:length(keypoints))
df$elbow_angle_R <- sapply(keypoints, function(x) {
  A <- x %>% filter(labs=="shoulder_R") %>% select(x, y) %>% unlist()
  B <- x %>% filter(labs=="elbow_R") %>% select(x, y) %>% unlist()
  C <- x %>% filter(labs=="wrist_R") %>% select(x, y) %>% unlist()
  angle_between_vectors(A, B, C)
})

# time series of the knee angle
df$knee_angle_R <- sapply(keypoints, function(x) {
  A <- x %>% filter(labs=="hip_R") %>% select(x, y) %>% unlist()
  B <- x %>% filter(labs=="knee_R") %>% select(x, y) %>% unlist()
  C <- x %>% filter(labs=="ankle_R") %>% select(x, y) %>% unlist()
  angle_between_vectors(A, B, C)
})


p_elbow_angle <- ggplot() +
  geom_path(aes(x=time, y=elbow_angle_R), data=df, linewidth=1, color="yellow") +
  geom_text(aes(x=mean(df$time), y=quantile(df$elbow_angle_R, 0.99), label="Elbow"), color="white", size=14) +
  theme_void() + theme(plot.background = element_rect(fill="black"))
elbow_angle_anim <- p_elbow_angle + transition_reveal(time)
ntime <- length(unique(df$time))
anim_save("./AnimatedPlots/elbow_angle.gif",
          animation=elbow_angle_anim, fps=20, nframes=ntime, res=150,
          width=2000, height=1000, renderer=gifski_renderer())

p_knee_angle <- ggplot() +
  geom_path(aes(x=time, y=knee_angle_R), data=df, linewidth=1, color="green") +
  geom_text(aes(x=mean(df$time), y=quantile(df$knee_angle_R, 0.99), label="Knee"), color="white", size=14) +
  theme_void() + theme(plot.background = element_rect(fill="black"))
knee_angle_anim <- p_knee_angle + transition_reveal(time)
ntime <- length(unique(df$time))
anim_save("./AnimatedPlots/knee_angle.gif",
          animation=knee_angle_anim, fps=20, nframes=ntime, res=150,
          width=2000, height=1000, renderer=gifski_renderer())


img1 <- image_read("./AnimatedPlots/elbow_angle.gif")
img2 <- image_read(paste0("./AnimatedPlots/", video_name, ".gif"))
img3 <- image_read("./AnimatedPlots/knee_angle.gif")

nframes <- min(length(img1), length(img2), length(img3))
img_app <- image_append(c(img1[1],img2[1],img3[1]), stack=TRUE)
for (k in 2:nframes) {
  img_app <- c(img_app,
               image_append(c(img1[k],img2[k],img3[k]), stack=TRUE))
}

image_write_gif(image=img_app, loop=TRUE, delay=1/10,
                paste0("./AnimatedPlots/", video_name,"_combined.gif"))

image_write_video(image = img_app, framerate=20,
                  paste0("./AnimatedPlots/", video_name,"_combined.mp4"))


####################### save figures
# dir.create("./Figures", showWarnings=FALSE)

pdf(file="./Figures/image9.pdf", width=8, height=24, paper="special")
plot(img_app[length(img1)])
dev.off()
#################################################################


######################
# angles time series and smoothed patterns

nps <- 100
df_sm <- data.frame(time=seq(1, nrow(df), length.out=nps),
                    timepoint = 1:nps,
                    sm_knee =ksmooth(df$time,df$knee_angle_R, kernel='normal',bandwidth=3,n.points=nps)$y,
                    sm_elbow=ksmooth(df$time,df$elbow_angle_R,kernel='normal',bandwidth=3,n.points=nps)$y)

p1 <- ggplot() +
  geom_line(data=df, aes(x=time, y=knee_angle_R, color="Knee"), linewidth=1)+
  geom_line(data=df_sm, aes(x=time, y=sm_knee, color="Knee smooth"),
            linewidth=1, linetype=2) +
  geom_line(data=df, aes(x=time, y=elbow_angle_R, color="Elbow"), linewidth=1)+
  geom_line(data=df_sm, aes(x=time, y=sm_elbow, color="Elbow smooth"),
            linewidth=1, linetype=4) +
  scale_color_manual(values=c("Knee"="cyan","Knee smooth"="gray50",
                              "Elbow"="blue","Elbow smooth"="gray50")) +
  labs(x="Timepoint", y="Angle (degree)", color="") +
  guides(color = guide_legend(override.aes = list(size=8))) +
  theme_bw() + theme(legend.position="inside",
                     legend.position.inside=c(0.15,0.15),
                     legend.text = element_text(size = 14),
                     text = element_text(size = 14),
                     legend.background=element_rect(fill = "transparent"))
plot(p1)



######################
# Smoothed angles path

p2basic <- ggplot(data=df_sm, aes(x=sm_knee, y=sm_elbow)) +
  geom_point(data=df, aes(x=knee_angle_R, y=elbow_angle_R)) + 
  geom_text(aes(label=timepoint)) + 
  geom_path() +
  theme_bw() +
  labs(x="Knee angle (degree)", y="Elbow angle (degree)")

plot(p2basic)


######################
# three phases 

v1 <- 48
v2 <- 72
df_sm1 <- df_sm %>% mutate(Phase=c(rep("Preliminary",v1), 
                                   rep("Loading",v2-v1), rep("Energy transfer",nps-v2)),
                           Phase=factor(Phase, levels=c("Preliminary","Loading","Energy transfer")))

p2 <- p2basic +          
  geom_point(data=df_sm1, aes(x=sm_knee, y=sm_elbow), size = 0, alpha = 0) +
  geom_text(data=df_sm1, aes(label=timepoint, color=Phase), show.legend=FALSE) +
  geom_path(data=df_sm1, aes(color=Phase)) +
  scale_color_manual(values=c("Preliminary"="gray50","Loading"="green",
                              "Energy transfer"="red")) +
  guides(color = guide_legend(override.aes = list(size=5, alpha=1))) + 
  theme(text = element_text(size = 14),
        legend.position="inside",
        legend.position.inside=c(0.20,0.85),
        legend.text = element_text(size = 14),
        legend.key.spacing.y=unit(.5,"cm"),
        legend.background=element_rect(fill = "transparent"))

plot(p2)


p3 <- p1 +
  annotate("text", x=10, y=175, label="Preliminary phase", size=4.5) +
  annotate("text", x=23.5, y=175, label="Loading phase", size=4.5) +
  annotate("text", x=35, y=175, label="Energy transfer", size=4.5) +
  geom_vline(aes(xintercept=nrow(df)*c(v1,v2)/nps), linewidth=1,
             linetype=2, color="black")

plot(p3)

####################### save figures
pdf(file="./Figures/traj_ang_1.pdf", width=11, height=8, paper="special")
plot(p1)
dev.off()

pdf(file="./Figures/traj_ang_2basic.pdf", width=9, height=9, paper="special")
plot(p2basic)
dev.off()

pdf(file="./Figures/traj_ang_2.pdf", width=9, height=9, paper="special")
plot(p2)
dev.off()

pdf(file="./Figures/traj_ang_3.pdf", width=11, height=8, paper="special")
plot(p3)
dev.off()
#################################################################

n <- length(all_keypoints)

# list of the elbow's and knee's angles time series for all the 13 shots 
df_all_list <- lapply(1:n, function(k) {
  keypointsk <- all_keypoints[[k]]
  
  # time series of the elbow angle
  nk  <- length(keypointsk)
  dfk <- data.frame(id=rep(k, nk), time=1:nk)
  dfk$elbow_angle_R <- sapply(keypointsk, function(x) {
    A <- x %>% filter(labs=="shoulder_R") %>% select(x, y) %>% unlist()
    B <- x %>% filter(labs=="elbow_R") %>% select(x, y) %>% unlist()
    C <- x %>% filter(labs=="wrist_R") %>% select(x, y) %>% unlist()
    angle_between_vectors(A, B, C)
  })
  
  # time series of the knee angle
  dfk$knee_angle_R <- sapply(keypointsk, function(x) {
    A <- x %>% filter(labs=="hip_R") %>% select(x, y) %>% unlist()
    B <- x %>% filter(labs=="knee_R") %>% select(x, y) %>% unlist()
    C <- x %>% filter(labs=="ankle_R") %>% select(x, y) %>% unlist()
    angle_between_vectors(A, B, C)
  })
  
  dfk <- na.omit(dfk)
  nk <- nrow(dfk)
  
  sm_knee <- ksmooth(c(1:nk), dfk$knee_angle_R, 'normal',bandwidth=3, n.points=nk)
  sm_elb <-  ksmooth(c(1:nk), dfk$elbow_angle_R,'normal',bandwidth=3, n.points=nk)
  
  dfk$x_sm <- sm_knee$y
  dfk$y_sm <- sm_elb$y
  return(dfk)
}) 

# list of the elbow's and knee's angles smoothed time series for all the 13 shots 
df_all_smooth_list <- lapply(1:length(df_all_list), function(k) {
  cat(k, "of 13\n")
  dfk <- df_all_list[[k]]
  dfk_ord <- dfk %>% 
    dplyr::rename(x=knee_angle_R, y=elbow_angle_R) %>%  
    arrange(time, id)
  sm_knee <- ksmooth(1:nrow(dfk), dfk$knee_angle_R, 'normal',bandwidth=3, n.points=nps)
  sm_elb <-  ksmooth(1:nrow(dfk), dfk$elbow_angle_R,'normal',bandwidth=3, n.points=nps)
  df <- data.frame(id=rep(k, nps), time=100*(1:nps)/nps, V1=sm_knee$y, V2=sm_elb$y)
}) 


df_all <- do.call(rbind, df_all_list)
df_all <- df_all %>% 
  mutate(MM = ifelse(id==1 | id==9 | id==12 | id==13, "missed", "made")) %>%
  arrange(time,id)

df_all_smooth <- do.call(rbind, df_all_smooth_list)
df_all_smooth <- df_all_smooth %>% 
  mutate(MM = ifelse(id==1 | id==9 | id==12 | id==13, "missed", "made")) %>%
  arrange(time, id)


#### all trajectories smoothing
sm_knee <- ksmooth(1:nrow(df_all_smooth), df_all_smooth$V1, 'normal',bandwidth=3, n.points=nps)
sm_elb <-  ksmooth(1:nrow(df_all_smooth), df_all_smooth$V2,'normal',bandwidth=3, n.points=nps)
df_all_smooth_all <- data.frame(V1=sm_knee$y, V2=sm_elb$y)

p4 <- ggplot(data=df_all, aes(x=knee_angle_R, y=elbow_angle_R, group=as.factor(id), color=as.factor(MM)))  +
  geom_point(alpha=0.5, show.legend=F) +
  geom_path(aes(x=V1, y=V2), data=df_all_smooth_all, show.legend=F, inherit.aes=F, linewidth=1.2) +
  geom_path(aes(x=V1, y=V2, group=factor(id), color=factor(MM)), data=df_all_smooth, show.legend=F, inherit.aes=F, alpha=0.5) +
  coord_fixed(ratio = 1, xlim=c(90,180)) +
  labs(x="Knee angle", y="Elbow angle", col="Made/Missed") +
  theme_bw()

plot(p4)

####################### save figures
pdf(file="./Figures/traj_ang_4.pdf", width=11, height=8, paper="special")
plot(p4)
dev.off()
#################################################################

