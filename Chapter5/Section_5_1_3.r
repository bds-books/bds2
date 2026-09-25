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
# 5.1.3 Monitoring speed and acceleration with combined animated plots
######################
rm(list=ls())
graphics.off()

sp_acc <- function(x, y, time, Dsp=1, Dacc=1) {
  n <- length(x)
  sp <- sapply((Dsp+1):(n-Dsp), function(t) {
    -sqrt((x[t+Dsp]-x[t-Dsp])^2+(y[t+Dsp]-y[t-Dsp])^2)/(time[t+Dsp]-time[t-Dsp])
  })
  sp <- c(rep(first(sp),Dsp), sp, rep(last(sp),Dsp))
  
  acc <- sapply((Dacc+1):(n-Dacc), function(t) {
    -(sp[t+Dacc]-sp[t-Dacc])/(time[t+Dacc]-time[t-Dacc])
  })
  acc <- c(rep(first(acc),Dacc), acc, rep(last(acc),Dacc))
  return(data.frame(sp, acc))
}

load("./Data/Section_5_1.Rdata")
pacman::p_load(ggplot2, gganimate, gifski, av, dplyr)

dts_sp_acc <- dts %>%
  filter(name=="Paul Pierce" | name=="LeBron James") %>%
  group_by(name) %>%
  mutate(sp=sp_acc(x, y, game_clock, Dsp=4, Dacc=4)$sp,
         acc=sp_acc(x, y, game_clock, Dsp=4, Dacc=4)$acc)

cols <- c("red", "blue")
names(cols) <- c("Paul Pierce","LeBron James")

# check numer of running clock segments
length(unique(dts$rcs))

ts_sp <- ggplot(data=dts_sp_acc, aes(x=game_clock, y=sp, group=name, color=name)) +
  geom_line(linewidth=1) +
  scale_x_reverse() +
  scale_color_manual(values=cols) +
  labs(x="Game clock", y="Speed", colour="") +
  theme_bw() %+replace% theme(legend.position="inside", 
                              legend.position.inside=c(0.1, 0.15),
                              legend.background = element_rect(fill = "transparent", color="transparent")) +
  guides(color = guide_legend(nrow = 1))

dev.new(width=12, height=4)
print(ts_sp)

ts_acc <- ggplot(data=dts_sp_acc, aes(x=game_clock, y=acc, group=name, color=name)) +
  geom_line(linewidth=1) +
  scale_x_reverse() +
  scale_color_manual(values=cols) +
  labs(x="Game clock", y="Acceleration", colour="") +
  theme_bw() %+replace% theme(legend.position="inside", 
                              legend.position.inside=c(0.1, 0.15),
                              legend.background = element_rect(fill = "transparent", color="transparent")) +
  guides(color = guide_legend(nrow = 1))

dev.new(width=12, height=4)
print(ts_acc)


####################### save figures
# dir.create("./Figures", showWarnings=FALSE)

pdf(file="./Figures/speed.pdf", width=12, height=4, paper="special")
print(ts_sp)
dev.off()

pdf(file="./Figures/acceleration.pdf", width=12, height=4, paper="special")
print(ts_acc)
dev.off()
#################################################################

ntime_sp_acc <- length(unique(dts_sp_acc$time))

# dir.create("./AnimatedPlots", showWarnings=FALSE)

ts_sp_anim <- ts_sp + transition_reveal(time)

anim_save("./AnimatedPlots/speed.gif", animation=ts_sp_anim, fps=20, 
          nframes=ntime_sp_acc, width=1800, height=400, res=150,
          renderer=gifski_renderer())

ts_acc_anim <- ts_acc + transition_reveal(time)

anim_save("./AnimatedPlots/acceleration.gif", animation=ts_acc_anim, fps=20, 
          nframes=ntime_sp_acc, width=1800, height=400, res=150,
          renderer=gifski_renderer())

##### combine GIFs

pacman::p_load(magick)

img1 <- image_read("./AnimatedPlots/speed.gif")
img2 <- image_read("./AnimatedPlots/animated_tracking.gif")
img3 <- image_read("./AnimatedPlots/acceleration.gif")

nframes <- min(length(img1), length(img2), length(img3))
img_app <- image_append(c(img1[1],img2[1],img3[1]), stack=TRUE)
for (k in 2:nframes) {
  img_app <- c(img_app, image_append(c(img1[k],img2[k],img3[k]), stack=TRUE))
}

image_write_gif(image = img_app, path = "./AnimatedPlots/sp_track_acc.gif",
                loop = TRUE, delay = 1/20)


####################### code for plotting Figure 5.7
frame <- which(unique(dts_sp_acc$time)==106037)

dev.new(width=12, height=12)
plot(img_app[frame])

####################### save figures
pdf(file="./Figures/sp_track_acc_singleframe.pdf", width=12, height=12, paper="special")
plot(img_app[frame])
dev.off()
#################################################################

