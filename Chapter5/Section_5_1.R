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
rm(list=ls())
graphics.off()

pacman::p_load(ggplot2, gganimate, gifski, av, dplyr)
load("./Data/tracking_data.Rdata")
source("additional_functions.r")

no_poss <- 2
dts <- tdata_long %>%
  filter(nposs==no_poss & !is.na(x) & !is.na(y)) %>%
  mutate(pl_num=ifelse(group1=="ball" | group1=="bcarr", NA, as.numeric(gsub("[^0-9]", "", group1))))

tm <- unique(dts$time)
ntime <- length(tm)

##### Prepare dataset for legend
df_txt <- dts %>%  filter(group1!="ball" & group1!="bcarr") 

##### Create legend
df_leg <- df_txt %>%
  group_by(time, name) %>%
  summarize(
    num = as.numeric(substr(first(group1), 2, 2)),
    ah  = substr(first(group1), 1, 1) 
  ) %>%
  mutate(group2 = ifelse(ah == "a", "away", "home")) %>%
  arrange(time, ah, desc(num)) %>%
  group_by(time) %>%
  mutate(x = 50, y = seq(-23,23,length.out=10) ) %>%
  ungroup()

#### Function to add a legend on the right side of the court
add_legend <- function(data) {
  list(
    geom_point(data=data,
               aes(x=x, y=y, color=group2),
               size=8, shape=21, stroke=1, inherit.aes=FALSE, show.legend=FALSE),
    geom_text(data=data,
              aes(x=x, y=y, color=group2, label=num),
              size=4, inherit.aes=FALSE, show.legend=FALSE),
    geom_text(data=data,
              aes(x=x, y=y, color=group2, label=name),
              hjust="left", nudge_x=1.8, size=4,
              inherit.aes=FALSE, show.legend=FALSE) )
}

#### Convert seconds to the format min:sec.decimal
sec_to_clock <- function(seconds) {
  min <- floor(seconds / 60)
  sec <- seconds %% 60
  sprintf("%02d:%04.1f", min, sec)
}

##### Plot court, players, and game clock
p1 <- ggplot() + plot_court() +
  geom_point(aes(x=x, y=y, group=group1, color=group2, shape=group2, size=group2), data=dts, stroke=1) +
  geom_text(aes(x=x, y=y, label=pl_num, color=group2), data=df_txt, size=6, show.legend=F) +
  geom_text(aes(x=0, y=Inf, label=paste("Quarter:", quarter, " - Game clock:", sec_to_clock(game_clock))), 
            data=dts, hjust=0.5, vjust=1, size=5) +
  scale_color_manual(values=c("away"="blue","home"="red","ball"="orange","bcarr"="orange")) +
  scale_shape_manual(values=c("away"=21,"home"=21,"ball"=19,"bcarr"=23)) +
  scale_size_manual(values=c("away"=8,"home"=8,"ball"=6,"bcarr"=8)) + 
  xlim(c(-47,65)) + 
  labs(color="", shape="", size="") +
  theme_void() %+replace% theme(legend.position="bottom")

##### Add legend
p1 <- p1 + add_legend(df_leg)


##### Animation
p1_anim <-  p1 + 
  transition_states(states=time, transition_length = 1, state_length = 1)

dir.create("./AnimatedPlots", showWarnings=FALSE)

anim_save("./AnimatedPlots/animated_tracking.gif", animation=p1_anim, fps=20, 
          nframes=ntime, width=1800, height=900, res=150, renderer=gifski_renderer())
anim_save("./AnimatedPlots/animated_tracking.mp4", animation=p1_anim, fps=20, 
          nframes=ntime, width=1800, height=900, res=150, renderer=av_renderer())


####################### code for plotting Figure 5.1
dts_sf <- dts %>% filter(time==106037) %>% mutate(pl_num = ifelse(is.na(pl_num),"",pl_num))
single_frame <- ggplot() + plot_court() +
  geom_point(aes(x=x, y=y, group=group1, color=group2, shape=group2, size=group2), data=dts_sf, stroke=1) +
  geom_text(aes(x=x, y=y, label=pl_num, color=group2), data=dts_sf, size=6, show.legend=F) +
  geom_text(aes(x=0, y=Inf, label=paste("Quarter:", quarter, " - Game clock:", sec_to_clock(game_clock))), 
            data=dts_sf, hjust=0.5, vjust=1, size=5) +
  scale_color_manual(values=c("away"="blue","home"="red","ball"="orange","bcarr"="orange")) +
  scale_shape_manual(values=c("away"=21,"home"=21,"ball"=19,"bcarr"=23)) +
  scale_size_manual(values=c("away"=8,"home"=8,"ball"=6,"bcarr"=8)) + 
  xlim(c(-47,65)) + 
  labs(color="", shape="", size="") +
  theme_void() %+replace% theme(legend.position="bottom")

df_leg_sf <- df_leg %>% filter(time==106037)
single_frame <- single_frame + add_legend(df_leg_sf)

dev.new(width=12, height=6)
plot(single_frame)

####################### save figures
dir.create("./Figures", showWarnings=FALSE)

pdf(file="./Figures/trackingsingleframe.pdf", width=12, height=6, paper="special")
print(single_frame)
dev.off()
#################################################################

save(no_poss, tm, ntime, dts, dts_sf, single_frame, p1, file="./Data/Section_5_1.Rdata")


