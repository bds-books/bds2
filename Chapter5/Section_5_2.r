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
# 5.2 Gravity and Distraction 
######################
rm(list=ls())
graphics.off()

load("./Data/tracking_data.Rdata")
load("./Data/Section_5_1.Rdata")
pacman::p_load(ggplot2, gganimate, gifski, av, dplyr)
source("additional_functions.r")

####################### Code for plotting Figure 5.8
dts_sf_GD <- dts_sf %>% filter(group1=="h2" | group1=="a3" | group1=="bcarr")

df_sgm_GD <- with(dts_sf_GD, data.frame(x=c(x[1], x[1]), y=c(y[1], y[1]), 
                                        xend=c(x[3],x[2]), yend=c(y[3],y[2]) ))
df_txt_GD <- with(dts_sf_GD, data.frame(x=c((x[1]+x[3])/2, (x[1]+x[2])/2), 
                                        y=c((y[1]+y[3])/2, (y[1]+y[2])/2), 
                                        lab=c("Distraction", "Gravity"),
                                        ang=c(180/pi*atan((y[1]-y[3])/(x[1]-x[3])),
                                              180/pi*atan((y[1]-y[2])/(x[1]-x[2]))),
                                        vj=c(1.5, -0.8) ))

single_frame_GD <- single_frame +
  geom_segment(aes(x = x, y = y, xend = xend, yend = yend), data = df_sgm_GD) +
  geom_text(aes(x=x, y=y, label=lab, angle=ang, vjust=vj), data = df_txt_GD)

dev.new(width=12, height=6)
plot(single_frame_GD)

####################### save figures
# dir.create("./Figures", showWarnings=FALSE)

pdf(file="./Figures/trackingsingleframeGD.pdf", width=12, height=6, paper="special")
print(single_frame_GD)
dev.off()
#################################################################

####################### Code for plotting Figure 5.9
df_tr <- data.frame(x=c(-0.3, 0.3, -0.5), y=c(0, 0.45, 0.74), mass=c(1,3,1),
                    hj=c(-0.8,1.5,-0.6), vj=c(1,-1.2,-0.8), pt=c("C","B","A"), 
                    bar=c(paste("omega[c]"),paste("omega[b]"),paste("omega[a]")))
wmn <- function(x, w) {sum(x*w)/sum(w)}
df_mn <- with(df_tr, data.frame(x=wmn(x,mass), y=wmn(y,mass)))


ang_dis <- -180/pi*atan2(abs(df_tr$y[3]-df_mn$y),abs(df_tr$x[3]-df_mn$x))
midpt_x_dis <- (df_tr$x[3]+df_mn$x)/2
midpt_y_dis <- (df_tr$y[3]+df_mn$y)/2

ang_gra <- 180/pi*atan2(abs(df_tr$y[2]-df_mn$y),abs(df_tr$x[2]-df_mn$x))
midpt_x_gra <- (df_tr$x[2]+df_mn$x)/2
midpt_y_gra <- (df_tr$y[2]+df_mn$y)/2


p <- ggplot() +
  geom_polygon(data=df_tr, aes(x=x, y=y), fill="blue", alpha=0.2) +
  geom_polygon(data=df_tr, aes(x=x, y=y), fill="transparent", color="navy") +
  geom_segment(data=df_tr, aes(x=x, y=y), xend=df_mn$x, yend=df_mn$y, linetype=2, linewidth=1, color="navy") +
  geom_point(data=df_tr, aes(x=x, y=y, color=pt), shape=21, size=20, 
             fill="white", stroke=1, show.legend=F) +
  geom_text(data=df_tr, aes(x=x, y=y, color=pt, label=pt), size=10, 
            show.legend=F) +
  geom_point(data=df_mn, aes(x=x, y=y), color="navy", shape=21, size=20, 
             fill="white", stroke=1, show.legend=F) +
  geom_point(aes(x=df_tr$x[3]-.025, y=df_tr$y[3]-0.025), color="orange", fill="orange", shape=21, size=15, 
             stroke=1, show.legend=F) +
  geom_text(data=df_mn, aes(x=x, y=y, label="K"), color="blue", size=10, 
            show.legend=F) +
  geom_text(data=df_tr, aes(x=x, y=y, color=pt, label=bar, hjust=hj, vjust=vj), size=10, 
            parse = TRUE, show.legend=F) +
  geom_text(aes(x=midpt_x_dis , y=midpt_y_dis , label="Distraction"), size=7, angle=ang_dis, color="navy",
            show.legend=F, vjust=-0.45) +
  geom_text(aes(x=midpt_x_gra, y=midpt_y_gra, label="Gravity"), size=7, angle=ang_gra, color="navy",
            show.legend=F, vjust=-0.45) +
  scale_color_manual(values=c("C"="black","A"="red","B"="red")) +
  coord_fixed(clip="off") +
  theme_void()

plot(p)

####################### save figures
pdf(file="./Figures/Fig_baric_coord.pdf", width=10, height=10)
plot(p)
dev.off()
#################################################################

####################### Code for animated plot with gravity and distraction

player_name <- "Paul Pierce"

dts_wide <- tdata_wide %>% filter(nposs==no_poss)

oppDist <- opponentDist(dts_wide, name=player_name)
nr <- nrow(oppDist$xyOpp)

qrt <- unique(dts_wide$quarter)
if (oppDist$isHome) {
  if (qrt==1 | qrt==2) bsk_x <- -41.75 else bsk_x <- 41.75
  plr_col <- "red"; opp_col <- "blue"
} else {
  if (qrt==1 | qrt==2) bsk_x <- 41.75 else bsk_x <- -41.75
  plr_col <- "blue"; opp_col <- "red"
}

df_segm <- with(oppDist, data.frame(time=dts_wide$time,
                                    quarter=dts_wide$quarter,
                                    oppx=xyOpp[,1], oppy=xyOpp[,2],
                                    bcarrx=dts_wide$bcarr_x,
                                    bcarry=dts_wide$bcarr_y, 
                                    plyrx=xyPlyr[,1], plyry=xyPlyr[,2],
                                    bskx =rep(bsk_x, nr), bsky=rep(0, nr) ))

barycentric_coord <- with(df_segm, bary_coord(bcarrx, plyrx, bskx, oppx, 
                                              bcarry, plyry, bsky, oppy))
df_segm <- cbind(df_segm, barycentric_coord)

df_poly <- with(df_segm, data.frame(x=c(rep(bsk_x, nrow(df_segm)), plyrx, bcarrx), 
                                    y=c(rep(0,     nrow(df_segm)), plyry, bcarry),
                                    time=rep(time, 3) ))

p4 <- p1 +
  geom_segment(data=df_segm, aes(x=oppx, y=oppy, xend=plyrx, yend=plyry, group=time), linewidth=1, color=plr_col, alpha=0.5) +
  geom_polygon(data=df_poly, aes(x=x, y=y, group=time), fill=opp_col, alpha=0.2, color=opp_col) +
  geom_text(data=df_segm, aes(x=bcarrx, y=bcarry, label=round(wA,2)), size=4.5, hjust=1, vjust=1) +
  geom_text(data=df_segm, aes(x=plyrx, y=plyry, label=round(wB,2)), size=4.5, hjust=1, vjust=1) +
  geom_text(data=df_segm, aes(x=bskx, y=bsky, label=round(wC,2)), size=4.5, hjust=1, vjust=1) 

# Animate p4     
p4_anim <-  p4 + transition_states(states=time) 

# dir.create("./AnimatedPlots", showWarnings=FALSE)
anim_save("./AnimatedPlots/triangle.gif", animation=p4_anim, fps=20, 
          nframes=ntime, width=1800, height=900, res=150, renderer=gifski_renderer())
anim_save("./AnimatedPlots/triangle.mp4", animation=p4_anim, fps=20, nframes=ntime, 
          width=1800, height=900, res=150, renderer=av_renderer())


####################### code for plotting Figure 5.10

df_segm_sf <- df_segm %>% filter(time==106037) 
df_poly_sf <- df_poly %>% filter(time==106037) 

single_frame_triangle <- single_frame +
  geom_segment(data=df_segm_sf, aes(x=oppx, y=oppy, xend=plyrx, yend=plyry, group=time), linewidth=1, color=plr_col, alpha=0.5) +
  geom_polygon(data=df_poly_sf, aes(x=x, y=y, group=time), fill=opp_col, alpha=0.2, color=opp_col) +
  geom_text(data=df_segm_sf, aes(x=bcarrx, y=bcarry, label=round(wA,2)), size=4.5, hjust=1, vjust=1) +
  geom_text(data=df_segm_sf, aes(x=plyrx, y=plyry, label=round(wB,2)), size=4.5, hjust=1, vjust=1) +
  geom_text(data=df_segm_sf, aes(x=bskx, y=bsky, label=round(wC,2)), size=4.5, hjust=1, vjust=1) 

dev.new(width=12, height=6)
plot(single_frame_triangle)

# Here we introduce the "plot_single_frame" function.
# It is contained in the file additional_functions.r 
# Input arguments: 
# - data in long format (data_long)
# - data in wide format (data_wide) 
# - time of the single frame (time_sf)
# - whether to plot the triangle with distraction and gravity (GD) for a given player (player_name) 
# - whether to display the legend with the names of the players on the field on the right side of the plot (player_legend).

single_frame_triangle <- plot_single_frame(data_long=tdata_long, data_wide=tdata_wide, time_sf=106037, 
                                           GD=TRUE, player_name="Paul Pierce", player_legend=TRUE)

dev.new(width=12, height=6)
plot(single_frame_triangle)

####################### save figures
pdf(file="./Figures/singleframetriangle.pdf", width=12, height=6, paper="special")
print(single_frame_triangle)
dev.off()
#################################################################


####################### code for plotting Figure 5.11
t1a <- 110477 # right panel
t1b <- 95557  # left panel

pacman::p_load(patchwork)
plot_list <- lapply(c(t1a,t1b), function(t) {
  plot_single_frame(data_long=tdata_long, data_wide=tdata_wide, time_sf=t, 
                    GD=TRUE, player_name="Paul Pierce", player_legend=FALSE)
})
dev.new(width=14, height=4.3)
wrap_plots(plot_list, nrow=1)

####################### save figures
pdf(file="./Figures/singleframes_ex.pdf", width=14, height=4.3, paper="special")
wrap_plots(plot_list, nrow=1)
dev.off()
#################################################################



##### Filter:
##### - when the player is around the 3-point line
##### AND
##### - when the massess are all positive

filt_3ptline <- between_3pt_line(df_segm$plyrx, df_segm$plyry, delta=5, lr="left")

filt_wpos <- apply(df_segm[, c("wA","wB","wC")], 1, function(x) all(x>0) & all(!is.na(x)) )

filt <- (filt_3ptline & filt_wpos) 

df_segm_filt <- df_segm[filt, ]


####################### code for plotting Figure 5.12
unique(df_segm_filt$time)
t2 <- 98798	     # top left
t3 <- 100918		# top right
t4 <- 101998		# bottom left
t5 <- 103558		# bottom right

pacman::p_load(patchwork)
plot_list <- lapply(c(t2,t3,t4,t5), function(t) {
  plot_single_frame(data_long=tdata_long, data_wide=tdata_wide, time_sf=t, 
                    GD=TRUE, player_name="Paul Pierce", player_legend=F)
})
dev.new(width=14, height=8)
wrap_plots(plot_list, ncol=2)

####################### save figures
pdf(file="./Figures/singleframes_sel.pdf", width=14, height=8, paper="special")
wrap_plots(plot_list, ncol=2)
dev.off()
#################################################################


####################### code for plotting Figure 5.13
df_segm_filtd <- df_segm[!filt, ]  # deleted situations
unique(df_segm_filtd$time)

t6 <- 95077	     # top left
t7 <- 98078		# top right
t8 <- 100358		# bottom left
t9 <- 110077		# bottom right

pacman::p_load(patchwork)
plot_list <- lapply(c(t6,t7,t8,t9), function(t) {
  plot_single_frame(data_long=tdata_long, data_wide=tdata_wide, time_sf=t, 
                    GD=TRUE, player_name="Paul Pierce", player_legend=F)
})

dev.new(width=14, height=8)
wrap_plots(plot_list, ncol=2)

####################### save figures
pdf(file="./Figures/singleframes_del.pdf", width=14, height=8, paper="special")
wrap_plots(plot_list, ncol=2)
dev.off()
#################################################################


grav0 <- plyr_nposs_gravity(tdata_wide, nposs=no_poss, player=player_name, delta=5, min_n=10) %>%
  filter(wA>0 & wB>0 & wC>0)

nposs_count <- tdata_long %>%
  filter(name %in% player_name) %>%
  group_by(nposs) %>%
  summarise(n=n()) %>%
  arrange(-n) %>%
  filter(n>500) %>%
  as.data.frame()

nposs_set <- nposs_count$nposs

grav <- plyr_gravity(data_wide=tdata_wide, player=player_name, np_set=nposs_set) %>%
  filter(wA>0 & wB>0 & wC>0)

p_grav <- ggplot(data=grav, aes(x=wB)) +
  geom_density(linewidth=0.8, bw = 0.04, fill="#CC4678FF") +
  labs(y="", x="Gravity") +
  xlim(c(0,1)) +
  theme_minimal()

dev.new(width=15, height=4)
print(p_grav)

####################### save figures
pdf(file="./Figures/plyrgravity.pdf", width=7.5, height=2.5, paper="special")
print(p_grav)
dev.off()
#################################################################

pacman::p_load(ggridges)

##### 
grav_bcarr <- grav %>% 
  group_by(bcarrName) %>%
  mutate(n=n(),ds=ifelse(wA<median(wA),"ds_low","ds_high")) %>%
  filter(n>100) %>%
  ungroup() %>%
  as.data.frame()

p_ridge1 <- ggplot(data=grav_bcarr, aes(x=wB, y=ds, fill=after_stat(x))) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01, 
                               show.legend=F, linewidth=1, bandwidth = 0.08) +
  scale_fill_viridis_c(option = "C") +
  labs(y="", x="Gravity") +
  xlim(c(0,1)) +
  theme_minimal()

dev.new(width=5, height=7)
print(p_ridge1)

####################### save figures
pdf(file="./Figures/ridgeplot_gravity1.pdf", width=5, height=7, paper="special")
print(p_ridge1)
dev.off()
#################################################################

p_ridge2 <- ggplot(data=grav_bcarr, aes(x=wB, y=bcarrName, fill=after_stat(x))) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01, 
                               show.legend=F, linewidth=1, bandwidth = 0.08) +
  scale_fill_viridis_c(option = "C") +
  labs(y="", x="Gravity") +
  xlim(c(0,1)) +
  theme_minimal()

dev.new(width=5, height=7)
print(p_ridge2)

####################### save figures
pdf(file="./Figures/ridgeplot_gravity2.pdf", width=5, height=7, paper="special")
print(p_ridge2)
dev.off()
#################################################################

plyr_count <- tdata_long %>%
  filter(name!="Ballcarrier" & name!="Ball") %>%
  group_by(name) %>%
  summarise(n=n(),team=unique(group2)) %>%
  select(name,n,team) %>%
  arrange(-n) %>%
  filter(n>20000) %>%
  as.data.frame()

player_set <- plyr_count$name

nposs_count <- tdata_long %>%
  filter(name %in% player_set) %>%
  group_by(nposs) %>%
  summarise(n=n()) %>%
  arrange(-n) %>%
  filter(n>500) %>%
  as.data.frame()

nposs_set <- nposs_count$nposs

GD_all_plyrs <- lapply(player_set, function(plyrk){
  plyr_gravity(data_wide=tdata_wide, player=plyrk, np_set=nposs_set)
})

GD_all_plyrs <- do.call(rbind, GD_all_plyrs) %>%
  filter(wA>0 & wB>0 & wC>0)

GD_all_plyrs <- merge(GD_all_plyrs, plyr_count, by="name") 

GD_all_plyrs_h <- GD_all_plyrs %>% filter(team=='home')
GD_all_plyrs_a <- GD_all_plyrs %>% filter(team=='away')

p_ridge3 <- ggplot(data=GD_all_plyrs_h, aes(x=wB, y=name, fill=after_stat(x))) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01, 
                               show.legend=F, linewidth=1, bandwidth = 0.04) +
  scale_fill_viridis_c(option = "C") +
  labs(y="", x="Gravity") +
  xlim(c(0,1)) +
  theme_minimal()

dev.new(width=5, height=7)
print(p_ridge3)

####################### save figures
pdf(file="./Figures/ridgeplot_gravity3.pdf", width=5, height=7, paper="special")
print(p_ridge3)
dev.off()
#################################################################

p_ridge4 <- ggplot(data=GD_all_plyrs_a, aes(x=wB, y=name, fill=after_stat(x))) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01, 
                               show.legend=F, linewidth=1, bandwidth = 0.04) +
  scale_fill_viridis_c(option = "C") +
  labs(y="", x="Gravity") +
  xlim(c(0,1)) +
  theme_minimal()

dev.new(width=5, height=7)
print(p_ridge4)

####################### save figures
pdf(file="./Figures/ridgeplot_gravity4.pdf", width=5, height=7, paper="special")
print(p_ridge4)
dev.off()
#################################################################

GD_bcarr_plyrs <- GD_all_plyrs %>% 
  group_by(bcarrName) %>%
  mutate(n=n()) %>%
  filter(n>100) %>%
  ungroup() %>%
  as.data.frame()

GD_bcarr_plyrs_h <- GD_bcarr_plyrs %>% filter(team=='home')
GD_bcarr_plyrs_a <- GD_bcarr_plyrs %>% filter(team=='away')

p_ridge5 <- ggplot(data=GD_bcarr_plyrs_h, aes(x=wA, y=bcarrName, fill=after_stat(x))) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01, 
                               show.legend=F, linewidth=1, bandwidth = 0.04) +
  scale_fill_viridis_c(option = "C") +
  labs(y="", x="Distraction") +
  xlim(c(0,.5)) +
  theme_minimal()

dev.new(width=5, height=7)
print(p_ridge5)

####################### save figures
pdf(file="./Figures/ridgeplot_distraction1.pdf", width=5, height=7, paper="special")
print(p_ridge5)
dev.off()
#################################################################

p_ridge6 <- ggplot(data=GD_bcarr_plyrs_a, aes(x=wA, y=bcarrName, fill=after_stat(x))) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01, 
                               show.legend=F, linewidth=1, bandwidth = 0.04) +
  scale_fill_viridis_c(option = "C") +
  labs(y="", x="Distraction") +
  xlim(c(0,.5)) +
  theme_minimal()

dev.new(width=5, height=7)
print(p_ridge6)

####################### save figures
pdf(file="./Figures/ridgeplot_distraction2.pdf", width=5, height=7, paper="special")
print(p_ridge6)
dev.off()
#################################################################
