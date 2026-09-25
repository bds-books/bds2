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
# 7.2 Ball detection data analysis
######################

######################
# 7.2.3 Inferential phase
######################

rm(list=ls())
graphics.off()

pacman::p_load(plotly,ggplot2,dplyr)
source("additional_functions.r")
load(file="./Data/ball_trajectories.Rdata")
load(file="./Data/list_traj.Rdata")

### Filter trajectories based on R2

dts_R2 <- lapply(traj_fit, function(x) {
  data.frame(id=x$df_pred$id[1],
             R2=summary(x$models$mod_z)$r.squared)  }) %>%
  do.call(rbind, .)

p1 <- ggplot(data=dts_R2) + 
  geom_histogram(aes(x=R2, y=after_stat(density)), bins=100, color="white") + 
  labs(x=expression(R^2), y = "Density") +
  theme_bw()

p2 <- ggplot(data=dts_R2) + 
  geom_boxplot(aes(x="", y=R2)) +
  labs(x="", y=expression(R^2)) +
  coord_flip() +
  theme_bw()

pacman::p_load(patchwork)
p12 <- p1 / p2
print(p12)

####################### save figures
# dir.create("./Figures", showWarnings=FALSE)

pdf(file="./Figures/R2distrib.pdf", width=9, height=7, paper="special")
print(p12)
dev.off()
#################################################################


### select a sample of 4 trajectories with R2 < 0.9
IDs <- unique(dts_R2 %>% dplyr::filter(R2<0.9) %>% dplyr::select(id))
subIDs <- IDs$id[seq(10, by=20, length.out=4)]

df_4traj <- lapply(subIDs, function(idk) {
  dfk <- dtset %>% dplyr::filter(id==idk) %>% dplyr::select(x, y, z, time, made_miss, id)
  trjk_stats <- trajectory_analysis(dfk)
  data.frame(trjk_stats$df_pred, R2=summary(trjk_stats$models$mod_z)$r.squared)
})


# plot
trj_lprop  <- list(width = 4, color="blue")
trj_mprop  <- list(size = 5, color="blue")
quad_mod_prop  <- list(width = 4, color="red")
p3 <- plot_ly() %>%
  add_trace(x = ~x, y = ~y, z = ~z, data=df_4traj[[1]],
            type="scatter3d", mode="lines+markers",
            line=trj_lprop, marker=trj_mprop, scene="scene") %>%
  add_trace(x = ~x_pred, y = ~y_pred, z = ~z_pred, 
            data=df_4traj[[1]], type="scatter3d", mode="lines",
            line=quad_mod_prop, scene = "scene") %>%
  add_trace(x = ~x, y = ~y, z = ~z, data=df_4traj[[2]],
            type="scatter3d", mode="lines+markers",
            line=trj_lprop, marker=trj_mprop, scene="scene2") %>%
  add_trace(x = ~x_pred, y = ~y_pred, z = ~z_pred, 
            data=df_4traj[[2]], type="scatter3d", mode="lines",
            line=quad_mod_prop, scene = "scene2") %>%
  add_trace(x = ~x, y = ~y, z = ~z, data=df_4traj[[3]],
            type="scatter3d", mode="lines+markers",
            line=trj_lprop, marker=trj_mprop, scene="scene3") %>%
  add_trace(x = ~x_pred, y = ~y_pred, z = ~z_pred, 
            data=df_4traj[[3]], type="scatter3d", mode="lines",
            line=quad_mod_prop, scene = "scene3") %>%
  add_trace(x = ~x, y = ~y, z = ~z, data=df_4traj[[4]],
            type="scatter3d", mode="lines+markers",
            line=trj_lprop, marker=trj_mprop, scene="scene4") %>%
  add_trace(x = ~x_pred, y = ~y_pred, z = ~z_pred, 
            data=df_4traj[[4]], type="scatter3d", mode="lines",
            line=quad_mod_prop, scene = "scene4") %>%
  layout(scene  = list(domain = list(x=c(0,0.5), y=c(0.5,1))),
         scene2 = list(domain = list(x=c(0.5,1), y=c(0.5,1))),
         scene3 = list(domain = list(x=c(0,0.5), y=c(0,0.5))),
         scene4 = list(domain = list(x=c(0.5,1), y=c(0,0.5))))

print(p3)

p3a <- plot_ly() %>%
  plot_single_traj(df_4traj[[1]], trace=T, fig=., scene="scene") %>%
  plot_single_traj(df_4traj[[2]], trace=T, fig=., scene="scene2") %>%
  plot_single_traj(df_4traj[[3]], trace=T, fig=., scene="scene3") %>%
  plot_single_traj(df_4traj[[4]], trace=T, fig=., scene="scene4") %>%
  layout(scene  = list(domain = list(x=c(0,0.5), y=c(0.5,1))),
         scene2 = list(domain = list(x=c(0.5,1), y=c(0.5, 1))),
         scene3 = list(domain = list(x=c(0,0.5), y=c(0,0.5))),
         scene4 = list(domain = list(x=c(0.5,1), y=c(0,0.5))))


titles <- lapply(seq_along(subIDs), function(k) {
  xpos <- c(0.25, 0.75, 0.25, 0.75)
  ypos <- c("1","1","0.5","0.5")
  dfk <- dtset %>%
    dplyr::filter(id==subIDs[k]) %>%
    dplyr::select(x, y, z, time, made_miss, id)
  made_miss <- ifelse(unique(dfk$made_miss)==1,
                      "MADE", "MISS")
  title <- paste0("Trajectory ID: ",
                  subIDs[k], " - ", made_miss)
  annotationk <- list(text=title, x = xpos[k],
                      y = ypos[k], xref = "paper",
                      yref = "paper", showarrow=FALSE,
                      font=list(size=14))
  return(annotationk) })

p3a <- p3a %>%
  layout(annotations = titles)

print(p3a)

# Filter trajectories with R2<0.9
pacman::p_load(purrr)
traj_fit_clean <- keep(traj_fit, function(x) !(unique(x$df_pred$id) %in% IDs$id))


### Examine vertex-midpoint distance

df_dist <- lapply(traj_fit_clean, function(x) {
  data.frame(id=x$df_pred$id[1], x$distV, 
             made_miss=x$df_pred$made_miss[1]) }) %>%
  do.call(rbind, .) %>%
  mutate(made_miss = factor(made_miss, levels=c(0,1), 
                            labels=c("Miss", "Made")))

p4 <- ggplot(data=df_dist) + 
  geom_histogram(aes(x=dist_VvsM, y=after_stat(density)), bins=100, color="white") +
  labs(x="Vertex-midpoint distance", y = "Density") +
  theme_bw()

p5 <- ggplot(data=df_dist) + 
  geom_boxplot(aes(x="", y=dist_VvsM)) +
  labs(x="", y="Vertex-midpoint distance") +
  coord_flip() +
  theme_bw()

p45 <- p4 / p5
print(p45)

####################### save figures
pdf(file="./Figures/DISTdistrib.pdf", width=9, height=7, paper="special")
print(p45)
dev.off()
#################################################################

IDs    <- unique(df_dist  %>% dplyr::filter(dist_VvsM>3) %>% dplyr::select(id))
subIDs <- IDs$id

df_4traj <- lapply(subIDs, function(idk) {
  dfk <- dtset %>% dplyr::filter(id==idk) %>% dplyr::select(x, y, z, time, made_miss, id)
  trjk_analysis <- trajectory_analysis(dfk)
  trjk_analysis$df_pred
})

p6 <- plot_ly() %>%
  plot_single_traj(df_4traj[[1]], trace=T, fig=., scene="scene") %>%
  plot_single_traj(df_4traj[[2]], trace=T, fig=., scene="scene2") %>%
  plot_single_traj(df_4traj[[3]], trace=T, fig=., scene="scene3") %>%
  plot_single_traj(df_4traj[[4]], trace=T, fig=., scene="scene4") %>%
  layout(scene  = list(domain = list(x=c(0,0.5), y=c(0.5,1))),
         scene2 = list(domain = list(x=c(0.5,1), y=c(0.5, 1))),
         scene3 = list(domain = list(x=c(0,0.5), y=c(0,0.5))),
         scene4 = list(domain = list(x=c(0.5,1), y=c(0,0.5))))

titles <- lapply(seq_along(subIDs), function(k) {
  xpos <- c(0.25, 0.75, 0.25, 0.75)
  ypos <- c("1","1","0.5","0.5")
  dfk <- dtset %>%
    dplyr::filter(id==subIDs[k]) %>%
    dplyr::select(x, y, z, time, made_miss, id)
  made_miss <- ifelse(unique(dfk$made_miss)==1,
                      "MADE", "MISS")
  title <- paste0("Trajectory ID: ",
                  subIDs[k], " - ", made_miss)
  annotationk <- list(text=title, x = xpos[k],
                      y = ypos[k], xref = "paper",
                      yref = "paper", showarrow=FALSE,
                      font=list(size=14))
  return(annotationk) })


p6 <- p6 %>% layout(annotations = titles)

print(p6)


# Predictors
traj_stats <- lapply(traj_fit_clean, function(objk) {
  dfTR  <- objk$df_pred
  dfDV  <- objk$distV
  dfSA  <- as.data.frame(t(objk$speed_angle))
  dfDir <- objk$directions
  dfk <- data.frame(id=dfTR$id[1], made_miss=dfTR$made_miss[1],
                    x0=dfTR$x_pred[1], y0=dfTR$y_pred[1], z0=dfTR$z_pred[1],
                    xV=dfDV$xV, yV=dfDV$yV, zV=dfDV$zV,
                    distVM=dfDV$dist_VvsM*dfDV$sign_V_pos,
                    dfSA, angle_dir=dfDir$angle_dir) %>%
    mutate(dist0b = sqrt(x0^2+y0^2+z0^2),
           made_miss=factor(made_miss, levels=c(0,1), labels=c("Miss","Made"))) 
})
traj_stats <- do.call(rbind, traj_stats)

save(traj_stats, file="./Data/traj_stats.Rdata")

######## example of univariate exploratory analysis: distVM

tapply(traj_stats$distVM, traj_stats$made_miss, quantile, probs = c(0.25, 0.5, 0.75))

p7 <- ggplot(data=traj_stats, aes(x=distVM, fill=made_miss, color=made_miss)) +
  geom_histogram(aes(y=after_stat(density)), position = "identity", 
                 show.legend=F, bins=50, color="white", alpha=0.3) +
  geom_density(show.legend=T, fill="transparent", linewidth=1, bw=0.065) +
  xlim(-2.5, 2.5) + labs(color="", fill="") +
  theme_bw() +
  theme(legend.position="inside", legend.position.inside=c(0.9,0.9))

print(p7)

####################### save figures
pdf(file="./Figures/distVMdistrib.pdf", width=8, height=5, paper="special")
print(p7)
dev.off()
#################################################################

interval <- ifelse(traj_stats$distVM < -1 | traj_stats$distVM > 0, "outside", "inside")
tab <- table(interval,traj_stats$made_miss)
made_inside <- tab[1,2] / sum(tab[1,])  
made_outside <- tab[2,2] / sum(tab[2,])
chisq.test(tab)

wilcox.test(distVM~made_miss, data=traj_stats)


######## example of bivariate exploratory analysis: angle and angle_dir

pacman::p_load(ggside)
p8 <- ggplot(data=traj_stats, aes(x=angle, y=angle_dir, color=made_miss)) +
  geom_point(alpha=0.3, show.legend=F) +
  geom_xsidehistogram(color="black", alpha = 0.5, show.legend=F) +
  geom_ysidehistogram(color="black", alpha = 0.5, show.legend=F) +
  facet_grid(made_miss~.) +
  labs(x="Release angle (°)", y="Angle between ideal and actual line (°)") +
  theme_bw() + theme_ggside_void()

print(p8)

####################### save figures
pdf(file="./Figures/angles1.pdf", width=8, height=8, paper="special")
print(p8)
dev.off()
#################################################################


p9 <- ggplot(data=traj_stats, aes(x=angle, y=angle_dir)) +
  stat_density_2d(aes(fill = after_stat(level)), geom = "polygon", alpha = 0.6) +
  scale_fill_viridis_c() +
  facet_grid(made_miss~.) +
  labs(x="Release angle (°)", y="Angle between ideal and actual line (°)") +
  labs(fill="") +
  theme_bw() + theme(legend.position="inside", legend.position.inside=c(0.9, 0.8))

print(p9)

####################### save figures
pdf(file="./Figures/angles2.pdf", width=8, height=8, paper="special")
print(p9)
dev.off()
#################################################################

pacman::p_load(MASS)

# Density estimation
df_made <- traj_stats %>% dplyr::filter(made_miss=="Made")
dens_made <- kde2d(x=df_made$angle, y=df_made$angle_dir, n=50)
df_miss <- traj_stats %>% dplyr::filter(made_miss=="Miss")
dens_miss <- kde2d(x=df_miss$angle, y=df_miss$angle_dir, n=50)


# Set axes properties and titles
xax.prop <- list(title="Release angle", range = c(37.5,55))
yax.prop <- list(title="Angle between ideal and actual line", range = c(0,3))
zax.prop <- list(title="Density", range = c(0,0.25))
titles <- list(list(text="Made", x=0.25, y=.8), 
               list(text="Miss", x=0.75, y=.8))


fig <- plot_ly() %>%
  add_trace(x= ~x, y= ~y, z= ~t(z),  data=dens_made,
            type = "surface", colorscale = "Viridis",
            scene="scene", cmin=0, cmax=0.18, showscale=FALSE) %>%
  add_trace(x= ~x, y= ~y, z= ~t(z),  data=dens_miss,
            type = "surface", colorscale = "Viridis",
            scene="scene2", cmin=0, cmax=0.18, showscale=FALSE) %>%
  layout(
    scene  = list(domain=list(x = c(0, 0.5), y = c(0, 1)),
                  xaxis=xax.prop, yaxis=yax.prop, zaxis=zax.prop),
    scene2 = list(domain=list(x = c(0.5, 1), y = c(0, 1)),
                  xaxis=xax.prop, yaxis=yax.prop, zaxis=zax.prop),
    annotations=titles)
print(fig)


intervalb <- ifelse((traj_stats$angle < 45 | traj_stats$distVM > 47.5) & traj_stats$angle_dir>0.75, "outside", "inside")
tabb <- table(intervalb,traj_stats$made_miss)
# made_inside <- tabb[1,2] / sum(tabb[1,])  
# made_outside <- tabb[2,2] / sum(tabb[2,])
chisq.test(tabb)


###### Local Regression (log-odds)

# numeric ouput
df_stats <- traj_stats %>% mutate(made_miss=as.numeric(made_miss)-1)

##############
# angle_dir
##############

bp <- ggplot(data=df_stats) + 
  geom_boxplot(aes(x="", y=angle_dir)) +
  labs(x="", y="Angle between the actual and the ideal shot line") +
  coord_flip() +
  theme_bw()

print(bp)

Q1 <- quantile(df_stats$angle_dir, 0.25, na.rm = TRUE)
Q3 <- quantile(df_stats$angle_dir, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1
lower_wskr <- Q1 - 1.5 * IQR
upper_wskr <- Q3 + 1.5 * IQR

df_lfit <- df_stats %>%
  dplyr::filter(angle_dir >= lower_wskr & angle_dir <= upper_wskr)

hist_obj <- hist(df_lfit[["angle_dir"]], breaks=40, plot=FALSE)

df_logit <- df_lfit %>%
  mutate(x_cut=cut(angle_dir, breaks=hist_obj$breaks,
                   include.lowest=T)) %>%
  group_by(x_cut, .drop=FALSE) %>%
  summarise(prop = ifelse(n()==0, 0, mean(made_miss))) %>%
  ungroup() %>%
  mutate(center=hist_obj$mids) %>%
  dplyr::filter(prop!=0) %>%
  mutate(logit = log(prop / (1 - prop)))

p10 <- ggplot(df_logit, aes(x = center, y = logit)) +
  geom_point(color="red", size=2) +
  labs(x="Angle between the actual and the ideal shot line",
       y="Estimated logit of P[made | angle_dir]") +
  theme_bw()

print(p10)

####################### save figures
pdf(file="./Figures/logit_angledir1.pdf", width=4, height=4, paper="special")
print(p10)
dev.off()
#################################################################
# R base graphics engine

pacman::p_load(locfit)

# smoothing parameter
sp <- 0.5
lfit <- locfit(made_miss ~ lp(angle_dir, nn=sp), family="binomial", data=df_lfit)

locfit_pred <- df_lfit %>%
  summarise(x_grid=list(seq(min(angle_dir), max(angle_dir), length.out = 100))) %>%
  tidyr::unnest(x_grid) %>%
  mutate(p_pred = predict(lfit, newdata=data.frame(x_grid) %>%
                            rename(angle_dir = x_grid), type="response"),
         logit_pred = log(p_pred / (1 - p_pred))) %>%
  dplyr::filter()

p11 <- p10 +
  geom_line(data=locfit_pred, aes(x=x_grid, y=logit_pred),
            colour = "blue", linewidth = 1, inherit.aes=F)

print(p11)

####################### save figures
pdf(file="./Figures/logit_angledir2.pdf", width=4, height=4, paper="special")
print(p11)
dev.off()
#################################################################


##############
# dist0b
##############
bp <- ggplot(data=df_stats) + 
  geom_boxplot(aes(x="", y=dist0b)) +
  labs(x="", y="") +
  coord_flip() +
  theme_bw()
print(bp)

Q1 <- quantile(df_stats$dist0b, 0.25, na.rm = TRUE)
Q3 <- quantile(df_stats$dist0b, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1
lower_wskr <- Q1 - 1.5 * IQR
upper_wskr <- Q3 + 1.5 * IQR

df_lfit <- df_stats %>%
  dplyr::filter(dist0b >= lower_wskr & dist0b <= upper_wskr)

df_pred <- plot_locreg(df_lfit, yvar="made_miss", xvar="dist0b", 
                       xtitle="Distance from the basket", breaks=40, nn=0.7)

p <- df_pred[["plot"]]   

print(p)                  

####################### save figures
pdf(file="./Figures/logit_dist0b.pdf", width=4, height=4, paper="special")
print(p)
dev.off()
#################################################################


##############
# angle
##############
bp <- ggplot(data=df_stats) + 
  geom_boxplot(aes(x="", y=angle)) +
  labs(x="", y="") +
  coord_flip() +
  theme_bw()
print(bp)

Q1 <- quantile(df_stats$angle, 0.25, na.rm = TRUE)
Q3 <- quantile(df_stats$angle, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1
lower_wskr <- Q1 - 1.5 * IQR
upper_wskr <- Q3 + 1.5 * IQR

df_lfit <- df_stats %>%
  dplyr::filter(angle >= lower_wskr & angle <= upper_wskr)

df_pred <- plot_locreg(df_lfit, yvar="made_miss", xvar="angle", 
                       xtitle="Release angle", breaks=40, nn=0.7)

# note: do not plot an outlier extreme value
# print(df_pred[["plot"]])
p <- df_pred[["plot"]] + ylim(NA,-0.25)

print(p)  

####################### save figures
pdf(file="./Figures/logit_angle.pdf", width=4, height=4, paper="special")
print(p)
dev.off()
#################################################################

############
# isp.feet
############
bp <- ggplot(data=df_stats) + 
  geom_boxplot(aes(x="", y=isp.feet)) +
  labs(x="", y="") +
  coord_flip() +
  theme_bw()
print(bp)

Q1 <- quantile(df_stats$isp.feet, 0.25, na.rm = TRUE)
Q3 <- quantile(df_stats$isp.feet, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1
lower_wskr <- Q1 - 1.5 * IQR
upper_wskr <- Q3 + 1.5 * IQR

df_lfit <- df_stats %>%
  dplyr::filter(isp.feet >= lower_wskr & isp.feet <= upper_wskr)

df_pred <- plot_locreg(df_lfit, yvar="made_miss", xvar="isp.feet", 
                       xtitle="Initial speed", breaks=40, nn=0.7)

p <- df_pred[["plot"]]   

print(p)     

####################### save figures
pdf(file="./Figures/logit_isp.pdf", width=4, height=4, paper="special")
print(p)
dev.off()
#################################################################


##########
# distVM
##########
bp <- ggplot(data=df_stats) + 
  geom_boxplot(aes(x="", y=distVM)) +
  labs(x="", y="") +
  coord_flip() +
  theme_bw()
print(bp)

Q1 <- quantile(df_stats$distVM, 0.25, na.rm = TRUE)
Q3 <- quantile(df_stats$distVM, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1
lower_wskr <- Q1 - 1.5 * IQR
upper_wskr <- Q3 + 1.5 * IQR

df_lfit <- df_stats %>%
  dplyr::filter(distVM >= lower_wskr & distVM <= upper_wskr)

df_pred <- plot_locreg(df_lfit, yvar="made_miss", xvar="distVM", 
                       xtitle="Distance vertex-midpoint", breaks=40, nn=0.7)

p <- df_pred[["plot"]]   

print(p)     

####################### save figures
pdf(file="./Figures/logit_distVM.pdf", width=4, height=4, paper="special")
print(p)
dev.off()
#################################################################


