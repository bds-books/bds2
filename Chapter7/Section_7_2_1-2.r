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
rm(list=ls())
graphics.off()

pacman::p_load(plotly,ggplot2,dplyr)
load(file="./Data/ball_trajectories.Rdata")

#### example of a made shot
IDs <- dtset %>% dplyr::filter(made_miss==1) %>% dplyr::select(id) %>% unique() 
ID_traj <- IDs$id[1]
df <- dtset %>% dplyr::filter(id==ID_traj)

######################
# 7.2.1 Exploratory phase
######################


#### 3D visualization

# trajectory
p <- plot_ly(df, x = ~x, y = ~y, z = ~z, 
             type= "scatter3d", mode = "lines+markers",
             line = list(width = 4, color = "blue"),
             marker = list(size = 5), showlegend = FALSE)

# np points to draw the rim
np <- 100
basket <- c(0, 0, 0)
angles <- seq(0, 2*pi, length.out = np)
radius <- 0.75
basket_rim <-  data.frame(x = basket[1] + radius * cos(angles),
                          y = basket[2] + radius * sin(angles),
                          z = rep(basket[3], np) )

# add rim and rim's center
p <- p %>%
  add_trace(x = ~x, y = ~y, z = ~z, data = basket_rim, 
            type = "scatter3d", mode = "lines", showlegend = FALSE,
            line = list(width = 4, color = "tomato"), inherit = FALSE) %>%
  add_trace(x = basket[1], y = basket[2], z = basket[3],
            type = "scatter3d", mode = "markers",
            marker = list(size = 8, color = "tomato"),
            inherit = FALSE, showlegend = FALSE)

# general settings
made_miss <- ifelse(unique(df$made_miss)==1, "MADE", "MISS")
title <- paste0("Trajectory ID: ", ID_traj, " - ", made_miss)
p <- p %>% layout(
  title = list(text = title, y = 0.8),
  scene = list(
    aspectmode = "data",
    aspectratio = list(x = 1, y = 1, z = 1),
    camera = list(eye=list(x = 0, y = 1.9, z = 1.1)),
    margin = list(l = 5, r = 5, t = 5, b = 5),
    xaxis = list(title = "X"),
    yaxis = list(title = "Y"),
    zaxis = list(title = "Z"),
    showlegend = FALSE))

print(p)

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
save_image(p, file="./Figures/trajectory1.pdf", width=1200,height=800)
#################################################################

######################
# 7.2.2 Descriptive phase
######################

#### Fit 3D parabola
############################
# x = b0x + b1x * time
# y = b0y + b1y * time
# z = b0z + b1z * time + b2z * time^2
############################
lm_x <- lm(x~time, data=df)
coef_x <- coef(lm_x)
# names(coef_x) <- NULL
df$x_pred <- predict(lm_x)

lm_y <- lm(y~time, data=df)
coef_y <- coef(lm_y)
# names(coef_y) <- NULL
df$y_pred <- predict(lm_y)

lm_z <- lm(z ~ time + I(time^2), data=df)
coef_z <- coef(lm_z)
# names(coef_z) <- NULL
df$z_pred <- predict(lm_z)
df$z_resid <- residuals(lm_z)

summary(lm_x)
summary(lm_y)
summary(lm_z)


#### Compute additional parabolic trajectory features

# Ideal direction and midpoint 
start_point <- df[1, c("x_pred","y_pred","z_pred")]
x0 <- start_point$x_pred
y0 <- start_point$y_pred
z0 <- start_point$z_pred
# x0 <- coef_x[1]; y0 <- coef_y[1]; z0 <- coef_z[1]
ideal_dir <- data.frame(x=c(x0, basket[1]), 
                        y=c(y0, basket[2]), 
                        z=c(z0, basket[3]))

mid_point <- (start_point+basket)/2

# line slope (ideal direction)
m1 <- (basket[2]-y0)/(basket[1]-x0)

# line slope (actual direction)
tps <- nrow(df)
m2 <- (df$y_pred[tps]-y0)/(df$x_pred[tps]-x0)

# Angle between the two lines
angle_dir <- atan(abs((m2-m1)/(1+m1*m2)))*180/pi


# Initial speed (ft/sec or m/sec)
isp.feet <- sqrt(coef_x[2]^2+coef_y[2]^2+coef_z[2]^2)
isp.meters <- isp.feet*0.3048

# Release angle
angle <- asin(abs(coef_z[2])/isp.feet)*180/pi

# Vertex
timeV <- -coef_z[2]/(2*coef_z[3])
xV    <- coef_x[1] + coef_x[2] * timeV
yV    <- coef_y[1] + coef_y[2] * timeV
zV    <- -(coef_z[2]^2-4*coef_z[1]*coef_z[3])/(4*coef_z[3])

# Project V onto the ideal direction
m <- (basket[2] - y0)/(basket[1] - x0)
xV_proj <- (xV + m * yV - m^2 * basket[1] + m * basket[2])/(1 + m^2)
yV_proj <- (basket[2] + m * xV + m^2 * yV - m * basket[1])/(1 + m^2)
tV_proj <- (yV_proj - y0)/(basket[2] - y0)
zV_proj <- z0 + tV_proj * (basket[3] - z0)

# Distance between the projection of the vertex on the ideal direction and its midpoint.
xm <- mid_point$x
ym <- mid_point$y
zm <- mid_point$z
dist_VvsM <- sqrt((xm - xV_proj)^2 + (ym - yV_proj)^2) 
dist_m_C    <- sqrt((xm - basket[1])^2 + (ym - basket[2])^2) 
dist_proj_C <- sqrt((xV_proj - basket[1])^2 + (yV_proj - basket[2])^2)
sign_V_pos <- ifelse(dist_proj_C < dist_m_C, -1, 1) 

df_V  <- data.frame(xV, yV, zV, timeV, 
                    xV_proj, yV_proj, zV_proj, 
                    dist_VvsM, sign_V_pos, 
                    made_miss=df$made_miss[1], id=df$id[1])

#### Update 3D visualization
# add parabola, ideal (with midpoint) and actual direction,
# vertex, projection
p <- p %>%
  add_trace(x = ~x_pred, y = ~y_pred, z = ~z_pred, data = df, 
            type = "scatter3d", mode = "lines", showlegend = FALSE,
            line = list(width = 8, color = "tomato"),
            inherit = FALSE)%>%
  add_trace(x = ~x, y = ~y, z = ~z, data=ideal_dir, 
            type = "scatter3d", mode = "lines",
            line = list(width = 3, color="orange"),
            inherit = FALSE, showlegend = FALSE) %>%
  add_trace(x= ~x_pred, y= ~y_pred, z= ~z_pred, data=mid_point,
            type = "scatter3d", mode = "markers",
            marker = list(size = 4, color = "orange"), 
            inherit=F, showlegend = FALSE) %>%
  add_trace(x = ~c(x0,x_pred[tps]),
            y = ~c(y0,y_pred[tps]),
            z = ~c(z0,z_pred[tps]),
            data=df, type = "scatter3d", mode = "lines",
            showlegend = FALSE, line = list(width = 4,
                                            color = "blue"), inherit = FALSE) %>%
  add_trace(x = ~xV, y = ~yV, z = ~zV, data=df_V,
            type = "scatter3d", mode = "markers",
            marker = list(size = 8, color = "lawngreen"),
            inherit=F, showlegend = FALSE) %>%
  add_trace(x = ~xV_proj, y = ~yV_proj, z = ~zV_proj, data=df_V,
            type = "scatter3d", mode = "markers",
            marker = list(size = 8, color = "lawngreen"),
            inherit=F, showlegend = FALSE) %>%
  add_trace(x = ~c(xV,xV_proj),
            y = ~c(yV,yV_proj),
            z = ~c(zV,zV_proj),
            data = df_V, type = "scatter3d", mode = "lines",
            showlegend = FALSE,
            line = list(width = 4, color = "lawngreen", dash="dash"),
            inherit = FALSE)

print(p)


####################### save figure
save_image(p, file="./Figures/trajectory2.pdf", width=1200,height=800)
#################################################################

# use functions 
source("additional_functions.r")

dff <- df %>% dplyr::select(time,x,y,z) 

trajectory_analysis(dff)
plot_single_traj(dff)

dff <- df %>% dplyr::select(id,made_miss,time,x,y,z) 

trajectory_analysis(dff)
plot_single_traj(dff)

plot_single_traj(dff, speed_dir=T)

# apply the function to all the trajectoris
traj_ids <- unique(dtset$id)
traj_fit <- list(length(traj_ids), mode="list")
for (k in seq_along(traj_ids)) {
  idk <- traj_ids[k]
  dfk <- dtset %>% dplyr::filter(id==idk) %>% dplyr::select(x, y, z, time, made_miss, id)
  traj_fit[[k]] <- trajectory_analysis(dfk)
}

save(traj_fit,file="./Data/list_traj.Rdata")






