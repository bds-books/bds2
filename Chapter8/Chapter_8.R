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

# set Chapter8 as working directory
# setwd("....")

############################################
############################################
# PART 3                                   #
# Spatial performance analysis             #
############################################
############################################

##############################################################
# CHAPTER 8                                                  #
# Basketball performance maps based on court segmentation    #
##############################################################

rm(list = ls())
graphics.off()

#Load libraries and prepare data
pacman::p_load(BasketballAnalyzeR, dplyr)
load("./Data/NBAPbP_BDB.Rdata")
PbP <- PbPmanipulation(PbP.BDB.GSW.rs)

subdata <- PbP %>%
  filter(player == "Stephen Curry") %>%
  mutate(xx = original_x / 10, yy = original_y / 10 - 41.75)

#################################################################
### SECTION 8.1 Sector-based performance maps
#################################################################

# Code for plotting Figure 8.1 (right)
shotchart(
  data = subdata,
  x = "xx",
  y = "yy",
  z = "playlength",
  z.fun = median,
  num.sect = 5,
  type = "sectors",
  scatter = FALSE
)

####################### save figures
dir.create("./Figures", showWarnings = FALSE)
pdf(
  file = "./Figures/sectors.pdf",
  width = 6,
  height = 6,
  paper = "special"
)
shotchart(
  data = subdata,
  x = "xx",
  y = "yy",
  z = "playlength",
  z.fun = median,
  num.sect = 5,
  type = "sectors",
  scatter = FALSE
)
dev.off()
#################################################################

#################################################################
### SECTION 8.2 Shot charts
#################################################################

# Code for plotting Figure 8.2
shotchart(
  data = subdata,
  x = "xx",
  y = "yy",
  z = "result",
  type = NULL,
  scatter = TRUE
)
shotchart(
  data = subdata,
  x = "xx",
  y = "yy",
  z = "playlength",
  z.fun = mean,
  num.sect = 5,
  type = "sectors",
  scatter = TRUE
)
shotchart(
  data = subdata,
  x = "xx",
  y = "yy",
  z = "playlength",
  z.fun = mean,
  num.sect = 5,
  type = "sectors",
  scatter = FALSE,
  result = "result"
)
shotchart(
  data = subdata,
  x = "xx",
  y = "yy",
  z = "points",
  z.fun = mean,
  num.sect = 5,
  type = "sectors",
  scatter = FALSE
)

######################## save figures
pacman::p_load(gridExtra)
plot1 <- shotchart(
  data = subdata,
  x = "xx",
  y = "yy",
  z = "result",
  type = NULL,
  scatter = TRUE
)
plot2 <- shotchart(
  data = subdata,
  x = "xx",
  y = "yy",
  z = "playlength",
  z.fun = mean,
  num.sect = 5,
  type = "sectors",
  scatter = TRUE
)
plot3 <- shotchart(
  data = subdata,
  x = "xx",
  y = "yy",
  z = "playlength",
  z.fun = mean,
  num.sect = 5,
  type = "sectors",
  scatter = FALSE,
  result = "result"
)
plot4 <- shotchart(
  data = subdata,
  x = "xx",
  y = "yy",
  z = "points",
  z.fun = mean,
  num.sect = 5,
  type = "sectors",
  scatter = FALSE
)
pdf(
  file = "./Figures/shotchartsSC.pdf",
  width = 12,
  height = 12,
  paper = "special"
)
grid.arrange(plot1, plot2, plot3, plot4, ncol = 2)
dev.off()
#################################################################

# Code for plotting Figure 8.3
subdata <- PbP %>%
  filter(player == "Andrew Wiggins") %>%
  mutate(xx = original_x / 10, yy = original_y / 10 - 41.75)

shotchart(
  data = subdata,
  x = "xx",
  y = "yy",
  z = "result",
  type = NULL,
  scatter = TRUE
)
shotchart(
  data = subdata,
  x = "xx",
  y = "yy",
  z = "playlength",
  z.fun = mean,
  num.sect = 5,
  type = "sectors",
  scatter = TRUE
)
shotchart(
  data = subdata,
  x = "xx",
  y = "yy",
  z = "playlength",
  z.fun = mean,
  num.sect = 5,
  type = "sectors",
  scatter = FALSE,
  result = "result"
)
shotchart(
  data = subdata,
  x = "xx",
  y = "yy",
  z = "points",
  z.fun = mean,
  num.sect = 5,
  type = "sectors",
  scatter = FALSE
)

####################### save figures
pacman::p_load(gridExtra)
plot1 <- shotchart(
  data = subdata,
  x = "xx",
  y = "yy",
  z = "result",
  type = NULL,
  scatter = TRUE
)
plot2 <- shotchart(
  data = subdata,
  x = "xx",
  y = "yy",
  z = "playlength",
  z.fun = mean,
  num.sect = 5,
  type = "sectors",
  scatter = TRUE
)
plot3 <- shotchart(
  data = subdata,
  x = "xx",
  y = "yy",
  z = "playlength",
  z.fun = mean,
  num.sect = 5,
  type = "sectors",
  scatter = FALSE,
  result = "result"
)
plot4 <- shotchart(
  data = subdata,
  x = "xx",
  y = "yy",
  z = "points",
  z.fun = mean,
  num.sect = 5,
  type = "sectors",
  scatter = FALSE
)
pdf(
  file = "./Figures/shotchartsAW.pdf",
  width = 12,
  height = 12,
  paper = "special"
)
grid.arrange(plot1, plot2, plot3, plot4, ncol = 2)
dev.off()
#################################################################

# Code for plotting Figure 8.4
subdata <- PbP %>%
  filter(player == "Stephen Curry") %>%
  mutate(xx = original_x / 10, yy = original_y / 10 - 41.75)

shotchart(
  data = subdata,
  x = "xx",
  y = "yy",
  z = "result",
  type = "density-polygons"
)
shotchart(
  data = subdata,
  x = "xx",
  y = "yy",
  z = "result",
  type = "density-polygons",
  scatter = TRUE
)
shotchart(
  data = subdata,
  x = "xx",
  y = "yy",
  z = "result",
  type = "density-raster"
)

pacman::p_load(hexbin)  
shotchart(
  data = subdata,
  x = "xx",
  y = "yy",
  z = "result",
  type = "density-hexbin"
)

####################### save figures
pacman::p_load(gridExtra)
plot1 <- shotchart(
  data = subdata,
  x = "xx",
  y = "yy",
  z = "result",
  type = "density-polygons"
)
plot2 <- shotchart(
  data = subdata,
  x = "xx",
  y = "yy",
  z = "result",
  type = "density-polygons",
  scatter = TRUE
)
plot3 <- shotchart(
  data = subdata,
  x = "xx",
  y = "yy",
  z = "result",
  type = "density-raster"
)
plot4 <- shotchart(
  data = subdata,
  x = "xx",
  y = "yy",
  z = "result",
  type = "density-hexbin"
)
pdf(
  file = "./Figures/shotdensitySC.pdf",
  width = 12,
  height = 12,
  paper = "special"
)
grid.arrange(plot1, plot2, plot3, plot4, ncol = 2)
dev.off()
#################################################################


#################################################################
### SECTION 8.3 Scoring probability maps based on CART
#################################################################

#################################################################
### SECTION 8.3.1 CART applied on Cartesian coordinates of the shots
#################################################################

#Figure 8.5
###################
# Load and preprocess data
###################
pacman::p_load(rpart, rattle)
source("additional_functions.r")

# Reference point in the court
Xc <- 0
Yc <- -41.75
PbP <- PbP %>%
  mutate(x = original_x / 10 + Xc,
         x = ifelse(x == 0, 0.0001, x),
         y = original_y / 10 + Yc)

player <- "Stephen Curry"
data.player <- PbP %>%
  filter(player == {{player}} &
           result != "" & event_type != "free throw") %>%
  mutate(result = droplevels(result), Made = 100 * (2 - as.numeric(result)))

#####################
# CART
#####################
# CART parameters
cpar <- 0.01 	# CART complexity parameter
pn <- 0.1		# Observations (%) in minnodesize argument
nshots.player <- nrow(data.player)
fit <- rpart(
  result ~ x + y,
  data = data.player,
  model = T,
  control = rpart.control(cp = cpar, minsplit = round(nshots.player *
                                                        pn))
)

#Figure 8.5 top
fancyRpartPlot(fit, sub = "")

out <- plot_cartesian_spatial_tree(
  fit,
  data = data.player,
  z = "Made",
  z.name = "Scoring\nprobability %",
  FUN.z = mean,
  print.z = T,
  round.print.z = 0,
  num_interv_legend = 8,
  low_lim_colormap = 0,
  up_lim_colormap = 100
)
#Figure 8.5 bottom
print(out$plotTree)

######################## save figures
pdf(
  file = "./Figures/CART-SC01-tree.pdf",
  width = 10,
  height = 10,
  paper = "special"
)
fancyRpartPlot(fit, sub = "")
dev.off()

pdf(
  file = "./Figures/CART-SC01-map.pdf",
  width = 8,
  height = 8,
  paper = "special"
)
print(out$plotTree)
dev.off()
#################################################################


#Figure 8.6-left
#####################
# CART parameters
cpar <- 0.005 	# CART complexity parameter

fit <- rpart(
  result ~ x + y,
  data = data.player,
  model = T,
  control = rpart.control(cp = cpar, minsplit = round(nshots.player *
                                                        pn))
)

#fancyRpartPlot(fit, sub="")

out <- plot_cartesian_spatial_tree(
  fit,
  data = data.player,
  z = "Made",
  z.name = "Scoring\nprobability %",
  FUN.z = mean,
  print.z = T,
  round.print.z = 0,
  num_interv_legend = 8,
  low_lim_colormap = 0,
  up_lim_colormap = 100
)

print(out$plotTree)

######################## save figures
pdf(
  file = "./Figures/CART-SC02-map.pdf",
  width = 8,
  height = 8,
  paper = "special"
)
print(out$plotTree)
dev.off()
#################################################################

#Figure 8.6-right
# CART parameters
cpar <- 0.001	# CART complexity parameter

fit <- rpart(
  result ~ x + y,
  data = data.player,
  model = T,
  control = rpart.control(cp = cpar, minsplit = round(nshots.player *
                                                        pn))
)

#fancyRpartPlot(fit, sub="")

out <- plot_cartesian_spatial_tree(
  fit,
  data = data.player,
  z = "Made",
  z.name = "Scoring\nprobability %",
  FUN.z = mean,
  print.z = T,
  round.print.z = 0,
  num_interv_legend = 8,
  low_lim_colormap = 0,
  up_lim_colormap = 100
)

print(out$plotTree)

######################## save figures
pdf(
  file = "./Figures/CART-SC03-map.pdf",
  width = 8,
  height = 8,
  paper = "special"
)
print(out$plotTree)
dev.off()
#################################################################


#####################
# Figure 8.7 - CART with optimal cp
#####################
df_tree <- data.player %>%
  select(result, x, y) %>%
  mutate(result = factor(result)) %>%
  na.omit()

pacman::p_load(caret)
pn <- 0.01		# % observations in minnodesize
nshots.player <- nrow(data.player)

set.seed(332)
fit_cv <- train(
  result ~ x + y,
  data = df_tree,
  method = "rpart",
  trControl = trainControl(
    method = "repeatedcv",
    number = 10,
    repeats = 20,
    classProbs = TRUE,
    summaryFunction = twoClassSummary
  ),
  tuneGrid =
    expand.grid(cp = seq(0.001, 0.009, length.out = 100)),
  control =
    rpart.control(minsplit = round(nshots.player * pn)),
  metric = "ROC"
)

# Figure 8.7 (left)
p <- ggplot(data = fit_cv$results, aes(x = cp, y = ROC)) +
  geom_line() +
  geom_point() +
  labs(x = "Complexity parameter", y = "Area under the ROC (repeated-cross validation)") +
  theme_bw()
print(p)

# optimal value of cp
opt_cp <- fit_cv$bestTune

# CART with optimal cp
fit_tree <- rpart(
  factor(result) ~ x + y,
  data.player,
  control = rpart.control(cp = opt_cp, minsplit = round(nshots.player *
                                                          pn)),
  model = T
)

# fancyRpartPlot(fit_tree, digits=3)

# Figure 8.7 (right)
out <- plot_cartesian_spatial_tree(
  fit_tree,
  data = data.player,
  z = "Made",
  z.name = "Scoring\nprobability %",
  FUN.z = mean,
  print.z = F,
  round.print.z = 0,
  area.labels = F,
  num_interv_legend = 8,
  low_lim_colormap = 0,
  up_lim_colormap = 100
)
print(out$plotTree)



######################## save figures
pdf(
  file = "./Figures/CART-SC04-map.pdf",
  width = 8,
  height = 8,
  paper = "special"
)
print(out$plotTree)
dev.off()

pdf(
  file = "./Figures/CART-SC04-cp.pdf",
  width = 8,
  height = 8,
  paper = "special"
)
print(p)
dev.off()


#####################
# Figure 8.8 - CART with arbitrary choice of cp=0.0010 e minsplit=20%
# (Note: 0.0010 is the optimal cp with minsize 20%)
#####################
pn <- 0.2	
opt_cp <- 0.0010
nshots.player <- nrow(data.player)

fit_tree <- rpart(
  factor(result) ~ x + y,
  data.player,
  control = rpart.control(cp = opt_cp, minsplit = round(nshots.player *
                                                          pn)),
  model = T
)

#fancyRpartPlot(fit_tree, digits=3)

out <- plot_cartesian_spatial_tree(
  fit_tree,
  data = data.player,
  z = "Made",
  z.name = "Scoring\nprobability %",
  FUN.z = mean,
  print.z = F,
  round.print.z = 0,
  num_interv_legend = 8,
  low_lim_colormap = 0,
  up_lim_colormap = 100
)
print(out$plotTree)


######################## save figures
pdf(
  file = "./Figures/CART-SC05-map.pdf",
  width = 8,
  height = 8,
  paper = "special"
)
print(out$plotTree)
dev.off()


#################################################################
### SECTION 8.3.2 CART applied on polar coordinates of the shots
#################################################################

######################################
# From cartesian to polar coordinates
# (center on basket)
######################################
r_theta_play <- cart2polar(data.player$x - Xc, data.player$y - Yc)
data.player$rho   <- r_theta_play$r
data.player$theta <- r_theta_play$theta


#####################
# Figure 8.9
#####################
cpar <- 0.01	 # CART complexity parameter
pn   <- 0.1		# Observations (\%) in minnodesize argument
nshots.player <- nrow(data.player)

fit <- rpart(
  factor(result) ~ theta + rho,
  data.player,
  model = T,
  control = rpart.control(cp = cpar, minsplit = round(nshots.player *
                                                        pn, 0))
)

# Figure 8.9 top
fancyRpartPlot(fit, sub = "")

out <- plot_polar_spatial_tree(
  fit,
  data = data.player,
  z = "Made",
  z.name = "Scoring\nprobability %",
  FUN.z = mean,
  palette = c("blue", "gray95", "red"),
  print.z = F,
  round.print.z = 0,
  col_labs = "white",
  expand_plot = F,
  num_interv_legend = 10,
  low_lim_colormap = 0,
  up_lim_colormap = 100
)

# Figure 8.9 bottom
print(out$plotTree)


######################## save figures
pdf(
  file = "./Figures/CART-SC01-polar-tree.pdf",
  width = 10,
  height = 10,
  paper = "special"
)
fancyRpartPlot(fit, sub = "")
dev.off()

pdf(
  file = "./Figures/CART-SC01-polar-map.pdf",
  width = 8,
  height = 8,
  paper = "special"
)
print(out$plotTree)
dev.off()
#################################################################

#####################
# Figure 8.10
#####################
pacman::p_load(caret)
pn <- 0.01		# % observations in minnodesize
nshots.player <- nrow(data.player)

df_tree <- data.player %>%
  select(result, theta, rho) %>%
  mutate(result = factor(result)) %>%
  na.omit()

set.seed(8765)
fit_cv <- train(
  result ~ theta + rho,
  data = df_tree,
  method = "rpart",
  trControl = trainControl(
    method = "repeatedcv",
    number = 10,
    repeats = 20,
    classProbs = TRUE,
    summaryFunction = twoClassSummary
  ),
  tuneGrid = expand.grid(cp = seq(0.001, 0.009, length.out = 100)),
  control = rpart.control(minsplit = round(nshots.player * pn)),
  metric = "ROC"
)

# Figure 8.10 left
p <- ggplot(data = fit_cv$results, aes(x = cp, y = ROC)) +
  geom_line() +
  geom_point() +
  labs(x = "Complexity parameter", y = "Area under the ROC (repeated-cross validation)") +
  theme_bw()
print(p)

# optimal value of cp
opt_cp <- fit_cv$bestTune
# CART with optimal cp
fit_tree <- rpart(
  factor(result) ~ theta + rho,
  data.player,
  control = rpart.control(cp = opt_cp, minsplit = round(nshots.player *
                                                          pn)),
  model = T
)

#fancyRpartPlot(fit_tree, digits=3)

# Figure 8.10 right
out <- plot_polar_spatial_tree(
  fit_tree,
  data = data.player,
  z = "Made",
  z.name = "Scoring\nprobability %",
  palette = c("blue", "gray95", "red"),
  FUN.z = mean,
  print.z = F,
  round.print.z = 0,
  col_labs = "white",
  expand_plot = F,
  num_interv_legend = 10,
  low_lim_colormap = 0,
  up_lim_colormap = 100
)
print(out$plotTree)


######################## save figures
pacman::p_load(gridExtra)
plot1 <- print(p)
plot2 <- print(out$plotTree)
pdf(
  file = "./Figures/CART-SC02-polar-cpmap.pdf",
  width = 12,
  height = 6,
  paper = "special"
)
grid.arrange(plot1, plot2, ncol = 2)
dev.off()

#####################
# Figure 8.11
#####################
pn <- 0.2		
nshots.player <- nrow(data.player)
opt_cp <- 0.0010 

df_tree <- data.player %>%
  select(result, theta, rho) %>%
  mutate(result = factor(result)) %>%
  na.omit()

fit_tree <- rpart(
  factor(result) ~ theta + rho,
  data.player,
  control = rpart.control(cp = opt_cp, minsplit = round(nshots.player *
                                                          pn)),
  model = T
)
#fancyRpartPlot(fit_tree, digits=3)

out <- plot_polar_spatial_tree(
  fit_tree,
  data = data.player,
  z = "Made",
  z.name = "Scoring\nprobability %",
  palette = c("blue", "gray95", "red"),
  FUN.z = mean,
  print.z = F,
  round.print.z = 0,
  col_labs = "white",
  expand_plot = F,
  num_interv_legend = 10,
  low_lim_colormap = 0,
  up_lim_colormap = 100
)
print(out$plotTree)


######################## save figures
pdf(
  file = "./Figures/CART-SC03-polar-map.pdf",
  width = 8,
  height = 8,
  paper = "special"
)
print(out$plotTree)
dev.off()
