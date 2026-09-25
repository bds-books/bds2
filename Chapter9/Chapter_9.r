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

# set Chapter9 as working directory
# setwd("....")

############################################
############################################
# PART 3                                   #
# Spatial performance analysis             #                            
############################################
############################################

##############################################################
# CHAPTER 9                                                  #
# Scoring probability maps via Machine Learning algorithms   #
##############################################################


#################################################################
### SECTION 9.1 Scoring probability maps via Random Forests
#################################################################
rm(list = ls())
graphics.off()

###########################
# Load and preprocess data
###########################
pacman::p_load(BasketballAnalyzeR,
               randomForest,
               ggplot2,
               grid,
               pROC,
               plotROC,
               dplyr)
source("additional_functions.r")
load("./Data/NBAPbP_BDB.Rdata")
PbP <- PbPmanipulation(PbP.BDB.GSW.rs)

# Reference point in the court
Xc <- 0
Yc <- -41.75
PbP <- PbP %>%
  mutate(x = original_x / 10 + Xc,
         x = ifelse(x == 0, 0.0001, x),
         y = original_y / 10 + Yc)

player <- "Stephen Curry"
data.player <- PbP %>%
  filter(player == {{player}} & result != "" & event_type != "free throw") %>%
  mutate(result = droplevels(result), Made = 100 * (2 - as.numeric(result)))

df_RF <- data.player %>%
  mutate(cart2polar(.$x - Xc, .$y - Yc), result = factor(result, levels = c("missed", "made"))) %>%
  rename(rho = r) %>%
  select(rho, theta, result) %>%
  na.omit()


##### Random Forests on POLAR COORDINATES
fit_RF <- randomForest(
  factor(result) ~ rho + theta,
  df_RF,
  ntree = 5000,
  mtry = 2,
  nodesize = 150
)

p_rocRF <- plot_RF_ROC(fit_RF, df_RF)
print(p_rocRF)

######################## save figures
dir.create("./Figures", showWarnings = FALSE)
pdf(
  file = "./Figures/RF-ROC.pdf",
  width = 10,
  height = 10,
  paper = "special"
)
print(p_rocRF)
dev.off()
########################

p_mapRF <- plot_RF_map(
  fit_RF,
  n_grid = 200,
  colormap_bins = 0,
  white_areas = c("top", "bottom")
)
print(p_mapRF)

######################## save figures
pdf(
  file = "./Figures/RF-SC.pdf",
  width = 10,
  height = 10,
  paper = "special"
)
print(p_mapRF)
dev.off()
#################################################################


##### Random Forests with Smoothing
pacman::p_load(MBA)
n <- 100
polar_grid <- expand.grid(x = seq(-25, 25, length.out = n),
                          y = seq(-47, 0, length.out = n)) %>% mutate(cart2polar(x - Xc, y - Yc)) %>% rename(rho = r)

polar_grid <- polar_grid %>% mutate(prob_RF = 100 * predict(fit_RF, ., type = "prob")[, "made"])

df_sm <- mba.surf(polar_grid[, c("x", "y", "prob_RF")],
                  no.X = 100,
                  no.Y = 100,
                  h = 6)

data_grid_sm <- data.frame(expand.grid(x = df_sm$xyz$x, y = df_sm$xyz$y), z = as.vector(df_sm$xyz$z)) %>% mutate(z = pmin(100, pmax(0, z)))

p_mapRFsm <- ggplot(data = data_grid_sm) + geom_raster(aes(x = x, y = y, fill = z), show.legend = TRUE) + geom_polygon(
  data = whiteAreas(),
  aes(x = x, y = y, group = area),
  fill = "white",
  color = NA,
  inherit.aes = FALSE
) +
  scale_fill_viridis_c(limits = c(0, 100), name = "Scoring\nprobability %") +
  coord_fixed(expand = T) +
  theme_void()
p_mapRFsm <- drawNBAcourt(p_mapRFsm)
print(p_mapRFsm)

######################## save figures
pdf(
  file = "./Figures/RF-SC-smooth.pdf",
  width = 10,
  height = 10,
  paper = "special"
)
print(p_mapRFsm)
dev.off()
#################################################################


#################################################################
### SECTION 9.2 Scoring probability maps via Oblique Random Forests
#################################################################
# Oblique Random Forests on CARTESIAN COORDINATES
pacman::p_load(aorsf)

df_ORF <- data.player %>%
  mutate(result = factor(result, levels = c("missed", "made"))) %>%
  select(x, y, result) %>%
  na.omit()

fit_ORF <- orsf(
  data = df_ORF,
  formula = result ~ x + y,
  n_tree  = 1000,
  n_split = 10,
  mtry = 2,
  split_min_obs = 40,
  n_thread = parallel::detectCores()
)

p_rocORF <- plot_RF_ROC(fit_ORF, df_ORF)
print(p_rocORF)

######################## save figures
pdf(
  file = "./Figures/ORF-ROC.pdf",
  width = 10,
  height = 10,
  paper = "special"
)
print(p_rocORF)
dev.off()
########################

p_mapORF <- plot_RF_map(fit_ORF)
print(p_mapORF)

######################## save figures
pdf(
  file = "./Figures/ORF-SC.pdf",
  width = 10,
  height = 10,
  paper = "special"
)
print(p_mapORF)
dev.off()
#################################################################


#######################################################################
### SECTION 9.3 Scoring probability maps via Extremely Randomized Trees
#######################################################################
##### Extra Trees on POLAR COORDINATES
pacman::p_load(ranger)

df_ET <- df_RF
fit_ET <- ranger(
  formula = result ~ rho + theta,
  data = df_ET,
  num.trees = 1000,
  mtry = 2,
  min.node.size = 100,
  splitrule = "extratrees",
  replace = FALSE,
  sample.fraction = 1,
  num.random.splits = 1,
  classification = TRUE,
  seed = 7654,
  probability = FALSE
)

p_rocET <- plot_RF_ROC(fit_ET, df_ET)
print(p_rocET)

######################## save figures
pdf(
  file = "./Figures/ET-ROC.pdf",
  width = 10,
  height = 10,
  paper = "special"
)
print(p_rocET)
dev.off()
########################

p_mapET <- plot_RF_map(fit_ET)
print(p_mapET)

######################## save figures
pdf(
  file = "./Figures/ET-SC.pdf",
  width = 10,
  height = 10,
  paper = "special"
)
print(p_mapET)
dev.off()
#################################################################

##### ExtraTrees on POLAR COORDINATES with and without Curry
name_cols <-  paste0(c("a", "h"), rep(1:5, each = 2))

select_player <- "Stephen Curry"
PbP <- PbP %>% mutate(player_on_court = apply(.[, name_cols], 1, \(x) select_player %in% x))

PbP <- PbP %>%
  filter(result != "" &
           event_type != "free throw" & team == "GSW") %>%
  mutate(
    result = droplevels(result),
    Made = 100 * (2 - as.numeric(result)),
    cart2polar(.$x - Xc, .$y - Yc),
    result = factor(result, levels = c("missed", "made"))
  ) %>%
  rename(rho = r) %>%
  select(rho, theta, result, player_on_court) %>%
  na.omit()

PbP_on <- PbP %>% filter(player_on_court)
PbP_off <- PbP %>% filter(!player_on_court)

#####################code for plotting Figure 9.8
##### Extra Trees on POLAR COORDINATES - Player ON the court

df_ET <- PbP_on
fit_ET <- ranger(
  formula = result ~ rho + theta,
  data = df_ET,
  num.trees = 1000,
  mtry = 2,
  min.node.size = 100,
  splitrule = "extratrees",
  replace = FALSE,
  sample.fraction = 1,
  num.random.splits = 1,
  classification = TRUE,
  seed = 7654,
  probability = FALSE
)

#p_rocET <- plot_RF_ROC(fit_ET, df_ET)
p_mapET <- plot_RF_map(fit_ET)
print(p_mapET)

##### Extra Trees on POLAR COORDINATES - Player OFF of the court
df_ET <- PbP_off
fit_ET <- ranger(
  formula = result ~ rho + theta,
  data = df_ET,
  num.trees = 1000,
  mtry = 2,
  min.node.size = 100,
  splitrule = "extratrees",
  replace = FALSE,
  sample.fraction = 1,
  num.random.splits = 1,
  classification = TRUE,
  seed = 7654,
  probability = FALSE
)
#p_rocET <- plot_RF_ROC(fit_ET, df_ET)
p_mapET <- plot_RF_map(fit_ET)
print(p_mapET)

######################## save figures
pdf(
  file = "./Figures/ET-on.pdf",
  width = 10,
  height = 10,
  paper = "special"
)
print(p_mapET)
dev.off()
pdf(
  file = "./Figures/ET-off.pdf",
  width = 10,
  height = 10,
  paper = "special"
)
print(p_mapET)
dev.off()
#################################################################


#######################################################################
### SECTION 9.4 Additional approaches for scoring probability maps
#######################################################################
######################## code for plotting Figure 9.9

# Note: Mac users need to preliminarily install XQuartz
pacman::p_load(adabag)

df_ADA <- data.player %>%
  mutate(cart2polar(.$x - Xc, .$y - Yc), result = factor(result, levels = c("missed", "made"))) %>%
  rename(rho = r) %>%
  select(rho, theta, result) %>%
  na.omit()
fit_ADA <- boosting(result ~ rho + theta, data = df_ADA, mfinal = 1000)
pred_ADA <- predict(fit_ADA, newdata = df_ADA)
probADA <- pred_ADA$prob[, 2]
df_roc <- data.frame(D = ifelse(df_ADA$result == "made", 1, 0), M = probADA)
ROCcurve <- roc(df_roc$D, df_roc$M)
AUC_CI <- round(ci.auc(ROCcurve, method = "delong"), 2)
auc_label <- sprintf("Adaboost - ROC curve \nAUC=%s (95%% CI %s - %s)",
                     AUC_CI[2],
                     AUC_CI[1],
                     AUC_CI[3])
p_rocADA <- ggplot() +
  geom_roc(data = df_roc, aes(d = D, m = M), n.cuts = 0) +
  geom_segment(
    aes(
      x = 0,
      y = 0,
      xend = 1,
      yend = 1
    ),
    color = "black",
    linetype = "solid",
    linewidth = 0.5
  ) +
  annotation_custom(grob = textGrob(
    auc_label,
    x = unit(0.02, "npc"),
    y = unit(0.98, "npc"),
    just = c("left", "top")
  )) +
  scale_x_continuous(minor_breaks = NULL) +
  scale_y_continuous(minor_breaks = NULL) +
  labs(x = "False Positive Rate (1 - Specificity)", y = "True Positive Rate (Sensitivity)")  +
  theme_bw()
print(p_rocADA)
n <- 100
polar_grid <- expand.grid(x = seq(-25, 25, length.out = n),
                          y = seq(-47, 0, length.out = n)) %>%
  mutate(cart2polar(x - Xc, y - Yc)) %>%
  rename(rho = r)
polar_grid <- polar_grid %>%
  mutate(prob_ADA = 100 * predict(fit_ADA, .)$prob[, 2])
polar_grid_mod <- polar_grid
polar_grid_mod$prob_ADA[1] <- 0
polar_grid_mod$prob_ADA[nrow(polar_grid)] <- 100
p_mapADA <- ggplot(data = polar_grid_mod) +
  geom_contour_filled(aes(x = x, y = y, z = prob_ADA),
                      bins = 8,
                      show.legend = TRUE) +
  scale_fill_viridis_d(name = "Scoring\nprobability %", drop = FALSE) +
  coord_fixed() +
  theme_void()
p_mapADA <- drawNBAcourt(p_mapADA)
print(p_mapADA)

#Smoothing
df_sm <- mba.surf(polar_grid[, c("x", "y", "prob_ADA")],
                  no.X = 100,
                  no.Y = 100,
                  h = 6)
data_grid_sm <- data.frame(expand.grid(x = df_sm$xyz$x, y = df_sm$xyz$y), z =
                             as.vector(df_sm$xyz$z)) %>% mutate(z = pmin(100, pmax(0, z)))
data_grid_mod <- data_grid_sm
data_grid_mod$z[1] <- 0
data_grid_mod$z[nrow(data_grid_sm)] <- 100
p_mapADAsm <- ggplot(data = data_grid_mod) +
  geom_contour_filled(aes(x = x, y = y, z = z), bins = 8, show.legend =
                        TRUE) +
  scale_fill_viridis_d(name = "Scoring\nprobability %", drop = FALSE) +
  coord_fixed(expand = T) +
  theme_void()
p_mapADAsm <- drawNBAcourt(p_mapADAsm)
print(p_mapADAsm)

######################## save figures
pdf(
  file = "./Figures/ADA-SC-smooth.pdf",
  width = 10,
  height = 10,
  paper = "special"
)
print(p_mapADAsm)
dev.off()
#################################################################
