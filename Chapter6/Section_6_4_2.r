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
# 6.4.2 Multipose detection
######################
rm(list = ls())
graphics.off()

pacman::p_load(ggplot2, grid, tensorflow, dplyr)
interpreter <- tf$lite$Interpreter(model_path = "./MoveNet/movenet_multipose.tflite")

url1 <- "https://bit.ly/multipose1"
url2 <- "https://bit.ly/multipose2"
url3 <- "https://bit.ly/multipose3"

dir.create("./Images", showWarnings = FALSE)
download.file(url1, "./Images/multipose1.jpg", mode = "wb")
download.file(url2, "./Images/multipose2.jpg", mode = "wb")
download.file(url3, "./Images/multipose3.jpg", mode = "wb")


source("additional_functions.r")
imgs <- preprocess_image(file = "./Images/multipose1.jpg",
                         resize = c(256, 256),
                         dtype = tf$uint8)
input_image <- imgs[["image_proc"]]
pred_keypts <- movenet_multipose_predict(interpreter, input_image)

labs <- c(
  "nose",
  "eye_L",
  "eye_R",
  "ear_L",
  "ear_R",
  "shoulder_L",
  "shoulder_R",
  "elbow_L",
  "elbow_R",
  "wrist_L",
  "wrist_R",
  "hip_L",
  "hip_R",
  "knee_L",
  "knee_R",
  "ankle_L",
  "ankle_R"
)

feat_list <- movenet_multipose_features(
  pred_keypts,
  keypoint_score_cutoff = 0.10,
  labs = labs,
  bounding_box_score_cutoff = 0.10
)

bndbxs <- feat_list[["bounding_boxes"]]
edges  <- feat_list[["edges"]]
keypoints <- feat_list[["keypoints"]]

image_raw <- imgs[["image_raw"]] / 255
dim_img <- as.integer(max(dim(image_raw)[1:2]))
image_raw <- tf$image$resize_with_pad(image_raw, dim_img, dim_img)


p <- ggplot() +
  annotation_custom(rasterGrob(
    image_raw,
    width = unit(1, "npc"),
    height = unit(1, "npc")
  ),
  -Inf,
  Inf,
  -Inf,
  Inf) +
  geom_rect(
    aes(
      xmin = xmin,
      ymin = ymin,
      xmax = xmax,
      ymax = ymax,
      linewidth = score,
      group = player
    ),
    color = "green",
    fill = "transparent",
    data = bndbxs
  ) +
  scale_linewidth_continuous(name = "Bounding\nbox score", range = c(.5, 1.5)) +
  geom_point(
    aes(
      x = x,
      y = y,
      color = score,
      group = player
    ),
    data = keypoints,
    size = 3,
    show.legend = T
  ) +
  geom_point(
    aes(x = x, y = y),
    data = keypoints,
    color = "white",
    size = 1.25,
    show.legend = T
  ) +
  scale_color_gradientn(
    name = "Keypoint score",
    limits = c(0, 1),
    colors = c("#FFCCCC", "#FF6666", "#FF0000", "#990000")
  ) +
  geom_segment(
    aes(
      x = x_start,
      y = y_start,
      xend = x_end,
      yend = y_end,
      group = player
    ),
    color = edges$color,
    linewidth = 1,
    data = edges
  ) +
  xlim(c(0, 1)) + ylim(c(0, 1)) + theme_void()
print(p)

####################### save figures
# dir.create("./Figures", showWarnings=FALSE)

# multipose1
pdf(
  file = "./Figures/mp_image1.pdf",
  width = 7,
  height = 5,
  paper = "special"
)
print(p)
dev.off()

# multipose2 (repeat analysis with file multipose2)
pdf(
  file = "./Figures/mp_image2.pdf",
  width = 7,
  height = 5.5,
  paper = "special"
)
print(p)
dev.off()

# multipose3 (repeat analysis with file multipose3)
pdf(
  file = "./Figures/mp_image3.pdf",
  width = 7,
  height = 5.8,
  paper = "special"
)
print(p)
dev.off()
