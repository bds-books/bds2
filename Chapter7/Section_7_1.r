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
# 7.1 Object detection with YOLO
######################
rm(list=ls())
graphics.off()


#################################################################
######################### WARNING
#################################################################
# It is worth pointing out that the code provided in the following sections
# may yield slightly different results depending on the operating system
# used (mainly, Windows or macOS), due to variations in hardware, software libraries,
# and numerical precision across platforms.
#################################################################


# extract all the individual frames from the video file RaufMoh_movie.mp4
# and store them in a dedicated folder
# (if not already done by running Section_4_1.r)
# pacman::p_load(av)
# dir.create("./Frames/posedt", showWarnings=FALSE, recursive=TRUE)
# unlink(paste0("./Frames/posedt", "/*"), recursive = FALSE)
# av_video_images(video="./Videos/RaufMoh_movie.mp4", destdir="./Frames/posedt", format="jpg")

img_path <- "./Images/image_010072.jpg"
video_path <- "./Videos/RaufMoh_movie_shot.mp4"


# install the torch version used in the book. Check for updates!
pacman::p_load(remotes)
if (!requireNamespace("torch", quietly = TRUE) ||
    packageVersion("torch") != "0.13.0") {
  install_version("torch", version = "0.13.0", 
                           repos = "https://cloud.r-project.org")
}
pacman::p_load(curl,grid, ggplot2, magick, gifski, torch, dplyr)

weights_file <- "./Yolo/yolo7_weights_file.pt"
class_file <- "./Yolo/class_labels.txt"

# where the files have been downloaded from
# pacman::p_load(curl)
# yolo7_url <- "https://github.com/openvolley/ovml/releases/download/v0.1.0/yolov7.torchscript.pt"
# if (!file.exists(weights_file)) {
#     curl_download(yolo7_url, destfile=weights_file, mode = "wb")
#	  }

# class_labels_url <- "https://github.com/amikelive/coco-labels/blob/master/coco-labels-2014_2017.txt?raw=true"
# if (!file.exists(class_file)) {
#     curl_download(class_labels_url, destfile=class_file, mode="w")
#	  }

######################
# 7.1.1 Ball detection in an image
######################

img <- image_read(img_path)

#######
# Crop image (example)
# img_info <- image_info(img)
# width    <- img_info$width
# height   <- img_info$height
# wbox <- 350  
# hbox <- 650
# offset_width  <- as.integer(width/2-wbox/2) 
# offset_height <- as.integer(height/2-hbox/2)  
# geom <- paste0(wbox,"x",hbox,"+",offset_width,"+",offset_height)
# geom_area <-  geometry_area(width=wbox, height=hbox, x_off=offset_width, y_off=offset_height)
# img <- image_crop(img, geometry=geom_area)

input_size <- 640L 
geom_size_px <- geometry_size_pixels(width=input_size, height=input_size, preserve_aspect=TRUE)
img_resized <- image_scale(img, geom_size_px)

img_ext <- image_extent(img_resized, geom_size_px, color="black")

# Obtain the model object from Torch
dn <- torch::jit_load(weights_file)

# Set the model to evaluation mode
dn$eval()

# Move the model to CPU (alternative: move to GPU using dn$cuda() )
dn$cpu()

# Create an input Torch tensor and store it on CPU
input_tensor <- as.numeric(image_data(img_ext, "rgb"))
input_tensor <- array(input_tensor, dim = c(1, dim(input_tensor)))
input_tensor <- aperm(input_tensor, c(1, 4, 2, 3))
input_tensor <- torch_tensor(input_tensor, device = torch_device("cpu"))

# Perform inference
output_list <- dn$forward(input_tensor)
output_tensor <- as_array(output_list[[1]]$to(device = torch_device("cpu")))

# bounding boxes corners
image_pred <- as.data.frame(output_tensor[1, , ])
xmin <- image_pred[, 1] - image_pred[, 3]/2
ymin <- image_pred[, 2] - image_pred[, 4]/2
xmax <- image_pred[, 1] + image_pred[, 3]/2
ymax <- image_pred[, 2] + image_pred[, 4]/2
image_pred[, 1:4] <- cbind(xmin, ymin, xmax, ymax)

class_labels <- read.table(class_file, sep="\t")[[1]]
num_classes <- length(class_labels)
names(image_pred) <- c("xmin","ymin","xmax","ymax", "confidence", 
                       paste0("CP",1:num_classes))

image_pred <- image_pred %>%
  mutate(max_prob = do.call(pmax, select(., CP1:CP80)),
         max_prob_class_num = max.col(select(., CP1:CP80)),
         max_prob_class = class_labels[max_prob_class_num],
         confidence = confidence * max_prob) %>%
  select(xmin, ymin, xmax, ymax, confidence, 
         max_prob, max_prob_class)

############
# Filter 1
min_confidence <- 0.05
image_pred <- image_pred %>% filter(confidence > min_confidence)

############
# Filter 2
# Non-Maximum Suppression (NMS)
obj_classes <- unique(image_pred$max_prob_class)
nms_conf <- 0.4
out <- lapply(obj_classes, function(class) {
  BBpred_class <- image_pred %>% 
    filter(max_prob_class==class) %>%
    arrange(-confidence)
  nBBs_init <- nrow(BBpred_class)
  for (k in seq_len(nBBs_init - 1)) {
    nBBsk <- nrow(BBpred_class)
    if (k >= nBBsk) break
    pos_rem <- (k+1):nBBsk
    BBk <- as.matrix(BBpred_class[k, 1:4])
    BBrem <- as.matrix(BBpred_class[pos_rem, 1:4])
    BBinters_x1 <- pmax(BBk[1], BBrem[, 1])
    BBinters_y1 <- pmax(BBk[2], BBrem[, 2])
    BBinters_x2 <- pmin(BBk[3], BBrem[, 3])
    BBinters_y2 <- pmin(BBk[4], BBrem[, 4])
    BBinters_area <- pmax(BBinters_x2 - BBinters_x1 + 1, 0) * 
      pmax(BBinters_y2 - BBinters_y1 + 1, 0)
    BBk_area <- (BBk[3] - BBk[1] + 1) * (BBk[4] - BBk[2] + 1)
    BBrem_area <- (BBrem[, 3] - BBrem[, 1] + 1) * (BBrem[, 4] - BBrem[, 2] + 1)
    IoUs <- BBinters_area/(BBk_area + BBrem_area - BBinters_area)
    if (is.null(IoUs)) break
    BBpred_class$confidence[pos_rem[IoUs >= nms_conf]] <- 0
    BBpred_class <- BBpred_class %>% filter(confidence > 0)
  }
  return(BBpred_class)
})
output <- do.call(rbind, out)

# data frame with bounding boxes
img_h <- dim(input_tensor)[3]
bbox_player <- output %>% 
  filter(grepl("person|ball", max_prob_class)) %>%
  mutate(ymin=img_h-ymin, ymax=img_h-ymax, confidence=round(confidence,3)) %>%
  rename(class = max_prob_class)

source("additional_functions.r")
p <- plot_bbox(img_ext, bbox_player, legend=TRUE)
print(p)


####################### save figures
dir.create("./Figures", showWarnings=FALSE)

pdf(file="./Figures/image6.pdf", width=10, height=10, paper="special")
print(p)
dev.off()
#################################################################

# ovml package
if (!requireNamespace("ovml", quietly = TRUE)) {
  install_github("openvolley/ovml")
}
if (!requireNamespace("ovideo", quietly = TRUE)) {
  install_github("openvolley/ovideo")
}

pacman::p_load(ovml, patchwork, av)

yolo_net_volley <- ovml_yolo(version="4-mvb", device="cpu")
obj_det <- ovml_yolo_detect(yolo_net_volley, img_path, conf = 0.001)


# Data frame with bounding boxes
bbox_ball <- obj_det %>% mutate(class = "ball") %>%
  rename(confidence = score)
       
p <- plot_bbox(img, bbox_ball)
print(p)

####################### save figures
pdf(file="./Figures/image7.pdf", width=10, height=10, paper="special")
print(p)
dev.off()
#################################################################

######################
# 7.1.2 Ball detection in a video
######################
# Warning: preliminarily install FFmpeg and add it to R’s PATH (if needed)
# https://www.ffmpeg.org/

# Sys.setenv(PATH = paste("<path_to_ffmpeg_bin>", Sys.getenv("PATH"), sep = ";"))
# windows users: sep = ";"
# macOS users: sep = ":"

# common path for macOS if FFmpeg is installed via homebrew
# Sys.setenv(PATH = paste("/opt/homebrew/bin", Sys.getenv("PATH"), sep=":"))

# Sys.which("ffmpeg")

obj_det_video <- ovml_yolo_detect(yolo_net_volley, video_path, conf=0.05)
obj_det_video <- obj_det_video[grepl("ball", obj_det_video$class), ]
obj_det_video$class <- "ball"

ggplot_list <- lapply(seq(8, by=10, length.out=4), function(k) {
  imgk <- image_read(obj_det_video$image_file[k])
  bbox_ballk <- obj_det_video[k,] %>% rename(confidence = score)
  plot_bbox(imgk, bbox_ballk)
})

wrap_plots(ggplot_list)


####################### save figures
pdf(file="./Figures/videoframes.pdf", width=15, height=9, paper="special")
wrap_plots(ggplot_list)
dev.off()
#################################################################

# Frame by frame procedure
dir.create("./Frames/objdtvideo1", showWarnings=FALSE, recursive=TRUE)
unlink(paste0("./Frames/objdtvideo1", "/*"), recursive = FALSE)

dir.create("./Frames/objdtvideo2", showWarnings=FALSE, recursive=TRUE)
unlink(paste0("./Frames/objdtvideo2", "/*"), recursive = FALSE)

imgs <- av_video_images(video=video_path, 
                        destdir="./Frames/objdtvideo1/", format = "jpg")

ggplot_list <- lapply(imgs, function(imgk) {
  bbox_ballk <- ovml_yolo_detect(yolo_net_volley, imgk, conf = 0.005) %>%
    filter(grepl("ball", class)) %>%
    rename(confidence = score)
  if (nrow(bbox_ballk) > 0) {
    bbox_ballk <- bbox_ballk %>% mutate(class = "ball")
  }
  pk <- plot_bbox(image_read(imgk), bbox_ballk, label = FALSE)
  imgk <- gsub("objdtvideo1", "objdtvideo2", imgk)
  ggsave(imgk, pk, units="px", width = 3412, height = 2330)
})

imgs <- gsub("objdtvideo1", "objdtvideo2", imgs)


dir.create("./AnimatedPlots", showWarnings=FALSE)
av_encode_video(imgs, framerate = 30, output = "./AnimatedPlots/RaufMoh_movie_shot_with_ball.mp4")

frames <- image_read(imgs)
frames <- image_scale(frames, "50%")
imgs_gif <- image_animate(frames)
image_write_gif(image=imgs_gif, loop=TRUE, delay=1/10,
                "./AnimatedPlots/RaufMoh_movie_shot_with_ball.gif")

