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
# 6.4.1 Animated pose detection
######################
rm(list=ls())
graphics.off()

pacman::p_load(ggplot2,grid,tensorflow,magick,dplyr)

#####
# Load interpreter (MoveNet single pose)
interpreter <- tf$lite$Interpreter(model_path="./MoveNet/movenet_singlepose.tflite")

# extract all the individual frames from the video file RaufMoh_movie.mp4
# and store them in a dedicated folder
# (if not already done by running Section_4_1.r)
# pacman::p_load(av)
# dir.create("./Frames/posedt", showWarnings=FALSE, recursive=TRUE)
# unlink(paste0("./Frames/posedt", "/*"), recursive = FALSE)
# av_video_images(video="./Videos/RaufMoh_movie.mp4", destdir="./Frames/posedt", format="jpg")

#####
# Set the image crop area
img1 <- tf$io$read_file("./Frames/posedt/image_002308.jpg")
img1 <- tf$compat$v2$image$decode_image(img1)
height <- dim(img1)[1]; width <- dim(img1)[2]
wbox <- 500L; hbox <- 500L
offset_width <- as.integer(width/2-wbox/2) 
offset_height <- as.integer(height/2-hbox/2)
crop_area <- c(offset_height, offset_width, hbox, wbox)

#####
# Preprocess images and predict keypoints
source("additional_functions.r")

labs <- c("nose", "eye_L", "eye_R", "ear_L", "ear_R", "shoulder_L", 
          "shoulder_R", "elbow_L", "elbow_R", "wrist_L", "wrist_R", "hip_L", 
          "hip_R", "knee_L", "knee_R", "ankle_L", "ankle_R")

frame_seq <- as.integer(2308:2552) 
image_list <- keypoints <- edges <- vector(length(frame_seq), mode="list")
for (k in seq_along(frame_seq)) {
  file_k <- paste0("./Frames/posedt/image_00", frame_seq[k], ".jpg")
  imgs_k <- preprocess_image(file=file_k, resize=c(256, 256), 
                             dtype=tf$uint8, crop=crop_area)
  pred_keypts_k <- movenet_singlepose_predict(interpreter, imgs_k[["image_proc"]])
  keypts_locs_edges <- movenet_singlepose_features(pred_keypts_k, labs=labs, score_threshold=0.2) 
  keypoints[[k]]  <- keypts_locs_edges[["keypoints"]]
  edges[[k]]      <- keypts_locs_edges[["edges"]]
  image_list[[k]] <- imgs_k
}

#### Create and save plots only skeletons
dir.create("./Frames/posedtvideo", showWarnings=FALSE, recursive=TRUE)
unlink(paste0("./Frames/posedtvideo", "/*"), recursive = FALSE)

plot_list <- vector(length(frame_seq), mode="list")
for (k in seq_along(frame_seq)) {
  keypoints_k <- keypoints[[k]]
  edges_k <- edges[[k]]
  p_k <- ggplot() + 
    geom_segment(aes(x=x_start, y=y_start, xend=x_end, yend=y_end), 
                 color=edges_k$color, linewidth=1, data=edges_k) +
    geom_point(aes(x=x, y=y, fill=score, color=score), data=keypoints_k,
               size=3, show.legend=T, pch=21) +
    geom_point(aes(x=x, y=y), data=keypoints_k, color="white",
               size=1.25, show.legend=T) +
    scale_color_gradientn(name="Keypoint score", limits = c(0, 1),
                          colors = c("#FFCCCC", "#FF6666", "#FF0000", "#990000")) +
    scale_fill_gradientn(name="Keypoint score", limits = c(0, 1),
                         colors = c("#FFCCCC", "#FF6666", "#FF0000", "#990000")) +
    xlim(c(0,1)) + ylim(c(0,1)) + theme_void()  +
    theme(legend.position="inside", 
          legend.position.inside=c(0.9,0.8),
          legend.text = element_text(color = "white"),
          legend.title = element_text(color = "white"),
          plot.background = element_rect(fill="black")) 
  plot_list[[k]] <- p_k
  jpeg(file=paste0("./Frames/posedtvideo/image",frame_seq[k],".jpg"), height=2000, width=2000, res=300)
  print(p_k)
  dev.off() 
}
img_seq <- do.call(c, lapply(paste0("./Frames/posedtvideo/image",frame_seq,".jpg"), image_read))

dir.create("./AnimatedPlots", showWarnings=FALSE)
image_write_video(image = img_seq, framerate=20,
                  path = "./AnimatedPlots/Single_Pose_Detection1.mp4")


#### Add background original images
for (k in seq_along(frame_seq)) {
  image_raw_k <- image_list[[k]]$image_raw/255
  dim_img_k <- as.integer(max(dim(image_raw_k)[1:2]))
  image_raw_k <- tf$image$resize_with_pad(image_raw_k, dim_img_k, dim_img_k)
  p_k <- plot_list[[k]] + 
    annotation_custom(rasterGrob(image_raw_k, width=unit(1,"npc"), 
                                 height=unit(1,"npc")), -Inf, Inf, -Inf, Inf) +
    theme(plot.background = element_rect(fill="transparent")) 
  p_k$layers <- p_k$layers[c(4,1,2,3)]
  jpeg(file=paste0("./Frames/posedtvideo/image",frame_seq[k],".jpg"), height=2000, width=2000, res=300)
  print(p_k)
  dev.off() 
}
img_seq <- do.call(c, lapply(paste0("./Frames/posedtvideo/image",frame_seq,".jpg"), image_read))

image_write_video(image = img_seq, framerate=20,
                  path = "./AnimatedPlots/Single_Pose_Detection2.mp4")