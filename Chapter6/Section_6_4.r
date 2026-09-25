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
# 6.4 Pose detection with MoveNet
######################
rm(list=ls())
graphics.off()

#########
# Download video da Youtube 
# https://www.youtube.com/watch?v=PTOxNcHAxHQ (Creative Commons license)
# @abdulraufmohammed6181
# store in the folder Videos, with the name RaufMoh_movie.mp4

pacman::p_load(av)

dir.create("./Frames/posedt", showWarnings=FALSE, recursive=TRUE)
unlink(paste0("./Frames/posedt", "/*"), recursive = FALSE)

av_video_images(video="./Videos/RaufMoh_movie.mp4", destdir="./Frames/posedt", format="jpg")

pacman::p_load(tensorflow)
install_tensorflow()

#######
# 1. Load the TFLite model into an interpreter.
# Interpreter interface for running TensorFlow Lite models.
# https://www.tensorflow.org/api_docs/python/tf/lite/Interpreter

# Movenet
# This model identifies 17 key points (landmarks) on the human body.
# nose, eyes, ears, shoulders, elbows, wrists, hips, knees, and ankles.
interpreter <- tf$lite$Interpreter(model_path="./MoveNet/movenet_singlepose.tflite")

#######
# 2. Retrieve information about the input tensor 
# of a TensorFlow Lite (TFLite) model

input_details <- interpreter$get_input_details()
print(input_details[[1]][["index"]])
print(input_details[[1]][["shape"]])
print(input_details[[1]][["dtype"]])

#######
# 3. Import the image related to a single frame and 
# convert it into a tensor suitable to be given as input to the neural network.

image0 <- tf$io$read_file("./Frames/posedt/image_002308.jpg")
image  <- tf$compat$v2$image$decode_image(image0)

# dim(image)

pacman::p_load(ggplot2,grid)
height <- dim(image)[1]
width  <- dim(image)[2]
aspect_ratio1 <- height/width
p1 <- ggplot() + 
  annotation_custom(rasterGrob(image/255, width=unit(1,"npc"), height=unit(1,"npc")), -Inf, Inf, -Inf, Inf) + 
  theme_void() +
  theme(aspect.ratio=aspect_ratio1)
print(p1)

####################### save figures
dir.create("./Figures", showWarnings=FALSE)

pdf(file="./Figures/image1.pdf", width=7.2/aspect_ratio1, height=7.2, paper="special")
print(p1)
dev.off()
#################################################################


####### height and width of the crop box
wbox <- 250L  
hbox <- 500L

####### x- and y- coordinates of the top-left corner of the crop box
offset_width = as.integer(width/2-wbox/2) 
offset_height = as.integer(height/2-hbox/2)  


####### Crop the image
image <- tf$image$crop_to_bounding_box(image, offset_height, offset_width, hbox, wbox)
height <- dim(image)[1]
width  <- dim(image)[2]
aspect_ratio2 <- height/width
p2 <- ggplot() + 
  annotation_custom(rasterGrob(image/255, width=unit(1,"npc"), height=unit(1,"npc")), -Inf, Inf, -Inf, Inf) + 
  theme_void() +
  theme(aspect.ratio=aspect_ratio2)
print(p2)

####################### save figures
pdf(file="./Figures/image2.pdf", width=5/aspect_ratio2, height=5, paper="special")
print(p2)
dev.off()
#################################################################

####### Insert a new dimension (or axis) of size 1 into a tensor at a specified position (axis).
image <- tf$expand_dims(image, axis=0L)

####### Resize and pad the image to keep the aspect ratio and fit the expected size.
image <- tf$image$resize_with_pad(image, 256L, 256L)

height <- dim(image)[2]
width  <- dim(image)[3]
aspect_ratio3 <- height/width
p3 <- ggplot() + 
  annotation_custom(rasterGrob(image[1, , , ]/255, width=unit(1,"npc"), height=unit(1,"npc")), -Inf, Inf, -Inf, Inf) + 
  theme_void() +
  theme(aspect.ratio=aspect_ratio3)
print(p3)

####################### save figures
pdf(file="./Figures/image3.pdf", width=5/aspect_ratio3, height=5, paper="special")
print(p3)
dev.off()
#################################################################

# print(image$dtype)

input_image <- tf$cast(image, dtype=tf$uint8)

#####
# 4. Run the model

interpreter$allocate_tensors()

output_details <- interpreter$get_output_details()
print(output_details[[1]][["shape"]])
output_details[[1]][["index"]]

interpreter$set_tensor(input_details[[1]][["index"]], input_image)

interpreter$invoke()

MNout <- interpreter$get_tensor(output_details[[1]][["index"]])

# dim(MNout)


########
# 5. Get the network's prediction and visualize the identified key points.

# Calculate keypoints and edges
source("additional_functions.r")

labs <- c("nose", "eye_L", "eye_R", "ear_L", "ear_R", "shoulder_L", 
          "shoulder_R", "elbow_L", "elbow_R", "wrist_L", "wrist_R", "hip_L", 
          "hip_R", "knee_L", "knee_R", "ankle_L", "ankle_R")

keypts_edges <- movenet_singlepose_features(MNout, labs=labs, score_threshold=0.20)
keypoints <- keypts_edges[["keypoints"]]
edges     <- keypts_edges[["edges"]]

p4 <- p3 + xlim(c(0,1)) + ylim(c(0,1))

#### Add edges
p4 <- p4 + geom_segment(aes(x=x_start, y=y_start, xend=x_end, yend=y_end), 
                        color=edges$color, linewidth=1, data=edges)        

#### Add keypoints
p4 <- p4 + geom_point(aes(x=x, y=y, color=score), data=keypoints, size=3) +
  geom_point(aes(x=x, y=y), data=keypoints, color="white", size=1.25) +
  scale_fill_gradientn(colors = c("#FFCCCC", "#FF6666", "#FF0000", "#990000")) +
  scale_color_gradientn(colors = c("#FFCCCC", "#FF6666", "#FF0000", "#990000")) +
  theme(legend.position="inside", 
        legend.position.inside=c(0.9,0.5),
        legend.background = element_rect(fill = "black", color = "black"),
        legend.text = element_text(color = "white"),
        legend.title = element_text(color = "white"))
print(p4)

####################### save figures
pdf(file="./Figures/image4.pdf", width=5/aspect_ratio3, height=5, paper="special")
print(p4)
dev.off()
#################################################################

p5 <- p4
p5$layers <- p5$layers[-1]
p5 <- p5  +
  theme(legend.position="inside", 
        legend.position.inside=c(0.9,0.5),
        legend.text = element_text(color = "white"),
        legend.title = element_text(color = "white"),
        plot.background = element_rect(fill="black")) 
print(p5)

####################### save figures
pdf(file="./Figures/image5.pdf", width=5/aspect_ratio3, height=5, paper="special")
print(p5)
dev.off()
#################################################################



