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
# 5.3 Analyzing a player's spatial distribution
######################
rm(list=ls())
graphics.off()

load("./Data/tracking_data.Rdata")
pacman::p_load(ggplot2, dplyr)
source("additional_functions.r")

# Select player
# Home
plyr <- "Shaun Livingston"
# plyr <- "Deron Williams"
# plyr <- "Paul Pierce"
# Away
# plyr <- "LeBron James"
# plyr <- "Dwyane Wade"
# plyr <- "Mario Chalmers"


plyrh <- is_home(tdata_wide,plyr)


##### flip coordinates
dts1 <- tdata_long %>%
  filter(name==plyr & (quarter==1 | quarter==2)) 
dts2 <- tdata_long %>%
  filter(name==plyr & (quarter==3 | quarter==4))  %>%
  mutate(x=-x, y=-y)

dts <- rbind(dts1, dts2)
if (plyrh) {
  dts <- dts %>% mutate(x=-x, y=-y)
}

ttl <- paste0("Player: ", plyr, " (", ifelse(plyrh, "Home team", "Away team"), ")")

p <- ggplot() +
  stat_density_2d(data=dts, aes(x=x, y=y, fill=after_stat(level)), alpha=0.4,
                  linewidth=2, bins=8, geom = "polygon", show.legend=F) +
  geom_text(aes(x=0, y=26, label=ttl), size=6, vjust=0.2) +
  geom_text(aes(x=-47, y=26, label="Defence side"), hjust=0, vjust=0.2, size=5) +
  geom_text(aes(x=47, y=26, label="Offence side"), hjust=1, vjust=0.2, size=5) +
  scale_fill_gradient(low = "blue", high = "red") +
  coord_fixed() +
  ylim(c(-30,30)) + xlim(c(-55,55)) +
  plot_court() +
  theme_void()

dev.new(width=15, height=8)
print(p)

####################### save figures
# dir.create("./Figures", showWarnings=FALSE)

filename <- paste("./Figures/spatialdensity ",plyr,".pdf",sep="")
pdf(file=filename, width=12, height=7, paper="special")
print(p)
dev.off()
#################################################################




