#script to reproduce publication figures
#the repository includes analysis output tables that are being used for this
#you can also replicate the entire analyses by using one of the other scripts (see the readme for an overview)

rm(list = ls())

sub <- FALSE

#install and load necessary packages
source("scripts/packages.R")

#create figure 5
source("scripts/plotting/boxplot.R")

#create fig 6
source("scripts/plotting/multimodality_maps.R")