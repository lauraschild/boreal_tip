#script to reproduce publication figures
#the repository includes analysis output tables that are being used for this
#you can also replicate the entire analyses by using one of the other scripts (see the readme for an overview)

rm(list = ls())

sub <- FALSE

#please adjust the working directory to the repository path
setwd(".")

{####working directory check ####
  #script to check the working directory
  cat("\n\t\tINFO\nYou will need to change the working directory to the repository path, i.e. '../boreal_tip'. 
    \nAlternatively you can open the R Project located in the repository first and then open this script. The working directory will then be set automatically.")
  
  if(!grepl(pattern = "boreal_tip-main$",getwd())){
    warning("It looks like you didn't adjust the working directory to the repository path yet.")
    warning("\n(Unless you renamed the directory. In that case you may ignore this warning.)")
  }
}

#check if input folder from Zenodo was downloaded (and download if needed)
source("scripts/downloads/download_input.R")

#install and load necessary packages
source("scripts/packages.R")

#create figure 5
source("scripts/plotting/boxplot.R")

#create fig 6
source("scripts/plotting/multimodality_maps.R")