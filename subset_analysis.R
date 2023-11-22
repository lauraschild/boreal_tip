#this script will let you run the complete analysis within a geographical subset
#all of the data necessary is already in this repository
#you won't need to download anything
#of course the results will differ from those with the entire data set, but you will be able to follow and check the code
#geographical subset boundaries:
#xlim = 5, xmax = 15, ymin = 45, ymax =55

rm(list = ls())

sub <- TRUE

#please adjust the working directory to the repository path
setwd(".")

{####working directory check ####
  #script to check the working directory
 cat("\n\t\tINFO\nYou will need to change the working directory to the repository path, i.e. '../boreal_tip'. 
    \nAlternatively you can open the R Project located in the repository first and then open this script. The working directory will then be set automatically.")
  
  if(!grepl(pattern = "boreal_tip",getwd())){
    warning("It looks like you didn't adjust the working directory to the repository path yet.")
    warning("\n(Unless you renamed the directory. In that case you may ignore this warning.)")
  }
}

#checking if necessary packages are installed
source("scripts/packages.R")

#check if input folder from Zenodo was downloaded (and download if needed)
source("scripts/downloads/download_input.R")

#extracting random points for modern climate and forest cover
source("scripts/extraction/extract_modern.R")

#extracting pollen points for paleo clim
source("scripts/extraction/extract_paleo_pollen.R")

#make stability landscapes from modern and paleo data
#you can have a look at the produced landscapes in the figures directory labeled "additional"
source("scripts/landscaping/make_landscapes.R")

#get continuous paleo climate
source("scripts/extraction/extract_paleo_continuous.R")

#make surrogates
source("scripts/surrogates/make_surrogates.R")

#analyse gridded multimodality
source("scripts/multimodality/multimod_tests.R")

#plot publication figures with results
source("scripts/plotting/multimodality_maps.R")

source("scripts/plotting/boxplot.R")
