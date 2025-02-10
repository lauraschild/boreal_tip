
#this script will let you run the complete analysis for the entire study area
#as this involves the extraction of data from many climate and forest cover rasters this will take a while
#when running for the first time the data will also be downloaded, which will take up to several hours
#you can also run the analyses using the subset option (see other script or adjust sub boolean)

rm(list = ls())

sub <- FALSE

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

#get the input folder from zenodo
source("scripts/downloads/download_input.R")

#downloading the necessary data (after checking if it has already been downloaded)
source("scripts/download.R")

pollen_file <- "input/pollen/final_reveals_large.csv"

#extracting random points for modern climate and forest cover
source("scripts/extraction/extract_modern.R")

#extracting pollen points for paleo clim
source("scripts/extraction/extract_paleo_pollen.R")

#make stability landscapes from modern and paleo data
#you can have a look at the produced landscapes in the figures directory labeled "additional"
select <- dplyr::select
source("scripts/landscaping/make_landscapes.R")

#get continuous paleo climate
source("scripts/extraction/extract_paleo_continuous.R")

#make surrogates
source("scripts/surrogates/make_surrogates.R")

#analyse gridded multimodality
exclude_temp <- FALSE
source("scripts/multimodality/multimod_tests.R")

#plot publication figures with results
source("scripts/plotting/multimodality_maps.R")

source("scripts/plotting/boxplot.R")

cowplot::plot_grid(p1,p2,
                   nrow = 1,
                   labels = "auto")

ggsave("output/figures/publication/panels_combined.png",
       dpi = 300,
       width = 14,
       height = 5,
       unit = "in") 

