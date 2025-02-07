#get forest cover for northern hemisphere records
rm(list = ls())
library(tidyverse)

old_data <- data.table::fread("C:/Users/lschild/Desktop/boreal_tip/input/Pollen/reveals_only_large.csv")
names(old_data)

get_new_data <- function(continent){
  path <- "C:/Users/lschild/Downloads/"
  data.table::fread(paste0(path,continent,
                           "_complete_revised_SD_rounded.csv"),
                    select = c("Dataset_ID", "Longitude","Latitude",
                               "Age_mean [yrs BP]","forest [mean of cover in %]","valid_as_site")) %>% 
    filter(valid_as_site) %>% 
    as.data.frame() %>% 
    select(-valid_as_site) %>% 
    filter(Latitude >= 45,
           Latitude <= 70) %>% 
    return()
}

continents <- c("Asia","Europe","North_America")

new_data  <- lapply(continents,
                    get_new_data) %>% 
  bind_rows()

names(new_data)
names(new_data)[4:5] <- c("Age_BP","tree_REVEALS")

new_data <- new_data[,c(1,2,3,5,4)]

data.table::fwrite(new_data,
                   "C:/Users/lschild/Desktop/boreal_tip/input/Pollen/final_reveals_large.csv")
