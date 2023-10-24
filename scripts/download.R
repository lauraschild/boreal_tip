#script to download all necessary data
rm(list=ls())

scripts <- list.files("scripts/downloads",
                      full.names = TRUE)

lapply(scripts,
       source)
