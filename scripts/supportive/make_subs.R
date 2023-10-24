rm(list = ls())
library(raster)
library(tidyverse)

path <- "input/MPI_ESM/glac1d"
files <- list.files(path = path,
                    pattern = ".nc$",
                    full.names = FALSE)

e <- as(extent(2,15,45,55), 'SpatialPolygons')
crs(e) <- "+proj=longlat +datum=WGS84 +no_defs"

crop_rasters <- function(file,
                         path){
  grid <- raster::brick(file.path(path,file))%>% 
    terra::rotate() #for MPI-ESM files
  grid <- crop(grid,e) 
  
  writeRaster(stack(grid),
              paste0(paste0(path,"/sub_",file)),
              overwrite = TRUE)
}

lapply(files,
       crop_rasters,
       path = path)


