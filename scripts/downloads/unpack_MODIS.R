#script to unpack MODIS data
rm(list = ls())

if(length(list.files("input/MODIS")) == 1){
  archive <-list.files("input/MODIS",
                       pattern = ".zip")
  cat("\nUnzipping MODIS data.")
  unzip(file.path("input/MODIS",archive),
        exdir = "input/MODIS",
        junkpaths = TRUE)
}
cat("\nMODIS unzipped.")


