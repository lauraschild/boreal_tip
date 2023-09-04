#script to unpack MODIS data
rm(list = ls())

if(!"MOD44" %in% list.files("input/MODIS")){
  archive <-list.files("input/MODIS",
                       pattern = ".zip")
  cat("\nUnzipping MODIS data.")
  unzip(file.path("input/MODIS",archive),
        exdir = "input/MODIS",
        junkpaths = TRUE)
}
cat("\nMODIS unzipped.")


