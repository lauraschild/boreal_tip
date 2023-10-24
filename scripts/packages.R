#installing all packages needed

#check package existence
package_exists <- function(package_string){
  check <- system.file(package = package_string)
  check <- nzchar(check)
  return(check)
}

install_missing <- function(package_string){
  check <- package_exists(package_string)
  
  if(!check) install.packages(package_string)
}

#necessary
packages <- c("dplyr",
              "R.utils",
              "tidyr",
              "ggplot2",
              "sf",
              "raster",
              "stars",
              "terra",
              "data.table")

# cat("\nInstalling missing packages.")
# lapply(packages,
#        install_missing)

#loaded from beginning
packages <- c("dplyr","tidyr","ggplot2",
              "sf")
lapply(packages,
       require,
       character.only = TRUE)
#to load from the beginning
#dplyr, tidyr, ggplot2