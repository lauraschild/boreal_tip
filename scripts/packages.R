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
              "data.table",
              "ggsci",
              "rnaturalearth",
              "zen4R",
              "ncdf4",
              "pastecs",
              "splus2R",
              "zoo",
              "diptest",
              "multimode")

#check for existence
missing <- packages[!unlist(lapply(packages,package_exists))]

if(length(missing) >0){
  cat("\nYou are missing the following packages: ")
  cat(paste("\n",missing))
  auto <- readline("Would you like to install them now? (Y or N): ")
  
  while(!(auto %in% c("N","Y"))){
    cat("please reply with Y or N.")
    auto <- readline("Would you like to install them now? (Y or N): ")
  }
  if(auto == "N"){
    stop("You can download and install the packages manually as well. But they are required to run the analysis code.")
  }
  if(auto == "Y"){
    cat("\nInstalling missing packages.")
    lapply(packages,
           install_missing)
  }
  
}

missing <- packages[!unlist(lapply(packages,package_exists))]

if(length(missing) == 0){
  #loaded from beginning
  packages <- c("dplyr","tidyr","ggplot2",
                "sf")
  lapply(packages,
         require,
         character.only = TRUE)
}
