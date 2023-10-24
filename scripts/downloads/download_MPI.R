#download MPI-ESM transient deglaciation paleo climate simulations
rm(list = ls())

#list MPI model runs
dirs <- list.dirs("input/MPI_ESM",
                  recursive = FALSE)

#get files for each run
for(directory in dirs){
  
  cat(paste0("\nGetting MPI-ESM files for model run '", gsub("input/MPI_ESM/","",directory),"'."))
  
  #list all files in directory
  files <- list.files(directory)
  script <- files[grepl(".sh",files)]
  #get links for wget from sh script
  wget <- read.csv(text = readLines(file.path(directory,script))[28:277],
                   sep = " ",
                   header = FALSE)
  download <- function(row){
    #get file link and name
    link <- gsub("'","",wget$V2[row])
    name <- gsub("'","",wget$V1[row])
    
    #exit function if it has already been downloaded
    if(name %in% files){
      return()
    } 
    
    #download
    download.file(link,
                  file.path(directory,name),
                  method = "wget",
                  quiet = TRUE)
  }
  
  shh <- lapply(1:nrow(wget),
         download)
}



