#download CHELSA-TraCE21k paleo climate
rm(list = ls())
files <- list.files("input/CHELSA_TraCE")
{
  if(length(files) < 121){
    cat("\n Downloading CHELSA-TraCE paleo climate (this might take a while)")
    #get urls for download
    paths <- readLines("input/CHELSA_TraCE/paths.txt")
    
    #function for each url
    download_nicely <- function(path){
      
      #clean space from path
      path <- gsub(" ","",path)
      #get filename
      name <- tail(strsplit(path,"/")[[1]],1)
      #make destination filepath
      dest <- paste0("input/CHELSA_TraCE/",name)
      #download if not already there
      if(!file.exists(dest)){
        download.file(path,
                      dest,
                      method = "wget",
                      quiet = TRUE)
      }
      
    }
    
    #download all files
    lapply(paths,
           download_nicely)
  }
}

