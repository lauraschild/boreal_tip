#download CRU modern climate data
files <- list.files("input/CRU")

{
    cat("\n Downloading and unzipping CRU modern climate. ")
    
    #get urls for download
    paths <- readLines("input/CRU/paths.txt")
    
    #function for each url
    download_nicely <- function(path){
      
      #clean space from path
      path <- gsub(" ","",path)
      #get filename
      name <- tail(strsplit(path,"/")[[1]],1)
      #make destination filepath
      dest <- paste0("input/CRU/",name)
      
      #download if not already there
      if(!file.exists(dest)){
        download.file(path,
                      dest,
                      quiet = TRUE)
        R.utils::gunzip(dest,
                        remove=FALSE)
      }
      
    }
    
    #download all files
    shh <- lapply(paths,
           download_nicely)
  
}
