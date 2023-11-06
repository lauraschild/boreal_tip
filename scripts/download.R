#script to download all necessary data
#script to download all necessary data

cat("\nThe complete input data needs to be downloaded from its sources to run the analyses.
    \nThis will take roughly 1 hour. Files that are already there will NOT be downloaded again.")
auto <- readline("Would you like to proceed with the download? (Y or N): ")

while(!(auto %in% c("N","Y"))){
  cat("please reply with Y or N.")
  auto <- readline("Would you like to proceed with the download? (Y or N): ")
}
if(auto == "N"){
  stop("You can download and unpack the data manually as well. But they are required to run the analysis code.
       \nAlternatively you can run the subset analysis. The data needed for this is included in the inpput directory.")
}
if(auto == "Y"){
  cat("\nDownloading data.")
  scripts <- list.files("scripts/downloads",
                        full.names = TRUE)
  
  lapply(scripts,
         source)
}
