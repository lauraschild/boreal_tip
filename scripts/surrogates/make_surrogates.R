#script to create surrogates
#output are surrogate tree cover data for all pollen records with differing errors

climate_sources <- c("CHELSA_TraCE","glac1d","ice6g")

source("scripts/surrogates/surrogate_functions3.R")
#load pollen-based forest cover and bin into 100yr slices
# pollen_file <- paste0("input/Pollen/",
#                       ifelse(sub,"sub_",""),
#                       "reveals_large.csv")
pollen_file <- pollen_file

pollen <- read.csv(pollen_file) %>% 
  mutate(bin = as.numeric(as.character(cut(Age_BP,
                                           seq(-150,20100,100),
                                           seq(-100,20000,100))))) %>% 
  select(-Age_BP) %>% 
  group_by(Dataset_ID, bin) %>% 
  summarize_all(mean) %>% 
  filter(bin <= 11000)

for(climate_source in climate_sources){
  
  #get continuous record climate
  climate_file <- paste0("input/MPI_ESM/",climate_source,"/",
                         ifelse(sub,"sub_",""),"points_cont_MPI.csv")
  if(climate_source == "CHELSA_TraCE"){
    climate_file <- paste0("input/",climate_source,"/",
                           ifelse(sub,"sub_",""),"points_cont_CHELSA.csv")
  }
  climate <- read.csv(climate_file) %>% 
    filter(!is.na(TJJA))
  
  #make surrogates for all record locations
  IDs <- unique(pollen$Dataset_ID)
  surrogates <- lapply(IDs,
                       surrogate_wrap,
                       pollen_df = pollen,
                       climate_df = climate) %>% 
    bind_rows()
  coords <- pollen %>% 
    distinct(Dataset_ID,Longitude, Latitude)
  
  surrogates <- merge(surrogates,
                      coords,
                      by = "Dataset_ID",
                      all.x = TRUE)

  
  write.csv(surrogates,
            paste0("output/tables/surrogates/",
                   ifelse(sub,"sub_",""), climate_source,".csv"),
            row.names = FALSE)
}
