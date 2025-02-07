#extract paleo climate data for pollen record locations and timesteps

#check if extractions have already run
CHELSA_check <- file.exists(paste0("input/CHELSA_TraCE/",
                                   ifelse(sub,"sub_",""),
                                   "points_cont_CHELSA.csv"))
glac1d_check <- file.exists(paste0("input/MPI_ESM/glac1d/",
                                   ifelse(sub,"sub_",""),
                                   "points_cont_MPI.csv"))
ice6g_check <- file.exists(paste0("input/MPI_ESM/ice6g/",
                                  ifelse(sub,"sub_",""),
                                  "points_cont_MPI.csv"))

#get spatial and temporal resolution from pollen records
if(sum(CHELSA_check,ice6g_check,glac1d_check) < 3){
  {
    # pollen <- read.csv(paste0("input/Pollen/",
    #                           ifelse(sub,
    #                                  "sub_",
    #                                  ""),
    #                           "reveals_large.csv"))
    pollen <- read.csv(pollen_file)
    
    #bin into 100yr time slices for CHELSA-TraCE and MPI-ESM
    binned_coords <- pollen %>% 
      filter(Age_BP <= 8000) %>% 
      mutate(bin = cut(Age_BP,
                       seq(-150,20100,100),
                       seq(-100,20000,100))) %>%  #bin label denotes the middle of the bin
      mutate(bin = as.numeric(as.character(bin))) %>% 
      distinct(Dataset_ID,Longitude,Latitude,bin) %>% 
      sf::st_as_sf(coords = c("Longitude",
                              "Latitude"))
  }
}


#extract info from CHELSA-TraCE
if(!CHELSA_check){
  {
    CHELSA_files <- list.files(path = "input/CHELSA_TraCE",
                               pattern = "tif$")
    
    sub_files <- grepl("sub",CHELSA_files)
    if(sub){
      CHELSA_files <- CHELSA_files[sub_files]
    }else{
      CHELSA_files <- CHELSA_files[!sub_files]
    }
    
    get_CHELSA <- function(file){
      #convert TRACE age index to yrs BP
      index <- as.numeric(tail(unlist(strsplit(file, split = "_")),2)[1])
      age <- (20-index)*100
      #get necessary bins for this ID
      points <- binned_coords %>% 
        filter(bin == age)
      
      #exit if no pollen samples at this age
      if(nrow(points) == 0) return()
      
      #extract points that are necessary
      CHELSA <- raster::brick(paste0("input/CHELSA_TraCE/",file))
      cbind(points,TJJA = c(raster::extract(CHELSA,points))) %>% 
        return()
    }
    
    lapply(CHELSA_files,
           get_CHELSA) %>% 
      bind_rows() %>% 
      sf::st_drop_geometry() %>% 
      arrange(Dataset_ID,bin) %>% 
      write.csv(paste0("input/CHELSA_TraCE/",
                       ifelse(sub,
                              "sub_",
                              ""),
                       "points_pollen_CHELSA.csv"),
                row.names = FALSE)
    
  }
}


#get MPI-ESM data at pollen resolution
{
  glaciers <- c()
  if(!glac1d_check) glaciers <- c(glaciers,"glac1d")
  if(!ice6g_check) glaciers <- c(glaciers, "ice6g")
  for(glacier in glaciers){
    MPI_files <- list.files(path = paste0("input/MPI_ESM/",
                                          glacier),
                            pattern = ".nc$")
    sub_files <- grepl("sub",MPI_files)
    if(sub){
      MPI_files <- MPI_files[sub_files]
    }else{
      MPI_files <- MPI_files[!sub_files]
    }
    
    get_MPI <- function(file){
    
      age_IDs <- gsub(pattern = "[.nc]",
                   replacement = "",
                   unlist(strsplit(tail(unlist(strsplit(file, 
                                                        split = "_")),1),
                                   split = "-"))) %>% 
        substr(1,5) %>% 
        as.numeric()
      
      Age_BP <- 25001 - age_IDs[1]

      points <- binned_coords %>% 
        filter(bin == Age_BP | bin == Age_BP-100)
      
      #exit if no pollen samples at this age
      if(nrow(points) == 0) return()
      
      #extract points that are necessary
      MPI <- raster::brick(paste0("input/MPI_ESM/",glacier,"/",file))
      if(!sub) MPI <- terra::rotate(MPI)
      MPI <- MPI - 273.15
      MPI_summer <- lapply(seq(0,12*99,12),
                           function(x) raster::calc(MPI[[(6:8)+x]],mean))%>% 
        raster::brick()
      names(MPI_summer) <- (Age_BP-1):(Age_BP-100) 
      MPI_TJJA <-raster::extract(MPI_summer,
                      points) %>% 
        cbind(points) %>% 
        sf::st_drop_geometry() %>% 
        as.data.frame()

      MPI_TJJA %>% 
        pivot_longer(starts_with("X"),
                     names_to = "year",
                     values_to = "TJJA") %>% 
        mutate(year = as.numeric(gsub("X","",
                                      year))) %>% 
        filter(year <= bin +50 & year >= bin-50) %>% 
        select(-year) %>% 
        return()
    }
    
    MPI_TJJA <- lapply(MPI_files,
                       get_MPI) %>% 
      bind_rows()%>% 
      group_by(Dataset_ID, bin) %>% 
      summarize_all(mean) %>% 
      write.csv(paste0("input/MPI_ESM/",
                       glacier,"/",
                       ifelse(sub,
                              "sub_",
                              ""),
                       "points_pollen_MPI.csv"),
                row.names = FALSE)
  }
}

