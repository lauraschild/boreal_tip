#extract paleo continuous
#for surrogate creation

#get spatial and temporal resolution from pollen records
{
  pollen <- read.csv(paste0("input/Pollen/",
                            ifelse(sub,
                                   "sub_",
                                   ""),
                            "reveals2.csv"))
  
  #bin into 100yr time slices for CHELSA-TraCE and MPI-ESM
  points <- pollen %>% 
    distinct(Dataset_ID,Longitude,Latitude) %>% 
    sf::st_as_sf(coords = c("Longitude",
                            "Latitude"))
}

#extract info from CHELSA-TraCE
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
    bin <- (20-index)*100
    
    #extract climate for all pollen records
    CHELSA <- raster::brick(paste0("input/CHELSA_TraCE/",file))
    cbind(points,TJJA = c(raster::extract(CHELSA,points))) %>% 
      cbind(bin = bin) %>% 
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
                     "points_cont_CHELSA.csv"),
              row.names = FALSE)
  
}

#get MPI-ESM data at pollen resolution
{
  for(glacier in c("glac1d","ice6g")){
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
      
      #climate older than 12000 will not be used for surrogate creation
      if(Age_BP > 12000) return()

      #extract climate for all points
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
        return()
    }
    
    MPI_TJJA <- lapply(MPI_files,
                       get_MPI) %>% 
      bind_rows()%>% 
      mutate(bin = cut(year,
                       seq(-150,20100,100),
                       seq(-100,20000,100))) %>% 
      select(-year) %>% 
      group_by(Dataset_ID, bin) %>% 
      summarize_all(mean) %>% 
      write.csv(paste0("input/MPI_ESM/",
                       glacier,"/",
                       ifelse(sub,
                              "sub_",
                              ""),
                       "points_cont_MPI.csv"),
                row.names = FALSE)
  }
}

