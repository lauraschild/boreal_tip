#script to extract modern tree cover, climate, and landcover

#check if extractions were already done
MODIS_check <- file.exists(paste0("input/MODIS/",
                                  ifelse(sub,"sub_",""),
                                  "point_MODIS.csv"))
CRU_check <- file.exists(paste0("input/CRU/",
                                ifelse(sub,"sub_",""),
                                "points_CRU.csv"))

#make random points or get points from extracted MODIS
if(sum(MODIS_check,CRU_check) < 2){
  {
    if(sub){
      #get world map for landmask
      land <- rnaturalearth::ne_countries(scale = "small",
                                          returnclass = "sp")
      
      #crop to boreal region
      bound <- raster::extent(-180,180,45,70)
      #or to geographical subset for testing
      if(sub)bound <- raster::extent(5,15,45,55)
      land <- (terra::crop(land,bound))
      
      #sample random points in the area
      points <- sp::spsample(land,
                             n = ifelse(sub,
                                        2000,
                                        40000),
                             type = "regular") %>% 
        sf::st_as_sf() %>% 
        tibble::rowid_to_column("ID")
      rm(bound)
      rm(land)
    }
    if(!sub){
      points <- read.csv("input/MODIS/point_MODIS_complete.csv") %>% 
        select(-tree, -year) %>% 
        distinct(ID, Longitude, Latitude) %>% 
        st_as_sf(coords = c("Longitude","Latitude"),
                 crs = 4326)
    }
    
  }
  
  
  #extract data from GLC and remove points with anthropogenic influence
  {
    GLC_file <- paste0("input/GLC2000/",
                       ifelse(sub,"sub_",""),
                       "glc2000_v1_1.tif")
    GLC <- raster::raster(GLC_file,
                          proxy = TRUE)
    GLC <- raster::extract(GLC,
                           points)
    points <- points[!(GLC %in% c(16,17,18,20,21,22,23)),]
    
  }
}

#extract data from MODIS
if(!MODIS_check){
  {
    if(sub){
      MODIS_files <- list.files(path = "input/MODIS",
                                pattern = ".tif$")
      
      #which ones are the subset files
      sub_files <- grepl(pattern = "sub", MODIS_files)
      
      #select whichever ones we need
      if(sub){MODIS_files <- MODIS_files[sub_files]
      }else{MODIS_files <- MODIS_files[!sub_files]}
      
      #define function to get MODIS forest cover
      get_MODIS <- function(MODIS_file){
        proxy_MODIS <- stars::read_stars(paste0("input/MODIS/",MODIS_file),
                                         proxy = TRUE)
        
        #get year from filename
        year <- gsub(pattern = "065",
                     replacement = "",
                     gsub(pattern = "doy",
                          replacement = "",
                          tail(strsplit(names(proxy_MODIS),split = "_")[[1]],2)[1])) %>% 
          as.numeric()
        
        MODIS_forest <- stars::st_extract(proxy_MODIS,
                                          sf::st_transform(points,
                                                           sf::st_crs(proxy_MODIS))) %>% 
          sf::st_drop_geometry()
        cbind(points,tree = MODIS_forest[,1],year = year) %>% 
          return()
      }
      lapply(MODIS_files,
             get_MODIS) %>% 
        bind_rows() %>% 
        # mutate(Longitude = st_coordinates(.)[,1],
        #        Latitude = st_coordinates(.)[,2]) %>% 
        sf::st_drop_geometry() %>%
        mutate(tree = ifelse(tree == 200,
                             NA,
                             tree)) %>% 
        filter(!is.na(tree)) %>% 
        write.csv(paste0("input/MODIS/",
                         ifelse(sub,
                                "sub_",
                                ""),
                         "point_MODIS.csv"),
                  row.names = FALSE)
      
    }
    if(!sub){
      read.csv("input/MODIS/point_MODIS_complete.csv") %>% 
        filter(ID %in% points$ID) %>% 
        select(-Longitude,-Latitude) %>% 
        write.csv("input/MODIS/point_MODIS.csv",
                  row.names = FALSE)
    }
    
  }
  
}

#extract data from CRU
if(!CRU_check){
  {
    CRU_files <- list.files(path = "input/CRU",
                            pattern = ".nc$")
    
    #which ones are the subset files
    sub_files <- grepl(pattern = "sub", CRU_files)
    
    #select whichever ones we need
    if(sub){CRU_files <- CRU_files[sub_files]
    }else{CRU_files <- CRU_files[!sub_files]}
    
    #define function to get CRU data
    get_CRU <- function(CRU_file){
      CRU <- raster::brick(paste0("input/CRU/",CRU_file))
      
      #get years from file name
      years <- as.numeric(unlist(strsplit(CRU_file, split = "[.]"))[3:4])
      
      #how many years are covered? (minus 1)
      span <- diff(years)
      
      #summarize only summer months
      CRU_summer <- lapply(seq(0,(12*span),12),
                           function(x) raster::calc(CRU[[(6:8)+x]],mean)) %>% 
        raster::brick()
      #add correct years as names
      names(CRU_summer) <- years[1]:years[2]
      
      #extract point data
      summer_T <- cbind(points,raster::extract(CRU_summer,
                                               points)) %>% 
        pivot_longer(starts_with("X"),
                     names_to = "year",
                     values_to = "summerT") %>% 
        mutate(year = gsub("X","",year))
      
      return(summer_T)
    }
    
    lapply(CRU_files,
           get_CRU) %>% 
      bind_rows() %>% 
      sf::st_drop_geometry() %>% 
      write.csv(paste0("input/CRU/",
                       ifelse(sub,
                              "sub_",
                              ""),
                       "points_CRU.csv"),
                row.names = FALSE)
  }
}

if(sum(MODIS_check,CRU_check) < 2) rm(points)

