#creating modern and paleo stability landscapes
#output from this script are files with stable and unstable point information

source("scripts/supportive/stability_functions.R")

landscape_types <- c("modern",
                     "CHELSA",
                     "glac1d",
                     "ice6g")

for(landscape in landscape_types){
  print(landscape)
  #load forest cover
  # forest_file <- paste0("input/pollen/",
  #                ifelse(sub,"sub_",""),
  #                "reveals_large.csv")
  forest_file <- pollen_file
  if(landscape == "modern"){
    forest_file <- paste0("input/MODIS/",ifelse(sub,"sub_",""),"point_MODIS.csv")
    
    forest <- read.csv(forest_file) %>% 
      rename(bin = year,
             Dataset_ID = ID,
             tree_final = tree)
  }else{
    forest <- read.csv(forest_file) %>% 
      mutate(bin = cut(Age_BP,
                       seq(-150,20100,100),
                       seq(-100,20000,100))) %>% 
      select(-Age_BP) %>% 
      group_by(Dataset_ID, bin) %>% 
      summarize_all(mean) %>% 
      rename(tree_final = tree_REVEALS)
  }
  
  
  
  #load climate
  climate_file <- paste0("CRU/",ifelse(sub,"sub_",""),
                         "points_CRU.csv")
  if(landscape == "CHELSA") climate_file <- paste0("CHELSA_TraCE/", ifelse(sub,"sub_",""),"points_pollen_CHELSA.csv")
  if(landscape %in% c("glac1d","ice6g")){
    climate_file <- paste0("MPI_ESM/",landscape,"/",ifelse(sub,"sub_",""),
                           "points_pollen_MPI.csv")
  }
  
  climate <- read.csv(file.path("input",climate_file))
  if(landscape == "modern"){
    climate <- climate %>% 
      rename(Dataset_ID = ID,
             bin = year,
             TJJA = summerT)
  }
  
  #merge data
  complete <-merge(forest,
                   climate,
                   by = c("Dataset_ID","bin")) %>% 
    mutate(cell = 1) #%>% 
    #filter(Latitude >=+ 55)
  
  #make landscape intervals
  steps <- make_steps(100,
                      0:25,
                      "window")
  window <- steps[[2]]
  steps <- steps[[1]]
  
  intervals <- data.frame(steps - window,
                          steps)
  
  
  names(intervals) <- paste("TJJA",c("start","stop"),
                            sep = "_")
  
  stable_df <- stability_points(intervals = intervals,
                                df = complete,
                                tree_variable = "final",
                                univariate = "TJJA",
                                threshold = "diff",
                                cell = 1,
                                parallel = FALSE)
  
  write.csv(stable_df,
            paste0("output/tables/stability_landscapes/",
                   ifelse(sub,"sub_",""),landscape,"_stable_df.csv"),
            row.names = FALSE)
  
  #quick plot
  p <- ggplot()+
    geom_point(data = complete,
               aes(TJJA,
                   tree_final),
               col ="grey")+
    geom_point(data =stable_df,
               aes((TJJA_start + 0.5),
                   tree,
                   col = factor(type,
                                labels = c("stable","unstable"))),
               size = 2)+
    theme_bw()+
    labs(y = "forest cover (%)",
         x = "TJJA (°C)",
         title = paste0("Stability landscape: ",landscape, ifelse(sub," (subset)","")))+
    theme(legend.title = element_blank(),
          legend.position = c(0.1,0.9))
  
  ggsave(plot = p,
         filename = paste0("output/figures/additional/stability_landscapes/",
                ifelse(sub,"sub_",""),landscape,".png"),
         width = 8,
         height = 5)
  
}

rm(forest,climate,complete,stable_df,forest_file,climate_file)
