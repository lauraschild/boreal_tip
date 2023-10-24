#multimodality analysis
source("scripts/supportive/stability_functions.R")

data_types <- c("pollen","CHELSA_TraCE", "glac1d","ice6g")

for(data_type in data_types){
  #load tree cover data
  tree_file <- paste0("input/Pollen/", 
                      ifelse(sub,"sub_",""),
                      "reveals2.csv")
  if(data_type != "pollen"){
    tree_file <- paste0("output/tables/surrogates", ifelse(sub,"/sub_","/"),
                        data_type,".csv")
  }
  
  trees <- read.csv(tree_file)
  
  #regional information
  grid <- raster::raster(ncol = 36,
                         nrow = 5,
                         ymn = 45,
                         ymx = 70)
  raster::values(grid) <-  1:(5*36)
  
  trees$cell <- raster::extract(grid,
                                trees[,c("Longitude","Latitude")])
  if("slice" %in% names(trees)){
    trees <- trees %>% 
      filter(!is.na(cell),
             slice <= 8000)
  }else{
    trees <- trees %>% 
      filter(!is.na(cell),
             Age_BP <= 8000)
  }

  
  cells <- unique(trees$cell)
  
  multimod_per_cell <- function(cell_number){
    pure_trees <- trees %>% 
      filter(cell == cell_number) %>% 
      select(starts_with("tree"))
    
    results <- apply(pure_trees,
                     2,
                     bimod_tests)
    
    bind_rows(results) %>% 
      mutate(tree_type = names(results),
             cell = cell_number) %>% 
      return()
  }
  
  lapply(cells,
                multimod_per_cell) %>% 
    bind_rows() %>% 
    write.csv(paste0("output/tables/multimodality/",
                     ifelse(sub,"sub_",""), data_type,".csv"),
              row.names = FALSE)
}
