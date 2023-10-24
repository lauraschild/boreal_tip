#surrogate functions
surrogate_wrap <- function(Dataset_ID,
                           pollen_df,
                           climate_df){
  record_ages <- pollen_df$bin[pollen_df$Dataset_ID == Dataset_ID]
  continuous_temp <- climate_df$TJJA[climate_df$Dataset_ID == Dataset_ID]
  continuous_ages <- climate_df$bin[climate_df$Dataset_ID == Dataset_ID]
  record_tree_cover <- pollen_df$tree_REVEALS[pollen_df$Dataset_ID == Dataset_ID]
  
  surrogate_per_record(record_ages,
                       continuous_temp,
                       continuous_ages,
                       record_tree_cover,
                       Dataset_ID) %>% 
    return()
}
#modern tipping surrogate
tip_it_paleoREVEALS <- function(temperature_vector,
                                age_vector,
                                tree_cover_vector = NULL){
  
  #make sure all vectors are in order
  age_vector <- as.numeric(as.character(age_vector))
  correct_order <- order(age_vector, decreasing = TRUE)
  temperature_vector <- temperature_vector[correct_order]
  age_vector <- age_vector[correct_order]
  
  #### GET STARTING CONDITIONS ####
  #set starting temperature
  temp_start <- temperature_vector[1]
  
  #function to get a new biome for a given temperature
  
  get_biome <- function(temperature){
    intervals <- c(8,9,11,16,17,18.5,21.5,26,100)
    possible_biomes <- list(c("tundra"),
                            c("tundra","boreal_forest"),
                            c("tundra","boreal_forest","savanna1"),
                            c("boreal_forest","savanna1"),
                            c("steppe","boreal_forest","savanna1"),
                            c("steppe","boreal_forest"),
                            c("steppe","boreal_forest","savanna2"),
                            c("steppe","boreal_forest"),
                            c("steppe"))
    interval_fit <- min(which(temperature <= intervals))
    matches <- possible_biomes[[interval_fit]]
    biome <- sample(matches,1)
    if(length(matches) == 1) biome <- matches
    return(biome)
  }
  
  #get (random) starting biome based on starting temperature
  biome_start <- get_biome(temp_start)
  
  #biome information
  biomes <- c("tundra","savanna1","savanna2","boreal_forest","steppe")
  cover <- c(5,25,35,80,7)
  
  #if given, get starting biome based on paleo tree cover
  if(!is.null(tree_cover_vector)){
    tree_start <- tree_cover_vector[1]
    possible_biomes <- cover
    p <- runif(1)
    if(p > 0.5) possible_biomes <- rev(possible_biomes)
    
    #which biome TC is closes to the paleo start
    TC_start <- possible_biomes[which.min(abs(possible_biomes - tree_start))]
    
    biome_start <- biomes[match(TC_start,cover)]
  }
  #### FORWARD MODELLING WITH MEMORY ####
  model_biomes <- c(biome_start)
  old_biome <- biome_start
  for(new_temperature in temperature_vector[-1]){
    
    if(old_biome == "boreal_forest"){
      new_biome <- old_biome
      if(new_temperature >= 26| new_temperature <= 8){
        new_biome <- get_biome(new_temperature)
      }
    }
    
    if(old_biome == "tundra"){
      new_biome <- old_biome
      if(new_temperature >= 11){
        new_biome <- get_biome(new_temperature)
      }
    }
    
    if(old_biome == "steppe"){
      new_biome <- old_biome
      if(new_temperature <= 16){
        new_biome <- get_biome(new_temperature)
      }
    }
    
    if(old_biome == "savanna1"){
      new_biome <- old_biome
      if(new_temperature <= 9 | new_temperature >= 17){
        new_biome <- get_biome(new_temperature)
      }
    }
    
    if(old_biome == "savanna2"){
      new_biome <- old_biome
      if(new_temperature >= 21.5 | new_temperature <=18.5){
        new_biome <- get_biome(new_temperature)
      }
    }
    
    model_biomes <- c(model_biomes,new_biome)
    old_biome <- new_biome
    rm(new_biome)
  }
  
  #### CREATE OUTPUT ####
  #translate string biomes into cover
  model_cover <- cover[match(model_biomes, biomes)]
  modeled_site <- data.frame(age = age_vector,
                             temperature = temperature_vector,
                             cover = model_cover)
  
  return(modeled_site)
}

surrogate_per_record <- function(record_ages,
                                 continuous_temp,
                                 continuous_ages,
                                 record_tree_cover,
                                 Dataset_ID){
  source("scripts/surrogates/paleospec_functions.R",
         local = TRUE)
  
  #make sure all vectors are in order
  continuous_ages <- as.numeric(as.character(continuous_ages))
  correct_order <- order(continuous_ages, decreasing = TRUE)
  continuous_temp <- continuous_temp[correct_order]
  continuous_ages <- continuous_ages[correct_order]
  record_tree_cover <- record_tree_cover[order(record_ages, decreasing = TRUE)]
  
  
  #define function to add scatter
  scatter_me <- function(signal,
                         SD){
    scattered_signal <- signal + rnorm(length(signal),
                                       sd = SD)
    scattered_signal <- ifelse(scattered_signal > 100,
                               100,
                               ifelse(scattered_signal <0,
                                      0,
                                      scattered_signal))
    
    return(scattered_signal)
  }
  
  start <- which(continuous_ages == max(record_ages))
  surrogates_start <- data.frame(Dataset_ID = Dataset_ID,
                                 slice = continuous_ages[start:length(continuous_ages)],
                                 TJJA = continuous_temp[start:length(continuous_temp)])
  
  # GAUSSIAN ####
  a <- 70 #highest value
  b <- 15 #position of center
  c <- 5  #SD
  
  surrogates_start$tree_uni <- a*exp(-(((surrogates_start$TJJA-b)^2)/(2*c^2)))
  
  surrogates_start$tree_tip <- tip_it_paleoREVEALS(surrogates_start$TJJA,
                                                   surrogates_start$slice,
                                                   record_tree_cover)$cover
  #scatter all surrogates
  scatter_all <- function(surrogates){
    scatter5 <- apply(surrogates[,4:ncol(surrogates)],
                      2,
                      scatter_me,
                      SD = 5)
    
    if(!is.matrix(scatter5)) scatter5 <- t(as.data.frame(scatter5))
    
    colnames(scatter5) <- paste0(names(surrogates[,4:ncol(surrogates)]),"_scatter5")
    
    scatter10 <- apply(surrogates[,4:ncol(surrogates)],
                       2,
                       scatter_me,
                       SD = 10)
    if(!is.matrix(scatter10)) scatter10 <- t(as.data.frame(scatter10))
    colnames(scatter10) <- paste0(names(surrogates[,4:ncol(surrogates)]),"_scatter10")
    
    scatter15 <- apply(surrogates[,4:ncol(surrogates)],
                       2,
                       scatter_me,
                       SD = 15)
    if(!is.matrix(scatter15)) scatter15 <- t(as.data.frame(scatter15))
    colnames(scatter15) <- paste0(names(surrogates[,4:ncol(surrogates)]),"_scatter15")
    
    surrogates <- cbind(surrogates,cbind(cbind(scatter5,scatter10),scatter15))
    return(surrogates)
  }
  
  #### Lowpass ####
  #tau <- mean(diff(record_ages))
  tau <- 1000
  #pad (length of Lowpass Filter (has to be odd))
  #pad <- 1*max(diff(record_ages))
  pad <- 2000
  if(pad %% 2 == 0) pad <- pad+1
  filter <- Lowpass(omega.c = 1/tau,
                    n = pad)
  
  filter_per_col_start <- function(column){
    annual <- approx(surrogates_start$slice,
                     column,
                     continuous_ages[start]:tail(continuous_ages,1))$y
    filtered <- cbind(Age = continuous_ages[start]:tail(continuous_ages,1),
                      tree = as.vector(ApplyFilter(annual,filter)))%>%
      as.data.frame()%>%
      mutate(slice = cut(Age,
                         breaks = c(-41, seq(50,11950,100)),
                         right = FALSE,
                         labels = c(seq(0,11900, 100))))%>%
      group_by(slice)%>%
      summarize(tree = mean(tree, na.rm = TRUE))%>%
      arrange(desc(slice))%>%
      pull(tree)
    return(filtered)
  }
  
  if(length(filter) > max(record_ages)){
    lowpass_surrogates_start <- surrogates_start
    lowpass_surrogates_start[,4:ncol(lowpass_surrogates_start)] <- NA
  }else{
    lowpass_surrogates_start <- surrogates_start[,1:3] %>%
      cbind(apply(surrogates_start[,4:ncol(surrogates_start)],
                  2,
                  filter_per_col_start))
  }
  #scatter all
  lowpass_surrogates_start <- scatter_all(lowpass_surrogates_start)
  
  lowpass_surrogates_start <- lowpass_surrogates_start[match(record_ages, lowpass_surrogates_start$slice),]
  
  return(lowpass_surrogates_start)
}