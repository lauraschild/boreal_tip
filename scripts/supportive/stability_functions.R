
#stability landscape functions
#### MANIPULATE DF ####
#create PFT df with ONE climate version
get_climate_df <- function(df,
                           climate = c("model","modern","WAPLS","MAT")){
  ID_variables <- c("Dataset_ID",
                    "slice",
                    "Longitude",
                    "Latitude")
  if(climate %in% c("WAPLS","MAT")){
    df <- df %>%
      dplyr::select(all_of(ID_variables), starts_with("tree"),
                    ends_with(climate)) %>%
      rename(prec = as.name(paste0("Prec_",
                                   climate)),
             MAT = as.name(paste0("MAAT_",
                                  climate)),
             summerT = as.name(paste0("TJul_",
                                      climate)))
    
    return(df)
  }
  df <- df %>%
    dplyr::select(all_of(ID_variables),
                  starts_with("tree"),
                  starts_with(climate))%>%
    rename(prec = as.name(paste0(climate,
                                 "_prec")),
           MAT = as.name(paste0(climate,
                                "_MAT")),
           summerT = as.name(paste0(climate,
                                    "_JJA")))
  return(df)
  
}

#assigns gridcell values to records
#can be done randomly --> same amount of samples per gridcell
assign_gridcells <- function(df,
                             nrow,
                             ncol,
                             random = c("none",
                                        "record",
                                        "sample")){ #randomize which samples are in which gridcell
  library(raster)
  coordinates <- df[,c("Longitude","Latitude")]
  
  grid <- raster(nrow = nrow,
                 ncol = ncol)
  values(grid) <- 1:(nrow*ncol)
  
  cells <- extract(grid,
                   coordinates)
  
  df$cell <- cells
  
  if(random == "sample"){ 
    df$cell <- sample(cells,
                      nrow(df),
                      replace = FALSE)
  }
  
  if(random == "record") {
    record_assign <- df %>%
      group_by(Dataset_ID)%>%
      summarize(cell = unique(cell))
    
    record_assign$cell <- sample(record_assign$cell,
                                 nrow(record_assign),
                                 replace = FALSE)
    #merge with orig df
    df <-   df %>%
      dplyr::select(-cell)%>%
      merge(record_assign,
            by = "Dataset_ID")
  }
  
  world <- rnaturalearth::ne_countries(scale = "medium", returnclass ="sf")
  
  plot(world$geometry, ylim = c(30,90),
       main = "Grid for regional seperation")
  points(coordinates,
         cex = 0.4,
         col = "darkblue")
  plot(rasterToPolygons(grid), add = TRUE)
  #plot(grid, add = TRUE)
  text(grid,
       halo = TRUE,
       hw = 0.2)
  
  return(df)
  
}

assign_regions <- function(df,
                           region,
                           random = c("none",
                                      "record",
                                      "sample")){
  index <- data.frame(region = c("complete",
                                 "east_west",
                                 "continents",
                                 "continents_latitude",
                                 "subregions",
                                 "köppen"),
                      column = c("complete_ID",
                                 "east_west_ID",
                                 "continent_ID",
                                 "cont_lat_ID",
                                 "subregion_ID",
                                 "köppen_ID"))
  region_ID <- as.character(index$column[index$region == region])
  #load file with assignments
  file <- "assignment4.csv"
  
  if("tree_RS" %in% names(df)){
    file <- "assignment5_RS.csv"
  } 
  
  path <- "new_data/spatial/"
  
  assignments <- read.csv(paste0(path,file))%>%
    mutate(complete_ID = 1) %>%
    dplyr::select(Dataset_ID, as.name(region_ID))
  #merge
  df <- df %>%
    merge(assignments,
          by = "Dataset_ID",
          all.x = TRUE)
  
  names(df)[ncol(df)] <- "cell"
  
  df <- filter(df, !is.na(cell))
  
  if(random == "sample") df$cell <- sample(df$cell, nrow(df), replace = FALSE)
  if(random == "record") {
    records <- df %>%
      group_by(Dataset_ID) %>%
      summarize(cell = unique(cell))
    
    records$cell <- sample(records$cell, nrow(records), replace = FALSE)
    
    df <- merge(df[,-ncol(df)],
                records,
                by = "Dataset_ID")
  }
  
  return(df)
}

#### MAKE CLIMATE BINS ####
#create intervals with equal number of group members
make_steps <- function(bin_number,
                       clim_vector,
                       bin_type){
  clim_vector <- clim_vector[!is.na(clim_vector)]
  if(bin_type == "width"){
    steps <- seq(min(clim_vector, na.rm = TRUE),
                 max(clim_vector, na.rm = TRUE),
                 diff(range(clim_vector))/bin_number)
    return(steps[-1])
  }
  
  if(bin_type == "window"){
    
    step_size <- (max(clim_vector, na.rm = TRUE)-min(clim_vector, na.rm = TRUE))/bin_number
    steps <- seq(min(clim_vector, na.rm = TRUE),
                 max(clim_vector, na.rm = TRUE),
                 step_size)
    window <- 2* step_size
    if(window < max(diff(sort(clim_vector)))) window <- max(diff(sort(clim_vector)))
    steps <- steps + window/2
    return(list(steps,window))
  }
  levels <- levels(cut_number(clim_vector,
                              bin_number))
  
  steps <- lapply(strsplit(levels,
                           split = ","),
                  function(x) gsub("]","",x[2]))%>%
    unlist()%>%
    as.numeric()
  
  return(steps)
}

make_intervals <- function(df,
                           bin_number,
                           T_variable,
                           bin_type = c("sample","width","moving")){ #set bins by sample size or var width
  
  prec_steps <- make_steps(bin_number,
                           df$prec,
                           bin_type)
  
  T_steps <- make_steps(bin_number,
                        df[,T_variable],
                        bin_type)
  
  if(bin_type == "window"){
    T_window <- T_steps[[2]]
    T_steps <- T_steps[[1]]
    prec_window <- prec_steps[[2]]
    prec_steps <- prec_steps[[1]]
    
    intervals <- expand.grid(prec_end = prec_steps,
                                 T_end = T_steps)%>%
      mutate(prec_start = prec_end - prec_window,
             T_start = T_end - T_window)
    return(intervals)
  }
  
  intervals_end <- expand.grid(prec_end = prec_steps,
                           T_end = T_steps)
  intervals_start <- expand.grid(prec_start = c(min(df$prec)-1,
                                                head(prec_steps,
                                                     (bin_number -1))),
                                 T_start = c(min(df[,T_variable])-1,
                                             head(T_steps,
                                                  (bin_number -1))))
  
  intervals <- cbind(intervals_end,
                     intervals_start)
  return(intervals)
}

#### MAKE STABILITY LANDSCAPES ####
apply_threshold <- function(points,
                            threshold){
  min_diff <- 10
  
  points <- points %>%
    as.data.frame() %>%
    mutate(V1 = as.numeric(as.character(V1)))
  
  type_table <- table(points[,2])
  max_count <- type_table[names(type_table) == "max"]
  
  if(max_count == 1) return(points)
  
  if(threshold == "diff"){
    diff <- mean(diff(points[points[,2] == "max",1]))
    
    if(diff >= min_diff) return(points)
    
    points <- cbind(mean(points[points[,2] == "max",1]),
                    "max")
    return(points)
  }
  
  ###implement dip threshold here (no if needed)
  #
  #
  #
  return(points)
  
}
#write function for getting stable and unstable states

#wrapper function to get stability points for all intervals at once
stability_points <- function(intervals,
                             df,
                             tree_variable,
                             T_variable = NA,
                             threshold = c("none","dip","diff"),
                             univariate = NA,
                             cell = NA,
                             parallel = TRUE){
  
  call_max_min <- function(row){
    #print(row/nrow(intervals))
    points <- stability_wrap(intervals = intervals[row,],
                          df = df,
                          tree_variable = tree_variable,
                          T_variable = T_variable,
                          univariate = univariate,
                          threshold = threshold,
                          cell = cell)
      return(points)
  }
  if(parallel){
    #start cluster
    cluster <- parallel::makeCluster(n_nodes)
    
    #export vars
    parallel::clusterExport(cluster,
                            c("intervals",
                              "df",
                              "tree_variable",
                              "T_variable",
                              "threshold",
                              "cell",
                              "get_stable_points",
                              "filter_intervals",
                              "stability_wrap",
                              "apply_threshold",
                              #"PFT_model",
                              "call_max_min",
                              "univariate",
                              "get_peaks",
                              "get_valleys",
                              "find_peaks"),
                            envir = environment())
    
    #parallel
    stability_df <- parallel::clusterApplyLB(cl = cluster,
                                             x = 1:nrow(intervals),
                                             fun  = call_max_min)
    parallel::stopCluster(cluster)
  }else{
    stability_df <- lapply(1:nrow(intervals),
                           call_max_min)
  }
  
  
  stability_df <- do.call(rbind, stability_df)
  
  return(stability_df)
}



#function to get bistability measure
#0 ... no bistability at all
#1 ... bistability in all bins
bistability_measure <- function(binned_stability_df,
                                univariate = FALSE){
  
  n_bins <- ifelse(univariate,
                   length(unique(binned_stability_df[,1])),
                   length(unique(paste(binned_stability_df[,1],
                                binned_stability_df[,3],
                                sep = "-"))))
  n_stable_states <- unname(table(binned_stability_df$type)["max"])
  
  measure <- n_stable_states/n_bins - 1
  
  return(measure)
}

#write function for filtering before getting min_max
filter_intervals <- function(intervals,
                             tree_variable = c("Pollen", "REVEALS","opti", "RS"),
                             T_variable = c("MAT","summerT"),
                             univariate = c("precipitation",
                                            "MAT",
                                            "summerT"),
                             cell = NA,
                             df){
  library(dplyr)
  tree_column <- paste0("tree_",tree_variable)
  
  cells <- unique(df$cell)
  if(!is.na(cell)) cells <- cell
  
  if(ncol(intervals) == 4){
    T_interval <- as.numeric(intervals[c(4,2)])
    P_interval <- as.numeric(intervals[c(3,1)])

    trees <- df %>%
      filter(cell %in% cells)%>%
      filter(.data[[T_variable]] > T_interval[1] & .data[[T_variable]] <= T_interval[2],
             prec > P_interval[1] & prec <= P_interval[2])%>%
      dplyr::select(as.name(tree_column))%>%
      unname()%>%
      unlist()
    return(trees)
  }
  
  trees <- df %>%
    filter(cell %in% cells) %>%
    filter(.data[[univariate]] > intervals[1,1] & .data[[univariate]] <= intervals[1,2]) %>%
    dplyr::select(as.name(tree_column))%>%
    unname()%>%
    unlist()
  return(trees)
}

get_stable_points <- function(trees,
                        threshold){ 
  
  #only one value
  if(length(unique(trees)) == 1){
    points <- data.frame(unique(trees), "max")
    return(points)
  }
  
  bw <- 1.06 * sd(trees)/length(trees)^(1/5)
  
  if(!is.na(bw) & length(trees) >= 15){
   
    d <- density(trees,
                 kernel = "gaussian")
    
    if(bw != 0){
      d <- density(trees,
                   bw = bw,
                   kernel = "gaussian")
    }

    #determine local maxima and minima
    ts_y <- ts(d$y)
    tp <- pastecs::turnpoints(ts_y)
    
    #plot for checking
    plot(d)
    points(d$x[tp$tppos], d$y[tp$tppos], col = "red")
    
    #collect max and min with thresholds
    max <- get_peaks(d$x,
                     d$y,
                     ignore_threshold = 0.1)
    min <- get_valleys(d$x,
                       d$y,
                       ignore_threshold = 0.1)
    
    min <- cbind(min$x,
                 rep("min", nrow(min)))
    max <- cbind(max$x,
                 rep("max",nrow(max)))
    points <- rbind(min,max)
    
    if(nrow(points) == 0) return(NULL)
    
    points <- apply_threshold(points, 
                              threshold = threshold)
    
    return(points)
  }
  return(NULL)
}

#wrapper function to calculate stable states for ONE interval
stability_wrap <- function(intervals,
                           tree_variable = NA,
                           T_variable = NA,
                           univariate = NA,
                           cell = NA,
                           df,
                          threshold){
  trees <- filter_intervals(intervals = intervals,
                            tree_variable = tree_variable,
                            T_variable = T_variable ,
                            univariate = univariate,
                            cell = cell,
                            df)
  
  if(length(trees) < 15)return()
  
  points <- get_stable_points(trees,
                              threshold = threshold)
  
  if(is.null(points)) return()
  
  if(ncol(intervals) == 4){
    factor_to_numeric <- function(x) as.numeric(as.character(x))
    
    T_interval <- as.numeric(as.character(intervals[c(4,2)]))
    P_interval <- as.numeric(as.character(intervals[c(3,1)]))
                             
    points <- cbind(rep(T_interval[1], nrow(points)),
                    rep(T_interval[2], nrow(points)),
                    rep(P_interval[1], nrow(points)),
                    rep(P_interval[2], nrow(points)),
                    points) %>%
      as.data.frame()
    
    points[,1] <- factor_to_numeric(points[,1])
    points[,2] <- factor_to_numeric(points[,2])
    points[,3] <- factor_to_numeric(points[,3])
    points[,4] <- factor_to_numeric(points[,4])
    points[,5] <- factor_to_numeric(points[,5])
    
    names(points) <- c(paste0(T_variable,"_start"),
                       paste0(T_variable, "_stop"),
                       "precipitation_start",
                       "precipitation_stop",
                       "tree",
                       "type")
    points <- cbind(points,
                    samples = rep(length(trees),
                                  nrow(points)))
    
    return(points)
  }
  
  factor_to_numeric <- function(x) as.numeric(as.character(x))
  points <- cbind(rep(factor_to_numeric(intervals[1,1]), nrow(points)),
                  rep(factor_to_numeric(intervals[1,2]), nrow(points)),
                  points)%>%
    as.data.frame()
  
  points[,1] <- factor_to_numeric(points[,1])
  points[,2] <- factor_to_numeric(points[,2])
  points[,3] <- factor_to_numeric(points[,3])
  
  names(points) <- c(paste0(univariate,"_start"),
                     paste0(univariate, "_stop"),
                     "tree",
                     "type")
  
  points <- cbind(points,
                  samples = rep(length(trees),
                                nrow(points)))
  return(points)
}

#### PLOT STABILITY LANDSCAPES ####
#function to plot 3d stability landscape
plot_stability <- function(df,
                           intervals,#all intervals to get even plot extensions throughout                
                           T_variable,
                           alpha = c("samples","diff"),
                           gridded = FALSE){
  
  xmin <- min(intervals[,4])
  xmax <- max(intervals[,2])
  ymin <- min(intervals[,3])
  ymax <- max(intervals[,1])
  if(alpha == "samples"){
    df <- df %>%
      filter(type == "max") %>%
      group_by(precipitation_start, precipitation_stop,
               get(paste0(T_variable,"_start")),
               get(paste0(T_variable,"_stop")))%>%
      summarize(states = n(),
                samples = mean(as.numeric(samples)))
    
    names(df) <- c("precipitation_start",
                   "precipitation_stop",
                   paste0(T_variable,"_start"),
                   paste0(T_variable, "_stop"),
                   "states",
                   "samples")
    library(ggsci)
    p <- ggplot(df,
                aes(xmin = as.numeric(get(paste0(T_variable,"_start"))),
                    xmax = as.numeric(get(paste0(T_variable,"_stop"))),
                    ymin = as.numeric(precipitation_start),
                    ymax = as.numeric(precipitation_stop)))+
      geom_rect(aes(fill = as.factor(states),
                    alpha = as.numeric(samples)))+
      labs(title = "Stable states in different climatic conditions",
           x = paste0(T_variable, " [°C]"),
           y = "precipitation [mm/a]",
           fill = "Number of stable states",
           alpha = "Number of samples in bin")+
      scale_fill_npg()+
      xlim(xmin, xmax)+
      ylim(ymin,ymax)+
      theme(legend.position = "bottom")
    print(p)
    
    return(p)
  }
  
  df <- df %>%
    filter(type == "max") %>%
    group_by(precipitation_start, precipitation_stop,
             get(paste0(T_variable,"_start")),
             get(paste0(T_variable,"_stop")))%>%
    summarize(states = n(),
              diff = ifelse(states > 1,
                            diff(as.numeric(tree)),
                            50))
  
  names(df) <- c("precipitation_start",
                 "precipitation_stop",
                 paste0(T_variable,"_start"),
                 paste0(T_variable, "_stop"),
                 "states",
                 "diff")
  
  colors <- wesanderson::wes_palette("Darjeeling1",
                                     4)
  library(ggsci)
  p <- ggplot(df,
              aes(xmin = as.numeric(get(paste0(T_variable,"_start"))),
                  xmax = as.numeric(get(paste0(T_variable,"_stop"))),
                  ymin = as.numeric(precipitation_start),
                  ymax = as.numeric(precipitation_stop)))+
    geom_rect(aes(fill = as.factor(states),
                  alpha = as.numeric(diff)))+
    labs(title = "Stable states in different climatic conditions",
         x = paste0(T_variable, " [°C]"),
         y = "precipitation [mm/a]",
         fill = "Number of stable states",
         alpha = "Difference between tree cover states [%]")+
    scale_fill_npg()+
    xlim(xmin, xmax)+
    ylim(ymin,ymax)+
    theme(legend.position = "bottom")
  
  return(p)
  
  
}

plot_stability_2d <- function(df,
                              stable_df,
                              color = c("continent","Longitude","Latitude","region"),
                              univariate,
                              cell = NA,
                              tree_variable){
  
  conts <- popa::P %>%
    group_by(Dataset_ID) %>%
    summarize(Continent = unique(Continent))
  
  cells <- unique(df$cell)
  if(!is.na(cell)) cells <- cell
  
  stable_df[,1:3] <- sapply(stable_df[,1:3], as.numeric)
  
  stable_df <- stable_df %>%
    mutate(mean = rowMeans(dplyr::select(.,starts_with(univariate)),
                           na.rm = TRUE))
  
  df <- df %>%
    merge(conts,
          by = "Dataset_ID",
          all.x = TRUE)%>%
    filter(cell %in% cells)
  
  subset <- sample(1:nrow(df),
                   ifelse(nrow(df) <2000,
                          nrow(df),
                          2000),
                   replace =FALSE)
  colors <- wesanderson::wes_palette("Darjeeling1",
                                     2)
  colors2 <- wesanderson::wes_palette("FantasticFox1",
                                      3)
  
  ggplot(df[subset,])+
    geom_point(aes(get(univariate),
                   get(paste0("tree_",
                              tree_variable)),
                   col = get(color)),
               alpha = 0.5)+
    geom_point(data = stable_df,
               aes(mean,
                   tree,
                   fill = type),
               color = rgb(1,1,1),
               pch = 23,
               size = 4,
               stroke = 1)+
    scale_fill_manual(values = colors,
                      labels = c("stable", "unstable"))+
   # scale_color_manual(values = colors2)+
    labs(x = univariate,
         y = "tree cover",
         fill = "Attractor",
         color = color)
  
}

plot_status <- function(df,
                        cell){
  now <- spatial_sep %>%
    mutate(region_now = as.factor(.data[[region]]))%>%
    dplyr::select(region_now)
  
  if("lon" %in% names(df)){
    df <- df %>%
      rename(Longitude = lon,
             Latitude = lat)
  }
  
  points <- df[df$cell == cell,]%>%
    group_by(Dataset_ID) %>%
    summarize(Latitude = mean(Latitude),
              Longitude = mean(Longitude))
  
  p <- ggplot(data = now)+
        geom_sf(aes(fill = region_now),
                color = NA)+
        geom_point(data = points,
                   aes(Longitude,
                       Latitude),
                   size = 1,
                   alpha = 0.5)+
        theme(legend.position = "none")+
        labs(title = paste0(toupper(region),": ",
                            cell))
             
  print(p)
}
#### BIMODALITY FUNCTIONS ####
bimod_tests <- function(trees){
  
  packages <- c("tidyr","dplyr","diptest","multimode")
  lapply(packages,
         library,
         character.only = TRUE)
  
  dip <- dip.test(trees)
  dip <- ifelse(dip$p.value <= 0.05,
                TRUE,
                FALSE)
  #return results if unimodal
  if(!dip) return(data.frame(bimod = FALSE, bistab = FALSE))
  
  #calculate modes and check if diff high enough
  modes <- locmodes(trees, mod0 = 2)
  modes <- modes$locations[-2]
  
  bistab <- diff(modes)
  
  # return bimodality results + number of records
  return(data.frame(bimod = dip, bistab = bistab))
}

#### PHOTOBIOLOGY FUNCTIONS ####
find_peaks <- function (x, ignore_threshold = 0, span = 3, 
                        strict = TRUE, na.rm = FALSE) 
{
  if (is.null(span)) {
    pks <- x == max(x, na.rm = na.rm)
    if (strict && sum(pks) != 1L) {
      pks <- logical(length(x))
    }
  }
  else {
    pks <- splus2R::peaks(x = x, span = span, strict = strict)
  }
  if (abs(ignore_threshold) < 1e-05) {
    pks
  }
  else {
    range_x <- range(x, na.rm = na.rm, finite = TRUE)
    min_x <- range_x[1]
    max_x <- range_x[2]
    x <- ifelse(!is.finite(x), min_x, x)
    delta <- max_x - min_x
    top_flag <- ignore_threshold > 0
    scaled_threshold <- delta * abs(ignore_threshold)
    if (top_flag) {
      ifelse(x - min_x > scaled_threshold, pks, FALSE)
    }
    else {
      ifelse(max_x - x > scaled_threshold, pks, FALSE)
    }
  }
}

get_peaks <- function (x, y, ignore_threshold = 0, span = 5, strict = TRUE, 
                       x_unit = "", x_digits = 3, na.rm = FALSE) 
{
  stopifnot(length(x) == length(y))
  selector <- find_peaks(x = y, ignore_threshold = ignore_threshold, 
                         span = span, strict = strict, na.rm = na.rm)
  if (sum(selector) < 1) {
    return(data.frame(x = numeric(0), y = numeric(0), label = character(0)))
  }
  else {
    peaks.x <- x[selector]
    peaks.y <- y[selector]
    return(data.frame(x = peaks.x, y = peaks.y, label = paste(as.character(signif(x = peaks.x, 
                                                                                  digits = x_digits)), x_unit, sep = "")))
  }
}

get_valleys <- function (x, y, ignore_threshold = 0, span = 5, strict = TRUE, 
                         x_unit = "", x_digits = 3, na.rm = FALSE) 
{
  xy.data <- get_peaks(x = x, y = -y, ignore_threshold = -ignore_threshold, 
                       span = span, strict = strict, x_unit = x_unit, x_digits = x_digits, 
                       na.rm = na.rm)
  xy.data[["y"]] <- -xy.data[["y"]]
  return(xy.data)
}

#### DEPRECATED ####
#use stability_wrap instead
get_min_max <- function(intervals,
                        tree_variable = c("Pollen", "REVEALS", "opti"),
                        T_variable = c("MAT", "summerT"),
                        threshold = c("none","dip","diff"),
                        cell = NA,
                        df){ #vector of continent or continents
  library(dplyr)
  T_interval <- as.numeric(intervals[c(4,2)])
  P_interval <- as.numeric(intervals[c(3,1)])
  
  tree_column <- paste0("tree_",tree_variable)
  
  cells <- unique(df$cell)
  if(!is.na(cell)) cells <- cell
  trees <- df %>%
    filter(cell %in% cells)%>%
    filter(.data[[T_variable]] > T_interval[1] & .data[[T_variable]] <= T_interval[2],
           prec > P_interval[1] & prec <= P_interval[2])%>%
    dplyr::select(as.name(tree_column))%>%
    unname()%>%
    unlist()
  
  bw <- 1.06 * sd(trees)/length(trees)^(1/5)
  
  if(!is.na(bw) & bw!=0 & length(trees) >= 15){
    d <- density(trees,
                 bw = bw,
                 kernel = "gaussian")
    
    #determine local maxima and minima
    ts_y <- ts(d$y)
    tp <- pastecs::turnpoints(ts_y)
    
    #plot for checking
    plot(d)
    points(d$x[tp$tppos], d$y[tp$tppos], col = "red")
    
    #collect max and mi with thresholds
    max <- photobiology::get_peaks(d$x,
                                   d$y,
                                   ignore_threshold = 0.1)
    min <- photobiology::get_valleys(d$x,
                                     d$y,
                                     ignore_threshold = 0.1)
    
    min <- cbind(min$x,
                 rep("min", nrow(min)))
    max <- cbind(max$x,
                 rep("max",nrow(max)))
    points <- rbind(min,max)
    
    points <- apply_threshold(points, 
                              threshold = threshold)
    
    points <- cbind(rep(T_interval[1], nrow(points)),
                    rep(T_interval[2], nrow(points)),
                    rep(P_interval[1], nrow(points)),
                    rep(P_interval[2], nrow(points)),
                    points) %>%
      as.data.frame()
    
    names(points) <- c(paste0(T_variable,"_start"),
                       paste0(T_variable, "_stop"),
                       "precipitation_start",
                       "precipitation_stop",
                       "tree",
                       "type")
    points <- cbind(points,
                    samples = rep(length(trees),
                                  nrow(points)))
    return(points)
  }
  
}
