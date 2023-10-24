#tipping collection
path <- "/bioing/user/lschild/bistab/new_surrogates"

scripts <- c("paleoREVEALS","paleoPollen",
             "Scheffer","modern",
             "modernREVEALS","modernPollen")
lapply(scripts,
       function(x) source(paste0(path,"/",x,"_tip.R"),echo = FALSE))