#make model figures
rm(list = ls())
library(tidyverse)

#make model surrogates
temp <- runif(2500, -5, 30)


#unimodal
# GAUSSIAN ####
a <- 70 #highest value
b <- 15 #position of center
c <- 5  #SD

uni <- a*exp(-(((temp-b)^2)/(2*c^2)))

#ASS
#large
# 
# get_biome <- function(temperature){
#   intervals <- c(2,11,12,13,16,17.5,21,24,100)
#   possible_biomes <- list(c("tundra"),
#                           c("tundra","savanna1"),
#                           c("tundra","boreal_forest","savanna1"),
#                           c("boreal_forest","tundra"),
#                           c("boreal_forest","tundra","steppe"),
#                           c("steppe","boreal_forest"),
#                           c("steppe","boreal_forest","savanna2"),
#                           c("steppe","savanna2"),
#                           c("steppe"))
#   interval_fit <- min(which(temperature <= intervals))
#   matches <- possible_biomes[[interval_fit]]
#   biome <- sample(matches,1)
#   if(length(matches) == 1) biome <- matches
#   return(biome)
# }
# 
# biome <- sapply(temp,get_biome)
# translation <- c(5,55,70,85,12)
# name <- c("tundra","savanna1","savanna2","boreal_forest","steppe")

# 
get_biome <- function(temperature){
  intervals <- c(7.5,10,11,12,13,15,17.5,20,22.5,25,100)
  possible_biomes <- list(c("tundra"),
                          c("tundra","savanna1"),
                          c("tundra","savanna2","savanna1"),
                          c("savanna1","savanna2"),
                          c("boreal_forest","savanna2","savanna1"),
                          c("savanna2","boreal_forest"),
                          c("steppe","boreal_forest","savanna2"),
                          c("steppe","boreal_forest"),
                          c("boreal_forest","steppe","savanna3"),
                          c("savanna3","steppe"),
                          c("steppe"))
  interval_fit <- min(which(temperature <= intervals))
  matches <- possible_biomes[[interval_fit]]
  biome <- sample(matches,1)
  if(length(matches) == 1) biome <- matches
  return(biome)
}

biome <- sapply(temp,get_biome)

#biome information
name <- c("tundra","savanna1","savanna2","savanna3","boreal_forest","steppe")
translation <- c(5,60,25,70,85,12.5)



ASS <- translation[match(biome,name)]
#make df
surrogates <- data.frame(TJJA = temp,
                        tree_uni = uni,
                        tree_paleoREVEALStip = ASS) %>% 
  pivot_longer(cols = 2:3,
               names_to = "surrogate",
               values_to = "TC")

ggplot(surrogates,
       aes(TJJA,
           TC))+
  geom_point(col = ggsci::pal_npg()(1))+
  facet_wrap(.~factor(surrogate,
                      levels = c("tree_uni","tree_paleoREVEALStip"),
                      labels = c("unimodal","alternative stability")))+
  theme_light()+
  ylim(c(0,100))+
  labs(x = expression(T[JJA]*" (°C)"),
       y = "tree cover (%)")


ggsave("output/figures/publication/surrogate_models.png",
       width = 7.16,
       height = 3,
       dpi = 300)

ggsave("output/figures/publication/surrogate_models.pdf",
       width = 7.16,
       height = 3,
       dpi = 300)
