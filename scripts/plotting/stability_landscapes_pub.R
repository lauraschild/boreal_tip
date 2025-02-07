#make pretty stability landscape

rm(list = ls())
library(tidyverse)

path <- "output/tables/stability_landscapes"
file_REVEALS <- "CHELSA_stable_df.csv"
file_RS <- "modern_stable_df.csv"

stable_REVEAS <- read.csv(file.path(path,file_REVEALS))%>%
  mutate(data_type = "paleo")
stable_modern <- read.csv(file.path(path,file_RS))%>%
  mutate(data_type = "modern") 

stable_df <- rbind(stable_modern,
                   stable_REVEAS)


RS_data <- data.table::fread("input/CRU/points_CRU.csv") %>%
  merge(data.table::fread("input/MODIS/point_MODIS_complete.csv"),
        by = c("ID","year")) %>%
  sample_n(5000) %>% 
  mutate(data_type = "modern") %>% 
  rename(TJJA = summerT)
names(RS_data) <- stringi::stri_replace_all_regex(names(RS_data),
                                                  pattern = c("_RS","obs_"),
                                                  replacement = c(""),
                                                  vectorize_all = FALSE)

paleo_data <- data.table::fread("input/pollen/final_reveals_all.csv")%>%
  filter(Age_BP <= 8000) %>% 
  mutate(bin = cut(Age_BP,
                   seq(-150,20100,100),
                   seq(-100,20000,100))) %>%  #bin label denotes the middle of the bin
  mutate(bin = as.numeric(as.character(bin))) %>% 
  merge(data.table::fread("input/CHELSA_TraCE/points_pollen_CHELSA.csv")) %>% 
  sample_n(5000)%>%
  mutate(data_type = "paleo")

names(paleo_data) <- stringi::stri_replace_all_regex(names(paleo_data),
                                                     pattern = c("_REVEALS","model_"),
                                                     replacement = c(""),
                                                     vectorise_all = FALSE)

cols <- c("tree","TJJA","data_type")

tree_data <- bind_rows(paleo_data,
                   RS_data) %>% 
  as.data.frame() %>% 
  select(any_of(cols))

colors <- ggsci::pal_npg()(3)[c(1,3)]
stable_df %>%
  mutate(TJJA = TJJA_start + 0.5)%>%
  ggplot()+  
  geom_point(data = tree_data,
             aes(TJJA,
                 tree),
             alpha = 0.5,
             size = 0.6,
             shape = 18)+
  geom_point(aes(TJJA,
                 tree,
                 col = type),
             size =1.5,
             shape = 16)+
  scale_color_manual(values = colors,labels = c("stable","unstable"))+
  labs(y = "tree cover (%)",
       x = expression(T[JJA]*" (°C)"),
       color = "Attractors")+
  scale_x_continuous(breaks = seq(0,30,5),
                     limits = c(0,30))+
  facet_wrap(.~data_type,
             ncol = 2)+
  theme_light()+
  theme(legend.position = c(0.08,0.8),
        legend.text = element_text(size = 8),
        legend.title = element_text(face = "bold", size = 9),
        axis.title = element_text(size = 12))
# 
ggsave("output/figures/publication/stability_landscape_REVEALS.png",
       width = 7.16,
       height = 3,
       dpi = 600)

ggsave("output/figures/publication/stability_landscape_REVEALS.pdf",
       width = 7.16,
       height = 3)
