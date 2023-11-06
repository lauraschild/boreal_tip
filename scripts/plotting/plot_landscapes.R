#make pretty stability landscape
rm(list = ls())

path <- "output/tables/stability_landscapes"
file_REVEALS <- paste0(ifelse(sub,"sub_",""),
                       "CHELSA_stable_df.csv")
file_RS <- paste0(ifelse(sub,"sub_",""),"modern_stable_df.csv")

stable_REVEAS <-read.csv(file.path(path,file_REVEALS))%>%
  mutate(data_type = "paleo")
stable_modern <- read.csv(file.path(path,file_RS))%>%
  mutate(data_type = "modern") 

stable_df <- rbind(stable_modern,
                   stable_REVEAS)

#get modern data
{
  forest_file <- paste0("input/MODIS/",ifelse(sub,"sub_",""),"point_MODIS.csv")
  
  forest <- read.csv(forest_file) %>% 
    rename(bin = year,
           Dataset_ID = ID,
           tree_final = tree)
  climate_file <- paste0("CRU/",ifelse(sub,"sub_",""),
                         "points_CRU.csv")
  climate <- read.csv(file.path("input",climate_file)) %>% 
    rename(Dataset_ID = ID,
           bin = year,
           TJJA = summerT)
  
  complete_modern <-merge(forest,
                          climate,
                          by = c("Dataset_ID","bin")) %>% 
    mutate(data_type = "modern") %>% 
    sample_n(5000)
}

#get paleo data
{
  forest_file <- paste0("input/pollen/",
                        ifelse(sub,"sub_",""),
                        "reveals2.csv")
  
  forest <- read.csv(forest_file) %>% 
    mutate(bin = cut(Age_BP,
                     seq(-150,20100,100),
                     seq(-100,20000,100))) %>% 
    select(-Age_BP) %>% 
    group_by(Dataset_ID, bin) %>% 
    summarize_all(mean) %>% 
    rename(tree_final = tree_REVEALS)
  
  climate_file <- paste0("CHELSA_TraCE/", ifelse(sub,"sub_",""),"points_pollen_CHELSA.csv")
  climate <- read.csv(file.path("input",climate_file))
  
  complete_paleo <- merge(forest,
                    climate,
                    by = c("Dataset_ID","bin")) %>% 
    mutate(data_type = "paleo") %>% 
    sample_n(5000)
}

columns <- c("TJJA", "tree_final","data_type")

complete <- rbind(complete_modern[,columns],
                  complete_paleo[,columns])


colors <- ggsci::pal_npg()(3)[c(1,3)]
stable_df %>%
  mutate(TJJA = TJJA_start + 0.5)%>%
  ggplot()+  
  geom_point(data =complete,
             aes(TJJA,
                 tree_final),
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

ggsave(paste0("output/figures/publication/",
              ifelse(sub,"sub_",""),
              "landscapes.png"),
       width = 7.16,
       height = 3,
       dpi = 300)
ggsave(paste0("output/figures/publication/",
              ifelse(sub,"sub_",""),
              "landscapes.pdf"),
       width = 7.16,
       height = 3,
       dpi = 300)
