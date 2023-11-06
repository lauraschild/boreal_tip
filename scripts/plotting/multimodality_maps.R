#multimodality maps
#make multimodality maps

surrogates <- c("CHELSA_TraCE","ice6g","glac1d")

prep_results <- function(surrogate){
  result_file <- paste0("output/tables/multimodality/",
                        ifelse(sub,"sub_",""),surrogate,".csv")
  
  read.csv(result_file) %>% 
    filter(grepl("scatter",tree_type)) %>% 
    mutate(tree_type = gsub("scatter",
                            "",
                            tree_type),
           tree_type = gsub("tree_",
                            "",
                            tree_type),
           tree_type = gsub("_[[:digit:]]+","",tree_type),
           bistab = ifelse(bistab>5,
                                  TRUE,
                                  FALSE)) %>% 
    return()
  }
#add pollen results
pollen_result <- read.csv(paste0("output/tables/multimodality/",ifelse(sub,"sub_",""),
                                 "pollen.csv")) %>% 
  mutate(tree_type = "Pollen",
         bistab = ifelse(bistab >=5,
                         TRUE,
                         FALSE))

results <- lapply(surrogates,
                  prep_results) %>% 
  bind_rows() %>% 
  rbind(pollen_result)



#summary version
summary <- 
  results %>%
  group_by(tree_type,cell)%>%
  summarize(weight = sum(bistab, na.rm = TRUE)/n(),
            bistab = ifelse(weight > 0,
                            1,
                            0))%>%
  pivot_wider(names_from = c("tree_type"),
              values_from = c("weight","bistab"))

all_cells_summary <- data.frame(cell = 1:180) %>%
  merge(summary,
        by = "cell",
        all.x = TRUE)
#prep raster
{
  ncol <- 36
  nrow <- 5
  
  empty <- raster::raster(ncol = ncol,
                  nrow = nrow,
                  ymn = 45,
                  ymx = 70)
  raster::values(empty) <- 1:(ncol * nrow)
  }

summary <- empty
for(i in 1:(ncol(all_cells_summary)-1)){
  summary <- raster::addLayer(summary, empty)
  summary <- raster::setValues(summary, all_cells_summary[,(i+1)],
                       layer = (i))
  names(summary)[i] <- names(all_cells_summary[(i+1)])
}


df <- as(summary[[-raster::nlayers(summary)]], "SpatialPixelsDataFrame")
df <- as.data.frame(df)
df <- df %>%
  pivot_longer(cols = 1:(last_col()-2),
               names_to = c("measure","tree_type"),
               names_sep = "_",
               values_to = "value")%>%
  pivot_wider(names_from = "measure")%>%
  mutate(bistab = as.logical(bistab))%>%
  filter(tree_type %in% c("uni","Pollen","tip"))

df$tree_type <- factor(df$tree_type,
                       levels= c("uni","tip","Pollen"),
                       labels = c("unimodal surrogate", "tipping surrogate", "pollen-based tree cover"))

world <- rnaturalearth::ne_countries(scale = "small",
                                     returnclass = "sf")
mycols <- c("black",ggsci::pal_npg()(1))
names(mycols) <- c("FALSE","TRUE")
df$weight[df$weight == 0] <- NA

ggplot(data = world)+
  geom_sf(col = "gainsboro",
          fill = "gainsboro")+
  coord_sf(ylim = c(42,72))+
  geom_tile(data = df,
            aes(x,y,
                #alpha = weight,
                fill = weight
            ),
            col = rgb(1,1,1,0))+
  scale_fill_gradient(low = alpha(mycols[2],0),
                      high = alpha(mycols[2],0.8),
                      na.value = rgb(0,0,0,0.2),
                      breaks = c(0,1),
                      labels = c("0%","100%"),
                      limits = c(0,1)) +
  facet_wrap(.~tree_type,
             nrow = 6)+
  labs(fill = "Multimodal ensemble members:")+
  guides(fill = guide_colorbar(title.position = "left",
                               frame.colour = rgb(0,0,0,0.2),
                               ticks.colour = NA,
                               title.vjust = .8,
                               title.hjust = 0))+
  theme_void()+
  theme(legend.position = "bottom",
        legend.justification = c(0.45,0),
        legend.spacing = unit(3,"cm"),
        #legend.title = element_blank(),
        legend.title = element_text(size = 9,
                                    margin = margin(0,0.5,0,1,"cm")),
        strip.text.x = element_text(size = 9, vjust = 1, margin = margin(0,0,5,0)),
        legend.text = element_text(size = 8),
        panel.spacing = unit(0.75,"lines"))

ggsave(paste0("output/figures/publication/",
              ifelse(sub,"sub_",""),"bimodality_map.png"),
       dpi = 300,
       width = 7.16,
       height = 5)
ggsave(paste0("output/figures/publication/",
              ifelse(sub,"sub_",""),"bimodality_map.pdf"),
       dpi = 300,
       width = 7.16,
       height = 5)


legacy <- read.csv("input/Pollen/reveals2.csv") %>% 
  distinct(Longitude, Latitude)
df$weight[is.na(df$weight)] <- 0
ggplot(data = world)+
  geom_sf(col = "gainsboro",
          fill = "gainsboro")+
  coord_sf(ylim = c(42,72))+
  geom_point(data = legacy,
             aes(Longitude,
                 Latitude),
             size = 1,
             alpha =0.2,
             shape = 16)+
  geom_tile(data = df[df$tree_type == "pollen-based tree cover",],
            aes(x,y,
                fill = bistab,
                alpha = weight))+
  scale_fill_manual(values = mycols
                    ,labels = c("multimodal cells"),
                    breaks = c("TRUE") )+
  scale_alpha_continuous(range = c(0.2,0.5),
                         guide = "none")+
  theme_void(base_size = 8)+
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        strip.text.x = element_text(size = 9, vjust = 1, margin = margin(0,0,5,0)),
        legend.text = element_text(size = 8),
        panel.spacing = unit(0.75,"lines"))

ggsave(paste0("output/figures/publication/",
       ifelse(sub,"sub_",""),"bimodality_map_pollen.png"),
       dpi = 300,
       width = 7.16,
       height = 2)
ggsave(paste0("output/figures/publication/",
              ifelse(sub,"sub_",""),"bimodality_map_pollen.pdf"),
       dpi = 300,
       width = 7.16,
       height = 2)


