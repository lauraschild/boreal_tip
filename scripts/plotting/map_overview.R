#overview figure for locations and time series

rm(list = ls())
library(tidyverse)
library(raster)
library(rnaturalearth)
library(cowplot)
library(ggsci)
library(sf)

select <- dplyr::select

#load ultimate dataframe
pollen <- read.csv("input/pollen/final_reveals_all.csv") %>% 
  filter(Age_BP <= 8000) %>% 
  mutate(bin = cut(Age_BP,
                   seq(-150,20100,100),
                   seq(-100,20000,100))) %>%  #bin label denotes the middle of the bin
  mutate(bin = as.numeric(as.character(bin))) %>% 
  rename(slice = bin) %>% 
  select(-Longitude,-Latitude)

file <- "output/tables/surrogates/CHELSA_TraCE.csv"

df <- read.csv(file) %>% 
  merge(pollen,
        by = c("Dataset_ID", "slice"))

records <- df %>%
  group_by(Dataset_ID)%>%
  summarize(Longitude = mean(Longitude),
            Latitude = mean(Latitude))

world <- ne_countries(scale = "small", returnclass = "sf")

grid <- raster(ncol = 36,
               nrow = 5,
               ymn = 45,
               ymx = 70)
values(grid) <- 1:(36*5)
records$cell <- extract(grid,records[,c("Longitude","Latitude")])
df$cell <- extract(grid,df[,c("Longitude","Latitude")])
grid <- rasterToPolygons(grid)
#set.seed(123)
#example_cell <- sample(as.numeric(names(table(records$cell))[which(table(records$cell) > 5)]),1)
example_cell <- 5
grid$highlight[grid$layer == example_cell] <- "A"
grid$highlight[grid$layer != example_cell] <- "B"

print(example_cell)
plot(grid)
plot(grid[grid$layer==example_cell,], col = "red", add = TRUE)

map <- 
  ggplot(data = world)+
  geom_sf(fill = "gainsboro",
          col = "gainsboro")+
  geom_point(data = records,
             aes(Longitude,
                 Latitude),
             col = pal_npg()(1),
             size = 0.8,
             alpha = 0.6,
             shape = 16)+
  geom_sf(data = st_as_sf(grid),
          aes(fill = highlight,
              alpha = highlight,
              col = highlight),
          lwd = 0.3,
          inherit.aes = FALSE,
          key_glyph = "rect")+
    scale_fill_manual(values = c("A" = pal_npg()(3)[3],"B" = "green"),
                      breaks = c("A"),
                      labels =c("example cell")
                      )+
    scale_color_manual(values = c("A"=pal_npg()(3)[3], "B" = "gray34"),
                       guide = "none")+
    scale_alpha_manual(values = c("A" = 0.3,"B" = 0),
                       breaks = c("A"),
                       labels = c("example cell")
                       )+
  coord_sf(ylim = c(41,74))+
  scale_y_continuous(limits = c(-180,180), expand = c(0,0))+
  theme_void()+
    theme(legend.position = c(0.11,0.25),
          legend.key.size = unit(0.4,"cm"),
          legend.text = element_text(size = 7),
          legend.box.background=element_rect(fill="white", color=NA),
          legend.title = element_blank(),
          legend.box.just = "center",
          legend.margin = margin(-0.1, 0.06, 0.06, 0.06, "cm"),
          plot.margin = unit(c(-4, -0.5, -4, 0), "cm"), #-4 in upper and lower here
          panel.background = element_rect(fill ="white",
                                          color ="white",
                                          linewidth  =0))

map

df_sub <-df %>%
  filter(cell == example_cell,
         slice <= 8000)%>%
  dplyr::select(slice, Dataset_ID, tree_uni_scatter5, tree_tip_scatter5, tree_REVEALS)%>%
  pivot_longer(cols = starts_with("tree"),
               names_to = "data_type",
               values_to = "tree_cover")%>%
  mutate(data_type = stringi::stri_replace_all_regex(data_type,
                                                     c("tree_","_scatter5"),
                                                     c(""),
                                                     vectorize_all =FALSE))

timeseries <- df_sub %>%
  ggplot(aes(slice,
             tree_cover,
             group = Dataset_ID,
             col = factor(Dataset_ID)))+
  geom_line(lwd = 0.8,
            alpha = 0.8)+
  scale_color_npg(guide = "none")+
  scale_x_reverse()+
  facet_wrap(.~factor(data_type,
                      levels = c("uni","tip","REVEALS"),
                      labels = c("unimodal surrogate","alternative stability surrogate","pollen-based")),
             ncol = 3)+
  theme_light()+
  labs(x = "Age (yrs BP)",
       y = "tree cover (%)")
timeseries


colors <- pal_npg()(3)
colors <- colors[c(2,1,3)]
histograms <-  df_sub %>%
  ggplot(aes(tree_cover))+
  geom_density(aes(fill = data_type,
                   col = data_type))+
  scale_fill_manual(values = colors,
                    guide = "none")+
  scale_color_manual(values = colors,
                     guide = "none")+
  facet_wrap(.~factor(data_type,
                      levels = c("uni","tip","REVEALS"),
                      labels = c("unimodal surrogate","alternative stability surrogate","pollen-based")),
             ncol = 3)+
  theme_light()+
  theme(strip.background = element_blank(),
        strip.text.x = element_blank())+
  labs(x = "tree cover (%)")


plot_grid(map, timeseries, histograms, 
          labels = "auto",
          label_size = 15,
          nrow = 3,
          rel_heights = c(2,3,2),
          axis = "r",
          align = "h")

ggsave("output/figures/publication/map_overview.pdf",
       width = 7.16,
       height = 5,
       dpi = 300)

ggsave("output/figures/publication/map_overview.png",
       width = 7.16,
       height = 5,
       dpi=300)
