#plot the boxplot
surrogates <- c("CHELSA_TraCE","ice6g","glac1d")

prep_results <- function(surrogate){
  result_file <- paste0("output/tables/multimodality/",
                        ifelse(sub,"sub_",""),surrogate,".csv")
  
  read.csv(result_file) %>% 
    filter(grepl("scatter",tree_type)) %>% 
    mutate(bistab = ifelse(bistab >= 5,
                           TRUE,FALSE),
           tree_type = gsub("scatter",
                            "",
                            tree_type),
           tree_type = gsub("tree_",
                            "",
                            tree_type)) %>% 
    group_by(tree_type) %>% 
    summarize(multistab = sum(bistab)/n()) %>% 
    mutate(tree_type = gsub("_[[:digit:]]+","",tree_type))%>% 
    return()
}
#add pollen results
pollen_result <- read.csv(paste0("output/tables/multimodality/",ifelse(sub,"sub_",""),
                                 "pollen.csv")) %>% 
  mutate(tree_type = "Pollen",
         bistab = ifelse(bistab >=5,
                         TRUE,
                         FALSE)) 

shoelace <- function(x){
  n <- nrow(pollen_result)
  multistab <-  sum(sample(pollen_result$bistab,n,replace = TRUE))/n
  return(multistab)
}

set.seed(123)
bootstrap_Pollen <- data.frame(tree_type = "Pollen",
                               multistab = sapply(1:500,
                                                  shoelace))

results <- lapply(surrogates,
                  prep_results) %>% 
  bind_rows() %>% 
  rbind(bootstrap_Pollen)

results %>% 
  ggplot(aes(factor(tree_type,
                    levels = c("uni","tip","Pollen"),
                    labels = c("unimodal surrogate","tipping surrogate","pollen-based tree cover")),
             multistab,
             fill = tree_type,
             col = tree_type))+
  geom_boxplot(alpha = 0.5)+
  ggsci::scale_color_npg()+
  ggsci::scale_fill_npg()+
  labs(x = "data type",
       y = "frequency of multimodal cells")+
  theme_light()+
  theme(legend.position = "none")

ggsave(paste0("output/figures/publication/",
              ifelse(sub,"sub_",""),"boxplot.png"),
       width = 7.16,
       height = 4,
       dpi = 300)
ggsave(paste0("output/figures/publication/",
              ifelse(sub,"sub_",""),"boxplot.pdf"),
       width = 7.16,
       height = 4,
       dpi = 300)
