library(datasauRus)
library(tidyverse)

dataset_id=c("dino", "slant_down", "slant_up", "h_lines", "star", "away", "v_lines", "high_lines", "wide_lines", "bullseye","x_shape", "circle", "dots")

datasaurus_dozen$dataset <- factor(datasaurus_dozen$dataset, levels = c("dino", "slant_down", "slant_up", "h_lines", "star", "away", "v_lines", "high_lines", "wide_lines", "bullseye","x_shape", "circle", "dots"))

for(i in 1:length(dataset)){
  cat(i)
  x=datasaurus_dozen %>% filter(dataset==dataset_id[i]) %>% 
    select(-dataset) 
  
    write.csv(x,file = paste0("data/dataset",i,".csv"), row.names = F)
}

ggplot(datasaurus_dozen, aes(x = x, y = y, colour = dataset)) +
  geom_point() +
  theme_void() +
  theme(legend.position = "none") +
  facet_wrap(~dataset, ncol = 3)

x=read.csv("data/activity2/dataset2a.csv")

x %>% ggplot(aes(x=x, y=y, group=group))+
  geom_point()