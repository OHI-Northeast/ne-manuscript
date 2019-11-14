#### Data Selection Criteria (DSC) Heatmap
#### This script contains code for creating the heatmap scoring each data layer used as 0,0.5, or 1 based on various criteria.
## Novemeber 13, 2019

library(ggplot2)
library(googledrive)
library(googlesheets4)
library(dplyr)

dsc_raw <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1N2-B1KXB1w7rQYHV8Kl6ZmGK5jhE-JLIJk_VAPgyC60/edit#gid=0",
                                    sheet = 1) 

## Data is in long format, need it short

#dsc_ord <- dsc_raw %>% 
 # unite(goal_lay, c("Goal", "Layer"), sep = " - ")

order_layers <- rev(c(dsc_raw$Layer)) %>% 
  unique() #THIS IS JUST FOR FIGURING OUT THIS CODING FOR NOW. THINK ABOUT IF WE WANT A GOAL COLUMN THAT LISTS THEM INSTEAD OF DUPLICATES

order_criteria <- c("Spatial Resolution", "Spatial Cover",
                    "Temporal Resolution", "Temporal Span", "Temporal Baseline",
                    "Fit Resolution", "Fit Comprehensiveness")
order_colors <- c("green", "yellow", "red", "NA")


dsc <- dsc_raw %>% 
  gather("criteria", "score",3:9) %>% 
 # mutate(goal_2 = Goal,
  #       layer_2 = Layer) %>% 
  #unite(goal_lay, c("goal_2", "Layer2"), sep = " - ") %>% 
  mutate(criteria = factor(criteria, levels = order_criteria),
         score = factor(score, levels = order_colors),
         Layer = factor(Layer, levels = order_layers)) 
                                          
dsc_heatmap <- ggplot(data = dsc, aes(x = criteria, y = Layer)) +
  geom_tile(aes(fill = score)) +
  scale_fill_manual(values = c("darkgreen", "orange", "darkred", "gray90"))+ 
  theme_dark()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        legend.title = element_blank()) +
#  annotate("text", x=7, y= 25, label= "CW", size = 2.5)+
  coord_cartesian(expand=FALSE) 
  
ggsave("figs/dsc_heatmap.jpg", width=10, height=8, dpi=300)

## when saving make sure to save it tall enough so that there's no over lap for the yaxis labels



