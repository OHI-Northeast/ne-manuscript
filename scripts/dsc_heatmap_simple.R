#### Data Selection Criteria (DSC) Heatmap
#### This script contains code for creating the heatmap scoring each data layer used as 0,0.5, or 1 based on various criteria.
## Novemeber 13, 2019

library(ggplot2)
library(googledrive)
library(googlesheets4)
library(dplyr)
library(cowplot)

### read in the data from google sheets
dsc_scores_raw <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1N2-B1KXB1w7rQYHV8Kl6ZmGK5jhE-JLIJk_VAPgyC60/edit#gid=271621882",
                                            sheet = 2)

### create list for the proper order that we want it to show up on the graph later
# layers order: from highest score to lowest
order_layers_df <- dsc_scores_raw %>% 
  select(Layer,`Average Score` ) %>% 
  unique()  %>% 
  arrange(`Average Score`)%>% 
  replace(., is.na(.), "NA") 

order_layers <- c(order_layers_df$Layer)

# criteria order: same as in the table
order_criteria <- c("Spatial Resolution", "Spatial Cover",
                    "Temporal Resolution", "Temporal Span", "Temporal Baseline",
                    "Fit Resolution", "Fit Comprehensiveness")

# colors order: so that they show up in the logical order in the legend
order_colors <- c("good", "medium", "bad", "NA")

### Now we want to create a df that we can use to make the heatmap, and apply all these orders that we created
dsc_layers <- dsc_scores_raw %>% 
  select(-Goal, -`Average Score`) %>% 
  unique() %>% 
  gather("criteria", "score", 2:8) %>% # Data is in long format, need it short
  mutate(score = as.character(score),
         criteria = as.character(criteria),
         Layer = as.character(Layer),
         score = ifelse(score == "3", "good",
                        ifelse(score == "2", "medium",
                               ifelse(score == "1", "bad", score))),
         criteria = factor(criteria, levels = order_criteria),
         score = factor(score, levels = order_colors),
         Layer = factor(Layer, levels = order_layers)) 

### Heatmap!
dsc_all_layers <- ggplot(data = dsc_layers, aes(x = criteria, y = Layer)) +
  geom_tile(aes(fill = score)) +
  scale_fill_manual(values = c("darkgreen", "orange", "darkred", "gray90"))+ 
  theme_dark()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        legend.title = element_blank(),
        legend.position = "none") +
  coord_cartesian(expand=FALSE) 

## when saving make sure to save it tall enough so that there's no over lap for the yaxis labels
#ggsave("figs/dsc_layers.jpg", width=10, height=8, dpi=300)

