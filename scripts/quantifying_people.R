#### Quantifying People 
#### This script contains code for creating the diagram that quantifies the number of people that helped us during the entire OHINE project.
## Novemeber 12, 2019

### Load Libraries
library(packcircles)
library(ggplot2)
library(fishualize)
library(googledrive)
library(googlesheets4)
library(viridis)

##change this link to the real full one, cant read it in while people are currently working on it
people <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/18xVyNCLBMPL9Z0uonMpJM5GxCn5fVP9pvFZPjp6YADU/edit#gid=1292190059",
                                    sheet = 2) 

## Tidy data, keep what we want
quant_people <- people %>% 
  select(`Short Name`, Type) %>% 
  group_by(`Short Name`, Type) %>% 
  summarize(n()) %>% 
  filter(!is.na(`Short Name`)) %>% 
  rename(num_ppl = "n()") %>% 
  ungroup() %>% 
  mutate(merging = seq(1:47)) # I am doing this because for some reason the cbind im trying to do later with quant_data and packing creates a list even though it really shouldnt

# Generate the layout
packing <- circleProgressiveLayout(quant_people$num_ppl, sizetype='area')
packing$radius <- 0.95*packing$radius
packing <- mutate(packing, merging = seq(1:47)) # because cbinding is not working...
#data <- cbind(quant_people, packing)
data <- left_join(quant_people, packing, by = c("merging")) %>% 
  select(!merging)
dat.gg <- circleLayoutVertices(packing, npoints=50)

# Plot 
quant_ppl <- ggplot() + 
  geom_polygon(data = dat.gg, aes(x, y, group = id, fill=id), colour = "black", alpha = 0.6) +
  scale_fill_fish(option = "Thalassoma_bifasciatum" ) +
  geom_text(data = data, aes(x, y, label = `Short Name`, size = num_ppl), color="black") +
  theme_void() + 
  theme(legend.position="none")+ 
  coord_equal()

ggsave("figs/quant_ppl.jpg", width=7, height=5, dpi=300)

#fish_palettes()







