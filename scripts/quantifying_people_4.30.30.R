#### Quantifying People 
#### This script contains code for creating the diagram that quantifies the number of people that helped us during the entire OHINE project.
## Novemeber 12, 2019

### Load Libraries
library(packcircles)
library(ggplot2)
library(fishualize)
library(googledrive)
library(googlesheets4)
library(dplyr)
library(stringr)

##change this link to the real full one, cant read it in while people are currently working on it
people <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/18xVyNCLBMPL9Z0uonMpJM5GxCn5fVP9pvFZPjp6YADU/edit#gid=1292190059",
                                    sheet = 2) 

## Tidy data, keep what we want
quant_people <- people %>% 
  select(`Short Name`, Type) %>% 
  group_by(`Short Name`, Type) %>% #there are 13 different types
  summarize(n()) %>% 
  filter(!is.na(`Short Name`)) %>% 
  rename(num_ppl = "n()") %>% 
  ungroup() 
 
# Generate the layout
packing <- circleProgressiveLayout(quant_people$num_ppl, sizetype='area')
packing$radius <- 0.95*packing$radius
data <- cbind(quant_people, packing) %>% 
  mutate(id = seq(1:52)) 

# The data used to create the circle polygons in ggplot need to have "Type" in the df in order to color the circles by type
merge_id <- data %>% 
  select(id, Type)

dat.gg <- circleLayoutVertices(packing, npoints=50)
dat.gg <- dat.gg %>% 
  left_join(merge_id, by = c("id")) %>% 
  rename("Type of Organization" = "Type")

num_ppl_1 <- data %>% 
  filter(num_ppl == 1) %>% 
  rename(short_name = "Short Name") %>% 
 # mutate(name = paste0(short_name, ",", sep = "")) %>% 
  dplyr::select(short_name) 
num_ppl_1 <- c(num_ppl_1$short_name)
num_ppl_list <- str_c(num_ppl_1, collapse = ", ")
  
# Plot 
quant_ppl <- ggplot() + 
  geom_polygon(data = dat.gg, aes(x, y, 
                                  group = id, 
                                  fill= `Type of Organization`), 
               colour = "black", alpha = 0.7) +
  geom_text(data = data, aes(x, y, 
                             label = ifelse(`Short Name`== "Seafood Watch", "Seafood\nWatch",
                                            ifelse(`Short Name`== "Ocean Conservancy", "Ocean\nConservancy",
                                            ifelse(`Short Name`== "Wampanoag Tribe", "Wampanoag\nTribe",
                                                   ifelse(`Short Name`== "CT Sea Grant", "CT\nSea Grant",
                                                        #  ifelse(`Short Name` == "Cape Cod Coop Extension", "Cape Cod\nCoop Extension",
                                                           #      ifelse(`Short Name` == "Footprints in the water LLC", "Footprints in\nthe water LLC",
                                                                        ifelse(num_ppl == 1 , "",
                                                   `Short Name`))))), 
                             size = num_ppl), 
            color="black") +
  theme_void() + 
  theme(legend.position="right",
        plot.caption = element_text(hjust = 0))+ 
  #scale_fill_fish_d(option = "Lepomis_megalotis" ) +
  scale_fill_brewer(palette ="Set2")+
  guides(size=FALSE)+
  coord_equal() +
  labs(caption = str_wrap(paste0("Empty bubbles indicate agencies where we consulted with one person. These include ", num_ppl_list, sep = " ")))

# Save the figure
ggsave("figs/quant_ppl_4.30.20.jpg", width=7, height=8, dpi=300)

#fish_palettes()







