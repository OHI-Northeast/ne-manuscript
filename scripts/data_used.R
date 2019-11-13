#### Data Used
#### This script contains code for creating the radial dendogram with the data sources used in the goals, pressures, and resilience
## Juliette Verstaen
## Novemeber 12, 2019

### Load Libraries
library(tidyverse)
library(readr)
library(d3r)
library(networkD3)
library(googledrive)
library(googlesheets4)
library(dplyr)
library(webshot)

### Read in Data
quant <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/18xVyNCLBMPL9Z0uonMpJM5GxCn5fVP9pvFZPjp6YADU/edit#gid=0",
                                   sheet = 1) 
quant <- select(quant, data_source, goal) %>% 
  filter(!is.na(goal)) %>% 
  mutate(data_source = if_else(is.na(data_source), "tbd", data_source)) %>%  # this can be removed later when filled out
  unique

### Create a hierachical data frame
### count the number of times a different souce was used for each goal/prs/res 
# in the end when the dataset is all filled out properly there shouldn't be any nas, but for now we have to do the sum ignoring them. can remove this bit of code later
prs <- sum(str_count(quant$goal, pattern = "pressure"), na.rm = TRUE)
res <- sum(str_count(quant$goal, pattern = "resilience"), na.rm = TRUE)
bd <- sum(str_count(quant$goal, pattern = "biodiversity"), na.rm = TRUE)
cw <- sum(str_count(quant$goal, pattern = "clean waters"), na.rm = TRUE)
le <- sum(str_count(quant$goal, pattern = "livelihoods and economies"), na.rm = TRUE)
fp <- sum(str_count(quant$goal, pattern = "food provision"), na.rm = TRUE)
hs <- sum(str_count(quant$goal, pattern = "habitat services"), na.rm = TRUE)
sop <- sum(str_count(quant$goal, pattern = "sense of place"), na.rm = TRUE)
rao <- sum(str_count(quant$goal, pattern = "resource access opportunity"), na.rm = TRUE)
tr <- sum(str_count(quant$goal, pattern = "tourism and recreation"), na.rm = TRUE)

### organize the sources in the same order as they are going to be read in below, so they match up with the right goal/prs/res 
sources_ordered <- quant %>% 
  arrange(goal) 

### save the sources as a list that can be added as a third level for the dendogram. They need to be in alphabetical order to match up with the sources 
data <- data.frame(
  # level1="OHI",
  level2= c(rep("biodiversity", bd),
            rep("clean waters", cw),
            rep("food provision", fp),
            rep("habitat services", hs),
            rep("livelihoods and economies", le),
            rep("pressure", prs), 
            rep("resilience", res),
            rep("resource access opportunity", rao),
            rep("sense of place", sop),
            rep("tourism and recreation", tr)),
  level3 = sources_ordered$data_source) # do level three with "Goal" instead of "Agencies and Orgs" as a check. they should all match up

### Transform data into .json file
data_json <- d3_nest(data)
Flare <- jsonlite::fromJSON(data_json, simplifyDataFrame = FALSE)

### create diagram
radialNetwork(List = Flare, 
              fontSize = 10, 
              opacity = 0.9,
              nodeStroke = "transparant")

#figure out how to save it as png ie better quality and keep the transparent nodes
# figure out how to get rid on node word in the middle
# figure out how to color coordiante the "goals
# make the goals font larger

# Main code
#save the manual way: viewer -> export -> save as web html
#install_phantomjs()
webshot::webshot("dendogram.html", file="figs/quant_sources.jpeg", zoom = 4)


