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

### Read in Data
# DO THIS WITH GOOGLE SHEETS INSTEAD!!

quant <- sheets_get("https://docs.google.com/spreadsheets/d/18xVyNCLBMPL9Z0uonMpJM5GxCn5fVP9pvFZPjp6YADU/edit#gid=0")
                    
quant <- read_csv("data/quant_data_simple.csv", na.strings = c("", "NA")) %>% 
  filter(!is.na(Agencies_Organizations)) 

### Create a hierachical data frame
### count the number of times a different souce was used for each goal/prs/res 
prs <- sum(str_count(quant$Goal, pattern = "pressure"))
res <- sum(str_count(quant$Goal, pattern = "resilience"))
bd <- sum(str_count(quant$Goal, pattern = "biodiversity"))
cw <- sum(str_count(quant$Goal, pattern = "clean waters"))
le <- sum(str_count(quant$Goal, pattern = "livelihoods and economies"))
fp <- sum(str_count(quant$Goal, pattern = "food provision"))
hs <- sum(str_count(quant$Goal, pattern = "habitat services"))
sop <- sum(str_count(quant$Goal, pattern = "sense of place"))
rao <- sum(str_count(quant$Goal, pattern = "resource access opportunity"))
tr <- sum(str_count(quant$Goal, pattern = "tourism and recreation"))
### organize the sources in the same order as they are going to be read in below, so they match up with the right goal/prs/res 
sources_ordered <- quant %>% 
  select(Agencies_Organizations, Goal) %>% 
  arrange(Goal) 

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
  level3 = sources_ordered$Agencies_Organizations) # do level three with "Goal" instead of "Agencies and Orgs" as a check. they should all match up

### Transform data into .json file
Flare <- jsonlite::fromJSON(data_json, simplifyDataFrame = FALSE)

### create diagram
radialNetwork(List = Flare, 
              fontSize = 10, 
              opacity = 0.9,
              nodeStroke = "transparant")



