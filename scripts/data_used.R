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
  level2= c(rep("BD", bd), #biodiversity
            rep("CW", cw), #clean waters
            rep("FP", fp), #food provision
            rep("HS", hs), #habitat services
            rep("LE", le), # livelihoods and economie
            rep("PRS", prs), #pressures
            rep("RES", res), #resilience
            rep("RAO", rao), #resource access opportunity
            rep("SOP", sop), #sense of place
            rep("TR", tr)), #tourism and recreation
  level3 = sources_ordered$data_source) # do level three with "Goal" instead of "Agencies and Orgs" as a check. they should all match up

### Transform data into .json file
data_json <- d3_nest(data)
Flare <- jsonlite::fromJSON(data_json, simplifyDataFrame = FALSE)

# Some jerry rigging to replace "root" with "OHI NE"
children <- Flare[1]
ohine <- Flare[2] %>% 
  as.data.frame() %>% 
  mutate(name = ifelse(name == "root", " ", name )) %>% 
  as.list()
Flare_ohine <- c(children, ohine)

### we are using the same colors as the dashboard to identify the goals
red <- c("#DD4B39")
purple <- c("#605CA8")
blue <- c("#0073B7")
aqua <- c("#00C0EF")
yellow <- c("#F39C12")
orange <- c("#FF851B")
navy <- c("#001F3F")
green <- c("#00A65A")
olive <- c("#3D9970")
fuscia <- c("#F012BE")
maroon <- c("#D81B60")
teal <- c("#39CCCC")
light_blue <- c("#3C8DBC")
lime <- c("#00FF00")

### assigning the colors to the goals
HS = maroon ## habitat services
RAO = fuscia ## resource access opportunities
#MAR = purple ## food provision: aquaculture/mariculture
FP = blue ## was FIS food provision: fishing
#SPP  = light_blue ## biodiversity: species
BD = aqua ## was HAB biodiversity: habitat
CW = teal ## clean waters
SOP = olive ## was LSPsense of place: lasting special places
#ICO = green ## sense of place: iconic species,
#SPFIS = lime ## sense of place: fishing engagement
#ECO = yellow ## livelihood/economies: economies,
LE = orange ## was LIV liveihood/economies: livelihoods
TR = red ## tourism and recreation
PRS = purple
RES = green


### the radialNetwork function does not support choosing colors directly for the groups. So we have to create a vector of colors in the proper order and then convert that to a JavaScript array
colorVector <- c(BD, CW, FP, HS, LE, PRS, RES, RAO, SOP, TR, 
                 rep(BD, bd), rep(CW, cw), rep(FP, fp), rep(HS, hs),
                 rep(LE, le), rep(PRS, prs), rep(RES, res), rep(RAO, rao),
                 rep(SOP, sop), rep(TR, tr))

jsarray <- paste0('["', paste(colorVector, collapse = '", "'), '"]')
nodeStrokeJS <- JS(paste0('function(d, i) { return ', jsarray, '[i]; }'))

### create the diagram
radialNetwork(List = Flare_ohine, 
              fontSize = 25, 
              opacity = 0.9,
              nodeStroke = "white",
              linkColour = nodeStrokeJS)
              
# Main code
#save the manual way: viewer -> export -> save as web html
#install_phantomjs()
webshot::webshot("dendogram.html", file="figs/quant_sources.jpeg", zoom = 4)

# Future Updates:
# make the goals font larger
