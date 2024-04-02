rm(list = ls())

library(tidyverse)
library(tidycensus)
library(plotly)
library(sf)
library(leaflet)
library(leafsync)
library(htmltools)
library(rjson)

options(scipen=999) #scientific notation setup

census_api_key("e40cc68dcd205c78498b9c938801139a9b8f6362", #personal key
               overwrite = T,
               install = T)
readRenviron("~/.Renviron") 

#### Importing 2021 ACS 5-year estimates ####

# https://www.socialexplorer.com/data/ACS2022/metadata/?ds=SE
var21 <- load_variables(year=2021,dataset = "acs5")

vars_transportation <- c(
  tot_pop = 'B08301_001',
  drovealone = 'B08301_003',
  carpooled = 'B08301_004',
  public_transit = 'B08301_010',
  bus = 'B08301_011',
  subway = 'B08301_012',
  comm_rail = 'B08301_013',
  home = 'B08301_021'
)

commute_transport <- get_acs(
  survey = "acs5",
  year = 2021,
  geography = "tract",
  county = "Philadelphia",
  state = "PA",
  output = "wide",
  variables = vars_transportation
)

#### Calculate new variables ####

transport <- commute_transport %>% 
  mutate(perc_drovealone = drovealoneE / tot_popE,
         perc_carpooled = carpooledE / tot_popE,
         perc_pubtrans = public_transitE / tot_popE,
         perc_bus = busE / tot_popE,
         perc_subway = subwayE / tot_popE,
         perc_commrail = comm_railE / tot_popE,
         perc_home = homeE / tot_popE
  ) %>% 
  select(c(GEOID, perc_drovealone, perc_carpooled, perc_pubtrans,
           perc_home))

head(transport)
summary(transport)

#### Create leaflet map ####

# upload tract shapefile
tracts10_shp <- st_read("~/Desktop/Economy League/Leading Indicators/2023_11_2_Commuting/Census_Tracts_2010-shp/c16590ca-5adf-4332-aaec-9323b2fa7e7d2020328-1-1jurugw.pr6w.shp") 
tracts10_shp$GEOID <- tracts10_shp$GEOID10

# join the data and shapefile
shp <- left_join(tracts10_shp, transport, by="GEOID")

## add a palette and bins

binsize = c(0, 0.2, 0.4, 0.6, 0.8, 1)

## drove alone
drove_pal <- colorBin(palette = c("#fdae61","#fee090",
                                   "#abd9e9",
                                   "#74add1" ,"#4575b4"),
                       domain = shp$perc_drovealone,
                       bins=binsize)

drove_lab <- sprintf(
  "<i>Drive Alone: <i><br>%s %s",
  round((shp$perc_drovealone)*100,digits = 1), "%") %>%
  lapply(htmltools::HTML)

## public transit
pubtrans_pal <- colorBin(palette = c("#fdae61","#fee090",
                                     "#abd9e9",
                                     "#74add1" ,"#4575b4"),
                       domain = shp$perc_pubtrans,
                       bins = binsize)

pubtrans_lab <- sprintf(
  "<i>Take Public Transit: <i><br>%s %s",
  round((shp$perc_pubtrans)*100,digits = 1), "%") %>%
  lapply(htmltools::HTML)

## work from home
home_pal <- colorBin(palette = c("#fdae61","#fee090",
                                    "#abd9e9",
                                    "#74add1" ,"#4575b4"),
                            domain = shp$perc_home,
                            bins = binsize)

home_lab <- sprintf(
  "<i>Work from Home: <i><br>%s %s",
  round((shp$perc_home)*100,digits = 1), "%") %>%
  lapply(htmltools::HTML)

#---------------------------------------------------------#

# Map
y <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron
  ) %>% # under 30 mins
  addPolygons(data=shp,
              fillColor=~drove_pal(perc_drovealone),
              weight=1,
              opacity=1,
              color="gray",
              fillOpacity=1,
              highlightOptions=highlightOptions(color="black",
                                                weight=2,
                                                bringToFront=T),
              label=drove_lab,
              group="Drive Alone"
  ) %>%
  addLegend(data=shp,
            pal=drove_pal,
            values=~perc_drovealone,
            opacity=0.7,
            title="Percent of Workers<br>Who Drive Alone",
            position = "bottomleft",
            labFormat = labelFormat(suffix = "%",
                                    transform = function(x) 100*x),
            group="Drive Alone"
  ) %>% # public transit
  addPolygons(data=shp,
              fillColor=~pubtrans_pal(perc_pubtrans),
              weight=1,
              opacity=1,
              color="gray",
              fillOpacity=1,
              highlightOptions=highlightOptions(color="black",
                                                weight=2,
                                                bringToFront=T),
              label=pubtrans_lab,
              group="Public Transit"
  ) %>%
  addLegend(data=shp,
            pal=pubtrans_pal,
            values=~perc_pubtrans,
            opacity=0.7,
            title="Percent of Workers<br>Who Take Public Transit",
            position = "bottomleft",
            labFormat = labelFormat(suffix = "%",
                                    transform = function(x) 100*x),
            group="Public Transit"
  ) %>% # home
  addPolygons(data=shp,
              fillColor=~home_pal(perc_home),
            weight=1,
            opacity=1,
            color="gray",
            fillOpacity=1,
            highlightOptions=highlightOptions(color="black",
                                              weight=2,
                                              bringToFront=T),
            label=home_lab,
            group="Work from Home"
  ) %>%
  addLegend(data=shp,
            pal=home_pal,
            values=~perc_home,
            opacity=0.7,
            title="Percent of Workers<br>Who Work from Home",
            position = "bottomleft",
            labFormat = labelFormat(suffix = "%",
                                    transform = function(x) 100*x),
            group="Work from Home"
  )

y1 <- y %>%
  addLayersControl(
    overlayGroups = c("Drive Alone",
                      "Public Transit",
                      "Work from Home"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  hideGroup(c("Public Transit",
              "Work from Home"))

y1


# optional dual map
y2 <- y %>%
  addLayersControl(
    overlayGroups = c("Drive Alone",
                      "Public Transit",
                      "Work from Home"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  hideGroup(c("Drive Alone",
              "Work from Home"))

t <- sync(y1,y2,ncol=1)
t
