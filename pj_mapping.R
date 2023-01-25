#Script to plot a dynamic map using ALA API

#-- LIBRARIES --#
library(leaflet)
library(rjson)
library(jsonlite)
library(httr)
library(tidyverse)
library(viridis)

# -- API REQUEST --#
#Port Jackson Shark
link <- "https://biocache-ws.ala.org.au/ws/occurrences/search?q=taxa%3A%22Port%20Jackson%20Shark%22&qualityProfile=ALA&pageSize=10000" #TODO: #3 the extraction of all data can be performed via a function and loop

#Get data
response <- GET(link)

#Turn list of lists into simple vectors and obtain content from response into JSON
df <- purrr::flatten(fromJSON(content(response, as = "text")))

#Convert list into a dataframe
pjdata <- as_tibble(df)

# -- CLEAN DATASET --#
pj_cleaned <- pjdata %>%
    select(uuid, scientificName, decimalLatitude, decimalLongitude, year, basisOfRecord, dataProviderName) #TODO: #2 change the basis of record into a user friendly format


# -- MINIMUM CONVEX POLYGON --#
#TODO: #1 minimum convex polygon and add to the leaflet map

#coords <- cbind(pj_cleaned$decimalLongitude,pj_cleaned$decimalLatitude)
#hull <- chull(coords)
#polygon <- coords[hull,]

# -- DEFINE PALETTE FOR MAP --#
palettini <- colorFactor(viridis(7), pj_cleaned$basisOfRecord)

leaflet(pj_cleaned) %>%
 addProviderTiles(providers$CartoDB.Positron, group = "Positron")%>%
  addProviderTiles(providers$OpenStreetMap)%>%
  addProviderTiles(providers$CartoDB.DarkMatter, group = "DarkMatter") %>%
  addCircleMarkers(lng = ~decimalLongitude,
                  lat = ~decimalLatitude,
                  radius = 4,
                  fillColor = ~palettini(basisOfRecord),
                  stroke = FALSE,
                  fillOpacity = 0.8,
                  popup = ~dataProviderName) %>%
  addLegend("bottomright", pal = palettini, values = ~basisOfRecord, 
  labels = "Basis of Records", title = "Map Replication from ALA") %>%
  addLayersControl(
    baseGroups = c("OpenStreetMap", "Positron", "DarkMatter"),
  options = layersControlOptions(collapsed = TRUE))