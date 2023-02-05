#Script to plot a dynamic map using ALA API
#SLM
#Created:
#Updated: 06022023


#-- LIBRARIES --#
library(leaflet) #mapping 
library(rjson) #json wrangling and requests
library(jsonlite) #json wrangling
library(httr) #http request
library(tidyverse) #data wrangling
library(viridis) #colour scheme

# -- API REQUEST --#
#Port Jackson Shark
link <- "https://biocache-ws.ala.org.au/ws/occurrences/search?q=taxa%3A%22Port%20Jackson%20Shark%22&qualityProfile=ALA&pageSize=40000" #TODO: #3 the extraction of all data can be performed via a function and loop

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
coords <- cbind(pj_cleaned$decimalLongitude,pj_cleaned$decimalLatitude) %>%
          na.omit()

hull <- chull(coords)
polygons <- as.data.frame(coords[hull,])

# -- DEFINE PALETTE FOR MAP --#
palettini <- colorFactor(viridis(7), pj_cleaned$basisOfRecord)

show_polygon <- TRUE

map <- leaflet(pj_cleaned) %>%
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
  addPolygons(lat = polygons$V2, lng = polygons$V1, 
    color = "red", opacity = 0.5, fillOpacity = 0.3,
    group = "Convex Hull",
    layerId = "convex-hull") %>%
  addLegend("bottomright", pal = palettini, values = ~basisOfRecord, 
    labels = "Basis of Records", title = "Map Replication from ALA") %>%
  addLayersControl(
    baseGroups = c("OpenStreetMap", "Positron", "DarkMatter", "Minimum Convex Hull"),
    options = layersControlOptions(collapsed = TRUE))

