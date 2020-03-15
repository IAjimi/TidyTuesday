library(sf)
library(mapview)
library(tmap)
library(spData)
library(htmltools)
library(leaflet)

confirmed_cases_map <- confirmed_cases %>% 
  st_as_sf( 
    coords = c("Long", "Lat"),
    crs = "+proj=longlat +datum=WGS84") %>%
  rename(`Data Retrieved` = date_key, `# Confirmed Cases` = date_val)

mapview(confirmed_cases_map,  cex = "# Confirmed Cases", alpha = 0.7, legend = FALSE)

tmap_mode("view")
us_states_map <- tm_shape(us_states, projection = 2163) + 
  tm_polygons() + 
  tm_layout(frame = FALSE)


us_states_map +
  tm_symbols(col = "black", border.col = "white", size = "population_millions") 

  ######

confirmed_cases %>%
  rename(Cases = date_val) %>%
  inner_join(rename(deaths, Deaths = date_val)) %>%
  inner_join(rename(recoveries, Recoveries = date_val)) %>%
  filter(date_key == max(.$date_key)) %>%
  mutate(map_label = if_else(is.na(`Province/State`), `Country/Region`, `Province/State`)) %>%
  mutate(map_label_cases = paste(map_label, "\n Confirmed Cases:", Cases),
         map_label_deaths = paste(map_label, "\n Deaths:", Deaths),
         map_label_recoveries = paste(map_label, "\n Recoveries:", Recoveries)) %>%
  leaflet() %>% 
  addProviderTiles(providers$CartoDB.Positron) %>%
  addCircleMarkers(weight = 1,
                   color = "red",
                  label = ~as.character(map_label_cases),
                   radius = ~ log10(Cases) * 1.5, 
                   popup = ~htmlEscape(map_label),
                   stroke = FALSE,
                  fillOpacity = 0.5,
                  labelOptions = labelOptions(textsize = "15px"),
                  group = "Confirmed Cases") %>%
  addCircleMarkers(weight = 1,
                   color = "black",
                   label = ~as.character(map_label_deaths),
                   radius = ~ log10(Deaths) * 1.5, 
                   popup = ~htmlEscape(map_label_deaths),
                   stroke = FALSE,
                   fillOpacity = 0.5,
                   labelOptions = labelOptions(textsize = "15px"),
                   group = "Deaths") %>%
  addCircleMarkers(weight = 1,
                   color = "green",
                   label = ~as.character(map_label_recoveries),
                   radius = ~ log10(Recoveries) * 1.5, 
                   popup = ~htmlEscape(map_label_recoveries),
                   stroke = FALSE,
                   fillOpacity = 0.5,
                   labelOptions = labelOptions(textsize = "15px"),
                   group = "Recoveries") %>%
  addLayersControl(
    overlayGroups = c("Confirmed Cases", "Deaths", "Recoveries"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>% hideGroup("Deaths") %>% hideGroup("Recoveries")


####
mapStates <- maps::map("state", fill = TRUE, plot = FALSE)

leaflet(data = mapStates) %>% 
  addTiles() %>%
  addPolygons(fillColor = topo.colors(10, alpha = NULL), stroke = FALSE) %>%
  addCircleMarkers(label = ~as.character(date_val),
                   popup = ~htmlEscape(date_val),
                   color = "red",
                   stroke = FALSE, fillOpacity = 0.5)


cities <- read.csv(textConnection("
City,Lat,Long,Pop
Boston,42.3601,-71.0589,645966
Hartford,41.7627,-72.6743,125017
New York City,40.7127,-74.0059,8406000
Philadelphia,39.9500,-75.1667,1553000
Pittsburgh,40.4397,-79.9764,305841
Providence,41.8236,-71.4222,177994
"))

leaflet(cities) %>% addTiles() %>%
  addCircles(lng = ~Long, lat = ~Lat, weight = 1,
             radius = ~sqrt(Pop) * 30, popup = ~City
  )
