## ----echo=FALSE----------------------------------------------------------
knitr::opts_chunk$set(eval=evaluate, cache=cache.me)

## ------------------------------------------------------------------------
events.df <- data.table::fread(file.path(project.dir, "data/phytoplankton/cedr_phyto_event.csv"),
                            data.table = FALSE,
                            na.strings = "")

station.df <- data.table::fread(file.path(project.dir,
                                          "data/phytoplankton/cedr_phyto_station.csv"),
                                data.table = FALSE,
                                na.strings = "")

## ------------------------------------------------------------------------
missing.taxa <- anti_join(events.df, bay.df, by = "station")

## ------------------------------------------------------------------------
library(leaflet)
events.sub <- events.df %>% 
  select(catalogingunitdescription, source, latitude, longitude, station) %>% 
  filter(!is.na(latitude)) %>% 
  mutate(no_taxa_data = station %in% unique(missing.taxa$station),
         no_taxa_data = factor(no_taxa_data)) %>% 
  distinct() %>% 
  mutate(longitude = jitter(longitude, amount = 0.0005),
         latitude = jitter(latitude, amount = 0.0005))

if (length(unique(events.sub$no_taxa_data)) == 2) {
  factpal <- colorFactor(c("blue", "red"), events.sub$no_taxa_data)
} else if (length(unique(events.sub$no_taxa_data)) == 1) {
  factpal <- colorFactor("blue", events.sub$no_taxa_data)
} else {
  factpal <- colorFactor("pink", events.sub$no_taxa_data)
}


leaflet(events.sub) %>% 
  addProviderTiles(providers$CartoDB.Positron,
                   options = leaflet::tileOptions(minZoom = 7, maxZoom = 18)) %>% 
  addCircleMarkers(~longitude, ~latitude,
                   color = ~factpal(no_taxa_data),
                   stroke = FALSE,
                   fillOpacity = 0.5,
                   popup = paste("Station:", events.sub$station, "<br/>",
                                 "Description:", events.sub$catalogingunitdescription, "<br/>",
                                 "Source:", events.sub$source, "<br/>",
                                 "Latitude:", events.sub$latitude, "<br/>",
                                 "Longitude:", events.sub$longitude),
                   popupOptions = popupOptions(style = list(color = "black"))) %>% 
  leaflet::setMaxBounds(lng1 = -78, lat1 = 36, lng2 = -75, lat2 = 40.5) %>% 
  leaflet::setView(-76.4, lat = 38, zoom = 7) 


