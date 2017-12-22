## ----echo=FALSE----------------------------------------------------------
knitr::opts_chunk$set(eval=evaluate, cache=cache.me)

## ------------------------------------------------------------------------
missing.taxa<- anti_join(events.df, bay.df, by = "station")

## ------------------------------------------------------------------------
station.df <- data.table::fread(file.path(project.dir, "data/phytoplankton/cedr_phyto_station.csv"),
                            data.table = FALSE,
                            na.strings = "")

## ------------------------------------------------------------------------
library(leaflet)
events.sub <- events.df %>% 
  select(catalogingunitdescription, source, latitude, longitude, station) %>% 
  filter(!is.na(latitude)) %>% 
  mutate(no_taxa_data = station %in% unique(missing.taxa$station),
         no_taxa_data = factor(no_taxa_data)) %>% 
  distinct() %>% 
  mutate(longitude = jitter(longitude, amount = 0.005),
         latitude = jitter(latitude, amount = 0.005))

factpal <- colorFactor(c("blue", "red"), events.sub$no_taxa_data)

leaflet(events.sub) %>% 
  addProviderTiles(providers$CartoDB.Positron) %>% 
  addCircleMarkers( ~longitude, ~latitude,
                    color = ~factpal(no_taxa_data),
                    stroke = FALSE,
                    fillOpacity = 0.5,
                    popup = paste("Station:", events.sub$station, "<br/>",
                                  "Description:", events.sub$catalogingunitdescription, "<br/>",
                                  "Source:", events.sub$source, "<br/>",
                                  "Latitude:", events.sub$latitude, "<br/>",
                                  "Longitude:", events.sub$longitude),
                    popupOptions = popupOptions(style = list(color = "black")))


