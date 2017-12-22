## ----echo=FALSE----------------------------------------------------------
knitr::opts_chunk$set(eval=evaluate, cache=cache.me)

## ------------------------------------------------------------------------
ibi.df <- ratings.df %>% 
  select(-metric, -value, -score) %>% 
  unite(season_sal, season, salzone) %>% 
  distinct() %>% 
  left_join(stations.df, by = "station") %>% 
  mutate(longitude = jitter(longitude, amount = 0.01),
         latitude = jitter(latitude, amount = 0.01))

## ------------------------------------------------------------------------
ibi.df <- ibi.df %>% 
  mutate(rating = factor(rating, c("poor", "fair_poor", "fair", "fair_good", "good")))

## ------------------------------------------------------------------------
factpal <- colorFactor(c("red", "orange", "yellow", "lightgreen", "green"), ibi.df$rating)

## ------------------------------------------------------------------------
leaflet(ibi.df) %>% 
  addProviderTiles(providers$CartoDB.Positron) %>%  
  addCircleMarkers( ~longitude, ~latitude,
                    color = ~factpal(rating),
                    stroke = FALSE,
                    fillOpacity = 0.5,
                    popup = paste("Station:", stations.df$station, "<br/>",
                                  "Agency:", stations.df$agency, "<br/>",
                                  "Source:", stations.df$datasource, "<br/>",
                                  "Latitude:", stations.df$latitude, "<br/>",
                                  "Longitude:", stations.df$longitude))

## ---- fig.width = 12, fig.height = 12, message=FALSE---------------------
min.date <- min(ibi.df$date, na.rm = TRUE)
max.date <- max(ibi.df$date, na.rm = TRUE)

ggplot(data = ibi.df, aes(date, ibi_score, group = season_sal, color = season_sal)) +
  theme_classic() +
  scale_x_date(limits = c(min(ibi.df$date, na.rm = TRUE), max(ibi.df$date, na.rm = TRUE)), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0.8, 5.2), expand = c(0, 0)) +
  annotate("rect", xmin = min.date, xmax = max.date, ymin = 0.8, ymax = 2, fill = "red", alpha = 0.25) +
  annotate("rect", xmin = min.date, xmax = max.date, ymin = 2, ymax = 2.67, fill = "orange", alpha = 0.25) +
  annotate("rect", xmin = min.date, xmax = max.date, ymin = 2.67, ymax = 3.33, fill = "yellow", alpha = 0.25) +
  annotate("rect", xmin = min.date, xmax = max.date, ymin = 3.33, ymax = 4, fill = "lightgreen", alpha = 0.25) +
  annotate("rect", xmin = min.date, xmax = max.date, ymin = 4, ymax = 5.2, fill = "green", alpha = 0.25) +
  geom_line() +
  geom_point() +
  facet_wrap(~ station, ncol = 5, scales = "free")


## ---- fig.width = 12, fig.height = 60, message=FALSE---------------------
ggplot(data = ibi.df, aes(date, ibi_score, group = season_sal, color = season_sal)) +
  theme_classic() +
  scale_x_date(limits = c(min(ibi.df$date, na.rm = TRUE), max(ibi.df$date, na.rm = TRUE)), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0.8, 5.2), expand = c(0, 0)) +
  annotate("rect", xmin = min.date, xmax = max.date, ymin = 0.8, ymax = 2, fill = "red", alpha = 0.25) +
  annotate("rect", xmin = min.date, xmax = max.date, ymin = 2, ymax = 2.67, fill = "orange", alpha = 0.25) +
  annotate("rect", xmin = min.date, xmax = max.date, ymin = 2.67, ymax = 3.33, fill = "yellow", alpha = 0.25) +
  annotate("rect", xmin = min.date, xmax = max.date, ymin = 3.33, ymax = 4, fill = "lightgreen", alpha = 0.25) +
  annotate("rect", xmin = min.date, xmax = max.date, ymin = 4, ymax = 5.2, fill = "green", alpha = 0.25) +
  geom_line(na.rm = TRUE) +
  geom_point(na.rm = TRUE) +
  facet_wrap(~ station + season_sal, ncol = 5, scales = "free")


## ---- fig.width = 12, fig.height = 12------------------------------------

station.vec <- sort(unique(ibi.df$station))
min.station <- station.vec[1]
max.station <- station.vec[length(station.vec)]
ggplot(data = ibi.df, aes(station, ibi_score)) +
  theme_classic() +
  #scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(limits = c(0.8, 5.2), expand = c(0, 0)) +
  annotate("rect", xmin = min.station, xmax = max.station, ymin = 0.8, ymax = 2, fill = "red", alpha = 0.25) +
  annotate("rect", xmin = min.station, xmax = max.station, ymin = 2, ymax = 2.67, fill = "orange", alpha = 0.25) +
  annotate("rect", xmin = min.station, xmax = max.station, ymin = 2.67, ymax = 3.33, fill = "yellow", alpha = 0.25) +
  annotate("rect", xmin = min.station, xmax = max.station, ymin = 3.33, ymax = 4, fill = "lightgreen", alpha = 0.25) +
  annotate("rect", xmin = min.station, xmax = max.station, ymin = 4, ymax = 5.2, fill = "green", alpha = 0.25) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  facet_wrap(~season_sal, ncol = 2)

