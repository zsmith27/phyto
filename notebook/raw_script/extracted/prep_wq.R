## ----echo=FALSE----------------------------------------------------------
knitr::opts_chunk$set(eval=evaluate, cache=cache.me)

## ------------------------------------------------------------------------
wq.df <- data.table::fread(file.path(project.dir, "data/water_quality/cedr_wq.csv"),
                            data.table = FALSE,
                           na.strings = c("")) %>% 
  filter(is.na(problem))

## ------------------------------------------------------------------------
stations.df <- wq.df %>% 
  dplyr::select(station, agency, source, latitude, longitude) %>% 
  distinct() %>% 
  mutate(longitude = jitter(longitude, amount = 0.0005),
         latitude = jitter(latitude, amount = 0.0005))

leaflet(stations.df) %>% 
    addProviderTiles(providers$CartoDB.Positron,
                   options = leaflet::tileOptions(minZoom = 7, maxZoom = 18)) %>% 
  addCircleMarkers( ~longitude, ~latitude,
                    stroke = FALSE,
                    fillOpacity = 0.5,
                    popup = paste("Station:", stations.df$station, "<br/>",
                                  "Agency:", stations.df$agency, "<br/>",
                                  "Source:", stations.df$source, "<br/>",
                                  "Latitude:", stations.df$latitude, "<br/>",
                                  "Longitude:", stations.df$longitude)) %>% 
   leaflet::setMaxBounds(lng1 = -78, lat1 = 36, lng2 = -75, lat2 = 40.5) %>% 
  leaflet::setView(-76.4, lat = 38, zoom = 7) 

## ------------------------------------------------------------------------
wq.df <- wq.df %>% 
  mutate(sampledate = as.Date(sampledate))

## ------------------------------------------------------------------------
wq.df <- wq.df %>% 
  mutate(pycnochline = case_when(
    is.na(upperpycnocline) ~ FALSE,
    upperpycnocline == 0 ~ FALSE,
    TRUE ~ TRUE
  ))

## ------------------------------------------------------------------------
issue.df <- wq.df %>% 
  select(monitoringstation, sampledate, pycnochline) %>% 
  distinct() %>% 
  group_by(monitoringstation, sampledate) %>% 
  count() %>% 
  filter(n == 2)

wq.df <- left_join(wq.df, issue.df, by = c("monitoringstation", "sampledate")) %>% 
  mutate(pycnochline2 = if_else(is.na(n), pycnochline, TRUE))


## ------------------------------------------------------------------------
wq.df <- wq.df %>% 
  select(station, sampledate, samplereplicatetype,
         depth, layer, pycnochline, parameter, measurevalue)

## ------------------------------------------------------------------------
wq.df <- wq.df %>% 
  filter(layer == "s", 
         parameter == "chla") %>% 
  unite(parameter, c("layer", "parameter"), remove = FALSE) %>% 
  bind_rows(wq.df)

## ------------------------------------------------------------------------
avg_wq <- function(x) {
  final.df <- x %>% 
    group_by_at(vars(-measurevalue, -samplereplicatetype)) %>% 
    summarize(measurevalue = mean(measurevalue, na.rm = TRUE)) %>% 
    group_by_at(vars(-measurevalue, -layer)) %>% 
    summarize(measurevalue = mean(measurevalue, na.rm = TRUE)) %>% 
    group_by_at(vars(-measurevalue, -depth)) %>% 
    summarize(measurevalue = mean(measurevalue, na.rm = TRUE)) %>% 
    ungroup()
}

## ------------------------------------------------------------------------
abp.df <- wq.df %>% 
  filter(pycnochline == TRUE,
         layer %in% c("ap", "s")) %>% 
  avg_wq()

wc.df <- wq.df %>% 
  filter(pycnochline == FALSE) %>% 
  avg_wq()

wq.df <- bind_rows(abp.df, wc.df) %>% 
  rename(date = sampledate)

## ------------------------------------------------------------------------
bay.sub <- bay.df %>% 
  select(station, sampledate) %>% 
  distinct() %>% 
  mutate(lower_date = sampledate - lubridate::days(3),
         upper_date = sampledate + lubridate::days(3))

## ----message=FALSE-------------------------------------------------------
library(parallel)
n.cores <- detectCores() - 1
cl <- makeCluster(n.cores)
clusterExport(cl = cl, varlist = c("wq.df", "bay.sub"))
clusterEvalQ(cl, c(library(dplyr))) %>% invisible()

## ------------------------------------------------------------------------
env.df <- parLapply(cl, 1:nrow(bay.sub), function(row.i) {

  sub.df <- slice(bay.sub, row.i)
  #----------------------------------------------------------------------------
  sub.env <- wq.df %>% 
    filter(station == sub.df$station,
           date >= sub.df$lower_date,
           date <= sub.df$upper_date)
  #----------------------------------------------------------------------------
  if (nrow(sub.env) == 0) return(data.frame(
    station = NA,
    date = NA,
    parameter = NA,
    measurevalue = NA
  ))
  #----------------------------------------------------------------------------
  final.df <- sub.env %>% 
    mutate(date_diff = date - sub.df$sampledate,
           abs_date_diff = abs(date_diff),
           sampledate = sub.df$sampledate) %>% 
    filter(abs_date_diff == min(abs_date_diff))
  #----------------------------------------------------------------------------
  if (nrow(final.df) > 1) {
    final.df <- final.df %>% 
      filter(date == min(date))
  }
  #----------------------------------------------------------------------------
  return(final.df)
}) %>% 
  bind_rows() %>% 
  filter(!is.na(station))

stopCluster(cl)
#closeAllConnections()

## ------------------------------------------------------------------------
env.wide <- env.df %>% 
  spread(parameter, measurevalue)

## ------------------------------------------------------------------------
env.wide <- env.wide %>% 
  mutate_at(vars(salinity, s_chla, chla, pheo, doc), as.numeric) %>% 
  rename(surface_chla = s_chla,
         pheophytin = pheo) %>% 
  select(station, sampledate, pycnochline, salinity, surface_chla, chla, pheophytin, doc)

## ------------------------------------------------------------------------
bay.df <- left_join(bay.df, env.wide, by = c("station", "sampledate"))

## ------------------------------------------------------------------------
bay.df <- bay.df %>% 
  mutate(salzone_calc = case_when(
    salinity <= 0.5 ~ "f",
    salinity <= 5.0 ~ "o",
    salinity <= 18.0 ~ "m",
    salinity > 18.0 ~ "p",
    TRUE ~ "missing"
  ))

## ------------------------------------------------------------------------
bay.sub <- bay.df %>% 
  mutate(salzone_disagree = if_else(salzone_calc != old_salzone, TRUE, FALSE))

## ------------------------------------------------------------------------
salzone.diff <- bay.sub %>% 
  select(station, sampledate, source, salzone_calc, old_salzone, salzone_disagree) %>% 
  unite(sal_group, salzone_calc, old_salzone) %>% 
  distinct() %>% 
  filter(salzone_disagree == TRUE) %>% 
  group_by(sal_group) %>% 
  summarize(count = n()) %>% 
  ungroup() %>% 
  arrange(count) %>% 
  mutate(sal_group = factor(sal_group, levels = unique(sal_group))) %>% 
  ggplot(aes(sal_group, count, fill = count)) + 
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  geom_bar(stat = "identity") +
  xlab("Salinity Zone Disagreement") +
  ylab("Number of Disagreements")
salzone.diff

## ------------------------------------------------------------------------
bay.df2 <- bay.df %>% 
  mutate(salzone = if_else(salzone_calc == "missing",
                           salzone,
                           salzone_calc))

## ------------------------------------------------------------------------
#rm(env.df, env.wide, wq.df, wq.df, bay.sub)

