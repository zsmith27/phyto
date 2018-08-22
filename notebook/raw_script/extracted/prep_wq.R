## ----echo=FALSE----------------------------------------------------------
knitr::opts_chunk$set(eval=evaluate, cache=cache.me)

## ------------------------------------------------------------------------
wq.raw <- data.table::fread(file.path(project.dir, "data/water_quality/cedr_wq.csv"),
                            data.table = FALSE,
                           na.strings = c("")) %>% 
  filter(is.na(problem))

## ------------------------------------------------------------------------
stations.df <- wq.raw %>% 
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
wq.df <- wq.raw %>% 
  mutate(sampledate = as.Date(sampledate))

## ------------------------------------------------------------------------
wq.df <- wq.df %>% 
  mutate(pycnocline = case_when(
    is.na(upperpycnocline) ~ FALSE,
    upperpycnocline == 0 ~ FALSE,
    TRUE ~ TRUE
  ))

## ------------------------------------------------------------------------
issue.df <- wq.df %>% 
  select(monitoringstation, sampledate,
         pycnocline, upperpycnocline, lowerpycnocline) %>% 
  distinct() %>% 
  group_by(monitoringstation, sampledate) %>% 
  mutate(count = n()) %>% 
  ungroup() %>% 
  filter(count == 2,
         complete.cases(.)) %>% 
  rename(up = upperpycnocline,
         lp = lowerpycnocline) %>% 
  select(-pycnocline)
  

wq.df <- left_join(wq.df, issue.df, by = c("monitoringstation", "sampledate")) %>% 
  mutate(pycnocline = if_else(is.na(count), pycnocline, TRUE),
         upperpycnocline = if_else(is.na(up), upperpycnocline, up),
         lowerpycnocline = if_else(is.na(lp), lowerpycnocline, lp)
         )

## ------------------------------------------------------------------------
wq.df <- wq.df %>% 
  select(station, source, sampledate, samplereplicatetype,
         depth, layer, 
         pycnocline, upperpycnocline, lowerpycnocline,
         parameter, measurevalue) %>% 
  distinct()

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

## ---- eval=FALSE, echo=FALSE---------------------------------------------
## # If the upper pycnocline depth is specified, then the mean for each parameter (i.e. salinity, chlorophyll a, pheophytin, DOC) is found using samples specified as above the pycnocline ("ap") and surface ("s") Buchanan et al. [-@BuchananPhytoplanktonreferencecommunities2005, p. 139]. If the upper pycnocline depth is not specified, then the mean for each parameter (i.e. salinity, chlorophyll a, pheophytin, DOC) is found using samples from the entire water column Buchanan et al. [-@BuchananPhytoplanktonreferencecommunities2005, p. 139]. Finally, join the two data frames back together to re-create `wq.df`.
## pdepth.df <- events.df %>%
##   mutate(sampledate = as.Date(sampledate)) %>%
##   select(station, sampledate, pdepth) %>%
##   dplyr::distinct()
## 
## wq.df <- left_join(wq.df, pdepth.df, by = c("station", "sampledate"))
## abp.df <- wq.df %>%
##   filter(pycnocline == TRUE,
##          # source != "vims",
##          depth <= upperpycnocline
##          # depth <= pdepth
##          ) %>%
##   avg_wq()
## 
## vims.df <- wq.df %>%
##   filter(pycnocline == TRUE,
##          source == "vims",
##          depth <= lowerpycnocline) %>%
##   avg_wq()
## 
## wc.df <- wq.df %>%
##   filter(pycnocline == FALSE) %>%
##   avg_wq()
## 
## wq.df <- bind_rows(abp.df,
##                    # vims.df,
##                    wc.df) %>%
##   rename(date = sampledate)

## ------------------------------------------------------------------------
pdepth.df <- events.df %>% 
  filter(layer %in% c("ap", "wc")) %>% 
  mutate(sampledate = as.Date(sampledate)) %>% 
  select(station, sampledate, pdepth) %>% 
  dplyr::distinct()

wq.df <- left_join(wq.df, pdepth.df, by = c("station", "sampledate"))
wq.df <- wq.df %>% 
  filter(depth <= pdepth) %>% 
  avg_wq() %>% 
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
  mutate(
    surface_chla = if_else(!is.na(s_chla), s_chla, as.numeric(NA)),
    pheophytin = if_else(!is.na(pheo), pheo, as.numeric(NA)),
    doc = if_else(!is.na(doc), doc, as.numeric(NA))
    ) %>% 
  select(station, sampledate, surface_chla, chla, pheophytin, doc)

## ------------------------------------------------------------------------
bay.df <- left_join(bay.df, env.wide, by = c("station", "sampledate"))

## ------------------------------------------------------------------------
#rm(env.df, env.wide, wq.df, wq.df, bay.sub)

