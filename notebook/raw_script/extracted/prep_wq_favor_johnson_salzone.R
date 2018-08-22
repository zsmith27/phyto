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
  mutate(pycnochline = if_else(is.na(n), pycnochline, TRUE))

## ------------------------------------------------------------------------
wq.df <- wq.df %>% 
  select(station, source, sampledate, samplereplicatetype,
         depth, layer, 
         pycnochline, upperpycnocline, lowerpycnocline,
         parameter, measurevalue)

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

## ---- eval=FALSE---------------------------------------------------------
## # If the upper pycnocline depth is specified, then the mean for each parameter (i.e. salinity, chlorophyll a, pheophytin, DOC) is found using samples specified as above the pycnocline ("ap") and surface ("s") Buchanan et al. [-@BuchananPhytoplanktonreferencecommunities2005, p. 139]. If the upper pycnocline depth is not specified, then the mean for each parameter (i.e. salinity, chlorophyll a, pheophytin, DOC) is found using samples from the entire water column Buchanan et al. [-@BuchananPhytoplanktonreferencecommunities2005, p. 139]. Finally, join the two data frames back together to re-create `wq.df`.
## pdepth.df <- events.df %>%
##   mutate(sampledate = as.Date(sampledate)) %>%
##   select(station, sampledate, pdepth) %>%
##   dplyr::distinct()
## 
## wq.df <- left_join(wq.df, pdepth.df, by = c("station", "sampledate"))
## abp.df <- wq.df %>%
##   filter(pycnochline == TRUE,
##          # source != "vims",
##          depth <= upperpycnocline
##          # depth <= pdepth
##          ) %>%
##   avg_wq()
## 
## vims.df <- wq.df %>%
##   filter(pycnochline == TRUE,
##          source == "vims",
##          depth <= lowerpycnocline) %>%
##   avg_wq()
## 
## wc.df <- wq.df %>%
##   filter(pycnochline == FALSE) %>%
##   avg_wq()
## 
## wq.df <- bind_rows(abp.df,
##                    # vims.df,
##                    wc.df) %>%
##   rename(date = sampledate)

## ------------------------------------------------------------------------
pdepth.df <- events.df %>% 
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
  mutate_at(vars(salinity, s_chla, chla, pheo, doc), as.numeric) %>% 
  rename(surface_chla = s_chla,
         pheophytin = pheo) %>% 
  select(station, sampledate, pycnochline, salinity, surface_chla, chla, pheophytin, doc)

## ------------------------------------------------------------------------
bay.df <- left_join(bay.df, env.wide, by = c("station", "sampledate"))

## ------------------------------------------------------------------------
events.df <- events.df %>% 
  mutate(sampledate = as.Date(sampledate),
         salzone = case_when(
           salzone %in% c("tf", "fe") ~ "f",
           salzone %in% c("m", "me") ~ "m",
           salzone %in% c("o", "oe") ~ "o",
           salzone %in% c("p", "pe") ~ "p",
           TRUE ~ as.character(NA)
         ))

## ------------------------------------------------------------------------
events.sub <- events.df %>% 
  select(source, station, sampledate, layer, salzone) %>% 
  distinct() %>% 
  rename(salzone_cedr = salzone)

## ------------------------------------------------------------------------
old.salzone <- readxl::read_excel(file.path(project.dir, "data/jackie_data/Data 2013_4plus-phyto-metrics.xlsx"),
                         sheet = "DATA_4+biometrics",
                         skip = 1) %>% 
  clean_up() %>% 
  select(station, sample_date, ibi_salzone) %>% 
  mutate(sample_date = as.Date(sample_date)) %>% 
  rename(salzone_johnson = ibi_salzone,
         sampledate = sample_date) %>% 
  distinct()

## ------------------------------------------------------------------------
events.sub <- inner_join(events.sub, old.salzone, by = c("station", "sampledate"))

## ------------------------------------------------------------------------
events.sub <- events.sub %>% 
  mutate(salzone_disagree = if_else(salzone_cedr != salzone_johnson, TRUE, FALSE))

## ---- fig.width=10-------------------------------------------------------
salzone.diff <- events.sub %>% 
  select(station, sampledate, source, salzone_cedr, salzone_johnson, salzone_disagree) %>% 
  unite(sal_group, salzone_cedr, salzone_johnson) %>% 
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
bay.df <- left_join(bay.df, events.sub, by = c("source", "station",
                                               "sampledate", "layer"))

## ------------------------------------------------------------------------
bay.df <- bay.df %>% 
  mutate(salzone_calc = case_when(
    salinity <= 0.5 ~ "f",
    salinity <= 5.0 ~ "o",
    salinity <= 18.0 ~ "m",
    salinity > 18.0 ~ "p",
    TRUE ~ as.character(NA)
  ))

## ------------------------------------------------------------------------
bay.sub <- bay.df %>% 
  mutate(salzone_disagree = if_else(salzone_calc != salzone_johnson, TRUE, FALSE))

## ---- fig.width=10-------------------------------------------------------
salzone.diff <- bay.sub %>% 
  select(station, sampledate, source, salzone_calc, salzone_johnson, salzone_disagree) %>% 
  unite(sal_group, salzone_calc, salzone_johnson) %>% 
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
# To be consistent with the data used to develop the PIBI, salinity zones specified by Jacqueline M. Johnson (`salzone_johnson`) were favored when salinity zones differed. If Jacqueline M. Johnson did not provide a salinity zone (`salzone_johnson`) for a sample, then the salinity zones calculated in this document were favored. If neither Jacqueline M. Johnson or this document reported a salinity zone, then the salinity zones reported in CEDR were used.
bay.df <- bay.df %>% 
  mutate(salzone = case_when(
    !is.na(salzone_johnson) ~ salzone_johnson,
    is.na(salzone_johnson) & !is.na(salzone_calc) ~ salzone_calc,
    is.na(salzone_johnson) & is.na(salzone_calc) ~ salzone_cedr,
    TRUE ~ "ERROR"
  )) %>% 
  mutate(unique_id = paste(unique_id, season, salzone, sep = "_"))

## ------------------------------------------------------------------------
bay.sub <- bay.df %>% 
  mutate(salzone_disagree = if_else(salzone_calc != salzone_cedr, TRUE, FALSE))

## ---- fig.width=10-------------------------------------------------------
salzone.diff <- bay.sub %>% 
  select(station, sampledate, source, salzone_calc, salzone_cedr, salzone_disagree) %>% 
  unite(sal_group, salzone_calc, salzone_cedr) %>% 
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

## ---- eval=FALSE, echo=FALSE---------------------------------------------
## bay.df <- bay.df %>%
##   mutate(salzone = case_when(
##     !is.na(salzone_calc) ~ salzone_calc,
##     is.na(salzone_calc) ~ salzone_cedr,
##     is.na(salzone_calc) & is.na(salzone_cedr) ~ as.character(NA),
##     TRUE ~ "ERROR"
##   )) %>%
##   mutate(unique_id = paste(unique_id, season, salzone, sep = "_"))

## ------------------------------------------------------------------------
points.df <- bay.df %>% 
  dplyr::select(unique_id, station, salzone) %>% 
  distinct() %>% 
  left_join(stations.df, by = c("station")) %>% 
  dplyr::select(unique_id, station, source, latitude, longitude, salzone) %>% 
  distinct() %>% 
  mutate(#longitude = if_else(is.na(salzone), longitude + 0.01, longitude),
         #latitude = if_else(is.na(salzone), latitude + 0.01, latitude),
         longitude = jitter(longitude, amount = 0.005),
         latitude = jitter(latitude, amount = 0.005),
         salzone = if_else(is.na(salzone), "missing", salzone), 
         salzone = factor(salzone, c("f", "o", "m", "p", "missing"))) %>% 
  arrange(desc(salzone))

factpal <- colorFactor(c("#440154FF", "#31688EFF",
                         "#35B779FF", "#FDE725FF",
                         "#808080"), points.df$salzone)

leaflet(points.df) %>% 
    addProviderTiles(providers$CartoDB.Positron,
                   options = leaflet::tileOptions(minZoom = 7, maxZoom = 18)) %>% 
  addCircleMarkers( ~longitude, ~latitude,
                    color = ~factpal(salzone),
                    stroke = FALSE,
                    fillOpacity = 0.5,
                    popup = paste("Unique ID:", points.df$unique_id, "<br/>",
                                  "Station:", points.df$station, "<br/>",
                                  "Source:", points.df$source, "<br/>",
                                  "Latitude:", points.df$latitude, "<br/>",
                                  "Longitude:", points.df$longitude, "<br/>",
                                  "Salinity Zone:", points.df$salzone)) %>% 
   leaflet::setMaxBounds(lng1 = -78, lat1 = 36, lng2 = -75, lat2 = 40.5) %>% 
  leaflet::setView(-76.4, lat = 38, zoom = 7) 

## ------------------------------------------------------------------------
# rm(env.df, env.wide, wq.df, bay.sub)

## ---- eval=FALSE, echo=FALSE---------------------------------------------
## jj.df <- readxl::read_excel(file.path(project.dir, "data/jackie_data/Data 2013_4plus-phyto-metrics.xlsx"),
##                                   sheet = "DATA_4+biometrics",
##                                   skip = 1) %>%
##   clean_up() %>%
##   select(station, sample_date, salinity, contains("salzone")) %>%
##   mutate(sample_date = as.Date(sample_date)) %>%
##   rename(date = sample_date,
##          salinity_johnson = salinity,
##          salzone_johnson = ibi_salzone#,
##          # salzone_jj = salzone
##          ) %>%
##   select(-salzone)
##   # mutate(salzone_test = case_when(
##   #   salinity_johnson <= 0.5 ~ "f",
##   #   salinity_johnson <= 5.0 ~ "o",
##   #   salinity_johnson <= 18.0 ~ "m",
##   #   salinity_johnson > 18.0 ~ "p",
##   #   TRUE ~ as.character(NA)
##   # )) %>%
##   # mutate(diff = if_else(salzone_johnson != salzone_test, TRUE, FALSE))
## 
## test.df <- bay.df %>%
##   select(station, sampledate, layer, salinity, salzone_cedr, salzone_calc) %>%
##   distinct() %>%
##   rename(date = sampledate) %>%
##   inner_join(jj.df, by = c("station", "date")) %>%
##   mutate(sal_diff = salinity - salinity_johnson,
##          salzone_diff = if_else(salzone_calc != salzone_johnson, TRUE, FALSE)) %>%
##   arrange(desc(salzone_diff), desc(abs(sal_diff))) %>%
##   select(station, date, layer, salzone_cedr, salzone_calc,
##          salzone_johnson, salzone_diff,
##          salinity, salinity_johnson, sal_diff)
## 
## data.table::fwrite(test.df, "data/salzones_8_21_18.csv")

