## ----echo=FALSE----------------------------------------------------------
knitr::opts_chunk$set(eval=evaluate, cache=cache.me)

## ------------------------------------------------------------------------
wq.df <- data.table::fread(file.path(project.dir, "data/water_quality/cedr_wq.csv"),
                            data.table = FALSE,
                           na.strings = c(""))

## ------------------------------------------------------------------------
wq.df <- wq.df %>% 
  mutate(sampledate = as.Date(sampledate))

## ------------------------------------------------------------------------
bay.salzone <- bay.df %>% 
  select(station, sampledate, salzone) %>% 
  rename(sampledate = sampledate) %>% 
  distinct()

## ------------------------------------------------------------------------
wq.df <- inner_join(wq.df, bay.salzone, by = c("station", "sampledate"))

## ------------------------------------------------------------------------
wq.df <- wq.df %>% 
  filter(is.na(problem)) %>% 
  unite(parameter, layer, parameter) %>% 
  filter(str_detect(parameter, "chla|pheo|doc"))

## ------------------------------------------------------------------------
stations.df <- wq.df %>% 
  dplyr::select(station, agency, source, latitude, longitude) %>% 
  distinct()

leaflet(stations.df) %>% 
  addProviderTiles(providers$CartoDB.Positron) %>%  
  addCircleMarkers( ~longitude, ~latitude,
                    stroke = FALSE,
                    fillOpacity = 0.5,
                    popup = paste("Station:", stations.df$station, "<br/>",
                                  "Agency:", stations.df$agency, "<br/>",
                                  "Source:", stations.df$source, "<br/>",
                                  "Latitude:", stations.df$latitude, "<br/>",
                                  "Longitude:", stations.df$longitude))

## ------------------------------------------------------------------------
wq.s_chla <- wq.df %>% 
  select(station, samplereplicatetype, sampledate, parameter, measurevalue) %>% 
  filter(parameter == "s_chla") %>% 
  distinct() %>% 
  select(-samplereplicatetype) %>% 
  group_by_at(vars(-measurevalue)) %>% 
  summarize(measurevalue = mean(measurevalue, is.na = TRUE)) %>% 
  ungroup()

## ------------------------------------------------------------------------
wq.sub.tf <- wq.df %>% 
  filter(salzone == "f") %>% 
  #filter(is.na(upperpycnocline)) %>% 
  #filter(startsWith(station, "f")) %>% 
  select(station, samplereplicatetype, sampledate, parameter, measurevalue) %>% 
  filter(grepl("chla|pheo|doc", parameter)) %>% 
  distinct() %>% 
  mutate(parameter = case_when(
    grepl("chla", parameter) ~ "chla",
    grepl("pheo", parameter) ~ "pheo",
    grepl("doc", parameter) ~ "doc",
    TRUE ~ "ERROR"
  )) %>% 
  select(-samplereplicatetype) %>% 
  group_by_at(vars(-measurevalue)) %>% 
  summarize(measurevalue = mean(measurevalue)) %>% 
  ungroup()

## ------------------------------------------------------------------------
wq.sub.pycno <- wq.df %>% 
  filter(salzone != "f",
    #!is.na(upperpycnocline),
         grepl("ap_|s_", parameter)) %>% 
  #filter(!startsWith(station, "f")) %>% 
  select(station, samplereplicatetype, sampledate, parameter, measurevalue) %>% 
  distinct() %>% 
  mutate(parameter = case_when(
    parameter %in% c("ap_chla", "s_chla") ~ "chla",
    parameter %in% c("ap_pheo", "s_pheo") ~ "pheo",
    parameter %in% c("ap_doc", "s_doc") ~ "doc",
    TRUE ~ "ERROR"
  )) %>% 
  select(-samplereplicatetype) %>% 
  group_by_at(vars(-measurevalue)) %>% 
  summarize(measurevalue = mean(measurevalue)) %>% 
  ungroup()

## ------------------------------------------------------------------------
wq.sub <- bind_rows(wq.s_chla, wq.sub.tf, wq.sub.pycno) %>% 
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
clusterExport(cl = cl, varlist = c("wq.sub", "bay.sub"))
clusterEvalQ(cl, c(library(dplyr))) %>% invisible()

## ------------------------------------------------------------------------
env.df <- parLapply(cl, 1:nrow(bay.sub), function(row.i) {

  sub.df <- slice(bay.sub, row.i)
  #----------------------------------------------------------------------------
  sub.env <- wq.sub %>% 
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
#rm(env.df, env.wide, wq.df, wq.sub, bay.sub)

