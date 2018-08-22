## ----child = 'prep_hierarchy.Rmd', eval=TRUE-----------------------------

## ----echo=FALSE----------------------------------------------------------
knitr::opts_chunk$set(eval=evaluate, cache=cache.me)

## ------------------------------------------------------------------------
col.class.vec <- c("samplenumber" = "character",
                   "tsn" = "character",
                   "speccode" = "character")

taxa.raw <- data.table::fread(file.path(project.dir, "data/phytoplankton/cedr_phyto_taxa.csv"),
                            data.table = FALSE,
                            colClasses = col.class.vec,
                            na.strings = "") %>% 
  mutate(sampledate = as.Date(sampledate))

## ------------------------------------------------------------------------
bay.df <- taxa.raw %>% 
  filter(layer %in% c("ap", "wc")) %>% 
  distinct() %>% 
  mutate(month = month(sampledate),
         season = case_when(
           month %in% c(3, 4, 5) ~ "spring",
           month %in% c(7, 8, 9) ~ "summer",
           TRUE ~ "remove"
         )) %>% 
  filter(season %in% c("spring", "summer"))

## ------------------------------------------------------------------------
bay.df <- bay.df %>% 
  group_by_at(vars(-reportingvalue)) %>% 
  summarize(reportingvalue = sum(reportingvalue)) %>% 
  ungroup()

## ------------------------------------------------------------------------
bay.df <- bay.df %>% 
  filter(grepl("ph", method),
         !latinname %in% c("micro-phytoflagellates",
                           "microflagellates",
                           #"green_cells",
                           #"blue_green_sphere",
                           "epiphytic_flagellates",
                           "hydrodictyon_reticulatum"))

## ------------------------------------------------------------------------
hier.wide <- data.table::fread(file.path(project.dir, "data/itis/itis_hierarchy.csv"),
                            data.table = FALSE,
                            na.strings = "") %>% 
  clean_up()

## ------------------------------------------------------------------------
bay.df <- left_join(bay.df, hier.wide, by = "org_tsn") %>% 
  mutate(unique_id = paste(station, layer, samplenumber, sampledate, sep = "_"))

## ------------------------------------------------------------------------
bay.df <- bay.df %>% 
  mutate(
    final_id = if_else(latinname == "trinacria_regina", "trinacria_regina",  final_id),
    final_id = case_when(
      latinname == "green_cells" ~ "green_cells",
      latinname == "blue_green_spheres" ~ "blue_green_spheres",
      TRUE ~ final_id
    ),
    kingdom = if_else(latinname == "trinacria_regina",
                      "chromista", kingdom),
    phylum = if_else(latinname == "trinacria_regina",
                     "bacillariophyta", phylum),
    subphylum = if_else(latinname == "trinacria_regina",
                        "bacillariophytina", subphylum),
    class = if_else(latinname == "trinacria_regina",
                    "mediophyceae", class),
    subclass = if_else(latinname == "trinacria_regina",
                       "chaetocerotophycidae", subclass),
    order = if_else(latinname == "trinacria_regina",
                    "hemiaulales", order),
    family = if_else(latinname == "trinacria_regina",
                     "hemiaulaceae", family),
    genus = case_when(latinname == "trinacria_regina" ~ "trinacria",
                      latinname == "didymocystis" ~ "didymocystis",
                      latinname == "Lauterborniella_elegantissima" ~ "lauterborniella",
                      TRUE ~ genus),
    species = case_when(latinname == "trinacria_regina" ~ "trinacria_regina",
                        latinname == "lauterborniella_elegantissima" ~ "lauterborniella_elegantissima",
                        TRUE ~ species)
  )

## ------------------------------------------------------------------------
rm(col.class.vec)

## ---- echo=FALSE---------------------------------------------------------
test <- bay.df %>% 
  filter(is.na(kingdom))


## ----child = 'prep_carbon.Rmd', eval=TRUE--------------------------------

## ----echo=FALSE----------------------------------------------------------
knitr::opts_chunk$set(eval=evaluate, cache=cache.me)

## ------------------------------------------------------------------------
carbon.df <- readxl::read_excel(file.path(project.dir, "data/carbon/carbon_list_2014.xlsx"),
                                sheet = "carbon_list_2014") %>% 
  clean_up() %>% 
  dplyr::select(-dplyr::contains("new"),
                -smyayda_carbon,
                -difference,
                -shape_change) %>% 
  dplyr::rename(carbon = old_carbon)

## ------------------------------------------------------------------------
carbon.df <- carbon.df %>% 
  mutate(size = case_when(
    size %in% "unidentified" ~ as.character(NA),
    str_detect(size, "species|sp.") ~ str_replace_all(size, "sp.#|species_#|species_|species", "sp#"),
    TRUE ~size
  ),
  latinname = if_else(latinname == "blue_green_trichome", "blue_green_sphere", latinname)) %>% 
  mutate(picoplankton = if_else(
    diameter <= 20 &
      height <= 20 &
      width <= 20 &
      width <= 20 &
      height_2 <= 20 &
      width_2 <= 20,
    TRUE,
    FALSE
  )) %>% 
  select(latinname, size, carbon) %>%
  distinct() %>% 
  bind_rows(data.frame(latinname = "asterionellopsis_glacialis", 
                       carbon = carbon.df[carbon.df$latinname == "asterionellopsis_kariana", "carbon"],
                       stringsAsFactors = FALSE))

## ------------------------------------------------------------------------
carbon.dups <- carbon.df %>% 
  group_by(latinname, size) %>% 
  mutate(count = n()) %>% 
  ungroup() %>% 
  filter(count > 1)

## ------------------------------------------------------------------------
carbon.df <- carbon.df %>% 
  filter(!(latinname == "chaetoceros_wighami" & is.na(carbon)),
         !(latinname == "biddulphia" & carbon == 7899.50),
         !(latinname == "gymnodinium" & carbon == 848)) # %>% 
#  mutate(size = case_when(
#    is.na(size) & latinname == "gymnodinium" & carbon == 848 ~ "msu",
#    is.na(size) & latinname == "gymnodinium" & carbon == 43.900 ~ "odu",
#    TRUE ~ size
#    ))
         

## ------------------------------------------------------------------------
bay.df <- bay.df %>% 
  mutate(size = case_when(
    size %in% c("not_applicable", "not_specified") ~ as.character(NA),
    size == "cell-_small" & latinname == "gymnodinium" ~ as.character(NA),
    str_detect(size, "species|sp.") ~ str_replace_all(size, "sp.#|species_#|species_|species", "sp#"),
    TRUE ~ size
  ))# %>% 
#  mutate(size = case_when(
#        is.na(size) & latinname == "gymnodinium" & str_detect(source, "msu") ~ "msu",
#    is.na(size) & latinname == "gymnodinium" & str_detect(source, "odu") ~ "odu",
#    TRUE ~ size
#  ))

## ------------------------------------------------------------------------
no.match.df <- anti_join(bay.df, carbon.df, by = c("latinname", "size")) %>% 
  select(latinname, size) %>% 
  distinct()

## ------------------------------------------------------------------------
partial.match.df <- lapply(1:nrow(no.match.df), function(row.i) {
  sub.df <- no.match.df %>% 
    slice(row.i)
  
  size.vec <- str_split(sub.df$size, "_") %>% unlist()
  
  getmode <- function(v) {
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
  }
  
  
  carbon.sub <- carbon.df %>% 
    filter(latinname == sub.df$latinname) %>% 
    mutate(row_max_matches = sapply(size.vec, function(size.i) grep(size.i, size)) %>% 
             unlist() %>% 
             getmode()) %>% 
    slice(unique(row_max_matches)) %>% 
    select(-row_max_matches, -carbon) %>% 
    rename(new_size = size) %>% 
    mutate(size = sub.df$size)
  
  return(carbon.sub)
}) %>% 
  bind_rows()

## ------------------------------------------------------------------------
bay.df <- left_join(bay.df, partial.match.df, by = c("latinname", "size")) %>% 
  mutate(reported_size = size,
         size = if_else(!is.na(new_size), new_size, size))

## ------------------------------------------------------------------------
bay.df <- left_join(bay.df, carbon.df, by = c("latinname", "size")) %>% 
  mutate(biomass = reportingvalue * carbon / 10 ^ 6)


## ----child = 'prep_events.Rmd', eval=TRUE--------------------------------

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



## ----child = 'appendix/prep_wq_favor_johnson_salzone.Rmd', eval=TRUE, echo=FALSE----

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


## ----child = 'metric_calc.Rmd', eval=TRUE, echo=FALSE--------------------

## ----echo=FALSE----------------------------------------------------------
knitr::opts_chunk$set(eval=evaluate, cache=cache.me)

## ------------------------------------------------------------------------
bay.taxa <- bay.df %>% 
  select(unique_id, sampledate, season,  salzone,
         surface_chla, chla, pheophytin, doc,
         reportingvalue, biomass, latinname, final_id,
         division, phylum, class, species)  %>% 
  filter(!is.na(salzone)) %>% 
  distinct() %>% 
  mutate(biomass = if_else(is.na(biomass), 0, biomass))

## ------------------------------------------------------------------------
metrics.df <- bay.taxa %>%
  select(unique_id, salzone, surface_chla, chla, doc, pheophytin) %>%
  distinct() %>%
  mutate(
    total_phyto_biomass = taxa_abund(bay.taxa,
                                     unique_id,
                                     biomass,
                                     species),
    total_phyto_biomass_chla_ratio = total_phyto_biomass / chla,
    pct_cryptophyte = taxa_pct(long.df = bay.taxa,
                               unique.id.col = unique_id,
                               count.col = biomass,
                               taxon.col = division,
                               taxon = "cryptophycophyta"),
    cyanophyte_biomass = taxa_abund(bay.taxa,
                                    unique_id,
                                    biomass,
                                    phylum,
                                    "cyanobacteria"),
    diatom_biomass = taxa_abund(bay.taxa,
                                unique_id,
                                biomass,
                                class,
                                "bacillariophyceae"),
    dinoflagellate_biomass = taxa_abund(bay.taxa,
                                        unique_id,
                                        biomass,
                                        division,
                                        "pyrrophycophyta"),
    scrippsiella_precaria_biomass = taxa_abund(bay.taxa,
                                               unique_id,
                                               biomass,
                                               latinname,
                                               "scrippsiella_precaria"
    ),
    dinoflagellate_biomass = dinoflagellate_biomass - scrippsiella_precaria_biomass,
    microcystis_aeruginosa_abundance = taxa_abund(bay.taxa,
                                                  unique_id,
                                                  reportingvalue,
                                                  species,
                                                  "microcystis_aeruginosa"
    ),
    #picoplankton_abundance = taxa_abund(bay.taxa, unique_id, reportingvalue, picoplankton, TRUE),
    prorocentrum_minimum_abundance = taxa_abund(bay.taxa,
                                                unique_id,
                                                reportingvalue,
                                                species,
                                                "prorocentrum_minimum"
    )
  ) %>%
  select(-scrippsiella_precaria_biomass)

## ------------------------------------------------------------------------
metrics.long <- metrics.df %>% 
  gather(metric, value, surface_chla:prorocentrum_minimum_abundance)


## ----child = 'scores_ratings.Rmd', eval=TRUE, echo=FALSE-----------------

## ----echo=FALSE----------------------------------------------------------
knitr::opts_chunk$set(eval=evaluate, cache=cache.me)

## ------------------------------------------------------------------------
score_spring_f <- function(metrics.long) {
  metric.vec <- c("total_phyto_biomass_chla_ratio",
                  "surface_chla",
                  "cyanophyte_biomass",
                  "doc",
                  "pheophytin",
                  "total_phyto_biomass")
  #----------------------------------------------------------------------------
  metrics.sub <- metrics.long %>% 
    filter(season == "spring", 
           salzone == "f",
           metric %in% metric.vec)
  #----------------------------------------------------------------------------
  if (any(!metric.vec %in% unique(metrics.sub$metric))) {
    stop(paste("score_spring_f requires the following metric(s):",
               paste(metric.vec[!metric.vec %in% unique(metrics.sub$metric)], collapse = ", ")))
  }
  #----------------------------------------------------------------------------
  final.df <- metrics.sub %>% 
    mutate(score = case_when(
      # Jackie's code 39.28 and 41.72
      # Lacouture (2006) 39.3 and 41.7
      metric == "total_phyto_biomass_chla_ratio" & value < 39.28 ~ 1,
      metric == "total_phyto_biomass_chla_ratio" & value >= 39.28 & value <= 41.72 ~ 3,
      metric == "total_phyto_biomass_chla_ratio" & value > 41.72 ~ 5,
      metric == "surface_chla" & (value < 3.42 | value > 14.45) ~ 1,
      metric == "surface_chla" & ((value >= 3.42 & value <= 4.37) |(value >= 13.98 & value <= 14.45)) ~ 3,
      metric == "surface_chla" & value > 4.37 & value < 13.98 ~ 5,
      # Jackie's code 23.02
      # Lacouture (2006) 23
      metric == "cyanophyte_biomass" & value > 23.02 ~ 1,
      metric == "cyanophyte_biomass" & value <= 23.02 ~ as.numeric(NA),
      metric == "doc" & value > 2.40 ~ 1,
      metric == "doc" & value >= 2.19 & value <= 2.40 ~ 3,
      metric == "doc" & value < 2.19 ~ 5,
      metric == "pheophytin" & value > 2.50 ~ 1,
      metric == "pheophytin" & value >= 1.55 & value <= 2.50 ~ 3,
      metric == "pheophytin" & value < 1.55 ~ 5,
      # Jackie's code 172.99, 583.9, and 828.5
      # Lacouture (2006) 173, 584, and 829
      metric == "total_phyto_biomass"  & (value <= 172.99 | value > 828.5) ~ 1,
      metric == "total_phyto_biomass"  & value >= 583.9 & value <= 828.5 ~ 3,
      metric == "total_phyto_biomass"  & value > 172.99 & value < 583.9 ~ 5,
      TRUE ~ as.numeric(NA)))
  return(final.df)
}


## ------------------------------------------------------------------------
score_spring_o <- function(metrics.long) {
  metric.vec <- c("total_phyto_biomass_chla_ratio",
                  "surface_chla",
                  "doc",
                  "pheophytin",
                  "total_phyto_biomass")
  #----------------------------------------------------------------------------
  metrics.sub <- metrics.long %>% 
    filter(season == "spring", 
           salzone == "o",
           metric %in% metric.vec)
  #----------------------------------------------------------------------------
  if (any(!metric.vec %in% unique(metrics.sub$metric))) {
    stop(paste("score_spring_o requires the following metric(s):",
               paste(metric.vec[!metric.vec %in% unique(metrics.sub$metric)], collapse = ", ")))
  }
  #----------------------------------------------------------------------------
  final.df <- metrics.sub %>%
    mutate(score = case_when(
      metric == "total_phyto_biomass_chla_ratio" & (value < 18.8 | value > 48.6) ~ 1,
      metric == "total_phyto_biomass_chla_ratio" & value >= 18.8 & value <= 21.2 ~ 3,
      metric == "total_phyto_biomass_chla_ratio" & value > 21.2 & value < 48.6 ~ 5,
      metric == "surface_chla" & (value < 6.77 | value > 33.64) ~ 1,
      metric == "surface_chla" & ((value >= 20.93 & value <= 33.64) |(value >= 6.77 & value <= 8.82)) ~ 3,
      metric == "surface_chla" & value > 8.82 & value < 20.93 ~ 5,
      metric == "doc" & value > 3.27 ~ 1,
      metric == "doc" & value >= 2.69 & value <= 3.27 ~ 3,
      metric == "doc" & value < 2.69 ~ 5,
      metric == "pheophytin" & value > 2.68 ~ 1,
      metric == "pheophytin" & value >= 2.23 & value <= 2.68 ~ 3,
      metric == "pheophytin" & value < 2.23 ~ 5,
      # Jackie's code 133.67, 426.31, 131.37, and 685.81
      # Lacouture (2006) 134, 426, 131, and 686
      metric == "total_phyto_biomass"  & value > 133.67 & value < 426.31 ~ 5,
      metric == "total_phyto_biomass"  & ((value >= 131.37 & value <= 133.67) | (value >= 426.31 & value < 685.81)) ~ 3,
      metric == "total_phyto_biomass"  & any(value < 131.37 | value >= 685.81) ~ 1,
      TRUE ~ as.numeric(NA)))
  return(final.df)
}


## ------------------------------------------------------------------------
score_spring_m <- function(metrics.long) {
  metric.vec <- c("total_phyto_biomass_chla_ratio",
                  "surface_chla",
                  "diatom_biomass",
                  "dinoflagellate_biomass",
                  "doc",
                  "pheophytin",
                  "prorocentrum_minimum_abundance",
                  "total_phyto_biomass")
  #----------------------------------------------------------------------------
  metrics.sub <- metrics.long %>% 
    filter(season == "spring", 
           salzone == "m",
           metric %in% metric.vec)
  #----------------------------------------------------------------------------
  if (any(!metric.vec %in% unique(metrics.sub$metric))) {
    stop(paste("score_spring_m requires the following metric(s):",
               paste(metric.vec[!metric.vec %in% unique(metrics.sub$metric)],
                     collapse = ", ")))
  }
  #----------------------------------------------------------------------------
  final.df <- metrics.sub %>% 
    mutate(score = case_when(
      # Jackie's code 69.52 and 45.04
      # Lacouture (2006) 69.5 and 45
      metric == "total_phyto_biomass_chla_ratio" & value < 45.04 ~ 1,
      metric == "total_phyto_biomass_chla_ratio" & value >= 45.04 & value <= 69.52 ~ 3,
      metric == "total_phyto_biomass_chla_ratio" & value > 69.52 ~ 5,
      metric == "surface_chla" & (value < 2.60 | value > 8.00) ~ 1,
      metric == "surface_chla" & ((value >= 2.60 & value <= 2.90) |(value >= 6.17 & value <= 8.00)) ~ 3,
      metric == "surface_chla" & value > 2.90 & value < 6.17 ~ 5,
      # Jackie's code 275.4
      # Lacouture (2006) 275
      metric == "diatom_biomass" & (value < 149 | value >= 2513) ~ 1,
      metric == "diatom_biomass" & value >= 149 & value <= 275.4 ~ 3,
      metric == "diatom_biomass" & value > 275.4 & value < 2513 ~ 5,
      # Jackie's code 28.3, 156.9, and 268.2
      # Lacouture (2006) 157 and 268
      metric == "dinoflagellate_biomass" & (value < 28.3 | value > 268.2) ~ 1,
      metric == "dinoflagellate_biomass" & value >= 156.9 & value <= 268.2 ~ 3,
      metric == "dinoflagellate_biomass" & value > 28.3 & value < 156.9 ~ 5,
      metric == "doc" & value > 3.17 ~ 1,
      metric == "doc" & value >= 2.84 & value <= 3.17 ~ 3,
      metric == "doc" & value < 2.84 ~ 5,
      metric == "pheophytin" & value > 1.03 ~ 1,
      metric == "pheophytin" & value >= 1.00 & value <= 1.03 ~ 3,
      metric == "pheophytin" & value < 1.00 ~ 5,
      metric == "prorocentrum_minimum_abundance" & value > 1477600 ~ 1,
      metric == "prorocentrum_minimum_abundance" & value <= 1477600 ~ as.numeric(NA),
      metric == "total_phyto_biomass"  & value > 1150 ~ 1,
      metric == "total_phyto_biomass"  & value <= 1150 ~ as.numeric(NA),
      TRUE ~ as.numeric(NA)))
  return(final.df)
}


## ------------------------------------------------------------------------
score_spring_p <- function(metrics.long) {
  metric.vec <- c("total_phyto_biomass_chla_ratio",
                  "surface_chla",
                  "pct_cryptophyte",
                  "doc",
                  "pheophytin",
                  "prorocentrum_minimum_abundance",
                  "total_phyto_biomass")
  #----------------------------------------------------------------------------
  metrics.sub <- metrics.long %>% 
    filter(season == "spring", 
           salzone == "p",
           metric %in% metric.vec)
  #----------------------------------------------------------------------------
  if (any(!metric.vec %in% unique(metrics.sub$metric))) {
    stop(paste("score_spring_p requires the following metric(s):",
               paste(metric.vec[!metric.vec %in% unique(metrics.sub$metric)], collapse = ", ")))
  }
  #----------------------------------------------------------------------------
  final.df <- metrics.sub %>% 
    mutate(score = case_when(
      metric == "total_phyto_biomass_chla_ratio" & value < 71.0 ~ 1,
      metric == "total_phyto_biomass_chla_ratio" & value >= 71.0 & value <= 107.5 ~ 3,
      metric == "total_phyto_biomass_chla_ratio" & value > 107.5 ~ 5,
      metric == "surface_chla" & value > 4.00 ~ 1,
      metric == "surface_chla" & value >= 2.80 & value <= 4.00 ~ 3,
      metric == "surface_chla" & value < 2.80 ~ 5,
      # Jackie's code 4.93 and 7.06
      # Lacouture (2006) 4.9 and 7.1
      metric == "pct_cryptophyte" & value > 7.06 ~ 1,
      metric == "pct_cryptophyte" & value >= 4.93 & value <= 7.06 ~ 3,
      metric == "pct_cryptophyte" & value < 4.93 ~ 5,
      metric == "doc" & value > 2.61 ~ 1,
      metric == "doc" & value >= 2.50 & value <= 2.61 ~ 3,
      metric == "doc" & value < 2.50 ~ 5,
      metric == "pheophytin" & value > 0.90 ~ 1,
      metric == "pheophytin" & value >= 0.55 & value <= 0.90 ~ 3,
      metric == "pheophytin" & value < 0.55 ~ 5,
      metric == "prorocentrum_minimum_abundance" & value > 7488 ~ 1,
      metric == "prorocentrum_minimum_abundance" & value >= 672 & value <= 7488 ~ 3,
      metric == "prorocentrum_minimum_abundance" & value < 672 ~ 5,
      # Jackie's code 1061.7
      # Lacouture (2006) 1062
      metric == "total_phyto_biomass"  & value > 1061.7 ~ 1,
      metric == "total_phyto_biomass"  & value <= 1061.7 ~ as.numeric(NA),
      TRUE ~ as.numeric(NA)))
  return(final.df)
}


## ------------------------------------------------------------------------
score_summer_f <- function(metrics.long) {
  metric.vec <- c("surface_chla",
                  "cyanophyte_biomass",
                  "diatom_biomass",
                  "doc", 
                  "microcystis_aeruginosa_abundance",
                  "pheophytin", 
                  "total_phyto_biomass")
  #----------------------------------------------------------------------------
  metrics.sub <- metrics.long %>% 
    filter(season == "summer", 
           salzone == "f",
           metric %in% metric.vec)
  #----------------------------------------------------------------------------
  if (any(!metric.vec %in% unique(metrics.sub$metric))) {
    stop(paste("score_summer_f requires the following metric(s):",
               paste(metric.vec[!metric.vec %in% unique(metrics.sub$metric)], collapse = ", ")))
  }
  #----------------------------------------------------------------------------
  final.df <- metrics.sub %>% 
    mutate(score = case_when(
      metric == "surface_chla" & value > 12.30 ~ 1,
      metric == "surface_chla" & ((value >= 12.00 & value <= 12.30) | value <= 5.4) ~ 3,
      metric == "surface_chla" & value > 5.40 & value < 12.00 ~ 5,
      # Jackie's code 38.87 and 67.4
      # Lacouture (2006) 39 and 67
      metric == "cyanophyte_biomass" & value > 67.4 ~ 1,
      metric == "cyanophyte_biomass" & value >= 38.87 & value <= 67.4 ~ 3,
      metric == "cyanophyte_biomass" & value < 38.87 ~ 5,
      # Jackie's code 122.1 and 192.6
      # Lacouture (2006) 122 and 193
      metric == "diatom_biomass" & value > 192.6 ~ 1,
      metric == "diatom_biomass" & value >= 122.1 & value <= 192.6 ~ 3,
      metric == "diatom_biomass" & value < 122.1 ~ 5,
      metric == "doc" & value > 3.18 ~ 1,
      metric == "doc" & value >= 2.67 & value <= 3.18 ~ 3,
      metric == "doc" & value < 2.67 ~ 5,
      metric == "microcystis_aeruginosa_abundance" & value > 262507 ~ 1,
      metric == "microcystis_aeruginosa_abundance" & value <= 262507 ~ as.numeric(NA),
      metric == "pheophytin" & value > 4.30 ~ 1,
      metric == "pheophytin" & value >= 2.40 & value <= 4.30 ~ 3,
      metric == "pheophytin" & value < 2.40 ~ 5,
      # Jackie's code 231.3 and 555.7
      # Lacouture (2006) 232 and 551
      metric == "total_phyto_biomass"  & value > 555.7 ~ 1,
      metric == "total_phyto_biomass"  & value < 231.3 ~ 3,
      metric == "total_phyto_biomass"  & value >= 231.3 & value <= 555.7 ~ 5,
      TRUE ~ as.numeric(NA)))
  return(final.df)
}

## ------------------------------------------------------------------------
score_summer_o <- function(metrics.long) {
  metric.vec <- c("surface_chla",
                  "cyanophyte_biomass",
                  "diatom_biomass",
                  "doc",
                  "pheophytin")
  #----------------------------------------------------------------------------
  metrics.sub <- metrics.long %>% 
    filter(season == "summer", 
           salzone == "o",
           metric %in% metric.vec)
  #----------------------------------------------------------------------------
  if (any(!metric.vec %in% unique(metrics.sub$metric))) {
    stop(paste("score_summer_o requires the following metric(s):",
               paste(metric.vec[!metric.vec %in% unique(metrics.sub$metric)], collapse = ", ")))
  }
  #----------------------------------------------------------------------------
  final.df <- metrics.sub %>% 
    mutate(score = case_when(
      metric == "surface_chla" & value >= 9.47 ~ 1,
      metric == "surface_chla" & value <= 4.20 ~ 3,
      metric == "surface_chla" & value > 4.20 & value < 9.47 ~ 5,
      # Jackie's code 1.79 and 26.55
      # Lacouture (2006) 1.8 and 27
      metric == "cyanophyte_biomass" & value >= 26.55 ~ 1,
      metric == "cyanophyte_biomass" & value < 1.79 ~ 3,
      metric == "cyanophyte_biomass" & value >= 1.79 & value < 26.55 ~ 5,
      # Jackie's code 44.14 and 126.59
      # Lacouture (2006) 44 and 127
      metric == "diatom_biomass" & value >= 126.59 ~ 1,
      metric == "diatom_biomass" & value <= 44.14 ~ 3,
      metric == "diatom_biomass" & value > 44.14 & value < 126.59 ~ 5,
      metric == "doc" & value > 4.00 ~ 1,
      metric == "doc" & value >= 3.15 & value <= 4.00 ~ 3,
      metric == "doc" & value < 3.15 ~ 5,
      metric == "pheophytin" & value > 2.81 ~ 1,
      metric == "pheophytin" & value >= 1.58 & value <= 2.81 ~ 3,
      metric == "pheophytin" & value < 1.58 ~ 5,
      TRUE ~ as.numeric(NA)))
  return(final.df)
}

## ------------------------------------------------------------------------
score_summer_m <- function(metrics.long, pico.warning) {
  metric.vec <- c("total_phyto_biomass_chla_ratio",
                  "surface_chla",
                  "dinoflagellate_biomass", 
                  "doc",
                  "pheophytin",
                  "total_phyto_biomass")
  pico.vec <- "picoplankton_abundance"
  #----------------------------------------------------------------------------
  metrics.sub <- metrics.long %>% 
    filter(season == "summer", 
           salzone == "m",
           metric %in% c(metric.vec, pico.vec))
  #----------------------------------------------------------------------------
  if (any(!metric.vec %in% unique(metrics.sub$metric))) {
    stop(paste("score_summer_m requires the following metric(s):",
               paste(metric.vec[!metric.vec %in% unique(metrics.sub$metric)], collapse = ", ")))
  }
  
  if (pico.warning == TRUE && any(!pico.vec %in% unique(metrics.sub$metric))) {
    warning("Warning: picoplankton_abundance missing from score_summer_m")
  }
  #----------------------------------------------------------------------------
  final.df <- metrics.sub %>% 
    mutate(score = case_when(
      metric == "total_phyto_biomass_chla_ratio" & value < 32.2 ~ 1,
      metric == "total_phyto_biomass_chla_ratio" & value >= 32.2 & value <= 36.9 ~ 3,
      metric == "total_phyto_biomass_chla_ratio" & value > 36.9 ~ 5,
      metric == "surface_chla" & value >= 9.74 ~ 1,
      metric == "surface_chla" & ((value >= 7.70 & value < 9.74) | value <= 4.00) ~ 3,
      metric == "surface_chla" & value > 4.00 & value < 7.70 ~ 5,
      # Jackie's code 200.92, 31.22, 55.98
      # Lacouture (2006) 201, 31, 56
      metric == "dinoflagellate_biomass" & (value <= 31.22 | value > 200.92) ~ 1,
      metric == "dinoflagellate_biomass" & value > 31.22 & value <= 55.98 ~ 3, 
      metric == "dinoflagellate_biomass" & value > 55.98 & value < 200.92 ~ 5,
      metric == "doc" & value > 3.35 ~ 1,
      metric == "doc" & value >= 2.99 & value <= 3.35 ~ 3,
      metric == "doc" & value < 2.99 ~ 5,
      metric == "pheophytin" & value > 1.60 ~ 1,
      metric == "pheophytin" & value >= 1.23 & value <= 1.60 ~ 3,
      metric == "pheophytin" & value < 1.23 ~ 5,
      # Jackie's code 598720000
      # Lacouture (2006) 598700000
      metric == "picoplankton_abundance" & value < 352000000 ~ 1,
      metric == "picoplankton_abundance" & value >= 352000000 & value <= 598720000 ~ 3,
      metric == "picoplankton_abundance" & value > 598720000 ~ 5,
      metric == "total_phyto_biomass"  & value > 660 ~ 1,
      metric == "total_phyto_biomass"  & value <= 660 ~ as.numeric(NA),
      TRUE ~ as.numeric(NA)))
  return(final.df)
}

## ------------------------------------------------------------------------
score_summer_p <- function(metrics.long, pico.warning) {
  
  metric.vec <- c("total_phyto_biomass_chla_ratio",
                  "surface_chla",
                  "pct_cryptophyte",
                  "diatom_biomass",
                  "dinoflagellate_biomass",
                  "doc",
                  "pheophytin",
                  "total_phyto_biomass")
  pico.vec <- "picoplankton_abundance"
  #----------------------------------------------------------------------------
  metrics.sub <- metrics.long %>% 
    filter(season == "summer", 
           salzone == "p",
           metric %in% c(metric.vec, pico.vec))
  #----------------------------------------------------------------------------
  if (any(!metric.vec %in% unique(metrics.sub$metric))) {
    stop(paste("score_summer_p requires the following metric(s):",
               paste(metric.vec[!metric.vec %in% unique(metrics.sub$metric)], collapse = ", ")))
  }
  
  if (pico.warning == TRUE && any(!pico.vec %in% unique(metrics.sub$metric))) {
    warning("Warning: picoplankton_abundance missing from score_summer_p")
  }
  #----------------------------------------------------------------------------
  final.df <- metrics.sub %>% 
    mutate(score = case_when(
      metric == "total_phyto_biomass_chla_ratio" & value < 37.7 ~ 1,
      metric == "total_phyto_biomass_chla_ratio" & value >= 37.7 & value <= 74.5 ~ 3,
      metric == "total_phyto_biomass_chla_ratio" & value > 74.5 ~ 5,
      metric == "surface_chla" & value > 5.33 ~ 1,
      metric == "surface_chla" & value >= 4.52 & value <= 5.33 ~ 3,
      metric == "surface_chla" & value < 4.52 ~ 5,
      metric == "pct_cryptophyte" & value > 6.5 ~ 1,
      metric == "pct_cryptophyte" & value >= 3.9 & value <= 6.5 ~ 3,
      metric == "pct_cryptophyte" & value < 3.9 ~ 5,
      metric == "diatom_biomass" & (value >= 799 | value < 137) ~ 1,
      metric == "diatom_biomass" & value >= 137 & value <= 181 ~ 3,
      metric == "diatom_biomass" & value > 181 & value < 799 ~ 5,
      metric == "dinoflagellate_biomass" & (value < 23 | value >= 544) ~ 1,
      metric == "dinoflagellate_biomass" & value >= 23 & value <= 37 ~ 3, 
      metric == "dinoflagellate_biomass" & value > 37 & value < 554 ~ 5,
      metric == "doc" & value > 2.80 ~ 1,
      metric == "doc" & value >= 2.58 & value <= 2.80 ~ 3,
      metric == "doc" & value < 2.58 ~ 5,
      metric == "pheophytin" & value > 1.50 ~ 1,
      metric == "pheophytin" & value >= 0.93 & value <= 1.50 ~ 3,
      metric == "pheophytin" & value < 0.93 ~ 5,
      metric == "picoplankton_abundance" & value < 208600000 ~ 1,
      metric == "picoplankton_abundance" & value >= 208600000 & value <= 269500000 ~ 3,
      metric == "picoplankton_abundance" & value > 269500000 ~ 5,
      # In Jackie's code 718
      # Lacouture (2006) 711
      metric == "total_phyto_biomass"  & (value < 181 | value > 831) ~ 1,
      metric == "total_phyto_biomass"  & ((value >= 181 & value <= 207) | (value >= 718 & value <= 831)) ~ 3,
      metric == "total_phyto_biomass"  & value > 207 & value < 718 ~ 5,
      TRUE ~ as.numeric(NA)))
  return(final.df)
}

## ------------------------------------------------------------------------
score_phyto <- function(metrics.long, pico.warning) {
  #----------------------------------------------------------------------------
  spring.f <- score_spring_f(metrics.long)
  spring.o <- score_spring_o(metrics.long)
  spring.m <- score_spring_m(metrics.long)
  spring.p <- score_spring_p(metrics.long)
  #----------------------------------------------------------------------------
  summer.f <- score_summer_f(metrics.long)
  summer.o <- score_summer_o(metrics.long)
  summer.m <- score_summer_m(metrics.long, pico.warning)
  summer.p <- score_summer_p(metrics.long, pico.warning)
  #----------------------------------------------------------------------------
  final.df <- bind_rows(spring.f, spring.o, spring.m, spring.p,
                        summer.f, summer.o, summer.m, summer.p) %>% 
    select(-layer)
  #----------------------------------------------------------------------------
  return(final.df)
}

## ------------------------------------------------------------------------
scores.df <- metrics.long %>% 
  separate(unique_id, c("station", "layer", "samplenumber",
                        "sampledate", "season", "salzone"), sep = "_",
           remove = FALSE) %>% 
  rename(date = sampledate) %>% 
  score_phyto(pico.warning = FALSE) %>% 
  select(-samplenumber)

## ------------------------------------------------------------------------
scores.df <- scores.df %>% 
  group_by(unique_id) %>% 
  mutate(ibi_score = mean(score, na.rm = TRUE),
         metric_count = n(),
         present_count = sum(!is.na(score)),
         na_count = sum(is.na(score))) %>% 
  ungroup() %>% 
  filter(present_count >= 4)

## ------------------------------------------------------------------------
rate_phyto <- function(scores.df) {
  final.df <- scores.df %>% 
    mutate(
      ibi_score = round(ibi_score, 2),
      rating = case_when(
        ibi_score < 2 ~ "poor",
        ibi_score >= 2 & ibi_score < 2.67 ~ "fair_poor",
        ibi_score >= 2.67 & ibi_score < 3.33 ~ "fair",
        ibi_score >= 3.33 & ibi_score < 4 ~ "fair_good",
        ibi_score >= 4 ~ "good",
        TRUE ~ "ERROR"
      ))
  return(final.df)
}

## ------------------------------------------------------------------------
ratings.df <- rate_phyto(scores.df) %>% 
  mutate(date = as.Date(date))


## ----child = 'map_ratings.Rmd', eval=TRUE, echo=FALSE--------------------

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
    addProviderTiles(providers$CartoDB.Positron,
                   options = leaflet::tileOptions(minZoom = 7, maxZoom = 18)) %>% 
  addCircleMarkers( ~longitude, ~latitude,
                    color = ~factpal(rating),
                    stroke = FALSE,
                    fillOpacity = 0.5,
                    popup = paste("Station:", stations.df$station, "<br/>",
                                  "Agency:", stations.df$agency, "<br/>",
                                  "Source:", stations.df$datasource, "<br/>",
                                  "Latitude:", stations.df$latitude, "<br/>",
                                  "Longitude:", stations.df$longitude)) %>% 
  leaflet::setMaxBounds(lng1 = -78, lat1 = 36, lng2 = -75, lat2 = 40.5) %>% 
  leaflet::setView(-76.4, lat = 38, zoom = 7) 


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


## ----child = 'appendix/summary.Rmd', eval=TRUE---------------------------



## ----child = 'appendix/validation_old_values.Rmd', eval=TRUE-------------

## ----echo=FALSE----------------------------------------------------------
knitr::opts_chunk$set(eval=evaluate, cache=cache.me)

## ------------------------------------------------------------------------
old.org.df <- read_excel(file.path(project.dir, "data/jackie_data/Data 2013_4plus-phyto-metrics.xlsx"),
                         sheet = "DATA_4+biometrics",
                         skip = 1) %>% 
  clean_up()

## ------------------------------------------------------------------------
metrics.vec <- c("chl_surf", "biomass_chl_ratio", "cyano_biomass", "doc",
                 "pheo", "tot_biomass", "diatom_biomass", "dino_biomass",
                 "prorocentrum_min_abund", "microcystis_aer_abund",
                 "crypto_bio_pct")

scores.vec <- paste0(metrics.vec, "__1")

## ------------------------------------------------------------------------
old.sub <- old.org.df %>% 
  mutate(old_rating = str_replace_all(pibi_rank, "-", "_")) %>% 
  rename(old_org_ibi_score = pibi_score) %>% 
  select(station, sample_date, season, ibi_layer, ibi_salzone,
         metrics.vec, scores.vec, old_rating, old_org_ibi_score
         ) 

## ------------------------------------------------------------------------
old.metrics <- old.sub %>%
  select(-one_of(scores.vec)) %>% 
  gather(metric, old_metric_value, -station, -sample_date,
         -season, -ibi_layer, -ibi_salzone,
         -old_rating, -old_org_ibi_score)

## ------------------------------------------------------------------------
old.score <- old.sub %>% 
  select(-one_of(metrics.vec)) %>% 
  gather(metric, old_score_value, -station, -sample_date,
         -season, -ibi_layer, -ibi_salzone, -old_rating, -old_org_ibi_score
         ) %>% 
  mutate(metric = str_replace(metric, "__1", "")) %>% 
  group_by(station, sample_date, season, ibi_layer, ibi_salzone) %>% 
  mutate(old_ibi_score = mean(old_score_value, na.rm = TRUE)) %>% 
  ungroup()

## ------------------------------------------------------------------------
old.df <- full_join(old.metrics, old.score,
                    by = c("station", "sample_date", "season",
                           "ibi_layer", "ibi_salzone", "metric",
                           "old_rating", "old_org_ibi_score"))

## ------------------------------------------------------------------------
old.df <- left_join(old.df, old.org.df,
                    by = c("station", "sample_date", "season", "ibi_layer", "ibi_salzone")) %>% 
  select(station, sample_date, season, ibi_layer, ibi_salzone, metric,
         old_metric_value, old_score_value,
         old_org_ibi_score,
         old_ibi_score, old_rating
         ) %>% 
  rename(layer = ibi_layer,
         salzone = ibi_salzone,
         date = sample_date) %>% 
  mutate(date = as.Date(date),
         metric = case_when(
           metric == "chl_surf" ~ "surface_chla",
           metric == "biomass_chl_ratio" ~ "total_phyto_biomass_chla_ratio",
           metric == "cyano_biomass" ~ "cyanophyte_biomass",
           metric == "pheo" ~ "pheophytin",
           metric == "tot_biomass" ~ "total_phyto_biomass",
           metric == "dino_biomass" ~ "dinoflagellate_biomass",
           metric == "prorocentrum_min_abund" ~ "prorocentrum_minimum_abundance",
           metric == "microcystis_aer_abund" ~ "microcystis_aeruginosa_abundance",
           metric == "crypto_bio_pct" ~ "pct_cryptophyte",
           TRUE ~ metric
         ))

## ------------------------------------------------------------------------
old.rescored <- old.df %>% 
  rename(value = old_metric_value) %>% 
  unite(unique_id, station, layer, date, season, salzone, remove = FALSE) %>% 
  score_phyto(pico.warning = FALSE)

## ------------------------------------------------------------------------
old.rescored <- old.rescored %>% 
  group_by(unique_id) %>% 
  mutate(ibi_score = mean(score, na.rm = TRUE),
         old_ibi_score = mean(old_score_value, na.rm = TRUE)) %>% 
  ungroup() 

## ------------------------------------------------------------------------
old.rescored <- rate_phyto(old.rescored)


## ----child = 'appendix/validation_scoring_disagreement.Rmd', eval=TRUE----

## ----echo=FALSE----------------------------------------------------------
knitr::opts_chunk$set(eval=evaluate, cache=cache.me, dev = c('png', 'postscript'))

## ------------------------------------------------------------------------
disagree.df <- old.rescored %>% 
  mutate_if(is.numeric, round, digits = 2) %>% 
  mutate(ibi_diff = abs(old_ibi_score - ibi_score)) %>% 
  arrange(desc(ibi_diff)) %>% 
  select(unique_id, metric, value, old_score_value, score, 
         old_ibi_score, ibi_score, ibi_diff, 
         old_org_ibi_score, old_rating,
         rating) %>% 
  separate(unique_id, c("station", "layer", "date", "season", "salzone"), sep = "_", remove = FALSE) %>% 
  unite(index, c("season", "salzone")) %>% 
  select(-station, -layer, -date) %>% 
  mutate(metric = factor(metric),
         index = factor(index))

## ------------------------------------------------------------------------
disagree.ibi <- disagree.df %>%  
  filter(ibi_diff > 0) %>% 
  select(unique_id, index, old_ibi_score, ibi_score, ibi_diff) %>% 
  mutate(disagree_bin = case_when(
    ibi_diff > 0 & ibi_diff <= 0.5 ~ "0 < IBI Score <= 0.5",
    ibi_diff > 0.5 & ibi_diff <= 1 ~ "0.5 < IBI Score <= 1.0",
    ibi_diff > 1 & ibi_diff <= 1.5 ~ "1.0 < IBI Score <= 1.5",
    ibi_diff > 1.5 & ibi_diff <= 2 ~ "1.5 < IBI Score <= 2.0",
    ibi_diff > 2 & ibi_diff <= 2.5 ~ "2.0 < IBI Score <= 2.5",
    ibi_diff > 2.5 & ibi_diff <= 3 ~ "2.5 < IBI Score <= 3.0",
    ibi_diff > 3 & ibi_diff <= 3.5 ~ "3.0 < IBI Score <= 3.5",
    ibi_diff > 3.5 & ibi_diff <= 4 ~ "3.5 < IBI Score <= 4.0",
    TRUE ~ "ERROR"
  )) %>% 
  distinct() 

## ------------------------------------------------------------------------
if (nrow(disagree.ibi) > 0) {
  disagree.ibi %>%
    rename(Index = "index") %>% 
    ggplot(aes(Index, fill = Index)) +
    geom_bar() +
    facet_wrap(~ disagree_bin, scales = "free_x") +
    coord_flip() +
    ylab("Count")
}

## ------------------------------------------------------------------------
disagree.rating <- disagree.df %>% 
  filter(rating != old_rating,
         ibi_score == old_org_ibi_score) %>% 
  select(unique_id, index, old_ibi_score, ibi_score, old_rating, rating) %>% 
  distinct()

## ------------------------------------------------------------------------
if (nrow(disagree.rating) > 0) {
  disagree.rating %>% 
  mutate(old_rating = factor(old_rating,
                             levels = c("poor", "fair_poor",
                                        "fair", "fair_good", "good")),
         rating = factor(rating,
                         levels = c("poor", "fair_poor",
                                    "fair", "fair_good", "good"))) %>% 
  group_by(old_rating, rating) %>% 
  summarize(count = n()) %>% 
  ungroup() %>% 
  tidyr::complete(old_rating, rating) %>% 
  mutate(count = if_else(is.na(count), as.integer(0), count)) %>% 
  ggplot(aes(rating, old_rating,
             fill = count)) +
  theme_minimal() +
  geom_tile() +
  scale_fill_gradient(low = "#ffffff", high = "#4286f4") +
  geom_text(aes(label = count, size = 5), hjust = 0.5, vjust = 0.5) +
  xlab("This Documents Rating") +
  ylab("Jacqueline M. Johnson's Rating") +
  theme(legend.position = "none")
}

## ------------------------------------------------------------------------
disagree.score <- disagree.df %>% 
  filter(ibi_diff > 0) %>% 
  select(unique_id, index, metric, value, old_score_value, score) %>% 
  distinct() %>% 
  mutate(score_diff = abs(old_score_value - score)) %>% 
  filter(score_diff > 0) %>% 
  group_by(metric, index) %>% 
  summarize(count = n()) %>% 
  ungroup() %>% 
  tidyr::complete(metric, index) %>% 
  mutate(count = if_else(is.na(count), as.integer(0), count))

## ---- fig.width = 8, fig.height = 6--------------------------------------
if (nrow(disagree.score) > 0) {
  disagree.score %>% 
    rename(Metric = metric,
           Count = count) %>% 
  ggplot(aes(Metric, Count, fill = Metric)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    facet_wrap(~index, ncol = 4) +
    theme(legend.position = "bottom") +
    guides(fill = guide_legend(ncol = 2, bycol = TRUE))
}


## ----child = 'appendix/validation_metric_disagreement.Rmd', eval=TRUE----

## ----echo=FALSE----------------------------------------------------------
knitr::opts_chunk$set(eval=evaluate, cache=cache.me)

## ------------------------------------------------------------------------
join.df <- left_join(ratings.df, old.df,
                     by = c("station", "date", "season",
                            "salzone",  "metric")) %>% 
  filter(date <= max(old.df$date)) %>% 
  unite(index, season, salzone) %>% 
  left_join(unique(bay.df[, c("unique_id", "source")]),
            by = "unique_id") %>% 
  mutate(index = factor(index),
         metric = factor(metric),
         source = factor(source))

## ------------------------------------------------------------------------
join.df <- join.df %>% 
  mutate_if(is.numeric, round, digits = 2) %>% 
  mutate(metric_diff = abs(value - old_metric_value),
         score_diff = abs(score - old_score_value),
         ibi_diff = abs(ibi_score - old_ibi_score)) %>% 
  select(unique_id,  metric, index, source,
         value, old_metric_value, metric_diff,
         score, old_score_value, score_diff,
         ibi_score, old_ibi_score, ibi_diff,
         rating, old_rating)

## ------------------------------------------------------------------------
diff.df <- join.df %>% 
  filter(ibi_diff > 0) %>% 
  arrange(desc(ibi_diff), desc(score_diff), desc(metric_diff)) %>% 
  distinct()

## ------------------------------------------------------------------------
diff.ibi <- diff.df %>% 
  select(-metric, -value, -old_metric_value, -metric_diff,
         -score, -old_score_value, - score_diff) %>% 
  mutate(disagree_bin = case_when(
    ibi_diff > 0 & ibi_diff <= 0.5 ~ "0 < IBI Score <= 0.5",
    ibi_diff > 0.5 & ibi_diff <= 1 ~ "0.5 < IBI Score <= 1.0",
    ibi_diff > 1 & ibi_diff <= 1.5 ~ "1.0 < IBI Score <= 1.5",
    ibi_diff > 1.5 & ibi_diff <= 2 ~ "1.5 < IBI Score <= 2.0",
    ibi_diff > 2 & ibi_diff <= 2.5 ~ "2.0 < IBI Score <= 2.5",
    ibi_diff > 2.5 & ibi_diff <= 3 ~ "2.5 < IBI Score <= 3.0",
    ibi_diff > 3 & ibi_diff <= 3.5 ~ "3.0 < IBI Score <= 3.5",
    ibi_diff > 3.5 & ibi_diff <= 4 ~ "3.5 < IBI Score <= 4.0",
    TRUE ~ "ERROR"
  )) %>% 
  distinct() %>% 
  arrange(desc(ibi_diff))

## ---- fig.width = 8, fig.height = 6--------------------------------------
if (nrow(diff.ibi) > 0) {
  diff.ibi %>% 
    rename(Index = index) %>% 
  ggplot(aes(Index, fill = Index)) +
    geom_bar() +
    facet_wrap(~ disagree_bin) + 
    xlab("Index") +
    ylab("Number of Discrepancies") +
    coord_flip() 
}

## ------------------------------------------------------------------------
diff.score <- diff.df %>% 
  select(unique_id, index, source,  metric, value, old_score_value, score) %>% 
  distinct() %>% 
  mutate(score_diff = abs(old_score_value - score)) %>% 
  filter(score_diff > 0)

## ---- fig.width = 8, fig.height = 3--------------------------------------
if (nrow(diff.score) > 0) {
  count.diff <- diff.score %>% 
    group_by(metric) %>% 
    summarize(count = n()) %>% 
    ungroup() %>% 
    arrange(count) %>% 
    mutate(metric = factor(metric, levels = metric)) %>% 
    rename(Metric = metric,
           Count = count)
  
  ggplot(count.diff, aes(Metric, Count, fill = Metric)) +
    geom_bar(stat = "identity") +
    coord_flip()
}

## ---- fig.width = 8, fig.height = 6--------------------------------------
if (nrow(diff.score) > 0) {
  diff.score %>% 
    mutate(metric = factor(metric, levels = levels(count.diff$Metric))) %>% 
    rename(Metric = "metric") %>% 
  ggplot(aes(Metric, fill = Metric)) +
    geom_bar() +
    coord_flip() +
    facet_wrap(~index, ncol = 4) +
    theme(legend.position = "bottom") +
    guides(fill = guide_legend(ncol = 2, bycol = TRUE)) +
    ylab("Count")
}

## ------------------------------------------------------------------------
diff.score.source <- diff.score %>% 
  group_by(metric, index, source) %>% 
  summarize(count = n()) %>% 
  ungroup() %>% 
  tidyr::complete(metric, index, source) %>% 
  mutate(count = if_else(is.na(count), as.integer(0), count))

## ---- fig.width = 8, fig.height = 25-------------------------------------
if (nrow(diff.score.source) > 0) {
  diff.score.source %>% 
    mutate(metric = factor(metric, levels = levels(count.diff$Metric))) %>% 
    filter(!is.na(metric)) %>% 
    rename(Metric = metric,
           Count = count) %>% 
  ggplot(aes(Metric, Count, fill = Metric)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    facet_wrap(~index + source, ncol = 2) +
    theme(legend.position = "bottom") +
    guides(fill = guide_legend(ncol = 2, bycol = TRUE))
}

## ------------------------------------------------------------------------
diff.metric <- diff.df %>% 
  select(unique_id, metric, index, value, old_metric_value, metric_diff) %>% 
  mutate(metric_diff = abs(metric_diff)) %>% 
  filter(metric_diff > 2)

## ---- fig.width = 8, fig.height = 3--------------------------------------
if (nrow(diff.metric) > 0) {
  diff.metric %>% 
    group_by(metric) %>% 
    summarize(count = n()) %>% 
    ungroup() %>% 
    arrange(count) %>% 
    mutate(metric = factor(metric, levels = metric)) %>% 
    rename(Metric = metric,
           Count = count) %>% 
  ggplot(aes(Metric, Count, fill = Metric)) +
    geom_bar(stat = "identity") +
    coord_flip()
}

## ---- fig.width = 8, fig.height = 15-------------------------------------
if (nrow(diff.metric) > 0) {
  diff.metric %>%
  ggplot(aes(old_metric_value, value, fill = metric, color = metric)) +
    geom_point() +
    geom_abline(intercept = 0, slope = 1) +
    facet_wrap(~metric, ncol = 1, scales = "free") +
    xlab("Jacqueline M. Johnson Values") +
    ylab("This Documents Values")
}

## ---- fig.width = 8, fig.height = 25-------------------------------------
if (nrow(diff.metric) > 0) {
diff.df %>% 
  select(unique_id, metric, index, source,  value, old_metric_value, metric_diff) %>% 
  mutate(metric_diff = abs(metric_diff)) %>% 
  filter(metric_diff > 2) %>% 
    group_by(metric, index, source) %>% 
    summarize(count = n()) %>% 
    ungroup() %>% 
    arrange(count) %>% 
    mutate(metric = factor(metric, levels = unique(metric))) %>% 
    rename(Metric = metric,
           Count = count) %>% 
  ggplot(aes(Metric, Count, fill = Metric)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    facet_wrap(~index + source, ncol = 2) +
    theme(legend.position = "bottom") +
    guides(fill = guide_legend(ncol = 2, bycol = TRUE))
}

## ---- echo=FALSE, eval=FALSE, fig.width = 8, fig.height = 200------------
## 
## test.df <- left_join(ratings.df, old.df,
##                      by = c("station", "date", "season",
##                             "salzone",  "metric")) %>%
##   filter(date <= max(old.df$date)) %>%
##   unite(index, season, salzone) %>%
##   left_join(unique(bay.df[, c("unique_id", "source")]),
##             by = "unique_id") %>%
##   mutate(index = factor(index),
##          metric = factor(metric),
##          source = factor(source)) %>%
##   select(unique_id, station, index, date, source, ibi_score, old_ibi_score) %>%
##   mutate(ratio = ibi_score / old_ibi_score)
## 
## if (nrow(diff.metric) > 0) {
##   test.df %>%
##     arrange(index) %>%
##   ggplot(aes(date, ratio, color = index)) +
##     geom_point() +
##     geom_abline(intercept = 0, slope = 1) +
##     facet_wrap(~station + index, ncol = 1, scales = "free") +
##     xlab("Jacqueline M. Johnson Values") +
##     ylab("This Documents Values")
## }


## ----child = 'appendix/conclusions.Rmd', eval=TRUE-----------------------



