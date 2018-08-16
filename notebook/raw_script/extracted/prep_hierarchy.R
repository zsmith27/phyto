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

