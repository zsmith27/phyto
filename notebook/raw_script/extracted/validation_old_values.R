## ----echo=FALSE----------------------------------------------------------
knitr::opts_chunk$set(eval=evaluate, cache=cache.me)

## ------------------------------------------------------------------------
old.org.df <- read_excel("D:/ZSmith/Projects/PIBI/phyto/data/jackie_data/Data 2013_4plus-phyto-metrics.xlsx",
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

