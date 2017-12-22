## ------------------------------------------------------------------------
file.dir <- "D:/ZSmith/Projects/PIBI/phyto/data/jackie_data/Data 2013_4plus-phyto-metrics.xlsx"

## ------------------------------------------------------------------------
old.org.df <- read_excel(file.dir, 
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
correct.df <- old.org.df %>% 
  #select(station, sample_date, season, ibi_layer, ibi_salzone, biomass_chl_ratio__1:tot_biomass__1, -pico_abund__1) %>% 
  select(station, sample_date, season, ibi_layer, ibi_salzone, metrics.vec, scores.vec) 


## ------------------------------------------------------------------------
correct.metrics <- correct.df %>%
  select(-one_of(scores.vec)) %>% 
  gather(metric, old_metric_value, -station, -sample_date, -season, -ibi_layer, -ibi_salzone)

## ------------------------------------------------------------------------
correct.score <- correct.df %>% 
  select(-one_of(metrics.vec)) %>% 
  gather(metric, old_score_value, -station, -sample_date, -season, -ibi_layer, -ibi_salzone) %>% 
  mutate(metric = str_replace(metric, "__1", "")) %>% 
  group_by(station, sample_date, season, ibi_layer, ibi_salzone) %>% 
  mutate(old_ibi_score = mean(old_score_value, na.rm = TRUE)) %>% 
  ungroup()

## ------------------------------------------------------------------------
old.df <- full_join(correct.metrics, correct.score,
                      by = c("station", "sample_date", "season",
                             "ibi_layer", "ibi_salzone", "metric"))

## ------------------------------------------------------------------------
old.sub <- left_join(old.df, old.org.df,
                     by = c("station", "sample_date", "season", "ibi_layer", "ibi_salzone")) %>% 
  select(station, sample_date, season, ibi_layer, ibi_salzone, metric,
         old_metric_value, old_score_value, old_ibi_score, pibi_rank) %>% 
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

metrics.long <- old.sub %>% 
  rename(value = old_metric_value) %>% 
  unite(unique_id, station, layer, date, season, salzone, remove = FALSE) %>% 
  score_phyto()

## ------------------------------------------------------------------------
ratings.df3 <- ratings.df2 %>% 
  separate(season_sal, c("season", "salzone"))

## ------------------------------------------------------------------------
join.df <- left_join(ratings.df, old.sub, by = c("station", "date", "season",  "metric")) %>% 
  filter(date <= max(old.sub$date)) %>% 
  mutate_if(is.numeric, round, digits = 2) %>% 
  mutate(diff_metric = abs(value - old_metric_value),
         diff_score = abs(score - old_score_value),
         diff_ibi = abs(ibi_score - old_ibi_score)) %>% 
  select(unique_id,  metric, season, salzone.x,
         value, old_metric_value, diff_metric,
         score, old_score_value, diff_score,
         ibi_score, old_ibi_score, diff_ibi,
         rating, pibi_rank) %>% 
  unite(season_salzone, season, salzone.x) %>% 
  left_join(unique(bay.df[, c("unique_id", "source", "salzone", "season")]), by = "unique_id")



## ------------------------------------------------------------------------
diff.ibi <- join.df %>% 
  select(-metric, -value, -old_metric_value, -diff_metric,
         -score, -old_score_value, - diff_score) %>% 
  distinct() %>% 
  filter(diff_ibi > 0.5) %>% 
  arrange(desc(diff_ibi))

## ------------------------------------------------------------------------
diff.df <- join.df %>% 
  filter(diff_ibi > 0.5,
         diff_score > 0.5) %>% 
  arrange(desc(diff_ibi), desc(diff_score), desc(diff_metric))

## ---- fig.width = 12, fig.height = 12------------------------------------
ggplot(diff.df, aes(metric)) +
  geom_bar() +
  coord_flip() +
  facet_wrap(~salzone + source + season)

## ------------------------------------------------------------------------
metric.test <- join.df %>% 
  filter(metric == "dinoflagellate_biomass") %>% 
  arrange(desc(diff_metric))

## ------------------------------------------------------------------------
sub.test <- bay.df %>% 
  filter(unique_id == "ret4.3_ap_fs1_1989-05-22_spring_m") %>% 
  group_by(class) %>% 
  mutate(count = sum(reportingvalue))

## ------------------------------------------------------------------------
sub.join <- join.df %>% 
   filter(unique_id == "ret4.3_ap_fs1_1989-05-22_spring_m") %>% 
  arrange(desc(diff_score), desc(diff_metric))

## ------------------------------------------------------------------------
sub.test2 <- bay.df2 %>% 
  filter(station == "wt5.1",
         sampledate == "1985-04-24") %>% 
  group_by(class) %>% 
  mutate(count = sum(reportingvalue))

## ------------------------------------------------------------------------
bay.df %>% 
  filter(division == "cryptophycophyta") %>% 
  select(class) %>% 
  distinct()

## ------------------------------------------------------------------------
test <- bay.df %>% 
  select(station, sampledate, layer, samplenumber) %>% 
  distinct() %>% 
  group_by(station, sampledate, layer) %>% 
  summarize(count = n())

