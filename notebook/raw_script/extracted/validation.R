## ----echo=FALSE----------------------------------------------------------
knitr::opts_chunk$set(eval=evaluate, cache=TRUE)

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
  select(station, sample_date, season, ibi_layer, ibi_salzone, metrics.vec, scores.vec) 

## ------------------------------------------------------------------------
old.metrics <- old.sub %>%
  select(-one_of(scores.vec)) %>% 
  gather(metric, old_metric_value, -station, -sample_date, -season, -ibi_layer, -ibi_salzone)

## ------------------------------------------------------------------------
old.score <- old.sub %>% 
  select(-one_of(metrics.vec)) %>% 
  gather(metric, old_score_value, -station, -sample_date, -season, -ibi_layer, -ibi_salzone) %>% 
  mutate(metric = str_replace(metric, "__1", "")) %>% 
  group_by(station, sample_date, season, ibi_layer, ibi_salzone) %>% 
  mutate(old_ibi_score = mean(old_score_value, na.rm = TRUE)) %>% 
  ungroup()

## ------------------------------------------------------------------------
old.df <- full_join(old.metrics, old.score,
                    by = c("station", "sample_date", "season",
                           "ibi_layer", "ibi_salzone", "metric"))

## ------------------------------------------------------------------------
old.df <- left_join(old.df, old.org.df,
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

## ------------------------------------------------------------------------
disagree.df <- old.rescored %>% 
  mutate_if(is.numeric, round, digits = 2) %>% 
  mutate(ibi_diff = abs(old_ibi_score - ibi_score)) %>% 
  arrange(desc(ibi_diff)) %>% 
  select(unique_id, metric, value, old_score_value, score, old_ibi_score, ibi_score, ibi_diff, pibi_rank, rating) %>% 
  separate(unique_id, c("station", "layer", "date", "season", "salzone"), sep = "_", remove = FALSE) %>% 
  unite(index, c("season", "salzone")) %>% 
  select(-station, -layer, -date) %>% 
  mutate(metric = factor(metric),
         index = factor(index)) %>% 
  filter(ibi_diff > 0)

## ------------------------------------------------------------------------
disagree.ibi <- disagree.df %>% 
  select(unique_id, index, old_ibi_score, ibi_score, ibi_diff, pibi_rank, rating) %>% 
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
  ggplot(disagree.ibi, aes(index, fill = index)) +
    geom_bar() +
    facet_wrap(~ disagree_bin, scales = "free_x") +
    coord_flip()
}

## ------------------------------------------------------------------------
disagree.score <- disagree.df %>% 
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
  ggplot(disagree.score, aes(metric, count, fill = metric)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    facet_wrap(~index, ncol = 4) +
    theme(legend.position = "bottom") +
    guides(fill = guide_legend(ncol = 2, bycol = TRUE))
}

## ------------------------------------------------------------------------
join.df <- left_join(ratings.df, old.df, by = c("station", "date", "season", "salzone",  "metric")) %>% 
  filter(date <= max(old.df$date)) %>% 
  unite(index, season, salzone) %>% 
  left_join(unique(bay.df[, c("unique_id", "source")]), by = "unique_id") %>% 
  mutate(index = factor(index),
         metric = factor(metric),
         source = factor(source))

## ------------------------------------------------------------------------
remove.vec <- c('et4.2_ap_fs1_1985-04-10_spring_m', 'et4.2_ap_fs1_1985-05-07_spring_m', 'et4.2_ap_fs1_1985-05-22_spring_m', 'et4.2_ap_fs1_1985-07-22_summer_m', 'et5.1_ap_fs1_1984-08-23_summer_f', 'et5.2_ap_fs1_1984-08-23_summer_m', 'et5.1_ap_fs1_1984-09-20_summer_o', 'et5.2_ap_fs1_1984-09-20_summer_m', 'et5.2_ap_fs1_1985-04-10_spring_m', 'et5.2_ap_fs1_1985-04-23_spring_m', 'et5.1_ap_fs1_1985-04-24_spring_o', 'et5.1_ap_fs1_1985-05-07_spring_o', 'et5.2_ap_fs1_1985-05-07_spring_m', 'et5.2_ap_fs1_1985-05-21_spring_m', 'et5.1_ap_fs1_1985-05-22_spring_o', 'ee3.1_ap_fs1_1985-04-23_spring_m', 'ee3.1_ap_fs1_1985-05-08_spring_m', 'ee3.1_ap_fs1_1985-05-21_spring_m', 'wt5.1_ap_fs1_1984-09-19_summer_m', 'wt5.1_ap_fs1_1985-04-24_spring_m', 'wt5.1_ap_fs1_1985-05-08_spring_m', 'wt5.1_ap_fs1_1985-05-22_spring_m', 'wt5.1_ap_fs1_1985-07-22_summer_m', 'le2.2_ap_fs1_1984-09-04_summer_m', 'le2.2_ap_fs1_1985-04-15_spring_m', 'ret2.2_ap_fs1_1985-04-15_spring_o', 'ret2.2_ap_fs1_1985-04-30_spring_o', 'le2.2_ap_fs1_1985-04-30_spring_m', 'le2.2_ap_fs1_1985-05-13_spring_m', 'ret2.2_ap_fs1_1985-05-13_spring_o', 'ret2.2_ap_fs1_1985-05-27_spring_o', 'le2.2_ap_fs1_1985-05-27_spring_m', 'ret2.2_ap_fs1_1985-07-08_summer_o', 'le2.2_ap_fs1_1985-07-08_summer_m', 'ret2.2_ap_fs1_1985-07-24_summer_m', 'le2.2_ap_fs1_1985-07-24_summer_m', 'ret2.2_ap_fs1_1985-08-06_summer_m', 'ret2.2_ap_fs1_1985-08-22_summer_o', 'le2.2_ap_fs1_1985-08-22_summer_m', 'cb1.1_ap_fs1_1984-08-07_summer_f', 'cb1.1_ap_fs1_1984-09-26_summer_f', 'cb1.1_ap_fs1_1985-03-21_spring_f', 'cb1.1_ap_fs1_1985-04-10_spring_f', 'cb1.1_ap_fs1_1985-04-24_spring_f', 'cb1.1_ap_fs1_1985-07-10_summer_f', 'tf2.3_ap_fs1_1984-08-06_summer_f', 'tf2.3_ap_fs1_1985-04-15_spring_f', 'tf2.3_ap_fs1_1985-04-30_spring_f', 'tf2.3_ap_fs1_1985-05-13_spring_f', 'tf2.3_ap_fs1_1985-05-27_spring_f', 'tf2.3_ap_fs1_1985-07-08_summer_f', 'tf2.3_ap_fs1_1985-07-24_summer_f', 'tf2.3_ap_fs1_1985-08-06_summer_f', 'tf2.3_ap_fs1_1985-08-22_summer_f', 'tf2.3_ap_fs1_1985-09-09_summer_f', 'le1.1_ap_fs1_1985-03-26_spring_m', 'le1.1_ap_fs1_1985-04-08_spring_m', 'le1.1_ap_fs1_1985-04-22_spring_m', 'tf1.7_ap_fs1_1985-04-22_spring_m', 'le1.1_ap_fs1_1985-05-06_spring_m', 'tf1.7_ap_fs1_1985-05-06_spring_m', 'tf1.5_ap_fs1_1985-05-06_spring_o', 'le1.1_ap_fs1_1985-05-20_spring_m', 'tf1.7_ap_fs1_1985-05-20_spring_m', 'tf1.5_ap_fs1_1985-05-20_spring_f', 'le1.1_ap_fs1_1985-07-17_summer_m', 'cb5.2_ap_fs1_1984-08-06_summer_m', 'cb2.2_ap_fs1_1984-08-07_summer_f', 'cb5.2_ap_fs1_1984-08-29_summer_m', 'cb4.3c_ap_fs1_1984-08-30_summer_m', 'cb3.3c_ap_fs1_1984-08-30_summer_m', 'cb2.2_ap_fs1_1984-08-30_summer_o', 'cb3.3c_ap_fs1_1984-09-11_summer_m', 'cb2.2_ap_fs1_1984-09-12_summer_o', 'cb5.2_ap_fs1_1984-09-24_summer_m', 'cb4.3c_ap_fs1_1984-09-25_summer_m', 'cb3.3c_ap_fs1_1984-09-25_summer_m', 'cb2.2_ap_fs1_1984-09-26_summer_m', 'cb5.2_ap_fs1_1985-03-19_spring_m', 'cb4.3c_ap_fs1_1985-03-20_spring_m', 'cb3.3c_ap_fs1_1985-03-20_spring_m', 'cb5.2_ap_fs1_1985-04-08_spring_m', 'cb4.3c_ap_fs1_1985-04-09_spring_m', 'cb3.3c_ap_fs1_1985-04-09_spring_m', 'cb2.2_ap_fs1_1985-04-10_spring_f', 'cb5.2_ap_fs1_1985-04-22_spring_m', 'cb4.3c_ap_fs1_1985-04-23_spring_m', 'cb3.3c_ap_fs1_1985-04-23_spring_m', 'cb2.2_ap_fs1_1985-04-24_spring_o', 'cb4.3c_ap_fs1_1985-05-07_spring_m', 'cb3.3c_ap_fs1_1985-05-07_spring_m', 'cb3.3c_ap_fs1_1985-05-21_spring_m', 'cb5.2_ap_fs1_1985-07-08_summer_m', 'cb4.3c_ap_fs1_1985-07-09_summer_m', 'cb3.3c_ap_fs1_1985-07-09_summer_m', 'cb2.2_ap_fs1_1985-07-10_summer_o', 'cb5.2_ap_fs1_1985-07-22_summer_p', 'cb3.3c_ap_fs1_1985-07-24_summer_m', 'cb2.2_ap_fs1_1985-07-24_summer_o')

join.df <- join.df %>% 
  filter(!unique_id %in% remove.vec)

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
         rating, pibi_rank)

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

## ------------------------------------------------------------------------
if (nrow(diff.ibi) > 0) {
  ggplot(diff.ibi, aes(index, fill = index)) +
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

## ---- fig.width = 8, fig.height = 6--------------------------------------
if (nrow(diff.score) > 0) {
  ggplot(diff.score, aes(metric, fill = metric)) +
    geom_bar() +
    coord_flip() +
    facet_wrap(~index, ncol = 4)+
    theme(legend.position = "bottom") +
    guides(fill = guide_legend(ncol = 2, bycol = TRUE))
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
  ggplot(diff.score.source, aes(metric, count, fill = metric)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    facet_wrap(~index + source, ncol = 2) +
    theme(legend.position = "bottom") +
    guides(fill = guide_legend(ncol = 2, bycol = TRUE))
}

## ------------------------------------------------------------------------
dino.df <- bay.taxa %>% 
  select(unique_id) %>% 
  separate(unique_id, c("station", "layer", "samplenumber",
                        "sampledate", "season", "salzone"), sep = "_",
           remove = FALSE) %>% 
  mutate(sampledate = as.Date(sampledate)) %>% 
  distinct() %>% 
  mutate(dino_abund = taxa_abund(bay.taxa, unique_id, reportingvalue, division, "pyrrophycophyta")) %>% 
  left_join(unique(bay.df[, c("unique_id", "source")]), by = "unique_id")

## ------------------------------------------------------------------------
dino.old <- old.org.df %>% 
  mutate(sampledate = as.Date(sample_date)) %>% 
  select(station, sampledate, dino_abund) %>% 
  rename(old_dino_abund = dino_abund) %>% 
  distinct()

## ------------------------------------------------------------------------
dino.join <- left_join(dino.df, dino.old, by = c("station", "sampledate")) %>% 
  mutate(diff = dino_abund - old_dino_abund,
         div = dino_abund / old_dino_abund)

## ------------------------------------------------------------------------
dino.join %>% 
  filter(diff != 0) %>% 
  unite(index, season, salzone) %>% 
  ggplot(aes(dino_abund, old_dino_abund, color = index)) +
  geom_abline(intercept = 0, slope = 1, color = "green", 
              linetype = "solid", size = 1.5, alpha = 0.1) +
  geom_abline(intercept = 0, slope = 0.5, color = "red", 
              linetype = "solid", size = 1.5, alpha = 0.1) +
  geom_point(alpha = 0.25)

## ------------------------------------------------------------------------
dino.diff <- dino.join %>% 
  filter(diff != 0,
         div != 2)

search.df <- pmap(list(1:nrow(dino.diff)), function(row.i) {
  dino.sub <- slice(dino.diff, row.i)
  
  raw.sub <- inner_join(taxa.raw, dino.sub, by = c("source", "station", "sampledate", "layer", "samplenumber"))
  
  final.df <- raw.sub %>% 
    filter(reportingvalue == abs(dino.sub$diff))
  
  return(final.df)
}) %>% 
  bind_rows() %>% 
  select(unique_id, latinname, size, reportingvalue, dino_abund, old_dino_abund, diff, div)

search.missing <- search.df %>% 
  filter(diff < 0)

search.remove <- search.df %>% 
  filter(diff > 0)

## ----fig.width = 10, fig.height = 10-------------------------------------
search.df %>% 
  separate(unique_id, c("station", "layer", "samplenumber",
                        "date", "season", "salzone"), sep = "_", remove = FALSE) %>% 
  left_join(unique(bay.df[, c("unique_id", "latinname",
                               "source")]), by = c("unique_id", "latinname")) %>% 
  mutate(year = year(date),
         dir = if_else(diff < 0, "too few", "too many")) %>% 
  group_by(latinname, dir) %>% 
  summarize(count = n()) %>% 
  ungroup() %>% 
  mutate(count = if_else(dir == "too few", as.integer(-1 * count), as.integer(count))) %>% 
  group_by(latinname) %>% 
  mutate(total = sum(abs(count))) %>% 
  ungroup() %>% 
  arrange(total) %>% 
  mutate(latinname = factor(latinname, levels = unique(latinname))) %>% 
  ggplot(aes(latinname, count, fill = dir)) +
  scale_fill_manual(values = c("too many" = "#56B4E9",
                               "too few" = "#E69F00")) +
  geom_bar(stat = "identity") +
  coord_flip() #+
#facet_wrap(~year)

## ------------------------------------------------------------------------
mass.df <- bay.taxa %>% 
  select(unique_id) %>% 
  separate(unique_id, c("station", "layer", "samplenumber",
                        "sampledate", "season", "salzone"), sep = "_",
           remove = FALSE) %>% 
  mutate(sampledate = as.Date(sampledate)) %>% 
  distinct() %>% 
  mutate(biomass = taxa_abund(bay.taxa, unique_id, biomass, species)) %>% 
  left_join(unique(bay.df[, c("unique_id", "source")]), by = "unique_id")

## ------------------------------------------------------------------------
mass.old <- old.org.df %>% 
  mutate(sampledate = as.Date(sample_date)) %>% 
  select(station, sampledate, tot_biomass) %>% 
  rename(old_biomass = tot_biomass) %>% 
  distinct()

## ------------------------------------------------------------------------
mass.join <- left_join(mass.df, mass.old, by = c("station", "sampledate")) %>% 
  mutate(diff = biomass - old_biomass,
         div = biomass / old_biomass)

## ------------------------------------------------------------------------
mass.join %>% 
  filter(diff != 0) %>% 
  unite(index, season, salzone) %>% 
  ggplot(aes(biomass, old_biomass, color = index)) +
  geom_abline(intercept = 0, slope = 1, color = "green", 
              linetype = "solid", size = 1.5, alpha = 0.1) +
  geom_abline(intercept = 0, slope = 0.5, color = "red", 
              linetype = "solid", size = 1.5, alpha = 0.1) +
  geom_point(alpha = 0.25)

## ------------------------------------------------------------------------
mass.diff <- mass.join %>% 
  filter(abs(diff) > 50)

search.df <- pmap(list(1:nrow(mass.diff)), function(row.i) {
  mass.sub <- slice(mass.diff, row.i)
  
  raw.sub <- inner_join(taxa.raw, mass.sub, by = c("source", "station", "sampledate", "layer", "samplenumber"))
  
  final.df <- raw.sub %>% 
    filter(biomass == abs(mass.sub$diff))
  
  return(final.df)
}) %>% 
  bind_rows() %>% 
  select(unique_id, latinname, size, reportingvalue, biomass, old_biomass, diff, div)

search.missing <- search.df %>% 
  filter(diff < 0)

search.remove <- search.df %>% 
  filter(diff > 0)

## ----fig.width = 10, fig.height = 25-------------------------------------
search.df %>% 
  separate(unique_id, c("station", "layer", "samplenumber", "date", "season", "salzone"), sep = "_", remove = FALSE) %>% 
  left_join(unique(bay.df[, c("unique_id", "latinname", "size", "source")]), by = c("unique_id", "latinname")) %>% 
  mutate(year = year(date),
         dir = if_else(diff < 0, "neg", "pos")) %>% 
  group_by(latinname, dir) %>% 
  summarize(count = n()) %>% 
  ungroup() %>% 
  mutate(count = if_else(dir == "neg", as.integer(-1 * count), as.integer(count))) %>% 
  group_by(latinname) %>% 
  mutate(total = sum(abs(count))) %>% 
  ungroup() %>% 
  arrange(total) %>% 
  mutate(latinname = factor(latinname, levels = unique(latinname))) %>% 
  ggplot(aes(latinname, count, fill = dir)) +
  scale_fill_manual(values = c("pos" = "#56B4E9",
                               "neg" = "#E69F00")) +
  geom_bar(stat = "identity") +
  coord_flip() #+
#facet_wrap(~year)

## ------------------------------------------------------------------------
abund.df <- bay.taxa %>% 
  select(unique_id) %>% 
  separate(unique_id, c("station", "layer", "samplenumber",
                        "sampledate", "season", "salzone"), sep = "_",
           remove = FALSE) %>% 
  mutate(sampledate = as.Date(sampledate)) %>% 
  distinct() %>% 
  mutate(abundance = taxa_abund(bay.taxa[!is.na(bay.taxa$biomass), ], unique_id, reportingvalue, species)) %>% 
  left_join(unique(bay.df[, c("unique_id", "source")]), by = "unique_id")

## ------------------------------------------------------------------------
abund.old <- old.org.df %>% 
  mutate(sampledate = as.Date(sample_date)) %>% 
  select(station, sampledate, tot_abund) %>% 
  rename(old_abund = tot_abund) %>% 
  distinct()

## ------------------------------------------------------------------------
abund.join <- left_join(abund.df, abund.old, by = c("station", "sampledate")) %>% 
  mutate(diff = abundance - old_abund,
         div = abundance / old_abund)

## ------------------------------------------------------------------------
abund.join %>% 
  filter(abundance < 2e+09,
         diff != 0) %>% 
  unite(index, season, salzone) %>% 
  ggplot(aes(abundance, old_abund, color = index)) +
  geom_abline(intercept = 0, slope = 1, color = "green", 
              linetype = "solid", size = 1.5, alpha = 0.1) +
  geom_abline(intercept = 0, slope = 0.5, color = "red", 
              linetype = "solid", size = 1.5, alpha = 0.1) +
  geom_point(alpha = 0.25)

## ---- fig.width = 10, fig.height = 15------------------------------------
abund.join %>% 
  filter(abundance < 2e+09,
         diff != 0) %>% 
  unite(index, season, salzone) %>% 
  ggplot(aes(abundance, old_abund, color = index)) +
  geom_point(alpha = 0.25) +
  facet_wrap(~index + source, ncol = 2) +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(ncol = 2, bycol = TRUE)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

## ------------------------------------------------------------------------
test <- abund.join %>% 
  filter(source == "msu/pearl",
         season == "summer",
         salzone == "m") %>% 
  mutate(year = year(sampledate))

## ---- fig.width = 10, fig.height = 25------------------------------------
test %>% 
  ggplot(aes(abundance, old_abund, color = samplenumber)) +
  geom_point(alpha = 0.25) +
  facet_wrap(~year, ncol = 2, scale = "free") +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(ncol = 2, bycol = TRUE)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

## ------------------------------------------------------------------------
abund.join %>% 
  filter(diff != 0,
         div != 2) %>% 
  unite(index, season, salzone) %>% 
  ggplot(aes(sampledate, diff, color = source)) +
  geom_point(alpha = 0.25)

## ------------------------------------------------------------------------
abund.diff <- abund.join %>% 
  filter(diff != 0)

search.df <- pmap(list(1:nrow(abund.diff)), function(row.i) {
  abund.sub <- slice(abund.diff, row.i)
  
  raw.sub <- inner_join(taxa.raw, abund.sub, by = c("source", "station", "sampledate", "layer", "samplenumber"))
  
  final.df <- raw.sub %>% 
    filter(reportingvalue == abs(abund.sub$diff))
  
  return(final.df)
}) %>% 
  bind_rows() %>% 
  select(unique_id, latinname, size, reportingvalue, abundance, old_abund, diff, div)

search.missing <- search.df %>% 
  filter(diff < 0)

search.remove <- search.df %>% 
  filter(diff > 0)

## ----fig.width = 10, fig.height = 25-------------------------------------
search.df %>% 
  separate(unique_id, c("station", "layer", "samplenumber", "date", "season", "salzone"), sep = "_", remove = FALSE) %>% 
  left_join(unique(bay.df[, c("unique_id", "latinname", "size", "source")]), by = c("unique_id", "latinname")) %>% 
  mutate(year = year(date),
         dir = if_else(diff < 0, "neg", "pos")) %>% 
  group_by(latinname, dir) %>% 
  summarize(count = n()) %>% 
  ungroup() %>% 
  mutate(count = if_else(dir == "neg", as.integer(-1 * count), as.integer(count))) %>% 
  group_by(latinname) %>% 
  mutate(total = sum(abs(count))) %>% 
  ungroup() %>% 
  arrange(total) %>% 
  mutate(latinname = factor(latinname, levels = unique(latinname))) %>% 
  ggplot(aes(latinname, count, fill = dir)) +
  scale_fill_manual(values = c("pos" = "#56B4E9",
                               "neg" = "#E69F00")) +
  geom_bar(stat = "identity") +
  coord_flip() #+
#facet_wrap(~year)

## ----fig.width = 10, fig.height = 7--------------------------------------
search.missing %>% 
  separate(unique_id, c("station", "layer", "samplenumber", "date", "season", "salzone"), sep = "_", remove = FALSE) %>% 
  left_join(unique(bay.df[, c("unique_id", "source")]), by = "unique_id") %>% 
  mutate(year = year(date)) %>% 
  group_by(year, source, latinname) %>% 
  summarize(count = n()) %>% 
  ggplot(aes(year, count, fill = source)) +
  scale_fill_manual(values = c("odu/pel" = "#56B4E9",
                               "msu/pearl" = "#E69F00")) +
  geom_bar(stat = "identity") +
  facet_wrap(~latinname + source, ncol = 5)

## ----fig.width = 10, fig.height = 20-------------------------------------
search.remove %>% 
  separate(unique_id, c("station", "layer", "samplenumber", "date", "season", "salzone"), sep = "_", remove = FALSE) %>% 
  left_join(unique(bay.df[, c("unique_id", "source")]), by = "unique_id") %>% 
  mutate(year = year(date)) %>% 
  group_by(year, source, latinname) %>% 
  summarize(count = n()) %>% 
  ggplot(aes(year, count, fill = source)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("odu/pel" = "#56B4E9",
                               "msu/pearl" = "#E69F00")) +
  facet_wrap(~latinname + source, ncol = 5)

