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



