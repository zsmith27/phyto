## ----echo=FALSE----------------------------------------------------------
knitr::opts_chunk$set(eval=evaluate, cache=cache.me)

## ------------------------------------------------------------------------
join.df <- left_join(ratings.df, old.df, by = c("station", "date", "season", "salzone",  "metric")) %>% 
  filter(date <= max(old.df$date)) %>% 
  unite(index, season, salzone) %>% 
  left_join(unique(bay.df[, c("unique_id", "source")]), by = "unique_id") %>% 
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

## ---- fig.width = 8, fig.height = 3--------------------------------------
if (nrow(diff.score) > 0) {
  ggplot(diff.score, aes(metric, fill = metric)) +
    geom_bar() +
    coord_flip()
}

## ---- fig.width = 8, fig.height = 6--------------------------------------
if (nrow(diff.score) > 0) {
  ggplot(diff.score, aes(metric, fill = metric)) +
    geom_bar() +
    coord_flip() +
    facet_wrap(~index, ncol = 4) +
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
diff.metric <- diff.df %>% 
  select(unique_id, metric, index, value, old_metric_value, metric_diff) %>% 
  filter(metric_diff > 2 | metric_diff < -2)

## ---- fig.width = 8, fig.height = 3--------------------------------------
if (nrow(diff.metric) > 0) {
  diff.metric %>% 
    group_by(metric) %>% 
    summarize(count = n()) %>% 
    ungroup() %>% 
    arrange(count) %>% 
    mutate(metric = factor(metric, levels = metric)) %>% 
  ggplot(aes(metric, count, fill = metric)) +
    geom_bar(stat = "identity") +
    coord_flip()
}

