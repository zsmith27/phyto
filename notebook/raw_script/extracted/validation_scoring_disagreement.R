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

