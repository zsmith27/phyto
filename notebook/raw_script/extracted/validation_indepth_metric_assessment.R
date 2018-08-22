## ------------------------------------------------------------------------
pheo.df <- diff.df %>% 
  filter(metric == "pheophytin") %>% 
  separate(unique_id, c("station", "layer", "samplenumber",
                        "sampledate", "season", "salzone"), sep = "_",
           remove = FALSE) %>% 
  mutate(sampledate = as.Date(sampledate))

## ------------------------------------------------------------------------
pheo.wq <- wq.df %>% 
  filter(str_detect(parameter, "pheo")) %>% 
  rename("sampledate" = "sample_date")

## ----eval=FALSE----------------------------------------------------------
## search.df <- pmap(list(1:nrow(pheo.df)), function(row.i) {
##   pheo.sub <- slice(pheo.df, row.i) %>%
##     mutate_if(is.numeric, round, digits = 2)
## 
##   wq.sub <- inner_join(pheo.wq, pheo.sub, by = c("station", "sampledate"))
## 
##   final.df <- wq.sub %>%
##     mutate(one_value = if_else(measurevalue == old_metric_value, TRUE, FALSE),
##            mean_wc = if_else(round(mean(wq.sub$measurevalue, na.rm = TRUE), 2) == old_metric_value, TRUE, FALSE),
##            mean_surface = if_else(mean(wq.sub[wq.sub$parameter == "s_pheo", "measurevalue"], na.rm = TRUE) == old_metric_value, TRUE, FALSE))
## 
##   return(final.df)
## }) %>%
##   bind_rows() %>%
##   select(unique_id, latinname, size, reportingvalue, dino_abund, old_dino_abund, diff, div)

## ------------------------------------------------------------------------
dino.df <- bay.taxa %>% 
  select(unique_id) %>% 
  separate(unique_id, c("station", "layer", "samplenumber",
                        "sampledate", "season", "salzone"), sep = "_",
           remove = FALSE) %>% 
  mutate(sampledate = as.Date(sampledate)) %>% 
  distinct() %>% 
  mutate(dino_abund = taxa_abund(bay.taxa, unique_id, reportingvalue, division, "pyrrophycophyta"),
         scrippsiella_precaria_abund = taxa_abund(bay.taxa, unique_id,
                                                  reportingvalue, latinname, "scrippsiella_precaria"),
         dino_abund = dino_abund - scrippsiella_precaria_abund) %>% 
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
         div = dino_abund / old_dino_abund) %>% 
  filter(!unique_id %in% remove.vec)

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

## ----eval=FALSE----------------------------------------------------------
## mass.df <- bay.taxa %>%
##   select(unique_id) %>%
##   separate(unique_id, c("station", "layer", "samplenumber",
##                         "sampledate", "season", "salzone"), sep = "_",
##            remove = FALSE) %>%
##   mutate(sampledate = as.Date(sampledate)) %>%
##   distinct() %>%
##   mutate(biomass = taxa_abund(bay.taxa, unique_id, biomass, species)) %>%
##   left_join(unique(bay.df[, c("unique_id", "source")]), by = "unique_id")

## ----eval=FALSE----------------------------------------------------------
## mass.old <- old.org.df %>%
##   mutate(sampledate = as.Date(sample_date)) %>%
##   select(station, sampledate, tot_biomass) %>%
##   rename(old_biomass = tot_biomass) %>%
##   distinct()

## ----eval=FALSE----------------------------------------------------------
## mass.join <- left_join(mass.df, mass.old, by = c("station", "sampledate")) %>%
##   mutate(diff = biomass - old_biomass,
##          div = biomass / old_biomass)

## ----eval=FALSE----------------------------------------------------------
## mass.join %>%
##   filter(diff != 0) %>%
##   unite(index, season, salzone) %>%
##   ggplot(aes(biomass, old_biomass, color = index)) +
##   geom_abline(intercept = 0, slope = 1, color = "green",
##               linetype = "solid", size = 1.5, alpha = 0.1) +
##   geom_abline(intercept = 0, slope = 0.5, color = "red",
##               linetype = "solid", size = 1.5, alpha = 0.1) +
##   geom_point(alpha = 0.25)

## ----eval=FALSE----------------------------------------------------------
## mass.diff <- mass.join %>%
##   filter(abs(diff) > 50)
## 
## search.df <- pmap(list(1:nrow(mass.diff)), function(row.i) {
##   mass.sub <- slice(mass.diff, row.i)
## 
##   raw.sub <- inner_join(taxa.raw, mass.sub, by = c("source", "station", "sampledate", "layer", "samplenumber"))
## 
##   final.df <- raw.sub %>%
##     filter(biomass == abs(mass.sub$diff))
## 
##   return(final.df)
## }) %>%
##   bind_rows() %>%
##   select(unique_id, latinname, size, reportingvalue, biomass, old_biomass, diff, div)
## 
## search.missing <- search.df %>%
##   filter(diff < 0)
## 
## search.remove <- search.df %>%
##   filter(diff > 0)

## ----fig.width = 10, fig.height = 25, eval=FALSE-------------------------
## search.df %>%
##   separate(unique_id, c("station", "layer", "samplenumber", "date", "season", "salzone"), sep = "_", remove = FALSE) %>%
##   left_join(unique(bay.df[, c("unique_id", "latinname", "size", "source")]), by = c("unique_id", "latinname")) %>%
##   mutate(year = year(date),
##          dir = if_else(diff < 0, "neg", "pos")) %>%
##   group_by(latinname, dir) %>%
##   summarize(count = n()) %>%
##   ungroup() %>%
##   mutate(count = if_else(dir == "neg", as.integer(-1 * count), as.integer(count))) %>%
##   group_by(latinname) %>%
##   mutate(total = sum(abs(count))) %>%
##   ungroup() %>%
##   arrange(total) %>%
##   mutate(latinname = factor(latinname, levels = unique(latinname))) %>%
##   ggplot(aes(latinname, count, fill = dir)) +
##   scale_fill_manual(values = c("pos" = "#56B4E9",
##                                "neg" = "#E69F00")) +
##   geom_bar(stat = "identity") +
##   coord_flip() #+
## #facet_wrap(~year)

## ------------------------------------------------------------------------
abund.df <- bay.taxa %>% 
  filter(!unique_id %in% remove.vec) %>% 
  select(unique_id) %>% 
  separate(unique_id, c("station", "layer", "samplenumber",
                        "sampledate", "season", "salzone"), sep = "_",
           remove = FALSE) %>% 
  mutate(sampledate = as.Date(sampledate)) %>% 
  distinct() %>% 
  mutate(abundance = taxa_abund(bay.taxa[!is.na(bay.taxa$biomass) & !(bay.taxa$unique_id %in% remove.vec), ], unique_id, reportingvalue, species)) %>% 
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

