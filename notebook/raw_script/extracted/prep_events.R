## ----echo=FALSE----------------------------------------------------------
knitr::opts_chunk$set(eval=evaluate, cache=cache.me)

## ------------------------------------------------------------------------
events.df <- data.table::fread(file.path(project.dir, "data/phytoplankton/cedr_phyto_event.csv"),
                            data.table = FALSE,
                            na.strings = "")

## ------------------------------------------------------------------------
events.df <- events.df %>% 
  mutate(sampledate = as.Date(sampledate),
         salzone = case_when(
           salzone %in% c("tf", "fe") ~ "f",
           salzone == "me" ~ "m",
           salzone == "oe" ~ "o",
           salzone == "pe" ~ "p",
           TRUE ~ salzone
         ))

## ------------------------------------------------------------------------
events.sub <- events.df %>% 
  select(source, station, sampledate, layer, salzone) %>% 
  distinct()

## ------------------------------------------------------------------------
old.salzone <- read_excel("D:/ZSmith/Projects/PIBI/phyto/data/jackie_data/Data 2013_4plus-phyto-metrics.xlsx",
                         sheet = "DATA_4+biometrics",
                         skip = 1) %>% 
  clean_up() %>% 
  select(station, sample_date, ibi_salzone) %>% 
  mutate(sample_date = as.Date(sample_date)) %>% 
  rename(old_salzone = ibi_salzone,
         sampledate = sample_date) %>% 
  distinct()

## ------------------------------------------------------------------------
events.sub <- inner_join(events.sub, old.salzone, by = c("station", "sampledate"))

## ------------------------------------------------------------------------
events.sub <- events.sub %>% 
  mutate(salzone_disagree = if_else(salzone != old_salzone, TRUE, FALSE))

## ------------------------------------------------------------------------
salzone.diff <- events.sub %>% 
  select(station, sampledate, source, salzone, old_salzone, salzone_disagree) %>% 
  unite(sal_group, salzone, old_salzone) %>% 
  distinct() %>% 
  filter(salzone_disagree == TRUE) %>% 
  group_by(sal_group) %>% 
  summarize(count = n()) %>% 
  ungroup() %>% 
  arrange(count) %>% 
  mutate(sal_group = factor(sal_group, levels = unique(sal_group))) %>% 
  ggplot(aes(sal_group, count, fill = count)) + 
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  geom_bar(stat = "identity") 

## ------------------------------------------------------------------------
events.sub <- events.sub %>% 
  rename(cedr_salzone = salzone) %>% 
  mutate(salzone = if_else(salzone_disagree == TRUE, old_salzone, cedr_salzone))

## ------------------------------------------------------------------------
bay.df <- left_join(bay.df, events.sub, by = c("source", "station",
                                               "sampledate", "layer")) %>% 
  mutate(unique_id = paste(unique_id, season, salzone, sep = "_"))

