test <- bay.df %>% 
  anti_join(hier.df, by = "tsn") %>% 
  select(tsn, LatinName) %>% 
  distinct() %>% 
  arrange(tsn, LatinName)

bay.tsn <- unique(bay.df$tsn)

taxa.units <- tbl(conn, "taxonomic_units") %>% 
  select(tsn, n_usage, complete_name) %>% 
  filter(tsn %in% bay.tsn) %>% 
  distinct() %>% 
  data.frame()

bay.df <- bay.df %>% 
  left_join(taxa.units, by = c("final_tsn" = "tsn"))


taxa.valid <- taxa.units %>% 
  filter(n_usage %in% c("valid", "accepted")) %>% 
  select(tsn, complete_name) %>% 
  rename(valid_tsn = tsn)

bay.df2 <- bay.df %>% 
  semi_join(taxa.valid, by = c("final_tsn" = "tsn"))

test <- bay.df2 %>% 
  filter(is.na(valid_tsn)) %>% 
  select(tsn, n_usage, complete_name, valid_tsn) %>% 
  distinct()

syn.df <- tbl(conn, "synonym_links") %>% 
  #select(1:2) %>% 
  data.frame() %>% 
  right_join(test, by = "tsn")

taxa.units <- tbl(conn, "longnames") %>% 
  data.frame()
