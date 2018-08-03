## ----echo=FALSE----------------------------------------------------------
knitr::opts_chunk$set(eval=evaluate, cache=cache.me)

## ------------------------------------------------------------------------
carbon.df <- readxl::read_excel(file.path(project.dir, "data/carbon/carbon_list_2014.xlsx"),
                                sheet = "carbon_list_2014") %>% 
  clean_up()
  

## ------------------------------------------------------------------------
carbon.df <- carbon.df %>% 
  mutate(size = case_when(
    size %in% "unidentified" ~ as.character(NA),
    str_detect(size, "species|sp.") ~ str_replace_all(size, "sp.#|species_#|species_|species", "sp#"),
    TRUE ~size
  ),
  latinname = if_else(latinname == "blue_green_trichome", "blue_green_sphere", latinname)) %>% 
  mutate(picoplankton = if_else(
    diameter <= 20 &
      height <= 20 &
      width <= 20 &
      width <= 20 &
      height_2 <= 20 &
      width_2 <= 20,
    TRUE,
    FALSE
  )) %>% 
  select(latinname, size, old_carbon) %>%
  distinct() %>% 
  bind_rows(data.frame(latinname = "asterionellopsis_glacialis", 
                       old_carbon = carbon.df[carbon.df$latinname == "asterionellopsis_kariana", "old_carbon"],
                       stringsAsFactors = FALSE))

## ------------------------------------------------------------------------
carbon.dups <- carbon.df %>% 
  group_by(latinname, size) %>% 
  mutate(count = n()) %>% 
  ungroup() %>% 
  filter(count > 1)

## ------------------------------------------------------------------------
carbon.df <- carbon.df %>% 
  filter(!(latinname == "chaetoceros_wighami" & is.na(old_carbon)),
         !(latinname == "biddulphia" & old_carbon == 7899.50),
         !(latinname == "gymnodinium" & old_carbon == 848)) # %>% 
#  mutate(size = case_when(
#    is.na(size) & latinname == "gymnodinium" & old_carbon == 848 ~ "msu",
#    is.na(size) & latinname == "gymnodinium" & old_carbon == 43.900 ~ "odu",
#    TRUE ~ size
#    ))
         

## ------------------------------------------------------------------------
bay.df <- bay.df %>% 
  mutate(size = case_when(
    size %in% c("not_applicable", "not_specified") ~ as.character(NA),
    size == "cell-_small" & latinname == "gymnodinium" ~ as.character(NA),
    str_detect(size, "species|sp.") ~ str_replace_all(size, "sp.#|species_#|species_|species", "sp#"),
    TRUE ~ size
  ))# %>% 
#  mutate(size = case_when(
#        is.na(size) & latinname == "gymnodinium" & str_detect(source, "msu") ~ "msu",
#    is.na(size) & latinname == "gymnodinium" & str_detect(source, "odu") ~ "odu",
#    TRUE ~ size
#  ))

## ------------------------------------------------------------------------
no.match.df <- anti_join(bay.df, carbon.df, by = c("latinname", "size")) %>% 
  select(latinname, size) %>% 
  distinct()

## ------------------------------------------------------------------------
partial.match.df <- lapply(1:nrow(no.match.df), function(row.i) {
  sub.df <- no.match.df %>% 
    slice(row.i)
  
  size.vec <- str_split(sub.df$size, "_") %>% unlist()
  
  getmode <- function(v) {
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
  }
  
  
  carbon.sub <- carbon.df %>% 
    filter(latinname == sub.df$latinname) %>% 
    mutate(row_max_matches = sapply(size.vec, function(size.i) grep(size.i, size)) %>% 
             unlist() %>% 
             getmode()) %>% 
    slice(unique(row_max_matches)) %>% 
    select(-row_max_matches, -old_carbon) %>% 
    rename(new_size = size) %>% 
    mutate(size = sub.df$size)
  
  return(carbon.sub)
}) %>% 
  bind_rows()

## ------------------------------------------------------------------------
bay.df <- left_join(bay.df, partial.match.df, by = c("latinname", "size")) %>% 
  mutate(reported_size = size,
         size = if_else(!is.na(new_size), new_size, size))

## ------------------------------------------------------------------------
bay.df <- left_join(bay.df, carbon.df, by = c("latinname", "size")) %>% 
  mutate(biomass = reportingvalue * old_carbon / 10 ^ 6)

