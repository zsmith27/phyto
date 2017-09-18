#==============================================================================
#==============================================================================
# Author: Zachary M. Smith
# Maintained: Zachary M. Smith
# Created: 07/01/2017
# Updated: 07/01/2017
# Purpose: Create master taxonomic attribute table for phytoplankton.
#==============================================================================
#==============================================================================
# Load dplyr for data manipulation.
library(dplyr)
#------------------------------------------------------------------------------
# Connect to ITIS postgreSQL database.
source("conn_postgres.R")
src_tbls(conn)
#------------------------------------------------------------------------------
hier.df <- tbl(conn, "hierarchy") %>% 
  data.frame()
#------------------------------------------------------------------------------
phyto.list <- list()
phyto.list["diatoms"] <- "630578-969910-969912-969914-969917-2287"
phyto.list["dinoflagellates"] <- "630578-590735-9873"
phyto.list["cyanobacteria"] <- "50-956096-956108"
phyto.list["chlorophyta"] <- "202422-954898-846493"
phyto.list["charophyta"] <- "202422-954898-846494"
phyto.list["idymozoidae"] <- "202423-914154-914155-914160-53963-914169-54556-55189-55621-55653-55857"
phyto.list["mastigophora"] <- "630577-43781-43782"
phyto.list["euglenophyceae "] <- "630577-9601-9602"
phyto.list["methanosarcinales"] <- "630578-590735-1447-1448"
phyto.list["chromista"] <- "630578-590735"
#------------------------------------------------------------------------------
phyto.df <- hier.df %>% 
  filter(grepl(paste(phyto.list, collapse = "|"), hierarchy_string))
#------------------------------------------------------------------------------
syn.df <- tbl(conn, "synonym_links") %>% 
  select(1:2) %>% 
  data.frame()
nodc.df <- tbl(conn, "nodc_ids") %>% 
  select(nodc_id, tsn) %>% 
  data.frame() %>% 
  mutate(nodc_id = gsub("^[0]", "", nodc_id))
bay.df <- data.table::fread("data/LivingResourcesReportedHUC8.csv") %>% 
  # Removes methods with prefix "BE" (Tidal Benthic Taxa Enumeration) or blanks.
  filter(grepl("PH", Method)) %>% 
  rename(tsn = TSN,
         nodc_id = NODCCode) %>% 
  left_join(syn.df, by = "tsn")
test <- bay.df %>% 
  left_join(nodc.df, by = "nodc_id")
bay.tsn <- unique(bay.df$TSN)

#------------------------------------------------------------------------------
# Identify any TSN in the bay dataset that is not found in the phyto data frame.
miss.vec <- setdiff(bay.tsn, phyto.df$tsn)
itis.miss <- hier.df %>% 
  filter(tsn %in% miss.vec)
#------------------------------------------------------------------------------
tsn.vec <- strsplit(phyto.df$hierarchy_string, "-") %>% 
  unlist() %>% 
  unique()
#------------------------------------------------------------------------------
kingdoms <- tbl(conn, "kingdoms") %>% 
  select(1:2) %>% 
  data.frame()
#------------------------------------------------------------------------------
taxa.units <- tbl(conn, "taxonomic_units") %>% 
  select(tsn, name_usage, kingdom_id, rank_id, complete_name) %>% 
  filter(tsn %in% tsn.vec) %>% 
  distinct() %>% 
  data.frame() %>% 
  mutate(complete_name = trimws(complete_name),
         complete_name = gsub(" ", "_", complete_name))
#------------------------------------------------------------------------------  
taxa.types <- tbl(conn, "taxon_unit_types") %>% 
  select(kingdom_id, rank_id, rank_name) %>% 
  distinct() %>% 
  arrange(rank_id) %>% 
  data.frame() %>% 
  left_join(kingdoms, by = "kingdom_id") %>% 
  mutate(rank_name = case_when(
    rank_id == 124 ~ "Section_Animalia",
    rank_id == 126 ~ "Subsection_Animalia",
    rank_id == 200 ~ "Section_Botany",
    rank_id == 210 ~ "Subsection_Botany",
    TRUE ~ rank_name
  ))
#------------------------------------------------------------------------------
taxa.df <- left_join(taxa.units, taxa.types, by = c("kingdom_id", "rank_id")) %>% 
  select(-kingdom_id, -rank_id, -kingdom_name)
#------------------------------------------------------------------------------
test <- strsplit(hier.df$hierarchy_string, "-")[1:2]
x <- 1
test2 <- lapply(seq_along(test), function(x) {
  sub.df <- data.frame(tsn = as.integer(test[[x]]))
  join.df <- left_join(sub.df, taxa.df, by = "tsn")
})