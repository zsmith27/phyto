## ----echo=FALSE----------------------------------------------------------
knitr::opts_chunk$set(eval=evaluate, cache=cache.me)

## ------------------------------------------------------------------------
bay.taxa <- bay.df %>% 
  select(unique_id, sampledate, season, salzone,
         surface_chla, chla, pheophytin, doc,
         reportingvalue, biomass, latinname, final_id,
         division, phylum, class, species)  %>% 
  filter(!is.na(salzone)) %>% 
  distinct()

## ------------------------------------------------------------------------
metrics.df <- bay.taxa %>% 
  select(unique_id, surface_chla, chla, doc, pheophytin) %>% 
  distinct() %>% 
  mutate(total_phyto_biomass = taxa_bund(bay.taxa, unique_id, biomass, species),
         total_phyto_biomass_chla_ratio = total_phyto_biomass / chla,
         pct_cryptophyte = taxa_pct(bay.taxa, unique_id, biomass, division, "cryptophycophyta"),
         cyanophyte_biomass = taxa_abund(bay.taxa, unique_id, biomass, phylum, "cyanobacteria"), 
         diatom_biomass = taxa_abund(bay.taxa, unique_id, biomass, class, "bacillariophyceae"),
         
         dinoflagellate_biomass = taxa_abund(bay.taxa, unique_id, biomass, division, "pyrrophycophyta"),
         scrippsiella_precaria_biomass = taxa_abund(bay.taxa, unique_id,
                                                    biomass, latinname, "scrippsiella_precaria"),
         dinoflagellate_biomass = dinoflagellate_biomass - scrippsiella_precaria_biomass,
         microcystis_aeruginosa_abundance = taxa_abund(bay.taxa, unique_id,
                                                       reportingvalue, species, "microcystis_aeruginosa"),
         #picoplankton_abundance = taxa_abund(bay.taxa, unique_id, reportingvalue, picoplankton, TRUE),
         prorocentrum_minimum_abundance = taxa_abund(bay.taxa, unique_id,
                                                     reportingvalue, species, "prorocentrum_minimum")
         ) %>% 
  select(-scrippsiella_precaria_biomass)

## ------------------------------------------------------------------------
metrics.long <- metrics.df %>% 
  gather(metric, value, "surface_chla":"prorocentrum_minimum_abundance")

