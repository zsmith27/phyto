main.dir <- "D:/ZSmith/Projects/PIBI/phyto/data"

phyto.odu <- file.path(main.dir, "ODU_2013_2016_Phytoplankton_Data_07jul17_mod2.csv") %>% 
  data.table::fread(data.table = FALSE)

events.odu <- file.path(main.dir, "ODU_2013_2016_Phytoplankton_Events_07jul17.csv") %>% 
  data.table::fread(data.table = FALSE)

pico.odu <- file.path(main.dir, "ODU_2013_2016_Picoplankton_Data_07jul17.csv") %>% 
  data.table::fread(data.table = FALSE)

phyto.cedar <- file.path(main.dir, "CEDR_CountedMetric_BiologicalTaxonomy_29nov16.csv") %>% 
  data.table::fread(data.table = FALSE)


events.lr <- file.path(main.dir, "LivingResourcesMonitorEventHUC8.csv") %>% 
  data.table::fread(data.table = FALSE)


reported.lr <- file.path(main.dir, "LivingResourcesReportedHUC8.csv") %>% 
  data.table::fread(data.table = FALSE)


station.lr <- file.path(main.dir, "LivingResourcesStationHUC8.csv") %>% 
  data.table::fread(data.table = FALSE)