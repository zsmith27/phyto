## ----echo=FALSE----------------------------------------------------------
knitr::opts_chunk$set(eval=run.cedr.acquisition)

## ------------------------------------------------------------------------
url.root <- "http://datahub.chesapeakebay.net/api.JSON"
todays.date <- format(Sys.Date(), "%m-%d-%Y")

## ------------------------------------------------------------------------
station.vec <- file.path(url.root,
                       "LivingResources",
                       "TidalPlankton",
                       "Reported",
                       "1-01-1970",
                       todays.date,
                       "17",
                       "Station") %>% 
  fromJSON() %>% 
  pull(unique(MonitoringLocationId))

## ------------------------------------------------------------------------
phyto.df <- file.path(url.root,
                      "LivingResources",
                      "TidalPlankton",
                      "Reported",
                      "1-01-1970",
                      todays.date, 
                      "17",
                      "Station",
                      paste(station.vec, collapse = ",")) %>%
  fromJSON() %>% 
  clean_up()

## ------------------------------------------------------------------------
phyto.df <- phyto.df %>% 
  filter(layer %in% c("ap", "wc"),
         !is.na(reportingvalue)) %>% 
  mutate(sampledate = as.Date(sampledate))

## ------------------------------------------------------------------------
phyto.df <- phyto.df %>% 
  mutate(
    tsn = as.integer(tsn),
    org_tsn = case_when(
      latinname == "navicula_notablis" ~ as.integer(4327),
      latinname == "pleurosigma_macrum" ~ as.integer(4650),
      latinname == "pleurosigma_obscurum" ~ as.integer(591383),
      latinname == "polykrikos_hartmannii" ~ as.integer(331299),
      latinname == "protoperidinium_aciculiderum" ~ as.integer(10329),
      latinname == "protoperidinium_paulseni" ~ as.integer(3568),
      latinname == "scrippsiella_favionese" ~ as.integer(10537),
      latinname == "tetrastrum_caudatum" ~ as.integer(5691),
      latinname == "didymocystis" ~ as.integer(5810),
      latinname == "lauterborniella_elegantissima" ~ as.integer(6097),
      latinname == "characium_sp." ~ as.integer(5756),
      latinname == "cylindrospermopsis_sp." ~ as.integer(203689),
      latinname == "chaetoceros_neogracilis" ~ as.integer(1004011),
      latinname == "navicula_retusa_cancellata" ~ as.integer(1020372),
      latinname == "karlodinium_micrum" ~ as.integer(180904),
      latinname == "lagerheimia" ~ as.integer(6017),
      latinname == "quadricoccus_euryhalinicus" ~ as.integer(957939),
      latinname == "scrippsiella_precaria" ~ as.integer(10536),
      latinname == "psuedosolenia_calcar-avis" ~ as.integer(970064),
      latinname == "centronella" ~ as.integer(970064),
      latinname == "amphidinium_tatrae" ~ as.integer(9997),
      latinname == "navicula_lata" ~ as.integer(4450),
      latinname == "nitzschia_vitrea_recta" ~ as.integer(5204),
      latinname == "rhaphoneis_gemmifera" ~ as.integer(3145),
      latinname == "delphineis_surirella" ~ as.integer(969978),
      latinname == "navicula_annulata" ~ as.integer(3649),
      latinname == "proboscia_alata_gracillima" ~ as.integer(610099),
      latinname == "guinardia_striata" ~ as.integer(2921),
      latinname == "guinardia_cylindrus" ~ as.integer(2921),
      latinname == "aphanizomenon_issatschenkoi" ~ as.integer(1191),
      latinname == "helicotheca_tamesis" ~ as.integer(590815),
      latinname == "corethron_valdivae" ~ as.integer(2386),
      latinname == "gonyaulax_conjuncta" ~ as.integer(10359),
      latinname == "lioloma_delicatulum" ~ as.integer(573597),
      latinname == "syracosphaera_histrica" ~ as.integer(2234),
      latinname == "rhizosolenia_formosa" ~ as.integer(2879),
      latinname == "proboscla_alata_curvirostris" ~ as.integer(610099),
      latinname == "membraneis_challengeri" ~ as.integer(3648),
      latinname == "chrysococcus_tesselatus" ~ as.integer(1751),
      latinname == "rhoicosphenia_abbreviata" ~ as.integer(3633),
      latinname == "protoperidinium_aciculiferum" ~ as.integer(10340),
      latinname == "protoperidinium_fimbriatum" ~ as.integer(10340),
      latinname == "licmophora_inflata" ~ as.integer(3155),
      latinname == "biddulphia_reticulata" ~ as.integer(2678),
      latinname == "caloneis_lepidula" ~ as.integer(4369),
      latinname == "caloneis_trinodis" ~ as.integer(4369),
      latinname == "amphiprora_cholnokyi" ~ as.integer(4674),
      latinname == "navicula_interrupta" ~ as.integer(3649),
      latinname == "cerataulus_radiatus" ~ as.integer(2709),
      latinname == "gyrosigma_balticum_silimis" ~ as.integer(4623),
      latinname == "dictyocha_siderea" ~ as.integer(1804),
      latinname == "odontella_alternans" ~ as.integer(573604),
      latinname == "nitzschia_vitrea_salinarum" ~ as.integer(5204),
      latinname == "proboscla_alata_indica" ~ as.integer(610099),
      latinname == "attheya_decora" ~ as.integer(2876),
      latinname == "synedra_closterioides" ~ as.integer(970065),
      latinname == "trinacria_regina" ~ as.integer(2747),
      latinname == "chattonella" ~ as.integer(969917),
      latinname == "chattonella_subsalsa" ~ as.integer(969917),
      latinname == "heterosigma_akashiwo" ~ as.integer(969917),
      latinname == "vibrio_fisheri" ~ as.integer(959178),
      TRUE ~ as.integer(tsn)
    )
  )

## ------------------------------------------------------------------------
dir.create(file.path(project.dir, "data/phytoplankton"),
           recursive = TRUE, showWarnings = FALSE)
phyto.df %>% 
  mutate(reportingvalue = as.character(reportingvalue)) %>% 
data.table::fwrite(file.path(rprojroot::find_rstudio_root_file(), "data/phytoplankton", "cedr_phyto_taxa.csv"))

## ------------------------------------------------------------------------
event.df <- file.path(url.root,
                      "LivingResources",
                      "TidalPlankton",
                      "MonitorEvent",
                      "1-01-1970",
                      todays.date, 
                      "17",
                      "Station",
                      paste(station.vec, collapse = ",")) %>%
  fromJSON() %>% 
  clean_up()

## ------------------------------------------------------------------------
dir.create(file.path(rprojroot::find_rstudio_root_file(), "data/phytoplankton"),
           recursive = TRUE, showWarnings = FALSE)

data.table::fwrite(event.df, file.path(rprojroot::find_rstudio_root_file(), "data/phytoplankton", "cedr_phyto_event.csv"))

## ------------------------------------------------------------------------
station.df <- file.path(url.root,
                        "LivingResources",
                        "TidalPlankton",
                        "Station",
                        "station",
                        paste(station.vec, collapse = ",")) %>%
  fromJSON() %>% 
  clean_up()

## ------------------------------------------------------------------------
dir.create(file.path(rprojroot::find_rstudio_root_file(), "data/phytoplankton"),
           recursive = TRUE, showWarnings = FALSE)

data.table::fwrite(station.df, file.path(rprojroot::find_rstudio_root_file(), "data/phytoplankton", "cedr_phyto_station.csv"))

## ------------------------------------------------------------------------
wq.df <- file.path(url.root,
                   "WaterQuality",
                   "WaterQuality",
                   format(min(phyto.df$sampledate) - days(3), "%m-%d-%Y"),
                   format(max(phyto.df$sampledate) + days(3), "%m-%d-%Y"), 
                   "6",
                   "7,16,23,24",
                   "station",
                   paste(station.vec, collapse = ","),
                   "21,34,74,83") %>% 
  fromJSON() %>% 
  clean_up()

## ------------------------------------------------------------------------
dir.create(file.path(rprojroot::find_rstudio_root_file(), "data/water_quality"),
           recursive = TRUE, showWarnings = FALSE)

data.table::fwrite(wq.df, file.path(rprojroot::find_rstudio_root_file(), "data/water_quality", "cedr_wq.csv"))

## ------------------------------------------------------------------------
rm(run.cedr.acquisition, url.root, todays.date, station.vec, phyto.df, event.df, station.df, wq.df)

