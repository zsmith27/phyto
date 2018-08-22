## ----echo=FALSE----------------------------------------------------------
knitr::opts_chunk$set(eval=run.itis.acquisition)

## ------------------------------------------------------------------------
col.class.vec <- c("samplenumber" = "character",
                   "tsn" = "character",
                   "speccode" = "character",
                   "reportingvalue" = "integer")

bay.temp <- data.table::fread(file.path(project.dir, "data/phytoplankton/cedr_phyto_taxa.csv"),
                            data.table = FALSE,
                            colClasses = col.class.vec)

## ------------------------------------------------------------------------

hier.wide <- lapply(unique(bay.temp$org_tsn), function(tsn.i) {
 #print(tsn.i)
  
  if(ncol(usage(tsn.i)) == 0) return(data.frame(org_tsn = as.integer(tsn.i),
                                                stringsAsFactors = FALSE))
  if(!ritis::usage(tsn.i)$taxonUsageRating %in% c("accepted", "valid")) {
    tsn.accepted <- ritis::accepted_names(as.integer(tsn.i))$acceptedTsn
  } else {
    tsn.accepted <- as.integer(tsn.i)
  }
  
  full.df <- ritis::hierarchy_full(tsn.accepted) %>% 
    select(rankname, taxonname, tsn) %>% 
    slice(1:which(tsn == tsn.accepted)) %>% 
    mutate(org_tsn = as.integer(tsn.i),
           final_tsn = as.integer(tsn.accepted),
           final_id = slice(., which(tsn == tsn.accepted))$taxonname)
}) %>% 
  bind_rows() %>% 
  select(-tsn) %>% 
  spread(rankname, taxonname) %>% 
  clean_up()

## ------------------------------------------------------------------------
column.vec <- c("org_tsn", "final_tsn", "final_id",
                "kingdom", "subkingdom", "infrakingdom", 
                "superdivision", "division", "subdivision", "infradivision",
                "superphylum", "phylum", "subphylum", "infraphylum", 
                "superclass", "class", "subclass", "infraclass", 
                "superorder", "order", "suborder", "infraorder", 
                "superfamily", "family", "subfamily", 
                "tribe", "subtribe", 
                "genus", "subgenus", 
                "species", "subspecies")

column.vec <- column.vec[column.vec %in% names(hier.wide)]

## ------------------------------------------------------------------------
hier.wide <- hier.wide[, column.vec]

## ------------------------------------------------------------------------
dir.create(file.path(project.dir, "data/itis"),
           recursive = TRUE, showWarnings = FALSE)

data.table::fwrite(hier.wide, file.path(rprojroot::find_rstudio_root_file(), "data/itis", "itis_hierarchy.csv"))

## ------------------------------------------------------------------------
rm(col.class.vec, bay.temp, hier.wide, column.vec)

