#------------------------------------------------------------------------------
# Extract
knitr::purl("notebook/sections/intro.Rmd", "notebook/raw_script/extracted/intro.R")
knitr::purl("notebook/sections/prep_hierarchy.Rmd", "notebook/raw_script/extracted/prep_hierarchy.R")
knitr::purl("notebook/sections/prep_carbon.Rmd", "notebook/raw_script/extracted/prep_carbon.R")
knitr::purl("notebook/sections/prep_events.Rmd", "notebook/raw_script/extracted/prep_events.R")
knitr::purl("notebook/sections/prep_station.Rmd", "notebook/raw_script/extracted/prep_station.R")
knitr::purl("notebook/sections/prep_wq.Rmd", "notebook/raw_script/extracted/prep_wq.R")
knitr::purl("notebook/sections/metric_calc.Rmd", "notebook/raw_script/extracted/metric_calc.R")
knitr::purl("notebook/sections/scores_ratings.Rmd", "notebook/raw_script/extracted/scores_ratings.R")
knitr::purl("notebook/sections/map_ratings.Rmd", "notebook/raw_script/extracted/map_ratings.R")
knitr::purl("notebook/sections/validation_old_values.Rmd", "notebook/raw_script/extracted/validation_old_values.R")
knitr::purl("notebook/sections/validation_scoring_disagreement.Rmd", "notebook/raw_script/extracted/validation_scoring_disagreement.R")
knitr::purl("notebook/sections/validation_metric_disagreement.Rmd", "notebook/raw_script/extracted/validation_metric_disagreement.R")
#------------------------------------------------------------------------------
# Run
evaluate <- TRUE
cache.me <- TRUE

source("notebook/raw_script/extracted/intro.R")
source("notebook/raw_script/extracted/prep_hierarchy.R")
source("notebook/raw_script/extracted/prep_carbon.R")
source("notebook/raw_script/extracted/prep_events.R")
source("notebook/raw_script/extracted/prep_station.R")
source("notebook/raw_script/extracted/prep_wq.R")
source("notebook/raw_script/extracted/metric_calc.R")
source("notebook/raw_script/extracted/scores_ratings.R")
source("notebook/raw_script/extracted/map_ratings.R")
source("notebook/raw_script/extracted/validation_old_values.R")
source("notebook/raw_script/extracted/validation_scoring_disagreement.R")
source("notebook/raw_script/extracted/validation_metric_disagreement.R")
#------------------------------------------------------------------------------


