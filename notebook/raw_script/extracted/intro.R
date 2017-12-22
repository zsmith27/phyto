## ----echo=FALSE----------------------------------------------------------
knitr::opts_chunk$set(eval=evaluate, cache=cache.me)

## ----load_packages, message=FALSE----------------------------------------
library(dplyr)
library(tidyr)
library(ggplot2)
library(purrr)
library(stringr)
library(leaflet)
library(lubridate)
library(readxl)
library(httr)
library(RCurl)
library(data.table)
library(jsonlite)
library(rprojroot)
library(ritis)

## ------------------------------------------------------------------------
library(mmir)

## ------------------------------------------------------------------------
sessionInfo()

## ------------------------------------------------------------------------
project.dir <- rprojroot::find_rstudio_root_file()

## ------------------------------------------------------------------------
clean_string <- function(x) {
  x %>% 
    stringr::str_trim() %>% 
    tolower() %>% 
    stringr::str_replace_all("\\s+", " ") %>% 
    stringr::str_replace_all(" ", "_") %>%  
    if_else(. == "", as.character(NA), .)
}

## ------------------------------------------------------------------------
clean_up <- function(x) {
  x %>% 
    rename_all(clean_string) %>% 
    mutate_if(is.character, funs(clean_string))%>% 
    distinct()
}

