## -------------------------
## Bootstrap
## -------------------------

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("..")

source("Scripts/PATHS.R")
source("Scripts/GLOBALS.R")
source("Scripts/set_league.R") 

src <- function(...) source(file.path(.scripts, ...))

## -------------------------
## References (run first)
## -------------------------

src("references/clean_references.R")
src("references/load_master_projections.R")
src("references/reliever_prep.R")
src("references/id_xwalk.R")

## -------------------------
## NFBC
## -------------------------

# set_league("nfbc")
# 
# src("nfbc/last_year_results.R")
# src("nfbc/logistic_models.R")
# src("nfbc/prep.R")
# src("nfbc/war.R")
# src("nfbc/export.R")
# src("nfbc/relievers.R")

## -------------------------
## Ottoneu
## -------------------------

set_league("ottoneu")

src("ottoneu/prep.R")
src("ottoneu/values_per_game.R")
src("ottoneu/export.R")

## -------------------------
## Mendoza
## -------------------------

set_league("mendoza")

src("mendoza/prep.R")
src("mendoza/value.R")
src("mendoza/relievers.R")
src("mendoza/export.R")

## -------------------------
## HC Baseballers
## -------------------------

set_league("hc")

src("hc/last_year_results.R")
src("hc/logistic_models.R")
src("hc/prep.R")
src("hc/war.R")
src("hc/export.R")
