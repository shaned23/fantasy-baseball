#### SET ENVIRONMENT #####

# packages that are needed. 
libs <- c("tidyverse", "DBI", "RPostgreSQL", "glue","openxlsx",'clipr','knitr','flextable','assertr','scales'
          ,'randomForest','rpart', 'googlesheets4','rstudioapi')

# install and load needed packages
lapply(libs, function(x){
  if(!(x %in% installed.packages())) install.packages(x)
})
lapply(libs,library, character.only = TRUE)

# Clear environment
rm(list = ls())

# Directory of the active script (…/fantasy-baseball/Scripts)
.scripts <- dirname(getActiveDocumentContext()$path)

# Project root (…/fantasy-baseball)
.project_root <- dirname(.scripts)

# Back out of GitHub to Documents
.github_root <- dirname(.project_root)
.documents_root <- dirname(.github_root)

# Now tack on Baseball statistics / 2026
.data_root <- file.path(.documents_root, "Baseball statistics", "2026")

# Build paths
.output <- file.path(.data_root, "Output")
.data   <- file.path(.data_root, "Data")
.proj   <- file.path(.data_root, "Projections")

.nfbc <- file.path(.data, "NFBC standings")
.rp   <- file.path(.data, "Relievers")

# Set working directory to project root
setwd(.project_root)
getwd()