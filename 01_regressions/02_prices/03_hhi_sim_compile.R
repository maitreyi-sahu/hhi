## ------------------------
## Compile betas from policy regressions including draw-level uncertainty from value scores
## Author: Sawyer Crosby
## Date: 11/09/2021
## ------------------------

rm(list = ls())
library(tidyverse)
library(data.table)
library(pdftools)

## get timestmap folder
sims_path <- "/ihme/resource_tracking/us_value/data/policy/simulation/"
args <- commandArgs(trailingOnly = TRUE)
stamp <- args[1]
reg_type <- args[2]
note <- args[3]
var <- args[4]
if(is.na(var)){
  var <- ""
}

# stamp <- "27Dec2021_1526_no_ldi_or_race"
sim_path <- paste0(sims_path, "simbetas_", stamp, "_", reg_type, "/")

## read all files
files <- list.files(paste0(sim_path, "/output"), full.names = T)
data <- rbindlist(lapply(files, fread))

## write 
fwrite(data, paste0(sim_path, "/compiled_output.csv"))

## merge all plots
pdf_combine(list.files(paste0(sim_path, "/plots"), pattern="pdf", full.names=TRUE), output  = paste0(sim_path, "/output/plots.pdf"))
