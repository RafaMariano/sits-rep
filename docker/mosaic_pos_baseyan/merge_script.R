#!/usr/bin/env Rscript

library(sits)
library(magrittr)

args <- commandArgs(trailingOnly = TRUE)

data_input  <- args[1][[1]]
data_output <- args[2][[1]]

if (!dir.exists(data_input))
  dir.create(data_input, recursive = TRUE)

files_input <- list.files(data_output, pattern = ".*\\.tif", full.names = TRUE)
files_years <- gsub("^.*smooth_MT_[^_]{6}_[0-9]+_[0-9]+_[0-9]+_[0-9]+_([0-9]+)_.*\\.tif", "\\1", files_input)

for (year in unique(files_years)) {

  year_list <- files_input[files_years == year]
  res <- lapply(year_list, raster::raster)
  res$filename <- paste(paste(data_input, sep = "/"), sprintf("mosaic_%s.tif", year), sep = "/")
  do.call(raster::merge, res)
}
