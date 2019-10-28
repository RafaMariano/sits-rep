library(sits)
library(magrittr)

outputDir  <- "MT"


if (!dir.exists(paste(outputDir, "3.Classification_Mosaic", sep = "/")))
  dir.create(paste(outputDir, "3.Classification_Mosaic", sep = "/"), recursive = TRUE)

files_input <- list.files(paste(outputDir, "2.Classification_Smooth", sep = "/"), pattern = ".*\\.tif", full.names = TRUE)
files_years <- gsub("^.*smooth_MT_[^_]{6}_[0-9]+_[0-9]+_[0-9]+_[0-9]+_([0-9]+)_.*\\.tif", "\\1", files_input)

for (year in unique(files_years)) {
  
  year_list <- files_input[files_years == year]
  res <- lapply(year_list, raster::raster)
  res$filename <- paste(paste(outputDir, "3.Classification_Mosaic", sep = "/"), sprintf("MT_%s.tif", year), sep = "/")
  do.call(raster::merge, res)
}