samples <- function(){

  library(inSitu)
  library(sits)
  data(br_mt_1_8K_9classes_6bands)
  bands <- c("evi") # , "ndvi", "nir", "mir"
  samples.tb <- sits_select_bands_(br_mt_1_8K_9classes_6bands, bands = bands)

  return(samples.tb)

}

# svm_model <- sits::sits_svm(cost = 1,
#                       formula = sits_formula_linear())

model <- list(units           = c(512, 512, 512),
              activation       = 'relu',
              dropout_rates    = c(0.50, 0.45, 0.40),
              epochs = 1,
              batch_size = 128,
              validation_split = 0.2)

coverage <- list(service = "EOCUBES",
                 name = "MOD13Q1/006",
                 bands = c("evi"),
                 geom = "~/geom/geom.shp")

# system.file("extdata/MT/shape/MT.shp", package = "inSitu")

cubes <- list(multicores = 4, interval = "48 month", memsize = 1, filter = NULL)


sits.rep::classify("arv_1", samples, "deeplearning", model, coverage, "cubes", cubes)









pos_p <- function(input, output, rds){

  library(raster)
  library(sp)
  library(sits)

  result <- sits_bayes_postprocess(raster_class = rds,
                         window = matrix(1, nrow = 3, ncol = 3, byrow = TRUE),
                         noise = 10,
                         file = output)

  return(result)
}

sits.rep::pos_processing("tree_6/classification", "pos_baseyan", pos_p)

# O último parâmetro, CUBES, pode ser um enum e, por tanto, os parâmetros do
# não obrigatórios do sits_classify_cubes serão usados o default
# sits.rep::sits.rep_classify("tree_1", samples, svm_model, coverage, CUBES)


merge <- function(input, output, rds){

  files_input <- list.files(input, pattern = ".*\\.tif", full.names = TRUE)
  files_years <- gsub("^.*_[^_]{6}_[0-9]+_[0-9]+_[0-9]+_[0-9]+_([0-9]+)_.*\\.tif", "\\1", files_input)

  for (year in unique(files_years)) {

    year_list <- files_input[files_years == year]
    res <- lapply(year_list, raster::raster)
    res$filename <- paste0(output, "_", sprintf("MT_%s.tif", year))
    do.call(raster::merge, res)
  }

}

sits.rep::pos_processing("arv_1/pos_baseyan", "mosaic", merge)
