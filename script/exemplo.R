samples <- function(){

  library(inSitu)
  library(sits)
  data(br_mt_1_8K_9classes_6bands)

  # # define bands to work with
  bands <- c("evi") # , "ndvi", "nir", "mir"

  # select working bands from time series
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

sits.rep::classify("tree_3", samples, "deeplearning", model, coverage, "cubes", cubes)

# O último parâmetro, CUBES, pode ser um enum e, por tanto, os parâmetros do
# não obrigatórios do sits_classify_cubes serão usados o default
# sits.rep::sits.rep_classify("tree_1", samples, svm_model, coverage, CUBES)
