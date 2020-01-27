library(sits)
library(inSitu)

data("br_mt_1_8K_9classes_6bands")
bands <- c("evi")

samples.tb <- sits_select_bands_(br_mt_1_8K_9classes_6bands, bands)

ml_deep_learning = sits_deeplearning( units           = c(512, 512, 512),
                                      activation       = 'relu',
                                      dropout_rates    = c(0.50, 0.45, 0.40),
                                      epochs = 1,
                                      batch_size = 128,
                                      validation_split = 0.2)

ml_model <-  sits_train(samples.tb, ml_deep_learning)

cov.tb <- sits_coverage(service = "EOCUBES",
                        name = "MOD13Q1/006",
                        bands = bands,
                        geom = sf::read_sf("~/geom/geom.shp"))

if (!dir.exists(paste0(getwd(), "/Classification/MT")))
  dir.create(paste0(getwd(), "/Classification/MT"), recursive = TRUE)

rasters.tb <- sits_classify_cubes(file = paste0(getwd(), "/Classification/MT"),
                                  coverage = cov.tb,
                                  ml_model = ml_model,
                                  memsize = 4,
                                  multicores = 4)