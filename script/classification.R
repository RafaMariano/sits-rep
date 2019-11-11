library(sits)
library(magrittr)
library(inSitu)

#-----------------------------------#
##### 1. Process classification #####
#-----------------------------------#
outputDir <- "MT"

# outputDir  <- "MT" # Output directory
# # classification memory and processors to be used
mem_size   <- 12      # Max memory to be used (in GB)
processors <- 4

# # Create directory
if (!dir.exists(paste(outputDir, "1.Classification", sep = "/")))
  dir.create(paste(outputDir, "1.Classification", sep = "/"), recursive = TRUE)

# # Get samples from inSitu package
data(br_mt_1_8K_9classes_6bands)
#
# # define bands to work with
bands <- c("evi") # , "ndvi", "nir", "mir"
#
# select working bands from time series
samples.tb <- br_mt_1_8K_9classes_6bands %>%
  sits_select_bands_(bands = bands)

# train SVM model using selected bands
# model.svm <-
#   samples.tb %>%
#   sits_train(ml_method = sits_svm(cost = 1, formula = sits_formula_linear()))

model.deeplearning <-  sits_train(samples.tb,
                                  ml_method = sits_deeplearning(
                                    units           = c(512, 512, 512),
                                    activation       = 'relu',
                                    dropout_rates    = c(0.50, 0.45, 0.40),
                                    epochs = 1,
                                    batch_size = 128,
                                    validation_split = 0.2))


# # # create a coverage to classify MT
cov.tb <- sits_coverage(service = "EOCUBES",
                        name = "MOD13Q1/006",
                        bands = bands,
                        # timeline = "48 month",
                         # geom = sf::read_sf(system.file("extdata/MT/shape/MT.shp", package = "inSitu")))
                        geom = sf::read_sf("~/github/mestrado-sits-rep/shape/MT-DUAS-GRADES/clip_mt_2.shp"))
                        #geom = sf::read_sf("~/geom/geom.shp"))

# classify the raster image
rasters.tb <- sits_classify_cubes(file = paste(paste(outputDir, "1.Classification", sep = "/"), "MT", sep = "/"),
                                  coverage = cov.tb,
                                  ml_model = model.deeplearning,
                                  memsize = mem_size,
                                  multicores = processors)
