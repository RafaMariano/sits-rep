#' - Description:
#' This R script was used to do the classification and post processing of the
#' land use and cover change maps for Mato Grosso Brazilian State.
#'
#' - Reference:
#' Gilberto Camara, Michelle Picoli, Adeline Maciel, Rolf Simões,
#' Lorena Santos, and Pedro Andrade
#' Land use and cover change maps for Mato Grosso State in Brazil: 2001-2017.
#' Originally submitted to Nature Scientific Data in May 2019.
#'
#' - Script Usage:
#' Before run this script, open it on any editor or R IDE (e.g. RStudio) to
#' inform the input parameters, in the section `Input Params`.
#' To install the required packages, see section `Installation` below.
#'
#' - Disclaimer:
#' This program is free software: you can redistribute it and/or modify
#' it under the terms of the GNU General Public License as published by
#' the Free Software Foundation, either version 3 of the License, or
#' (at your option) any later version.
#'
#' This program is distributed in the hope that it will be useful,
#' but WITHOUT ANY WARRANTY; without even the implied warranty of
#' MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#' GNU General Public License for more details.
#'
#' You should have received a copy of the GNU General Public License
#' along with this program.  If not, see <https://www.gnu.org/licenses/>.


#----------------------#
##### Installation #####
#----------------------#

# install.packages('devtools')
# install.packages('rgeos')
# install.packages('sf')
# install.packages('raster')
# devtools::install_github("pedro-andrade-inpe/sits.validate")
# devtools::install_github("e-sensing/inSitu")
# devtools::install_github("e-sensing/lucCalculus")
# devtools::install_github("e-sensing/sits")


#----------------------#
##### Input Params #####
#----------------------#

# define output path
outputDir  <- "~/MT" # Output directory

# classification memory and processors to be used
mem_size   <- 12      # Max memory to be used (in GB)
processors <- 1      # For Windows: set processors = 1


#------------------------------#
##### Function definitions #####
#------------------------------#

# Process <- function(outputDir) {
#   
#   library(sits)
#   library(magrittr)
#   library(inSitu)
# 
#   #-----------------------------------#
#   ##### 1. Process classification #####
#   #-----------------------------------#
#   
#   # Create directory
#   if (!dir.exists(paste(outputDir, "1.Classification", sep = "/")))
#     dir.create(paste(outputDir, "1.Classification", sep = "/"), recursive = TRUE)
#   
#   # Get samples from inSitu package
#   data(br_mt_1_8K_9classes_6bands)
#   
#   # define bands to work with
#   bands <- c("evi", "ndvi", "nir", "mir")
#   
#   # select working bands from time series
#   samples.tb <- br_mt_1_8K_9classes_6bands %>%
#     sits_select_bands_(bands = bands)
# 
#   # train SVM model using selected bands
#   model.svm <-
#     samples.tb %>%
#     sits_train(ml_method = sits_svm(cost = 1, formula = sits_formula_linear()))
#   
#   # create a coverage to classify MT
#   cov.tb <- sits_coverage(service = "EOCUBES",
#                           name = "MOD13Q1/006",
#                           bands = bands,
#                           # geom = sf::read_sf(system.file("extdata/MT/shape/MT.shp", package = "inSitu")))
#                           geom = sf::read_sf("~/github/mestrado-sits-rep/shape/MT/clip_mt.shp"))
#   
#   # classify the raster image
#   rasters.tb <- sits_classify_cubes(file = paste(paste(outputDir, "1.Classification", sep = "/"), "MT", sep = "/"),
#                                     coverage = cov.tb,
#                                     ml_model = model.svm,
#                                     memsize = mem_size,
#                                     multicores = processors)
# }

Process <- function(outputDir) {
  
  library(sits)
  library(magrittr)
  library(inSitu)
  
  #-----------------------------------#
  ##### 1. Process classification #####
  #-----------------------------------#
  
  # Create directory
  if (!dir.exists(paste(outputDir, "1.Classification", sep = "/")))
    dir.create(paste(outputDir, "1.Classification", sep = "/"), recursive = TRUE)
  
  # Get samples from inSitu package
  data(br_mt_1_8K_9classes_6bands)
  
  # define bands to work with
  bands <- c("evi", "ndvi", "nir", "mir")
  
  # select working bands from time series
  samples.tb <- br_mt_1_8K_9classes_6bands %>%
    sits_select_bands_(bands = bands)
  
  # train SVM model using selected bands
  model.svm <-
    samples.tb %>%
    sits_train(ml_method = sits_svm(cost = 1, formula = sits_formula_linear()))
  
  # create a coverage to classify MT
  cov.tb <- sits_coverage(service = "EOCUBES",
                          name = "MOD13Q1/006",
                          bands = bands,
                          #geom = sf::read_sf(system.file("extdata/MT/shape/MT.shp", package = "inSitu")))
                          geom = sf::read_sf("~/github/mestrado-sits-rep/shape/MT-DUAS-GRADES/clip_mt_2.shp"))
  
  # classify the raster image
  rasters.tb <- sits_classify_cubes(file = paste(paste(outputDir, "1.Classification", sep = "/"), "MT", sep = "/"),
                                    coverage = cov.tb,
                                    ml_model = model.svm,
                                    memsize = mem_size,
                                    multicores = processors)
  
  #----------------------------------#
  ##### 2. Apply Bayesian Filter #####
  #----------------------------------#

  # Create directory
  if (!dir.exists(paste(outputDir, "2.Classification_Smooth", sep = "/")))
    dir.create(paste(outputDir, "2.Classification_Smooth", sep = "/"), recursive = TRUE)
  
  # smooth parameters
  noise = 10
  window = matrix(1, nrow = 3, ncol = 3, byrow = TRUE)
  
  # apply the Bayesian filter on the output maps
  sits_bayes_postprocess(rasters.tb,
                         window = window,
                         noise = noise,
                         file = paste(paste(outputDir, "2.Classification_Smooth", sep = "/"), "smooth", sep = "/"))

  #---------------------------#
  ##### 3. Mosaic results #####
  #---------------------------#
  
  # Create directory
  # Como vou identificar essa parte se não usa nenhuma função sits? 
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
}


outputDir <- path.expand(outputDir)
if (!dir.exists(outputDir))
  dir.create(outputDir, recursive = TRUE)

Process(outputDir = outputDir)