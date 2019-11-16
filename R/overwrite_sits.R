sits_train <- function (data.tb, ml_method = sits::sits_svm())
{

  train <- NULL
  if (!dir.exists("train"))
    dir.create("train")

  model_args <- ls(environment(ml_method))
  model_args <- model_args[!model_args %in% c("result", "result_fun", "...")]

  if(.is_model(model_args, sits::sits_deeplearning)){
    keras::use_session_with_seed(42)

    train <- sits::sits_train(data.tb, ml_method = ml_method)

    sits::sits_save_keras(train, "train/model.h5", "train/model.rds")

    # keras::use_session_with_seed(42, disable_gpu = FALSE, disable_parallel_cpu = FALSE)
    # train <- sits::sits_load_keras("train/model.h5", "train/model.rds")
    # device_lib.list_local_devices()
  }

  # else if (is_model(model_args, sits::sits_svm))
  #   return("sits_svm")

  json_save(list(train = list(var_name = "train", location = "train/model.rds")))
  return(train)

}


sits_coverage <- function (service = "RASTER", name, timeline = NULL, bands = NULL,
                           missing_values = NULL, scale_factors = NULL, minimum_values = NULL,
                           maximum_values = NULL, files = NA, tiles_names = NULL, geom = NULL,
                           from = NULL, to = NULL)
{

  if(dir.exists("coverage"))
    unlink("coverage", recursive = TRUE)

  dir.create("coverage/geom", recursive = TRUE)

  if (is.character(geom))
    geom <- sf::read_sf(geom)

  layer <- "geom"
  sf::write_sf(geom, dsn = "coverage/geom", layer = layer, driver = "ESRI Shapefile",
               delete_layer = TRUE, delete_dsn = TRUE)

  json <- list(coverage = list(service = service,
                               name = name,
                               timeline = timeline,
                               missing_values = missing_values,
                               scale_factors = scale_factors,
                               minimum_values = minimum_values,
                               maximum_values = maximum_values,
                               files = files, tiles_names = tiles_names,
                               geom = paste0(sits.rep.env$config$DIR_PRINCIPAL, "/", get_tree(), "/",
                                             get_process(), "/coverage/geom/", layer, ".shp"),
                               from = from, to = to))

  json_save(json)
  return(sits::sits_coverage(service, name, timeline, bands,
                             missing_values, scale_factors, minimum_values,
                             maximum_values, files, tiles_names, geom,
                             from, to))
  ##pegar o intervalo
}


sits_classify_cubes <- function (file = NULL, coverage = NULL, ml_model = NULL, interval = "12 month",
                                 filter = NULL, memsize = 4, multicores = NULL)
{

  if (!dir.exists("result") || !dir.exists("result/raster"))
    dir.create("result/raster", recursive = TRUE)


  path <- paste0(sits.rep.env$config$DIR_PRINCIPAL, sep = "/", get_tree(), sep = "/", "classification")
  path <- paste0(dirname(path), sep = "/", "classification")

  print(get_tree())
  print(path)

  print( paste0(path, sep = "/", "result/raster", sep = "/", base::basename(file)))

  rasters.tb <- sits::sits_classify_cubes(file = paste0(path, sep = "/", "result/raster", sep = "/", base::basename(file)),
                                          coverage = coverage,
                                          ml_model = ml_model, interval = interval,
                                          filter = filter, memsize = memsize,
                                          multicores = multicores)

  rds_path <- paste0("result", sep = "/", "rds")
  if (!dir.exists(rds_path))
    dir.create(rds_path, recursive = TRUE)

  base::saveRDS(list(rasters.tb = rasters.tb),
                file = paste0(rds_path, sep = "/", "classify_cubes.rds"))

  json <- list(classification = list(param = list(interval = interval, filter = filter,
                                                  memsize = memsize, multicores = multicores)),
               result = list(raster = paste0("result", sep = "/", "raster"),
                             rds = paste0(rds_path, sep = "/", "classify_cubes.rds")))


  json_save(json)
  return(rasters.tb)
}


sits_bayes_postprocess <- function(raster_class,
                                   window = matrix(1, nrow = 3, ncol = 3, byrow = TRUE),
                                   noise = 100,
                                   file)
{

  if(!dir.exists("result") || !dir.exists("result/raster"))
    dir.create("result/raster", recursive = TRUE)

  rds <- get_result_rds(get_tree(), get_parent())
  path_result <- paste0(sits.rep.env$config$DIR_PRINCIPAL, sep = "/", get_tree(), sep = "/", get_process())

  result <- sits::sits_bayes_postprocess(raster_class = rds$rasters.tb,
                                         window = window,
                                         noise = noise,
                                         file = paste0(path_result, sep = "/", "result/raster", sep = "/",
                                                       base::basename(file)))
  # base::saveRDS(list(rasters.tb = rasters.tb),
  #               file = paste0(rds_path, sep = "/", "classify_cubes.rds"))

  json_save(list(result = list(raster = paste0("result", sep = "/", "raster"))))

}


.is_model <- function(model_args_unknown, model_sits){

  model_sits_args <- formalArgs(model_sits)
  model_sits_args <- model_sits_args[!model_sits_args %in% c("...")]

  return(.is_arrays_equal(model_args_unknown, model_sits_args))

}


.is_arrays_equal <- function(array_1, array_2){

  length <- length(array_1)

  if(length != length(array_2))
    return(FALSE)

  array_1 <- sort(array_1)
  array_2 <- sort(array_2)

  for(pos in 1:length)
    if(array_1[pos] != array_2[pos])
      return(FALSE)

  return(TRUE)

}
