#' @export
classify <- function(tree, samples, type_model, param_model, coverage, type_classify, param_classify = NULL){

  seed = 42
  set.seed(seed)

  if (tree_exists(tree))
    stop(paste0("Already exist the tree name: '", tree, "'."))

  tree <- start_tree(gsub('^\\.|/| |\\$|?|@|#|%|&|\\*|\\(|\\)|^|¨', '', tree))

  new_process <- new_process(tree = tree,
                             parent = NULL,
                             process_name = "classification")

  script <- list()
  script <- c("library(\"sits\")")
  script <- append(script, " ")
  script <- append(script, paste("samples <- ", (paste0(deparse(samples), collapse = "\n"))))
  script <- append(script, " ")

  script <- append(script, c("args <- commandArgs(trailingOnly = TRUE)"))
  script <- append(script, c("data_output  <- args[1][[1]]"))
  script <- append(script, " ")

  if(type_model == "deeplearning"){

    model <- do.call(sits::sits_deeplearning, param_model)

    p <- base::strsplit(paste0(deparse(param_model), collapse = "\n"), "list")[[1]][2]
    script <- append(script, paste0("model <- sits_deeplearning", p))
    script <- append(script, " ")

  }


  ml_trained <- .sits_train(samples(), model, new_process)
  script <- append(script, "ml_trained <- sits_train(samples(), model)")
  script <- append(script, " ")

  cv <- do.call(.sits_coverage, c(coverage, save_path = new_process))

  # coverage$geom

  cv_p <- base::strsplit(paste0(deparse(coverage), collapse = "\n"), "list")[[1]][2]
  script <- append(script, paste0("cv <- sits_coverage", cv_p))
  script <- append(script, " ")

  if(!is.null(param_classify$file))
    file <- normalizePath(paste0(new_process, "/", base:basename(param_classify$file)), mustWork = FALSE)
  else
    file <- normalizePath(paste0(new_process, "/result/raster/classify"), mustWork = FALSE)


  # param_classify <- append(list(file = file,
                                # coverage = cv, ml_model = ml_trained), param_classify)
  rasters.tb <- NULL
  if(type_classify == "cubes"){

    param <- base::strsplit(paste0(deparse(param_classify), collapse = "\n"), "list|\\(|\\)")[[1]]
    param <- param[sapply(param, function(x){return(x == "")}) == FALSE]

    param <- paste0("file = data_output, coverage = cv, ml_model = ml_trained,", param)

    p <- append(list(file = file, coverage = cv, ml_model = ml_trained), param_classify)
    rasters.tb <- do.call(sits::sits_classify_cubes, p)
    # cl_p <- base::strsplit(paste0(deparse(p), collapse = "\n"), "list")[[1]][2]
    # script <- append(script, paste0("sits_classify_cubes", cl_p))
    script <- append(script, paste0("sits_classify_cubes(", param, ")"))

  }

  base::saveRDS(list(rasters.tb = rasters.tb),
                file = paste0(new_process, "/result/rds/classify_cubes.rds"))


  info_r <- list(system = list(seed = seed,
                               r_version = paste0(R.Version()[c("major","minor")],collapse = "."),
                               arch = R.Version()$arch,
                               platform = R.Version()$platform),
                 classification = list(param = param_classify),
                 result = list(raster = paste0(new_process, "/result/raster"),
                               rds = paste0(new_process, "/result/rds/classify_cubes.rds")),
                 args = list(input = NULL, output = file, rds = FALSE))


  json_save(info_r, new_process)
  json_save(import("sits"), new_process)

  for(f in .get_library(samples))
    json_save(import(f), new_process)

  json_save(list(hash = hash_result(tree, "classification"),
                 script = create_script(script, new_process, "classification")), new_process)

}


.get_library <- function(fun){

  list_library <- list()
  for(f in deparse(fun)){
    f <- gsub(" ", "", f)

    if(grepl("library", f))
      list_library <- append(list_library, base::gsub("library|\\\\|\\(|\\)|\"", "" , f))

  }

  return(list_library)

}


.sits_train <- function (data.tb, ml_method, save_path)
{

  train_path <- paste0(save_path, "/train")

  if (!dir.exists(train_path))
    dir.create(train_path, recursive = TRUE)

  model_args <- ls(environment(ml_method))
  model_args <- model_args[!model_args %in% c("result", "result_fun", "...")]

  train <- NULL
  if(.is_model(model_args, sits::sits_deeplearning)){
    keras::use_session_with_seed(42)

    train <- sits::sits_train(data.tb, ml_method = ml_method)

    sits::sits_save_keras(train, paste0(train_path, "/model.h5"), paste0(train_path, "/model.rds"))

    # keras::use_session_with_seed(42, disable_gpu = FALSE, disable_parallel_cpu = FALSE)
    # train <- sits::sits_load_keras("train/model.h5", "train/model.rds")
    # device_lib.list_local_devices()
  }

  # else if (is_model(model_args, sits::sits_svm))
  #   return("sits_svm")

  json_save(list(train = list(var_name = "train", location = paste0(train_path, "/model.rds"))), save_path)
  return(train)

}


.sits_coverage <- function (service = "RASTER", name, timeline = NULL, bands = NULL,
                           missing_values = NULL, scale_factors = NULL, minimum_values = NULL,
                           maximum_values = NULL, files = NA, tiles_names = NULL, geom = NULL,
                           from = NULL, to = NULL, save_path)
{

  cv_path <- paste0(save_path, "/coverage")

  if(!dir.exists(cv_path))
    dir.create(cv_path, recursive = TRUE)
     # unlink(cv_path, recursive = TRUE)

  if(!dir.exists(paste0(cv_path, "/geom")))
    dir.create(paste0(cv_path, "/geom"))

  if (is.character(geom))
    geom <- sf::read_sf(geom)

  layer <- "geom"
  sf::write_sf(geom, dsn = paste0(cv_path, "/geom"), layer = layer, driver = "ESRI Shapefile",
               delete_layer = TRUE, delete_dsn = TRUE)

  json <- list(coverage = list(service = service,
                               name = name,
                               timeline = timeline,
                               missing_values = missing_values,
                               scale_factors = scale_factors,
                               minimum_values = minimum_values,
                               maximum_values = maximum_values,
                               files = files, tiles_names = tiles_names,
                               geom = paste0(cv_path, "/geom/", layer, ".shp"),
                               from = from, to = to))

  json_save(json, save_path)
  return(sits::sits_coverage(service, name, timeline, bands,
                             missing_values, scale_factors, minimum_values,
                             maximum_values, files, tiles_names, geom,
                             from, to))
  ##pegar o intervalo
}
