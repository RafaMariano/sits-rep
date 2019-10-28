
get_result_raster <- function(tree, process){
  
  path <- paste0(get_dir_principal(), sep = "/", tree, sep = "/" , process)
  metadata_json <- jsonlite::fromJSON(paste0(path, sep = "/", get_metadata_json_name())
                                      , simplifyVector = FALSE)
  
  return(list.files(paste0(path, sep = "/", metadata_json$result$raster),
                    full.names = TRUE))
}

get_result_rds <- function(tree, process){

  path_principal <- paste0(get_dir_principal(), sep = "/", tree, sep = "/" , process)
  metadata_json <- jsonlite::fromJSON(paste0(path_principal, sep = "/", get_metadata_json_name())
                                      , simplifyVector = FALSE)
  
  return(base::readRDS(paste0(path_principal, sep = "/", 
                              metadata_json$result$rds)))
}

new_process <- function(tree, parent = NULL, process_name){
  
  if(!tree_exists(tree))
    stop("tree not exist!!!")
  
  process_name <- gsub('^\\.|/| |\\$|?|@|#|%|&|\\*|\\(|\\)|^|¨', '', process_name)
  new_process_path <- paste0(get_dir_principal(),
                             sep = "/",
                             tree,
                             sep = "/",
                             process_name) 
  
  if(dir.exists(new_process_path))
    stop(paste0("process '", process_name, "' exists in tree '", tree,"'."))
  
  dir.create(new_process_path)
  dir.create(paste0(new_process_path, sep = "/", "result"))
  dir.create(paste0(new_process_path, sep = "/", "result/raster"))
  
  graph_path <- paste0(get_dir_principal(),
                       sep = "/",
                       tree,
                       sep = "/",
                       get_graph_path())
  
  connect_process <- NULL
  if(is.null(parent)){
    
    connect_process <- matrix(c(tree, process_name), nc = 2, byrow = TRUE)
    
  }else {
    if(!parent_exists(tree, parent))
      stop(paste0("process parent not exist in tree '", tree ,"' !!!"))
    
    ls <- read.table(graph_path, stringsAsFactors = TRUE)
    colnames(ls) <- NULL
    
    connect_process <- rbind(as.matrix(ls),
                             matrix(c(parent, process_name), nc = 2, byrow = TRUE))
    
    # connect_process <- matrix(c(parent, process_name), nc = 2, byrow = TRUE)
  }
  
  write.table(connect_process,
              file = graph_path, row.names=FALSE, col.names=FALSE)
  
  return(new_process_path)
}



parent_exists <- function (tree_name, parent){
  
  dir_rep <- get_dir_principal()
  path_parent <- paste0(dir_rep, sep = "/", tree_name, sep = "/", parent)
  
  if (dir.exists(path_parent))
    return(TRUE)
  
  return(FALSE)
}

tree_exists <- function (tree_name){
  
  dir_rep <- get_dir_principal()
  
  if (!dir.exists(dir_rep))
    dir.create(dir_rep, recursive = FALSE)
  
  path_tree <- paste0(dir_rep, sep = "/", tree_name)
  
  if (dir.exists(path_tree))
    return(TRUE)
  
  return(FALSE)
}

start_tree <- function (tree_name = NULL){
  
  dir_rep <- get_dir_principal()
  
  if (!dir.exists(dir_rep))
    dir.create(dir_rep, recursive = FALSE)
  
  if (is.null(tree_name))
    tree_name <- paste0("tree_", length(list.dirs(dir_rep, recursive = FALSE))+1)
  
  dir.create(paste0(dir_rep, sep = "/", tree_name))
  
  return(tree_name)
}

is_arrays_equal <- function(array_1, array_2){
  
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

is_model <- function(model_args_unknown, model_sits){
  
  model_sits_args <- formalArgs(model_sits)
  model_sits_args <- model_sits_args[!model_sits_args %in% c("...")]
  
  # return(all(sort(model_args_unknown) == sort(model_sits_args)))
  return(is_arrays_equal(model_args_unknown, model_sits_args))
}

is_tree_valid <- function(parent){
  
  parent_split <- strsplit(parent, "/")[[1]]
  
  if (parent_split[1] == "")
    return(is_valid_path(parent_split[2]))
  return(is_valid_path(parent_split[1]))
  
  # parent_split <- strsplit(parent, "/")[[1]]
  # 
  # if (parent_split[1] == "~")
  #     return(exist_tree(parent_split[3]))
  # 
  # dir_name_principal <- dirname(get_dir_principal())
  # path_principal <- paste0(dir_name_principal, sep = "/", basename(get_dir_principal()))
  # lenght <- nchar(dir_name_principal) + 8
  # 
  # parent_sub_string <- if (parent_split[1] != "") 
  #               paste0("/", substring(parent, 0, lenght))
  # 
  # 
  # if(parent_sub_string == path_principal)
  #    return(exist_tree(strsplit(substring(parent, lenght + 2), "/")[[1]][1]))
  #   
  # 
  # return(exist_tree(parent_split[1]))
  
}

is_valid_path <- function(path){
  return(dir.exists(paste0(get_dir_principal(), sep = "/", path)))
}


# is_parent_valid <- function(parent){
#   
#   parent_split <- strsplit(parent, "/")[[1]]
#   if (length(parent_split) < 2)
#     return("Parent is not have a process. It is necessary to set process!")
#   
#   pos_inicial_process <- 2
#   if(parent_split[1] == "")
#     pos_inicial_process <- 3
#   
#   parent_directory <- parent_split[pos_inicial_process-1]
#   lenght <- seq(from = pos_inicial_process, to = (length(parent_split)), by = 2)
#   
#   for(pos in lenght){
#     if(is.na(parent_split[pos]) || is.null(parent_split[pos]))
#       return(paste0("Process is NULL!!!
#                      Verify if dir pattern is correct.
#                      Ex pattern: tree_x/process_x/child/process_x/child"))
#     
#     if(is.na(parent_split[pos+1]) || is.null(parent_split[pos+1]))
#       return(paste0("Child is NULL!!!
#                      Verify if child of process '", parent_split[pos],"' is correct.
#                      Ex pattern: tree_x/process_x/child/process_x/child"))
#     
#     
#     parent_directory <- paste0(parent_directory, sep = "/", parent_split[pos],
#                                sep = "/", parent_split[pos+1])
#     is_valid <- is_valid_path(parent_directory)
#     
#     if(is_valid == FALSE)
#       return(paste0("Path not valid! = ", parent_directory))
#   }  
#   
#   return(TRUE)
# }


# parent_directory <- paste0(parent_directory, sep = "/", parent_split[pos_])
# 
# 
# is_valid <- is_valid_path(parent_directory)
# 
# if(is_valid == FALSE)
#   return(parent_split[pos_])
# }


# parent_directory <- parent_split[pos]
# for(pos_ in (pos+1):length(parent_split)){
#   
#   parent_directory <- paste0(parent_directory, sep = "/", parent_split[pos_])
#   is_valid <- is_valid_path(parent_directory)
# 
#   if(is_valid == FALSE)
#     return(parent_split[pos_])
# }


is_path_valid <- function(parent){
  
  if(is_tree_valid(parent) == FALSE)
    stop("tree invalid!!!")
  else
    is_valid <- is_parent_valid(parent)
  if(is_valid != TRUE)
    stop(paste0("parent '", is_valid ,"' invalid!!!"))
  
  return(TRUE)
}

copy_script <- function(from, to){
  
  # dir_name <- paste0(dirname(from), sep = "/" , base::basename(from))
  new_location_script <-  paste0(to, sep = "/", base::basename(from))
  base::file.copy(from, new_location_script, overwrite = TRUE, copy.date = TRUE)
  
  return(new_location_script)
}


# process_dir_create <- function (dir_rep){
#   
#   process_name <- paste0("process_", length(list.dirs(dir_rep, recursive = FALSE))+1)
#   process_dir <- paste0(dir_rep, sep = "/", process_name)
#   dir.create(process_dir)
#   
#   return(process_dir)
# }
# 
# key_dir_create <- function (dir, key_name){
#   
#   key_dir <- paste0(dir, sep = "/", key_name)
#   dir.create(key_dir)
#   
#   return(key_dir)
# }

dir_create <- function (dirname, basename){
  
  dir <- paste0(dirname, sep = "/", basename)
  dir.create(dir)
  
  return(dir)
}

get_config_json <- function(){
  
  PATH_TO_JSON_CONFIG <- "~/.sits-rep"
  FILE_JSON_CONFIG_NAME <- "config.json"
  
  PATH_CONFIG_COMPLETE <- paste0(PATH_TO_JSON_CONFIG, sep = "/", FILE_JSON_CONFIG_NAME)
  
  if (dir.exists(PATH_TO_JSON_CONFIG) == FALSE)
    base::dir.create(PATH_TO_JSON_CONFIG, recursive = FALSE)
  
  if (base::file.exists(PATH_CONFIG_COMPLETE) == FALSE)
    write_json(list(dir_principal = "~/sits-rep", 
                    metadata_name = "metadata_rep.JSON"), PATH_CONFIG_COMPLETE)
  
  return(PATH_CONFIG_COMPLETE)
  
}

get_graph_path <- function(){
  
  config_json <- jsonlite::fromJSON(get_config_json(),
                                    simplifyVector = FALSE)
  return(config_json$graph)
  
}

get_dir_principal <- function (){
  
  config_json <- jsonlite::fromJSON(get_config_json(),
                                    simplifyVector = FALSE)
  return(config_json$dir_principal)
}

get_metadata_json_name <- function (){
  
  config_json <- jsonlite::fromJSON(get_config_json(),
                                    simplifyVector = FALSE)
  
  return(config_json$metadata_name)
}

## nao eh o sentido literal da palavra path_json, só não encontrei um nome de váriavel melhor 
json_append <- function(list_param, path_json, order = 1){
  
  if(base::file.exists(path_json) && 
     base::file.info(path_json)$size > 0){
    
    json_append_exist(list_param, path_json)
    
  }else{
    json_append_new(list_param, path_json)
  }
}

json_is_array <- function (json){
  
  return(substr(jsonlite::toJSON(json), 1, 1) == "[")
}


write_json <- function(json, path){
  
  if (base::is.list(json))
    json <- jsonlite::toJSON(json, 
                             pretty = TRUE, 
                             auto_unbox = TRUE)
  base::write(json, path)
}


json_append_new <- function (list_param, path_json){
  
  write_json(list_param, path_json)
}


json_append_exist <- function (list_param, path_json){
  
  old_json <- jsonlite::fromJSON(path_json, simplifyVector = FALSE)
  key <- names(list_param)
  
  list_temp <- base::sapply(key, function(k, l, o){
    
    if (k %in% names(o)){
      return(base::sapply(k, function(k_, list_p, old_j){
        
        if(json_is_array(old_j)){
          old_j[[length(old_j)+1]] <- list_p
          l = list(old_j)
          
        }else{
          l = list(list(old_j, list_p))
        }
        
        names(l) = k_
        return(l)
        
      }, l[[k]], o[[k]], simplify = TRUE, USE.NAMES = FALSE))
      
    }else{
      return(l[k])
    }
  }, list_param, old_json, simplify = TRUE, USE.NAMES = FALSE)
  
  old_json[key] <- list_temp[key]
  write_json(old_json, path_json)
}



library <- function (package, help, pos = 2, lib.loc = NULL, character.only = FALSE,
                     logical.return = FALSE, warn.conflicts, quietly = FALSE,
                     verbose = getOption("verbose"), mask.ok, exclude, include.only,
                     attach.required = missing(include.only)){
  
  # keras::use_session_with_seed(42)
  # keras::use_session_with_seed(42, disable_gpu = FALSE, disable_parallel_cpu = FALSE)
  
  package <- as.character(substitute(package))
  
  base::library(package, character.only = TRUE)
  packinfo <- installed.packages()[package,]
  
  Imports <- gsub("\n", " " , packinfo["Imports"])
  Imports <- base::strsplit(Imports, ", ")
  
  if (is.na(Imports$Imports[1]) == FALSE){
    list_imp <- base::lapply(Imports$Imports, function(lib){
      
      lib <- base::strsplit(lib, " ")
      if (!is.null(packageDescription(lib[[1]][1])$GithubSHA1))
        return(list(library = lib[[1]][1],
                    version = getNamespaceVersion(lib[[1]][1])[["version"]],
                    git_commit = packageDescription(lib[[1]][1])$GithubSHA1))
      
      
      return(list(library = lib[[1]][1],
                  version = getNamespaceVersion(lib[[1]][1])[["version"]]))
                    # base::toString(utils::packageVersion(lib[[1]][1]))))
    })
  }else{
    list_imp <- list(library = Imports$Imports)
                     # ,version = getNamespaceVersion(Imports$Imports)[["version"]])
  }
  
  import_obj <- list(import = list(package = package,
                                   version = unname(packinfo["Version"]),
                                   dependencies = list_imp))
  
  if (!is.null(packageDescription(package)$GithubSHA1))
    import_obj$import$git_commit <- packageDescription(package)$GithubSHA1
  
  json_append(import_obj, get_metadata_json_name())
  
  
}


sits_deeplearning <- function (data.tb = NULL, units = c(512, 512, 512, 512, 512),
                               activation = "elu", dropout_rates = c(0.5, 0.4, 0.35, 0.3,0.2),
                               optimizer = keras::optimizer_adam(lr = 0.001),
                               epochs = 500, batch_size = 128, validation_split = 0.2,
                               verbose = 1, binary_classification = FALSE){
  
  
  model <- sits::sits_deeplearning(data.tb = data.tb, units = units, activation = activation,
                                   dropout_rates = dropout_rates, optimizer = optimizer,
                                   epochs = epochs, batch_size = batch_size,
                                   validation_split = validation_split, verbose = verbose,
                                   binary_classification = binary_classification)
  
  
  
  return (model)
}


# # 
# # 
# sits_svm <- function (data.tb = NULL, formula = sits::sits_formula_logref(), scale = FALSE,
#                       cachesize = 1000, kernel = "radial", degree = 3, coef0 = 0, cost = 10,
#                       tolerance = 0.001, epsilon = 0.1, cross = 0, ...) {
#   
#   
#   if (!dir.exists("train/"))
#     dir.create("train/", recursive = TRUE)
#   
#   
#   path_rds = paste0("train/", "functions.rds")
#   param_list <- list(model = list(type = "svm", #data.tb = path_rds,
#                                   model = path_rds, formula = path_rds,
#                                   scale = scale, cachesize = cachesize, kernel = kernel,
#                                   degree = degree, coef0 = coef0, cost = cost, tolerance = tolerance,
#                                   epsilon = epsilon, cross = cross, ...))
#   
#   json_append(param_list, get_metadata_json_name())
#   
#   model <- sits::sits_svm(data.tb = data.tb, formula = formula,
#                           scale = scale, cachesize = cachesize,
#                           kernel = kernel, degree = degree,
#                           coef0 = coef0, cost = cost,
#                           tolerance = tolerance, epsilon = epsilon,
#                           cross = cross, ...)
#   
#   if(file.exists("train/functions.rds")){
#     rds <- base::readRDS(path_rds)
#     rds$model <- model
#     rds$formula <- formula
#     base::saveRDS(rds, file=path_rds)
#     
#   } else{
#     base::saveRDS(list(model = model, formula = formula),
#                   file=path_rds)
#   }
#   
#   return(model)
# }
# get_type_model <- function(model){
#   
#   model_args <- ls(environment(model))
#   model_args <- model_args[!model_args %in% c("result", "result_fun", "...")]
#   
#   if(is_model(model_args, sits::sits_deeplearning))
#     return("sits_deeplearning")
#   
#   else if (is_model(model_args, sits::sits_svm))
#     return("sits_svm")
#   
#   return("desconhecido")
# }

sits_train <- function (data.tb, ml_method = sits::sits_svm())
{
  
  train <- NULL 
  if (!dir.exists("train/"))
    dir.create("train/")
  
  
  model_args <- ls(environment(ml_method))
  model_args <- model_args[!model_args %in% c("result", "result_fun", "...")]
  
  if(is_model(model_args, sits::sits_deeplearning)){
    keras::use_session_with_seed(42)
    
    train <- data.tb %>%
      sits::sits_train(ml_method = ml_method)
    
    sits::sits_save_keras(train, "train/model.h5", "train/model.rds")
    
    # keras::use_session_with_seed(42, disable_gpu = FALSE, disable_parallel_cpu = FALSE)
    # train <- sits::sits_load_keras("train/model.h5", "train/model.rds")
    
    
    # device_lib.list_local_devices()
  }
  
  
  # else if (is_model(model_args, sits::sits_svm))
  #   return("sits_svm")
  
  
  json_append(list(train = list(var_name = "train", location = "train/model.rds")), get_metadata_json_name())
  
  # keras::use_session_with_seed(42, disable_gpu = FALSE, disable_parallel_cpu = FALSE)
  return(train)
}
#
# sits_coverage <- function (service = "RASTER", name, timeline = NULL, bands = NULL,
#                            missing_values = NULL, scale_factors = NULL, minimum_values = NULL,
#                            maximum_values = NULL, files = NA, tiles_names = NULL, geom = NULL,
#                            from = NULL, to = NULL) {
# 
#   dir_geom = "other/geom/"
# 
#   if(!dir.exists("other/"))
#     dir.create(dir_geom)
# 
#   sf::write_sf(geom, dsn = dir_geom, layer = "geom", driver = "ESRI Shapefile",
#                delete_layer = TRUE, delete_dsn = TRUE)
# 
#   json <- list(coverage = list(service = service, name = name,
#                                timeline = timeline,
#                                missing_values = missing_values,
#                                scale_factors = scale_factors,
#                                minimum_values = minimum_values,
#                                maximum_values = maximum_values,
#                                files = files, tiles_names = tiles_names,
#                                geom = paste0(file.path(getwd(), dir_geom)),
#                                from = from, to = to))
# 
#   json_append(json, get_metadata_json_name())
#   return(sits::sits_coverage(service, name, timeline, bands,
#                              missing_values, scale_factors, minimum_values,
#                              maximum_values, files, tiles_names, geom,
#                              from, to))
# 
#   ##pegar o intervalo
# }

sits_classify_cubes <- function (file = NULL, coverage = NULL, ml_model = NULL, interval = "12 month",
                                 filter = NULL, memsize = 4, multicores = NULL) {
  

  # dir.create("result/raster")

  path <- paste0(get_dir_principal(), sep = "/", get_tree(), sep = "/", "classification")
  path <- paste0(dirname(path), sep = "/", "classification")

  rasters.tb <- sits::sits_classify_cubes(file = paste0(path, sep = "/", "result/raster", sep = "/", base::basename(file)),
                                          # file = file,
                                          coverage = coverage,
                                          ml_model = ml_model, interval = interval,
                                          filter = filter, memsize = memsize,
                                          multicores = multicores)
  
  # file_basename <- base::basename(base::dirname(file))
  # file_dir_name <- base::dirname(file)
  
  # dir.create("result")
  # file.rename(file_dir_name, "result/raster")
  
  # r <- function(file){
  #   file_ <- base::dirname(file)
  #   if (file_ == ".")
  #     return(file)
  #   
  #   r(file_)
  # }
  # 
  # file.remove(r(file_dir_name))
  # 
  rds_path <- paste0("result", sep = "/", "rds")
  if (!dir.exists(rds_path))
    dir.create(rds_path, recursive = TRUE)
  
  base::saveRDS(list(rasters.tb = rasters.tb),
                file = paste0(rds_path, sep = "/", "classify_cubes.rds"))
  
  json <- list(classification = list(param = list(interval = interval, filter = filter,
                                                  memsize = memsize, multicores = multicores)),
               result = list(raster = paste0("result", sep = "/", "raster"),
                             rds = paste0(rds_path, sep = "/", "classify_cubes.rds")))
  
  
  json_append(json, get_metadata_json_name())
  return(rasters.tb)
}

get_process <- function(){
  
  metadata_json <- paste0(".", sep = "/", get_metadata_json_name())
  json <- jsonlite::fromJSON(metadata_json, simplifyVector = FALSE)
  
  return(json$process)
}

get_parent <- function(){
  
  metadata_json <- paste0(".", sep = "/", get_metadata_json_name())
  json <- jsonlite::fromJSON(metadata_json, simplifyVector = FALSE)

  return(json$parent)
}

get_tree <- function(){
  
  metadata_json <- paste0(".", sep = "/", get_metadata_json_name())
  json <- jsonlite::fromJSON(metadata_json, simplifyVector = FALSE)
  
  return(json$tree)
}

sits_bayes_postprocess <- function(raster_class, 
                                   window = matrix(1, nrow = 3, ncol = 3, byrow = TRUE),
                                   noise = 100, 
                                   file) {
  
  # rds <- get_result_rds("deep_learning", "classification")
  # rds_2 <- rds$rasters.tb
  # 
  
  rds <- get_result_rds(get_tree(), get_parent())
  # dir.create("result/raster")
  path_result <- paste0(get_dir_principal(), sep = "/", get_tree(), sep = "/", get_process())
  
  # print(path_result) 
  # print(base::basename(file))
  # print(paste0(path_result, sep = "/", "result/raster", sep = "/",
  #              base::basename(file)))
  
  # path_result <- paste0(get_dir_principal(), sep = "/", "deep_learning", sep = "/" , "pos_baseyan")
  result <- sits::sits_bayes_postprocess(raster_class = rds$rasters.tb,
                                         window = window,
                                         noise = noise,
                                         file = paste0(path_result, sep = "/", "result/raster", sep = "/",
                                                       base::basename(file)))



  # base::saveRDS(list(rasters.tb = rasters.tb),
  #               file = paste0(rds_path, sep = "/", "classify_cubes.rds"))
  # 
  json <- list(result = list(raster = paste0("result", sep = "/", "raster")))
  json_append(json, get_metadata_json_name())
}

get_relative_path <- function(path, dir_name = "sits_rep"){
  path_split <- strsplit(path, "/")[[1]]

  pos <- 0
  for (x in path_split){
    if(x == dir_name)
      break

    pos <- pos + nchar(x) + 1
  }

  return(substr(path, pos+1, nchar(path)))
}
