# Acessa o metadado do diretório base
get_base_metadata <- function(){
  return(jsonlite::fromJSON(paste0(".", sep = "/", "metadata.json"),
                            simplifyVector = FALSE))
}

# Acessa o metadado de um processo em outro diretório
get_metadata_another_process <- function(process){
  return(jsonlite::fromJSON(paste0("..", sep = "/", process , "/metadata.json"),
                            simplifyVector = FALSE))
}

# Acessa um metadado de qualquer diretório
get_metadata_of_process <- function(process){
  return(jsonlite::fromJSON(paste0(".", sep = "/", process , "/metadata.json"),
                            simplifyVector = FALSE))
}

# get_metadata <- function(){
#   return(jsonlite::fromJSON(paste0(".", sep = "/", "metadata.json"),
#                      simplifyVector = FALSE))
# }
# 
# get_metadata_by_process <- function(process){
# 
#   return(jsonlite::fromJSON(paste0("..", sep = "/", process, "/", "metadata.json"),
#                             simplifyVector = FALSE))
# }
# 
# get_dir_by_process <- function(process){
#   return(get_metadata_by_process(process)$metadata)
# }
# 
# 
# get_process <- function(){
#   return(jsonlite::fromJSON(paste0( "./", "metadata.json"),
#                             simplifyVector = FALSE)$name)
# }
# get_metadata_by_another_process <- function(process){
#   
#   for (p in get_metadata()$process)
#     if(p$name == process)
#       return(p$metadata)
#   
#   return(NULL)
# }

###################################################################
## Funções para acessar um diretório do processo na base  #########
###################################################################

get_args_of_process <- function(process){
  return(get_metadata_of_process(process)$args)
}

get_hash_of_process <- function(process){
  
  hash <- get_metadata_of_process(process)$hash
  return(paste0("./", process, "/", hash))
}


############################################################################################################
## Funções para voltar o diretório atual e acessar outro processo ##########################################
############################################################################################################

get_rds_by_another_process <- function(process){
  return(get_metadata_another_process(process)$dir_output$rds)
}

get_output_by_another_process <- function(process){

  out <- get_metadata_another_process(process)$dir_output
  if(is.null(out))
    return("./result/raster")

  return(out)
}

##########################################
## Funções para acessar o diretório base #
##########################################
get_process <- function(){
  return(get_base_metadata()$name)
}

get_args <- function(){
  return(get_base_metadata()$args)
}

get_parent <- function(){
  return(get_base_metadata()$parent)
}

get_seed <- function(){
  return(get_base_metadata()$seed)
}

get_script <- function(process){
  metadata <- get_metadata_of_process(process)
  return(paste0("./", metadata$name, "/",metadata$script))
}


hash_result <- function(process){

  output <- paste0("./", process, "/rep_checksum.txt")
  commands <- paste0("sha1sum ", "./", process, "/result/raster", "/*.* >> ", output)
  
  library(digest)
  system(commands)
  
  return(output)
}

# hash_result <- function(process){
#   
#   library(digest)
#   system("sha1sum ./result/raster/*.* >> /rep_checksum.txt")
#   
#   return("/rep_checksum.txt")
# }


# get_result_raster <- function(tree, process){
#   
#   path <- paste0(get_dir_principal(), sep = "/", tree, sep = "/" , process)
#   metadata_json <- jsonlite::fromJSON(paste0(path, sep = "/", get_metadata_json_name())
#                                       , simplifyVector = FALSE)
#   
#   return(list.files(paste0(path, sep = "/", metadata_json$result$raster),
#                     full.names = TRUE))
# }



get_result_rds <- function(){
  
  parent <- get_parent()
  rds <- paste0("../", parent, "/", get_rds_by_another_process(parent))
  return(base::readRDS(rds))
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


is_hash_equals <- function(list_hash_1, list_hash_2){
  
  if(nrow(list_hash_1) != nrow(list_hash_2))
    return(FALSE)
  
  for(pos in 1:nrow(list_hash_1))
    if(list_hash_1[pos,] != list_hash_2[pos,])
      return(FALSE)
  
  return(TRUE)
}

################################################################################################################
################################################################################################################
##########################- FUNÇÕES MAPEADAS DO SITS - ################################################
################################################################################################################
################################################################################################################

sits_train <- function (data.tb, ml_method = sits::sits_svm())
{
  
  train <- NULL 
  
  model_args <- ls(environment(ml_method))
  model_args <- model_args[!model_args %in% c("result", "result_fun", "...")]
  
  if(is_model(model_args, sits::sits_deeplearning)){
    
    keras::use_session_with_seed(42)
    
    train <- data.tb %>%
      sits::sits_train(ml_method = ml_method)
    
    # sits::sits_save_keras(train, "train/model.h5", "train/model.rds")
    
    # keras::use_session_with_seed(42, disable_gpu = FALSE, disable_parallel_cpu = FALSE)
    # train <- sits::sits_load_keras("train/model.h5", "train/model.rds")
    
  }
  # else if (is_model(model_args, sits::sits_svm))
  #   return("sits_svm")
  
  # keras::use_session_with_seed(42, disable_gpu = FALSE, disable_parallel_cpu = FALSE)
  return(train)
}


sits_classify_cubes <- function (file = NULL, coverage = NULL, ml_model = NULL, interval = "12 month",
                                 filter = NULL, memsize = 4, multicores = NULL) {

  if(!dir.exists("./result"))
    dir.create("./result")

  dir.create("./result/raster")

  rasters.tb <- sits::sits_classify_cubes(file = paste0(getwd(), "/", "result/raster/", base::basename(file)),
                                          # file = file,
                                          coverage = coverage,
                                          ml_model = ml_model, interval = interval,
                                          filter = filter, memsize = memsize,
                                          multicores = multicores)

  rds_path <- paste0("./result", "/", "rds")
  if (!dir.exists(rds_path))
    dir.create(rds_path, recursive = TRUE)

  base::saveRDS(list(rasters.tb = rasters.tb),
                file = paste0(rds_path, sep = "/", "classify_cubes.rds"))


  return(rasters.tb)
}

sits_bayes_postprocess <- function(raster_class, 
                                   window = matrix(1, nrow = 3, ncol = 3, byrow = TRUE),
                                   noise = 100, 
                                   file) {
  if(!dir.exists("./result"))
    dir.create("./result")
  
  dir.create("./result/raster")
  sits::sits_bayes_postprocess(raster_class =  get_result_rds()$rasters.tb,
                                         window = window,
                                         noise = noise,
                                         file = paste0( "./result/raster", sep = "/",
                                                       base::basename(file)))

}


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



