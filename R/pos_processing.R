#' @export
pos_processing <- function(parent, process, func){
  print("a")
  # seed = 42
  # set.seed(seed)
  #
  # parent_split <- strsplit(parent, "/")[[1]]
  # tree <- gsub('^\\.|/| |\\$|?|@|#|%|&|\\*|\\(|\\)|^|¨', '', parent_split[1])
  # parent <- gsub('^\\.|/| |\\$|?|@|#|%|&|\\*|\\(|\\)|^|¨', '', parent_split[2])
  #
  # new_process <- new_process(tree = tree, parent = parent, process_name = process)
  #
  # input <- normalizePath(.get_result_raster(tree, parent))
  # output <- normalizePath(paste0(new_process, "/result/raster/", process), mustWork = FALSE)
  # rds <- .get_result_rds(tree, parent)
  #
  # # input <- if(is.null(input)) "NULL" else input
  # ## get_rds() <- colocar dentro do script
  # ## essa função precisa ser implementada no container docker
  # result <- func(input, output, rds)
  #
  #
  # json <- list()
  #
  # script <- c("#!/usr/bin/env Rscript")
  # script <- append(script, " ")
  # script <- append(script, c(paste("func <- ", (paste0(deparse(func), collapse = "\n")))))
  # script <- append(script, " ")
  #
  # script <- append(script, c("args <- commandArgs(trailingOnly = TRUE)"))
  # script <- append(script, c("data_input  <- args[1][[1]]"))
  # script <- append(script, c("data_output <- args[2][[1]]"))
  #
  #
  # if(!is.null(rds)){
  #
  #   script <- append(script, c("data_rds <- args[3][[1]]"))
  #   script <- append(script, " ")
  #   script <- append(script, paste0("func(data_input, data_output, data_rds)", collapse = "\n"))
  #   json$args <- list(input = input, output = output, rds = TRUE)
  #
  # }else{
  #
  #   script <- append(script, " ")
  #   script <- append(script, paste0("func(data_input, data_output, NULL)", collapse = "\n"))
  #   json$args <- list(input = input, output = output, rds = FALSE)
  #
  # }
  #
  #
  # if(!is.null(result)){
  #
  #   base::saveRDS(list(rasters.tb = result),
  #                 file = paste0(new_process, "/result/rds/", process, ".rds"))
  #
  #   json$result$rds <- paste0(new_process, "/result/rds/", process, ".rds")
  #
  # }
  #
  #
  # if(length(list.files(normalizePath(paste0(new_process, "/result/raster/")))) < 1)
  #   stop("Not tiff raster save in output path.")
  #
  #
  # json$result$raster <- paste0(new_process, "/result/raster")
  # json$hash <- hash_result(tree, process)
  # json$script <- create_script(script, new_process, base::basename(new_process))
  #
  # json_save(json, new_process)

}


# .get_env <- function(){
#
#   env <- new.env()
#   env$library <- library
#   env$train <- sits_train
#
#   return(env)
# }
#
#
# .get_result_rds <- function(tree, process){
#
#   path_principal <- paste0(sits.rep.env$config$DIR_PRINCIPAL, "/", tree, "/", process)
#   metadata_json <- jsonlite::fromJSON(paste0(path_principal, "/", sits.rep.env$config$METADATA_BASE_NAME), simplifyVector = FALSE)
#
#   if(!is.null(metadata_json$result$rds))
#     return(base::readRDS(metadata_json$result$rds)$rasters.tb)
#
#   return(NULL)
#
# }
#
#
# .get_result_raster <- function(tree, process){
#
#   path_principal <- paste0(sits.rep.env$config$DIR_PRINCIPAL, "/", tree, "/", process)
#   metadata_json <- jsonlite::fromJSON(paste0(path_principal, "/", sits.rep.env$config$METADATA_BASE_NAME), simplifyVector = FALSE)
#
#   return(metadata_json$result$raster)
#
# }
#
#
# .get_parent_path <- function(tree, process){
#
#   path_principal <- paste0(sits.rep.env$config$DIR_PRINCIPAL, "/", tree, "/", process)
#   json <- jsonlite::fromJSON(paste0(path_principal, "/", sits.rep.env$config$METADATA_BASE_NAME), simplifyVector = FALSE)
#
#   return(paste0(sits.rep.env$config$DIR_PRINCIPAL, "/", json$tree, "/", json$parent))
#
# }

