#' @export
pos_processing <- function(tree, parent, process, func){

  # parent_split <- strsplit(parent, "/")[[1]]
  # tree <- gsub('^\\.|/| |\\$|?|@|#|%|&|\\*|\\(|\\)|^|¨', '', parent_split[1])
  # parent <- gsub('^\\.|/| |\\$|?|@|#|%|&|\\*|\\(|\\)|^|¨', '', parent_split[2])
  tryCatch({
      new_process <- new_process(tree = tree, parent = parent, process_name = process)

      seed = .get_seed(tree, parent)
      set.seed(seed)

      input_file <- .get_file_result_if_exist(tree, parent)
      input_rds <- .get_rds_result_if_exist(tree, parent)

      print(input_rds)

      if(identical(input_file, FALSE) && identical(input_rds, FALSE))
        stop("The parent process has no output results (file or rds).")

      output_file <- .get_output_file_path(tree, process)

      if(is.function(func)){
        output_rds <- .execution_function_user(func = func, env = .get_environment(), path = new_process,
                                           input_file = input_file, input_rds = input_rds, output = output_file)

        script <- .create_script(func, tree, process, base::basename(new_process))

      }else if(file.exists(func)){
        # copiar script
        cp_script_name <- copy_script(func, new_process)

        script <- paste0(dirname(cp_script_name), "/", base::basename(new_process))
        file.rename(cp_script_name, script)

        output_rds <- .execution_file_user(file = script, env = .get_environment(), path = new_process,
                                           input_file = input_file, input_rds = input_rds, output = output_file)

      }
      else{
        stop("You need to specify a function or file.")
      }
        # # input <- if(is.null(input)) "NULL" else input
        # get_rds() <- colocar dentro do script
        # ## essa função precisa ser implementada no container docker

      json <- list()
      json$seed <- seed
      json$args <- list(input_file = !identical(input_file, FALSE),
                        input_rds = !identical(input_rds, FALSE))

      json$result$file <- .exist_file_output(tree, process)

      if(!is.null(output_rds)){
        .save_rds(output_rds, tree, process)
        json$result$rds <- TRUE

      }else{
        json$result$rds <- FALSE
      }

      json$hash <- hash_result(tree, process)
      json$script <-
      json_save(json, new_process)

    }, error = function(cond){

      if(delete_path(paste0(tree, "/", process)) == 1)
        message(paste0("It is not possible delete tree directory '", paste0(tree, "/", process), "'."))

      # delete_branch_of_tree(tree = tree, parent = parent, process = process)
      message(paste0("error during processing: ", cond))

    })

}


.create_script <- function(func, tree, process, script_name){
  # script <- list()
  path_principal <- paste0(sits.rep.env$config$DIR_PRINCIPAL, "/", tree, "/", process, "/", script_name, ".R")

  write(c("#!/usr/bin/env Rscript"), path_principal, append = TRUE, sep = " ")
  write(c(" "), path_principal, append = TRUE, sep = " ")
  write(c(paste("func <- ", (paste0(deparse(func), collapse = "\n")))), path_principal, append = TRUE, sep = " ")
  write(c(" "), path_principal, append = TRUE, sep = " ")
  write(c("args <- commandArgs(trailingOnly = TRUE)"), path_principal, append = TRUE, sep = " ")
  # write(c("data_input_raster  <- args[1][[1]]"), path_principal, append = TRUE, sep = " ")
  # write(c("data_output <- args[2][[1]]"), path_principal, append = TRUE, sep = " ")
  # write(c("data_input_rds <- args[3][[1]]"), path_principal, append = TRUE, sep = " ")
  # write(c(" "), path_principal, append = TRUE, sep = " ")
  write(paste0("result <- func(args$input_file, args$input_rds, args$output)", collapse = "\n"), path_principal, append = TRUE, sep = " ")

  # script <- c("#!/usr/bin/env Rscript")
  # script <- append(script, " ")
  # script <- append(script, c(paste("func <- ", (paste0(deparse(func), collapse = "\n")))))
  # script <- append(script, " ")
  # script <- append(script, c("args <- commandArgs(trailingOnly = TRUE)"))
  # script <- append(script, c("data_input_raster  <- args[1][[1]]"))
  # script <- append(script, c("data_output <- args[3][[1]]"))
  # script <- append(script, c("data_input_rds <- args[3][[1]]"))
  # script <- append(script, " ")
  # script <- append(script, paste0("func(data_input_raster, data_input_rds, data_output)", collapse = "\n"))


  # for(s in script){
  #   write(s, path_principal, append = TRUE, sep = " ")
  # }

  return(paste0(script_name, ".R"))

}


.execution_function_user <- function(func, env, path, input_file, input_rds, output){

  # func_1 <- paste0(deparse(func), collapse = "\n")
  # func <- eval(parse(text = func_1), envir = envir)
  # func("aa", "input_rds", "output")
  env
  owd <- getwd()
  on.exit(setwd(owd), add = TRUE)

  setwd(path)
  string_func <- paste0(deparse(func), collapse = "\n")
  func <- eval(parse(text = string_func), envir = env)
  return (func(input_file, input_rds, output))

}


.execution_file_user <- function(file, env, path, input_file, input_rds, output){

  args <- list(input_file = input_file, input_rds = input_rds, output = output)
  env$commandArgs <- function(...) args

  source(file = file, chdir = TRUE, local = env)

  if(!is.logical(env) && !is.null(env$result))
    return(env$result)

  return(NULL)
}


.exist_file_output <- function(tree, process){
  path_principal <- paste0(sits.rep.env$config$DIR_PRINCIPAL, "/", tree, "/", process, "/", sits.rep.env$config$FILE_PATH)

  return(!identical(list.files(path_principal), character(0)))

}


.save_rds <- function(rds_file, tree, process){

  path_principal <- paste0(sits.rep.env$config$DIR_PRINCIPAL, "/", tree, "/", process, "/", sits.rep.env$config$RDS_PATH)
  if(!dir.exists(path_principal))
    dir.create(path_principal, recursive = TRUE)

  base::saveRDS(list(rasters.tb = rds_file),
                file = paste0(path_principal,  "/",  sits.rep.env$config$RDS_NAME))

  # if(!dir.exists(path_principal))
  #   stop("The RDS result directory could not be created. Check your permissions and try again!")

}


.get_environment <- function(){

  # env <- new.env()
  env <- new.env(parent = .GlobalEnv)
  env$library <- library

  return(env)
}


.get_output_file_path <- function(tree, process){

  path_principal <- normalizePath(paste0(sits.rep.env$config$DIR_PRINCIPAL, "/", tree, "/", process, "/", sits.rep.env$config$FILE_PATH), mustWork = FALSE)

  if(!dir.exists(path_principal))
    dir.create(path_principal, recursive = TRUE)

  return(path_principal)

  # stop(paste0("The file result directory could not be created. Check your permissions and try again!."))
  # return(normalizePath(paste0(path, sits.rep.env$config$FILE_PATH, process), mustWork = FALSE))

}


# .get_result_rds <- function(tree, process){
#
#   path_principal <- paste0(sits.rep.env$config$DIR_PRINCIPAL, "/", tree, "/", process)
#   metadata_json <- jsonlite::fromJSON(paste0(path_principal, "/", sits.rep.env$config$METADATA_BASE_NAME), simplifyVector = FALSE)
#
#   if(!is.null(metadata_json$result$rds))
#     return(base::readRDS(paste0(path_principal, "/", metadata_json$result$rds))$rasters.tb)
#
#   return(NULL)
#
# }


# .get_result_raster <- function(tree, process, complete = TRUE){
#
#   path_principal <- paste0(sits.rep.env$config$DIR_PRINCIPAL, "/", tree, "/", process)
#   metadata_json <- jsonlite::fromJSON(paste0(path_principal, "/", sits.rep.env$config$METADATA_BASE_NAME), simplifyVector = FALSE)
#
#   if(complete)
#     return(paste0(path_principal, "/", metadata_json$result$raster))
#
#   return(metadata_json$result$raster)
# }


.get_file_result_if_exist <- function(tree, process){

  path_principal <- paste0(sits.rep.env$config$DIR_PRINCIPAL, "/", tree, "/", process)
  metadata_json <- jsonlite::fromJSON(paste0(path_principal, "/", sits.rep.env$config$METADATA_BASE_NAME), simplifyVector = FALSE)

  if(metadata_json$result$file)
    return(paste0(path_principal, "/", sits.rep.env$config$FILE_PATH))

  return(FALSE)
}


.get_rds_result_if_exist <- function(tree, process){

  path_principal <- paste0(sits.rep.env$config$DIR_PRINCIPAL, "/", tree, "/", process)
  metadata_json <- jsonlite::fromJSON(paste0(path_principal, "/", sits.rep.env$config$METADATA_BASE_NAME), simplifyVector = FALSE)

  if(metadata_json$result$rds)
    return(base::readRDS(paste0(path_principal, "/", sits.rep.env$config$RDS_PATH, "/",  sits.rep.env$config$RDS_NAME)))

  return(FALSE)

}


.get_parent_path <- function(tree, process){

  path_principal <- paste0(sits.rep.env$config$DIR_PRINCIPAL, "/", tree, "/", process)
  json <- jsonlite::fromJSON(paste0(path_principal, "/", sits.rep.env$config$METADATA_BASE_NAME), simplifyVector = FALSE)

  return(paste0(sits.rep.env$config$DIR_PRINCIPAL, "/", json$tree, "/", json$parent))

}


.get_seed <- function(tree, parent){

  json <- jsonlite::fromJSON(paste0(sits.rep.env$config$DIR_PRINCIPAL, "/", tree, "/", parent, "/", sits.rep.env$config$METADATA_BASE_NAME),
                             simplifyVector = FALSE)

  return(json$seed)

}

