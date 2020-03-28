#' @export
reproduce <- function(tree, process, dir_name){

  dir_rep_process <- .get_dir_rep(tree, process, dir_name)
  list_of_process <- branch_to_json(tree, get_branch_of_tree(tree, process))

  .create_dockerfile(list_of_process, dir_rep_process)

  write("#!/usr/bin/env Rscript", paste0(dir_rep_process, "/script-rep.R"), sep = " ")
  write("source(\"sits-rep-docker.R\")", append = TRUE, paste0(dir_rep_process, "/script-rep.R"), sep = " ")

  for(p in list_of_process){

    process_rep_dir <- paste0(dir_rep_process, "/", p$process)
    dir.create(process_rep_dir)

    json <- list(name = p$process,
                 parent = if(is.null(p$parent))"root" else p$parent,
                 script = p$script)
                 # dir_output = p$result,
                 # metadata = "metadata.JSON")


    json$hash <- .get_hash_path(p$tree, p$process)
    file.copy(json$hash, process_rep_dir)
    file.copy(.get_script_path(p$tree, p$process), process_rep_dir)

    if (!is.null(p$seed))
      json$seed <- p$seed

    # json <- list(name = p$process,
    #              parent = if(is.null(p$parent))"root" else p$parent,
    #              hash = base::basename(p$hash),
    #              script = base::basename(p$script),
    #              dir_output = p$result,
    #              metadata = "metadata.JSON")

    if (!is.null(p$coverage$geom)){

      geom <- .get_geom_path(p$tree, p$process)

      file.copy(base::dirname(geom), process_rep_dir, recursive = TRUE)
      json$coverage <- list(geom = paste0(base::basename(base::dirname(geom)), "/", base::basename(geom)))

    }

    if(!is.null(p$args))
      json$args <- p$args

    json_save(json, process_rep_dir)

    json <- NULL
    json <- list(process = list(name = p$process,
                                dir = paste0("./",p$process),
                                metadata = paste0("./", p$process, "/", "metadata.JSON"),
                                script = paste0("./", p$process, "/",p$script)))

    json_save(json, dir_rep_process)
    write(paste0("execution(\"", p$process,"\")"), append = TRUE, paste0(dir_rep_process, "/script-rep.R"), sep = " ")

  }

  write(paste0("verify_hash()"), append = TRUE, paste0(dir_rep_process, "/script-rep.R"), sep = " ")

  file.copy(system.file("extdata/scripts/algoritms_sits_rep_docker.R", package = "sits.rep"), dir_rep_process)
  file.copy(system.file("extdata/scripts/sits-rep-docker.R", package = "sits.rep"), dir_rep_process)
  file.copy(system.file("extdata/scripts/install_dependencies.R", package = "sits.rep"), dir_rep_process)

  # TODO - TIRAR O dir_name e o dir_principal
  json_save(list(dir_principal = sits.rep.env$config$DIR_PRINCIPAL,
                 dir_name = dir_name), dir_rep_process)

}


# .create_dockerfile <- function(list_of_process, dir_rep_process){
#
#   url <- paste0("curl --silent -f -lSL https://index.docker.io/v1/repositories/rafaelmariano/sits/tags/", getNamespaceVersion("sits")[["version"]])
#
#   if (system(url) == 0){
#     mout_container_exists(paste("R -e \"", .get_install_packages(list_of_process,
#                                                                  install_sits = FALSE), "\""), dir_rep_process, getNamespaceVersion("sits")[["version"]])
#   }else{
#     create_container(paste("R -e \"", .get_install_packages(list_of_process,
#                                                             install_sits = TRUE), "\""), dir_rep_process)
#   }
#
# }


.create_dockerfile <- function(list_of_process, dir_rep_process){

  url <- paste0("curl --silent -f -lSL https://index.docker.io/v1/repositories/rafaelmariano/sits/tags/", getNamespaceVersion("sits")[["version"]])

  if (system(url) == 0)
    mout_container_exists(dir_rep_process, getNamespaceVersion("sits")[["version"]])

  else
    create_container(dir_rep_process)


  json_save(.get_install_packages(list_of_process, install_sits = FALSE), dir_rep_process)

}


.get_install_packages <- function(list_process_json, install_sits = FALSE){

  dep_formated <- NULL
  is_pk <- NULL
  list_install_packages <- list()
  lib_name <- list("sits")

  for (process_json in list_process_json){
    if(is.null(process_json$import))
      next

    if(!is.null(process_json$import$library))
      process_json$import <- list(process_json$import)

    for(imp in process_json$import){
      if(!(imp$library %in% lib_name)){

        pos <- 1
        while(length(imp$dependencies) >= pos){

          if(!(imp$dependencies[[pos]]$library %in% lib_name)){

            lib_name <- append(lib_name, imp$dependencies[[pos]]$library)
            pos <- pos + 1
          } else{

            imp$dependencies[[pos]] <-NULL

          }
        }

        list_install_packages <- append(list_install_packages, list(imp))
        lib_name <- append(lib_name, imp$library)

      }
    }
  }

  return(list(import = list_install_packages))

}


# .get_install_packages <- function(list_process_json, install_sits = FALSE){
#
#   dep_formated <- NULL
#   is_pk <- NULL
#   list_install_packages <- list()
#   lib_name <- list()
#
#   for (process_json in list_process_json){
#     if(is.null(process_json$import))
#       next
#
#     if(!is.null(process_json$import$library))
#       process_json$import <- list(process_json$import)
#
#     for(imp in process_json$import){
#       if(!(imp$library %in% lib_name)){
#
#         pos <- 1
#         while(length(imp$dependencies) >= pos){
#
#           if(!(imp$dependencies[[pos]]$library %in% lib_name)){
#
#             lib_name <- append(lib_name, imp$dependencies[[pos]]$library)
#             pos <- pos + 1
#           } else{
#
#             imp$dependencies[[pos]] <-NULL
#
#           }
#         }
#
#         is_pk <- paste(is_pk, .get_import(imp, install_sits))
#         lib_name <- append(lib_name, imp$library)
#
#       }
#     }
#     dep_formated <- paste(dep_formated, gsub(" ", "", is_pk, fixed = TRUE))
#     list_install_packages <- c(list_install_packages, dep_formated)
#     dep_formated <- NULL
#     is_pk <- NULL
#   }
#
#   return(gsub(" ", "", paste("install.packages('devtools');", paste0(unlist(list_install_packages), collapse = '')), fixed = TRUE))
#
# }


#
# .get_install_packages <- function(list_process_json, install_sits = FALSE){
#
#   dep_formated <- NULL
#   is_pk <- NULL
#   list_install_packages <- list()
#   lib_name <- list()
#
#   for (process_json in list_process_json){
#
#     if(!is.null(process_json$import)){
#       if(!is.null(process_json$import$library)){
#         if(!(process_json$import$library %in% lib_name)){
#           is_pk <- .get_import(process_json$import, install_sits)
#           lib_name <- append(lib_name, process_json$import$library)
#         }
#       }else{
#         for(imp in process_json$import){
#           if(!(imp$library %in% lib_name)){
#             is_pk <- paste(is_pk, .get_import(imp, install_sits))
#             lib_name <- append(lib_name, imp$library)
#           }
#         }
#       }
#       dep_formated <- paste(dep_formated, gsub(" ", "", is_pk, fixed = TRUE))
#       list_install_packages <- c(list_install_packages, dep_formated)
#       dep_formated <- NULL
#       is_pk <- NULL
#     }
#   }
#
#   return(gsub(" ", "", paste("install.packages('devtools');", list_install_packages), fixed = TRUE))
#
# }


.get_import <- function(imp, install_sits){

  is_pk <- NULL
  dep_formated <- NULL

  if(!is.null(packageDescription(imp$library)$Priority))
    if(packageDescription(imp$library)$Priority == 'base')
      return(NULL)

  if(!is.null(imp$dependencies) && length(imp$dependencies) > 0 ){
    dep_formated <- paste(dep_formated,
                          .get_dependencies(imp$dependencies))
  }

  if(imp$library == "sits" && install_sits == FALSE)
    return(gsub(" ", "", dep_formated, fixed = TRUE))

  if(!is.null(imp$git_commit)){
    is_pk <- paste("devtools::install_github('",imp$git_repository, "/", imp$library,"',ref='", imp$git_commit,"');")
  }else{
    is_pk <- paste("devtools::install_version('", imp$library,"',version='", imp$version,"',force = TRUE, upgrade = FALSE, repos = 'http://cloud.r-project.org/');") #,
  }

  return(paste(dep_formated, gsub(" ", "", is_pk, fixed = TRUE)))

}


# .get_import <- function(imp, install_sits){
#
#   is_pk <- NULL
#   dep_formated <- NULL
#
#   if(!is.null(packageDescription(imp$library)$Priority))
#     if(packageDescription(imp$library)$Priority == 'base')
#       return(NULL)
#
#   if(!is.null(imp$dependencies)){
#     dep_formated <- paste(dep_formated,
#                           .get_dependencies(imp$dependencies))
#   }
#
#   if(imp$library == "sits" && install_sits == FALSE)
#     return(gsub(" ", "", dep_formated, fixed = TRUE))
#
#   if(!is.null(imp$git_commit)){
#     is_pk <- paste("devtools::install_github('",imp$git_repository, "/", imp$library,"',ref='", imp$git_commit,"');")
#   }else{
#     is_pk <- paste("devtools::install_version('", imp$library,"',version='", imp$version,"',force = TRUE, upgrade = FALSE, repos = 'http://cloud.r-project.org/');") #,
#   }
#
#   return(paste(dep_formated, gsub(" ", "", is_pk, fixed = TRUE)))
#
# }


.get_dependencies <- function(dependencies){

  dep_formated <- NULL
  for(p in 1:length(dependencies)){

    if (is.null(dependencies[[p]]) || length(dependencies[[p]]$library) == 0)
      next

    if(!is.null(packageDescription(dependencies[[p]]$library)$Priority))
      if(packageDescription(dependencies[[p]]$library)$Priority == 'base')
        next

    if(!is.null(dependencies[[p]]$git_commit))
      dep_formated <- paste(dep_formated,
                            paste("devtools::install_github('",dependencies[[p]]$git_repository, "/", dependencies[[p]]$library,
                                  "',ref='", dependencies[[p]]$git_commit,"');"))
    else
      dep_formated <- paste(dep_formated,
                            paste("devtools::install_version('", dependencies[[p]]$library,
                                  "',version='", dependencies[[p]]$version,"',force = TRUE, upgrade = FALSE, repos = 'http://cloud.r-project.org/');"))
  }

  return(gsub(" ", "", dep_formated, fixed = TRUE))

}


.get_dir_rep <- function(tree, process, dir_name){

  dir_rep <- paste0(sits.rep.env$config$DIR_PRINCIPAL,  "/", tree, "/", sits.rep.env$config$REPRODUCE_NAME, "/", dir_name)

  if(dir.exists(dir_rep))
    stop(paste0("Already exist a reproducible diretory of '", process,"' in tree '", tree,"'."))
  else
    dir.create(dir_rep, recursive = TRUE)

  return(dir_rep)
  # dir_rep_process <- paste0(dir_rep, "/", dir_name)
  # if(dir.exists(dir_rep_process))
  #   stop(paste0("Already exist a reproducible diretory of '", process,"' in tree '", tree,"'."))

  # dir.create(dir_rep_process)

}


.get_hash_path <- function(tree, process){

  path_principal <- paste0(sits.rep.env$config$DIR_PRINCIPAL, "/", tree, "/", process, "/", sits.rep.env$config$HASH_NAME)

  if(!file.exists(path_principal))
    stop(paste0("The ", sits.rep.env$config$HASH_NAME," file does not exist in the process ", process, "."))

  return(path_principal)

}


.get_script_path <- function(tree, process){

  path_principal <- paste0(sits.rep.env$config$DIR_PRINCIPAL, "/", tree, "/", process)
  json <- jsonlite::fromJSON(paste0(path_principal, "/", sits.rep.env$config$METADATA_BASE_NAME), simplifyVector = FALSE)

  return(paste0(path_principal, "/", json$script))

  # if(!file.exists(path_principal))
  #   stop(paste0("The ", sits.rep.env$config$HASH_NAME," file does not exist in the process ", process, "."))
  #
  # return(path_principal)


}


.get_geom_path <- function(tree, process){

  path_principal <- paste0(sits.rep.env$config$DIR_PRINCIPAL, "/", tree, "/", process)
  json <- jsonlite::fromJSON(paste0(path_principal, "/", sits.rep.env$config$METADATA_BASE_NAME), simplifyVector = FALSE)

  return(paste0(path_principal, "/", json$coverage$geom))

}

#' reproduce <- function(tree, process, dir_name){
#'
#'   dir_rep <- paste0(sits.rep.env$config$DIR_PRINCIPAL,
#'                     "/", tree,
#'                     "/", "reproducible")
#'
#'   if(!dir.exists(dir_rep))
#'     dir.create(dir_rep)
#'
#'   dir_rep_process <- paste0(dir_rep, "/", dir_name)
#'   if(dir.exists(dir_rep_process))
#'     stop(paste0("Already exist a reproducible diretory of '", process,"' in tree '", tree,"'."))
#'
#'   dir.create(dir_rep_process)
#'
#'   list_of_process <- branch_to_json(tree, get_branch_of_tree(tree, process))
#'
#'   .create_dockerfile(list_of_process, dir_rep_process)
#'
#'   write("#!/usr/bin/env Rscript", paste0(dir_rep_process, "/script-rep.R"), sep = " ")
#'   write("source(\"sits-rep-docker.R\")", append = TRUE, paste0(dir_rep_process, "/script-rep.R"), sep = " ")
#'
#'   seed <- NULL
#'   for(p in list_of_process){
#'
#'     process_rep_dir <- paste0(dir_rep_process, "/", p$process)
#'     dir.create(process_rep_dir)
#'
#'     file.copy(p$hash, process_rep_dir)
#'     file.copy(p$script, process_rep_dir)
#'
#'     if (!is.null(p$system$seed))
#'       seed <- p$system$seed
#'
#'     json <- list(name = p$process,
#'                  parent = if(is.null(p$parent))"root" else p$parent,
#'                  hash = base::basename(p$hash),
#'                  script = base::basename(p$script),
#'                  dir_output = p$result,
#'                  metadata = "metadata.JSON")
#'
#'     if (!is.null(p$coverage$geom)){
#'       file.copy(base::dirname(p$coverage$geom), process_rep_dir, recursive = TRUE)
#'       json$coverage <- list(geom = paste0(base::basename(base::dirname(p$coverage$geom)),
#'                                           "/", base::basename(p$coverage$geom)))
#'     }
#'
#'     if(!is.null(p$args))
#'       json$args <- p$args
#'
#'     json_save(json, process_rep_dir)
#'
#'     json <- NULL
#'     json <- list(process = list(name = p$process,
#'                                 dir = paste0("./",p$process),
#'                                 metadata = paste0("./", p$process, "/", "metadata.JSON"),
#'                                 script = paste0("./", p$process, "/", base::basename(p$script))))
#'
#'     json_save(json, dir_rep_process)
#'     write(paste0("execution(\"", p$process,"\")"), append = TRUE, paste0(dir_rep_process, "/script-rep.R"), sep = " ")
#'
#'   }
#'
#'   write(paste0("verify_hash()"), append = TRUE, paste0(dir_rep_process, "/script-rep.R"), sep = " ")
#'   file.copy(system.file("extdata/scripts/algoritms_sits_rep_docker.R", package = "sits.rep"), dir_rep_process)
#'   file.copy(system.file("extdata/scripts/sits-rep-docker.R", package = "sits.rep"), dir_rep_process)
#'
#'   # TIRAR O dir_name e o dir_principal
#'   json_save(list(seed = seed,
#'                  dir_principal = sits.rep.env$config$DIR_PRINCIPAL,
#'                  dir_name = dir_name), dir_rep_process)
#'
#' }
#'
#'
#' .create_dockerfile <- function(list_of_process, dir_rep_process){
#'
#'   url <- paste0("curl --silent -f -lSL https://index.docker.io/v1/repositories/rafaelmariano/sits/tags/", getNamespaceVersion("sits")[["version"]])
#'
#'   if (system(url) == 0){
#'     mout_container_exists(paste("R -e \"", .get_install_packages(list_of_process,
#'                                                                 install_sits = FALSE), "\""), dir_rep_process, getNamespaceVersion("sits")[["version"]])
#'   }else{
#'     create_container(paste("R -e \"", .get_install_packages(list_of_process,
#'                                                            install_sits = TRUE), "\""), dir_rep_process)
#'   }
#'
#' }
#'
#'
#' .get_install_packages <- function(list_process_json, install_sits = FALSE){
#'
#'   dep_formated <- NULL
#'   is_pk <- NULL
#'   list_install_packages <- list()
#'   lib_name <- list()
#'
#'   for (process_json in list_process_json){
#'
#'     if(!is.null(process_json$import)){
#'       if(!is.null(process_json$import$library)){
#'         if(!(process_json$import$library %in% lib_name)){
#'           is_pk <- .get_import(process_json$import, install_sits)
#'           lib_name <- append(lib_name, process_json$import$library)
#'         }
#'       }else{
#'         for(imp in process_json$import){
#'           if(!(imp$library %in% lib_name)){
#'             is_pk <- paste(is_pk, .get_import(imp, install_sits))
#'             lib_name <- append(lib_name, imp$library)
#'           }
#'         }
#'       }
#'       dep_formated <- paste(dep_formated, gsub(" ", "", is_pk, fixed = TRUE))
#'       list_install_packages <- c(list_install_packages, dep_formated)
#'       dep_formated <- NULL
#'       is_pk <- NULL
#'     }
#'   }
#'
#'   return(gsub(" ", "", paste("install.packages('devtools');", list_install_packages), fixed = TRUE))
#'
#' }
#'
#'
#' .get_import <- function(imp, install_sits){
#'
#'   is_pk <- NULL
#'   dep_formated <- NULL
#'
#'   if(!is.null(packageDescription(imp$library)$Priority))
#'     if(packageDescription(imp$library)$Priority == 'base')
#'       return(NULL)
#'
#'   if(!is.null(imp$dependencies)){
#'     dep_formated <- paste(dep_formated,
#'                           .get_dependencies(imp$dependencies))
#'   }
#'
#'   if(imp$library == "sits" && install_sits == FALSE)
#'     return(gsub(" ", "", dep_formated, fixed = TRUE))
#'
#'   if(!is.null(imp$git_commit)){
#'     is_pk <- paste("devtools::install_github('",imp$git_repository, "/", imp$library,"',ref='", imp$git_commit,"');")
#'   }else{
#'     is_pk <- paste("devtools::install_version('", imp$library,"',version='", imp$version,"',force = TRUE, upgrade = FALSE, repos = 'http://cloud.r-project.org/');") #,
#'   }
#'
#'   return(paste(dep_formated, gsub(" ", "", is_pk, fixed = TRUE)))
#'
#' }
#'
#'
#' .get_dependencies <- function(dependencies){
#'
#'   dep_formated <- NULL
#'   for(p in 1:length(dependencies)){
#'
#'     if (is.null(dependencies[[p]]) || length(dependencies[[p]]$library) == 0)
#'       next
#'
#'     if(!is.null(packageDescription(dependencies[[p]]$library)$Priority))
#'       if(packageDescription(dependencies[[p]]$library)$Priority == 'base')
#'         next
#'
#'     if(!is.null(dependencies[[p]]$git_commit))
#'       dep_formated <- paste(dep_formated,
#'                             paste("devtools::install_github('",dependencies[[p]]$git_repository, "/", dependencies[[p]]$library,
#'                                   "',ref='", dependencies[[p]]$git_commit,"');"))
#'     else
#'       dep_formated <- paste(dep_formated,
#'                             paste("devtools::install_version('", dependencies[[p]]$library,
#'                                   "',version='", dependencies[[p]]$version,"',force = TRUE, upgrade = FALSE, repos = 'http://cloud.r-project.org/');"))   }
#'   return(gsub(" ", "", dep_formated, fixed = TRUE))
#' }
