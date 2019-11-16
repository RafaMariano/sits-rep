tree_exists <- function (tree_name){

  dir_rep <- sits.rep.env$config$DIR_PRINCIPAL

  if (!dir.exists(dir_rep))
    dir.create(dir_rep, recursive = FALSE)

  path_tree <- paste0(dir_rep, sep = "/", tree_name)

  if (dir.exists(path_tree))
    return(TRUE)

  return(FALSE)

}


start_tree <- function (tree_name = NULL){

  dir_rep <- sits.rep.env$config$DIR_PRINCIPAL

  if (!dir.exists(dir_rep))
    dir.create(dir_rep, recursive = FALSE)

  if (is.null(tree_name))
    tree_name <- paste0("tree_", length(list.dirs(dir_rep, recursive = FALSE))+1)

  dir.create(paste0(dir_rep, sep = "/", tree_name))

  return(tree_name)

}


new_process <- function(tree, parent = NULL, process_name){

  if(!tree_exists(tree))
    stop("Tree not exist.")

  process_name <- gsub('^\\.|/| |\\$|?|@|#|%|&|\\*|\\(|\\)|^|¨', '', process_name)
  new_process_path <- paste0(sits.rep.env$config$DIR_PRINCIPAL,
                             sep = "/",
                             tree,
                             sep = "/",
                             process_name)

  if(dir.exists(new_process_path))
    stop(paste0("ALready exist this process '", process_name, "' in tree '", tree,"'."))

  dir.create(new_process_path)
  dir.create(paste0(new_process_path, sep = "/", "result"))
  dir.create(paste0(new_process_path, sep = "/", "result/raster"))
  dir.create(paste0(new_process_path, sep = "/", "result/rds"))

  json_save(list(tree = tree,
                 process = process_name), new_process_path)

  graph_path <- paste0(sits.rep.env$config$DIR_PRINCIPAL,
                       sep = "/",
                       tree,
                       sep = "/",
                       sits.rep.env$config$GRAPH_BASE_NAME)

  connect_process <- NULL
  if(is.null(parent)){

    connect_process <- matrix(c(tree, process_name), nc = 2, byrow = TRUE)

  }else {

    if(!.parent_exists(tree, parent))
      stop(paste0("The parent not exist in tree '", tree ,"' !!!"))

    ls <- read.table(graph_path, stringsAsFactors = TRUE)
    colnames(ls) <- NULL

    connect_process <- rbind(as.matrix(ls),
                             matrix(c(parent, process_name), nc = 2, byrow = TRUE))

  }

  write.table(connect_process,
              file = graph_path, row.names=FALSE, col.names=FALSE)

  return(new_process_path)

}


get_process <- function(){

  metadata_json <- paste0(".", sep = "/", sits.rep.env$config$METADATA_BASE_NAME)
  json <- jsonlite::fromJSON(metadata_json, simplifyVector = FALSE)

  return(json$process)

}


get_parent <- function(){

  metadata_json <- paste0(".", sep = "/", sits.rep.env$config$METADATA_BASE_NAME)
  json <- jsonlite::fromJSON(metadata_json, simplifyVector = FALSE)

  return(json$parent)

}


get_tree <- function(){

  metadata_json <- paste0(".", sep = "/", sits.rep.env$config$METADATA_BASE_NAME)
  json <- jsonlite::fromJSON(metadata_json, simplifyVector = FALSE)

  return(json$tree)

}


get_result_raster <- function(tree, process){

  path <- paste0(sits.rep.env$config$DIR_PRINCIPAL, sep = "/", tree, sep = "/" , process)
  metadata_json <- jsonlite::fromJSON(paste0(path, sep = "/", sits.rep.env$config$METADATA_BASE_NAME), simplifyVector = FALSE)

  return(list.files(paste0(path, sep = "/", metadata_json$result$raster),
                    full.names = TRUE))
}


get_result_rds <- function(tree, process){

  path_principal <- paste0(sits.rep.env$config$DIR_PRINCIPAL, sep = "/", tree, sep = "/" , process)
  metadata_json <- jsonlite::fromJSON(paste0(path_principal, sep = "/", sits.rep.env$config$METADATA_BASE_NAME), simplifyVector = FALSE)

  return(base::readRDS(paste0(path_principal, sep = "/",
                              metadata_json$result$rds)))

}


get_branch_of_tree <- function(tree, process){

  ls <- get_tree_as_matrix(tree)

  list_p <- NULL
  process_aux <- process

  for(pos in nrow(ls):1){

    if (ls[pos, ][2] == process_aux){
      list_p <- c(list_p, ls[pos, ][2])
      process_aux <- ls[pos, ][1]
    }
  }

  return(rev(list_p))

}


get_tree_as_matrix <- function(tree){

  ls <- read.table(paste(sits.rep.env$config$DIR_PRINCIPAL, tree, sits.rep.env$config$GRAPH_BASE_NAME, sep = "/"),
                   stringsAsFactors = TRUE)
  colnames(ls) <- NULL
  return(as.matrix(ls))

}


branch_to_json <- function(tree, list_process){

  list_json <- list()
  for(p in list_process){
    path_json <- paste(sits.rep.env$config$DIR_PRINCIPAL, tree, p, sits.rep.env$config$METADATA_BASE_NAME, sep = "/")
    list_process <- jsonlite::fromJSON(path_json, simplifyVector = FALSE)
    list_json <- append(list_json, list(list_process))
  }

  return(list_json)
}


.parent_exists <- function (tree_name, parent){

  dir_rep <- sits.rep.env$config$DIR_PRINCIPAL
  path_parent <- paste0(dir_rep, sep = "/", tree_name, sep = "/", parent)

  if (dir.exists(path_parent))
    return(TRUE)

  return(FALSE)

}
