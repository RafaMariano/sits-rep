#' @export
buildContainer <- function(reproduce, tag = NULL){

  # Verificar se existe o Python instalado e o reticulate para usar o docker.
  # talvez adicionar uma opção bash ou pacote docker?
  # https://github.com/bhaskarvk/docker

  tree <- getOption("sits.rep.env$CURRENT_TREE")

  if(is.null(tree))
    stop("No tree defined. Use the function 'useTree' to define a tree.")


  reproduce_dir <- .reproduce_dir_exists(tree, reproduce)
  if(reproduce_dir == FALSE)
    stop("Reproduce directory not exist!")

  if(is.null(tag))
    tag <- paste0(tree, "_", reproduce)

  .exec_docker_command(reproduce_dir, tag)
}


.reproduce_dir_exists <- function (tree_name, reproduce_dir){

  dir_rep <- sits.rep.env$config$DIR_PRINCIPAL

  if (!dir.exists(dir_rep))
    dir.create(dir_rep, recursive = FALSE)

  reproduce_dir <- paste0(dir_rep, "/", tree_name, "/reproduce/", reproduce_dir)

  if (dir.exists(reproduce_dir))
    return(reproduce_dir)

  return(FALSE)

}


.exec_docker_command <- function(dockerfile, tag){

  commands <- paste0("docker build -t ", tag, " ", dockerfile)
  library("digest")
  system(commands)

}
