hash_result <- function(tree, process){

  # library(digest)
  tryCatch({

    path_principal <- paste0(sits.rep.env$config$DIR_PRINCIPAL, "/", tree, "/", process)
    input <- paste0(path_principal, "/", sits.rep.env$config$FILE_PATH)
    output <- paste0(path_principal, "/", sits.rep.env$config$HASH_NAME)

    base::system(paste0("sha1sum ", input, "/*.* >> ", output))
    return(TRUE)

  }, error = function(cond){
    stop("The hash file 'checksum.txt could not be created. Check your permissions and try again.")

  })
}


get_hash_file_if_exist <- function(tree, process){

  path_principal <- paste0(sits.rep.env$config$DIR_PRINCIPAL, "/", tree, "/", process, "/", sits.rep.env$config$HASH_NAME)

  if(file.exists(path_principal))
    return(path_principal)

  return(FALSE)

}
