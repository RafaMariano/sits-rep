hash_result <- function(tree, process){

  library(digest)

  path_principal <- paste0(sits.rep.env$config$DIR_PRINCIPAL, "/", tree, "/", process)
  input <- paste0(path_principal, "/", "result/raster")
  output <- paste0(path_principal, "/", process, "_checksum.txt")

  system(paste0("sha1sum ", input, "/*.* >> ", output))
  return(output)
}
