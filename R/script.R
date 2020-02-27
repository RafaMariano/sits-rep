create_script <- function(script, tree, process, script_name){

  path_principal <- paste0(sits.rep.env$config$DIR_PRINCIPAL, "/", tree, "/", process, "/", script_name, ".R")
  # path <- paste0(path, "/", script_name, ".R")

  for(s in script){
    write(s, path_principal, append = TRUE, sep = " ")
  }

  return(paste0(script_name, ".R"))

}

