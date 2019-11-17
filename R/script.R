create_script <- function(script, path, script_name){

  path <- paste0(path, "/", script_name, ".R")

  for(s in script){
    write(s, path, append = TRUE, sep = " ")
  }

  return(path)

}

