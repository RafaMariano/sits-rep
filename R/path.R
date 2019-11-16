get_relative_path <- function(path, dir_name = "sits_rep"){

  path_split <- strsplit(path, "/")[[1]]

  pos <- 0
  for (x in path_split){
    if(x == dir_name)
      break

    pos <- pos + nchar(x) + 1
  }

  return(substr(path, pos+1, nchar(path)))

}
