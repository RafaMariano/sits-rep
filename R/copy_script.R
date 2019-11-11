copy_script <- function(from, to){

  new_location_script <-  paste0(to, sep = "/", base::basename(from))
  base::file.copy(from, new_location_script, overwrite = TRUE, copy.date = TRUE)

  return(new_location_script)

}
