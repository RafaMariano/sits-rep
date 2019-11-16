execution <- function(process){
  
  source("./algoritms_sits_rep_docker.R")
  
  my_env <- FALSE
  set.seed(get_seed())
  keras::use_session_with_seed(get_seed())
  
  args <- get_args_of_process(process)
  if(!is.null(args)){
    my_env <- new.env()
    my_env$commandArgs <- function(...) args
  }
  
  source(file = get_script(process), chdir = TRUE, local = my_env)
  hash_rep <- hash_result(process)
  
  json_append(list(reproducible = list(name = process,
                                       hash_original = get_hash_of_process(process),
                                       hash_rep = hash_rep)), "./metadata.JSON")
  
  json_append(list(hash_rep = base::basename(hash_rep)), paste0("./", process, "/metadata.JSON"))
}



verify_hash <- function(){
  
  source("./algoritms_sits_rep_docker.R")
  
  metadata <- get_base_metadata()
  json <- list()
  
  for(p in metadata$reproducible){
    
    result <- is_hash_equals(read.table(p$hash_original, sep=" ")[1],
                             read.table(p$hash_rep, sep=" ")[1])
    
    json <- append(json, list(append(p, list(is_reproducible = result))))
    
    if(result)
      print(paste("O processo ", p$name, " foi reproduzido com sucesso!!!"))
    else
      print(paste("O processo ", p$name, " não foi reproduzido com sucesso!!!"))
  }
  
  metadata$reproducible <- json
  write(jsonlite::toJSON(metadata, 
                         pretty = TRUE, 
                         auto_unbox = TRUE), "./metadata.JSON")
  
}


