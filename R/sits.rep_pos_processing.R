#' @export
sits.rep_pos_processing <- function(parent, process, script){

  parent_split <- strsplit(parent, "/")[[1]]
  tree <- gsub('^\\.|/| |\\$|?|@|#|%|&|\\*|\\(|\\)|^|¨', '', parent_split[1])
  parent <- gsub('^\\.|/| |\\$|?|@|#|%|&|\\*|\\(|\\)|^|¨', '', parent_split[2])

  new_process <- new_process(tree = tree, parent = parent, process_name = process)
  copy_script_path <- copy_script(script, new_process)

  # TODO - Gerar uma semente aleatoria
  seed = 42
  set.seed(seed)

  json_save(list(tree = parent_split[1],
                   parent = parent_split[2],
                   process = process), new_process)

  source(file = copy_script_path, chdir = TRUE, local = .get_env())
  json_save(list(hash = hash_result(tree, process),
                   script = copy_script_path), new_process)

}


.get_env <- function(){

  env <- new.env()
  env$library <- library
  env$train <- sits_bayes_postprocess

  return(env)
}
