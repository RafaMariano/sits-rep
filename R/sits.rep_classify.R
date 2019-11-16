#' @export
sits.rep_classify <- function(tree_name, script){

  if (tree_exists(tree_name))
    stop(paste0("Already exist the tree name: '", tree_name, "'."))

  tree_name <- start_tree(gsub('^\\.|/| |\\$|?|@|#|%|&|\\*|\\(|\\)|^|¨', '', tree_name))

  tryCatch({

    new_process <- new_process(tree = tree_name,
                               parent = NULL,
                               process_name = "classification")

    copy_script_path <- copy_script(script, new_process)

    # TODO
    # #gerar uma semente aleatoria
    seed = 42
    set.seed(seed)

    info_r <- list(system = list(seed = seed,
                   r_version = paste0(R.Version()[c("major","minor")],collapse = "."),
                   arch = R.Version()$arch,
                   platform = R.Version()$platform),
                   script = copy_script_path)


    json_save(info_r, new_process)
    source(file = copy_script_path, chdir = TRUE, local = .get_env())
    json_save(list(hash = hash_result(tree_name, "classification")), new_process)

  }, error = function(cond){

    # if(dir.exists(tree_name))
    #   unlink(tree_name, recursive = TRUE)

    message(cond)
    return(NA)
  })

}

.get_env <- function(){

  env <- new.env()
  env$library <- library
  env$train <- sits_train
  env$sits_coverage <- sits_coverage
  # env$sits_deeplearning <- sits_deeplearning
  # env$sits_svm <- sits_svm

  return(env)
}
