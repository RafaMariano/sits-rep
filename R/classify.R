#' @export
classify <- function(script){

  tree <- getOption("sits.rep.env$CURRENT_TREE")

  if(is.null(tree))
    stop("No tree defined. Use the function 'useTree' to define a tree.")


  # if (tree_exists(tree))
  #   stop(paste0("Already exist the tree name: '", tree, "'."))
  #
  # tree <- start_tree(gsub('^\\.|/| |\\$|?|@|#|%|&|\\*|\\(|\\)|^|¨', '', tree))

  tryCatch({

    new_process <- new_process(tree = tree, process_name = sits.rep.env$config$CLASSIFY_PROCESS_DIR_NAME)
    copy_script_path <- copy_script(script, new_process)

    # TODO
    # #gerar uma semente aleatoria


    # Documentação R: Random Number Generation (RNG)
    # RNGversion can be used to set the random generators as they were in an earlier R version (for reproducibility).

    # seed = 42
    seed = sample(0:2^18, 1)
    set.seed(seed)

    info_r <- list(seed = seed,
                   script = base::basename(copy_script_path))

    json_save(info_r, new_process)
    source(file = copy_script_path, chdir = TRUE, local = .get_env(), verbose = FALSE)
    json_save(list(hash = hash_result(tree, sits.rep.env$config$CLASSIFY_PROCESS_DIR_NAME)), new_process)

  }, error = function(cond){

    if(delete_path(tree) == 1)
      message(paste0("It is not possible delete tree directory '", tree, "'."))

    # delete_branch_of_tree(tree = tree, process = sits.rep.env$config$CLASSIFY_PROCESS_DIR_NAME)
    message(cond)

  })

}

.get_env <- function(){

  env <- new.env()
  env$library <- library
  env$train <- sits_train
  env$sits_coverage <- sits_coverage
  env$sits_classify_cubes <- sits_classify_cubes
  # env$sits_deeplearning <- sits_deeplearning
  # env$sits_svm <- sits_svm

  return(env)
}

