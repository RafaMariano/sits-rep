#' @export
useTree <- function(tree_name){

  if(!tree_exists(tree_name))
    tree_name = start_tree(gsub('^\\.|/| |\\$|?|@|#|%|&|\\*|\\(|\\)|^|¨', '', tree_name))

  options("sits.rep.env$CURRENT_TREE"=tree_name)
  return(tree_name)
}

