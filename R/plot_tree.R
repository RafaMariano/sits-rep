#' @export
plot_tree <- function(tree){

  matrix <- get_tree_as_matrix(tree)
  g <- igraph::graph.edgelist(matrix)

  plot(g, edge.arrow.size=.9, edge.arrow.color = "yellow", vertex.label.color="black",
       vertex.label.dist=2.5,
       vertex.color=c("green", "red", "skyblue","skyblue","skyblue" ))

}
