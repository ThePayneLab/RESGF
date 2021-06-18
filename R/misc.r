.onAttach <- function(libname,pkgname) {
  resgf_set_indexNode("http://esgf-node.llnl.gov/esg-search")
  message(sprintf("RESGF starting with %s as the default index node.",resgf_get_indexNode()))
} 

#' Get the address of the default index node
#'
#' @return A text url of the currently configured default index node
#' 
#' @details This value is stored as the option `resgf.indexNode`
#' @name default
#' @export
resgf_get_indexNode <- function() {
  getOption("resgf.indexNode")
}

#' @param node URL (including http://) of the default index node to be used
#' @name default
#' @export
resgf_set_indexNode <- function(node) {
  options("resgf.indexNode"=node)
}