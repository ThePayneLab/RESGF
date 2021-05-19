
#' Builds a set of search constraints for use with ESGF restful API.
#'
#' Internal function
#'
#' @param ... Constraints supplied as a named list
#'
resgf_build_constraints <- function(...) {
  #Get facet list
  facet.list <- list(...)
  #Build a search command
  facet.constraints <-
    enframe(facet.list) %>%
    unnest(value) %>%
    mutate(cmd=sprintf("%s=%s",name,value)) %>%
    pull(cmd) %>%
    paste(sep="",collapse="&")
  return(facet.constraints)
}


#' Search ESGF
#'
#' @param ... A list of constraints on which to apply the search. See the ESGF RESTful API documentation for help, https://esgf.github.io/esg-search/ESGF_Search_RESTful_API.html
#' @param node URL (including "http://") of the ESGF index node to search
#' @param search.limit Maximum number of values to return in the search. ESGF currently limits this to 10000
#'
#' @return A tibble detailing the search results
#' @export
#' @examples
#' resgf_search(variable_id=c("tos","so"),
#'              experiment_id="historical",
#'              table_id="Omon",
#'              project="CMIP6")
resgf_search <-
  function(...,
           node="http://esgf-node.llnl.gov/esg-search",
           search.limit=10000) {

  #Check inputs
  assertthat::assert_that(search.limit<=10000,msg = "ESGF currently only supports searchs of up to 10000 items")

  #Build search command

  search.cmd <- sprintf("%s/search?format=application%%2Fsolr%%2Bjson&limit=%i&%s",
                        node,
                        search.limit,
                        resgf_build_constraints(...))
  #Do first search
  search.res <- fromJSON(search.cmd,flatten=TRUE)
  #Convert to a results table
  rtn <-
    search.res$response$docs %>%
    as_tibble()

  #Check output
  assertthat::assert_that(nrow(rtn)!=search.limit,
                          msg="Number of results may be constrained by the search limit.")


  return(rtn)

  }





