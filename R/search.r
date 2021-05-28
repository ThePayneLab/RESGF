#========================================================================
# Setup classes ####
#========================================================================

#' @export
is.resgfSearchResult <- function(x) inherits(x, "resgfSearchResult")

#' @export
is.resgfDataset <- function(x) inherits(x, "resgfDataset")

#' @export
as.resgfDataset <- function(x) new_tibble(x,nrow=nrow(x),class="resgfDataset")

#' @export
as.resgfFileset <- function(x) new_tibble(x,nrow=nrow(x),class="resgfFileset")

#' @export
is.resgfFileset <- function(x) inherits(x, "resgfFileset")

#' @export
print.resgfSearchResult <-
  function(object,n=NULL) {
    cat(sprintf("%-30s : %s\n","Search performed",attr(object,"search.performed")))
    cat(sprintf("%-30s : %i\n","Number of results",nrow(object)))
    cat(sprintf("%-30s : %s\n","Search command",attr(object,"search.cmd")))
    object %>%
      as_tibble() %>%
      print(n=n)
  }

#' @export
print.resgfDataset <-
  function(object,n=NULL) {
    cat(sprintf("%-30s : %s\n","Search performed",attr(object,"search.performed")))
    cat(sprintf("%-30s : %i\n","Number of datasets",nrow(object)))
    cat(sprintf("%-30s : %i\n","Number of files",sum(object$number_of_files)))
    cat(sprintf("%-30s : %s bytes\n","Data size",format(sum(object$size),
                                                        digits=3,scientific=TRUE)))
    cat(sprintf("%-30s : %s\n","Proportion replicas",format(mean(object$replica),
                                                            digits=2)))
    cat(sprintf("%-30s : %s\n","Search command",attr(object,"search.cmd")))
    object %>%
      as_tibble() %>%
      print(n=n)
  }

#' @export
print.resgfFileset <-
  function(object,n=NULL) {
    cat(sprintf("%-30s : %s\n","Search performed",attr(object,"search.performed")))
    cat(sprintf("%-30s : %i\n","Number of datasets",length(unique(object$dataset_id))))
    cat(sprintf("%-30s : %i\n","Number of files",nrow(object)))
    cat(sprintf("%-30s : %s bytes\n","Data size",format(sum(object$size),
                                                        digits=3,scientific=TRUE)))
    cat(sprintf("%-30s : %s\n","Proportion replicas",format(mean(object$replica),
                                                            digits=2)))
    cat(sprintf("%-30s : %s\n","Search command",attr(object,"search.cmd")))
    object %>%
      as_tibble() %>%
      print(n=n)
  }

#========================================================================
# Search ####
#========================================================================


#' Search ESGF
#'
#' Functions to search ESGF.
#'
#' @details For documentation of the constraints, see the ESGF RESTful API documentation for help, https://esgf.github.io/esg-search/ESGF_Search_RESTful_API.html. Details about the metadata returned can be found in the metadata setctions of https://esgf.github.io/esg-search/index.html.
#'
#' resgf_search_datasets() looks specifically for datasets, while resgf_search() is a general interface to the
#' ESGF search functionality (and can work with aggregations and files as well).
#'
#' @param ... A list of constraints on which to apply the search.
#' @param node URL (including "http://") of the ESGF index node to search
#' @param search.limit Maximum number of values to return in the search. ESGF currently limits this to 10000
#'
#' @return resegf_search_datasets() returns an resgfDataset object detailing the search results. resgf_search()
#' returns an resgfSearchResult object.
#' @name searchESGF
#' @export
#' @examples
#' resgf_search_datasets(variable_id=c("tos","so"),
#'                       experiment_id="historical",
#'                       table_id="Omon",
#'                       project="CMIP6")
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
  if(search.res$response$numFound>0) {
    rtn <-
      search.res$response$docs %>%
      as_tibble() %>%
      #Simplify where possible
      mutate(across(.fns=function(x) {
        simp.res <- simplify(x)
        if(length(simp.res)!=length(x) | is.null(simp.res)) {
          return(x)
        } else {
          simp.res
        }})) %>%
      #Convert to new class
      new_tibble(class="resgfSearchResult",
                 search.performed=Sys.time(),
                 search.cmd=search.cmd,
                 search.res=search.res,
                 nrow=nrow(.))

    #Check output
    if(nrow(rtn)>=search.limit) warning("Number of results may be constrained by the search limit.")

  } else {
    rtn <- NULL
  }

  return(rtn)
}


#' @export
#' @name searchESGF
resgf_search_datasets <-
  function(...) {
    #Parse arglist
    arg.list <- list(...)
    #Overwrite type, if supplied
    arg.list$type <- "Dataset"
    #Make call to generic search function
    rtn <-
      do.call(resgf_search,arg.list) %>%
      new_tibble(class=c("resgfDataset","resgfSearchResult"))

    return(rtn)
  }

#' @export
#' @name searchESGF
resgf_search_files <-
  function(...) {
    #Parse arglist
    arg.list <- list(...)
    #Overwrite type, if supplied
    arg.list$type <- "File"
    #Make call to generic search function
    rtn <-
      do.call(resgf_search,arg.list) %>%
      new_tibble(class=c("resgfFileset","resgfSearchResult"))

    return(rtn)
  }

#========================================================================
# Helpers ####
#========================================================================

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


#' Get the filelist
#'
#' Gets the list of files associated with the datasets. This is done in a piecewise manner to avoid
#' overloading both the number of return values and the ability to issue the command, and can optionally
#' be done in parallel as well.
#'
#' @param object resegf_dataset object from which to retrieve the underlying files
#' @param max.files maximum number of files to be returned in a chunk
#' @param max.datasets maximum number of datasets to request in a chunk
#' @param processes Number of processes to perform in parallel
#'
#' @return
#' @export
resgf_get_filelist <-
  function(object,max.files=1000,max.datasets=10,processes=1) {
    #Require input object to be dataset search result
    assert_that(max(object$number_of_files)< max.files,
                msg="File chunk size is too small.")
    assert_that(is.resgfDataset(object),
                msg="Input must be an object returned by resgf_search_datasets().")

    #Because we can only receive 10000 results at a time, we need to process the results piecewise
    this.chunk <- 1
    to.assign <-
      object %>%
      as_tibble() %>%
      select(id,number_of_files)
    chunk.l <- list()

    while (nrow(to.assign)>0) {
      #Make cumulative list
      to.assign$cumsum <- cumsum(to.assign$number_of_files)
      #Move those that are valid onto the chunk list
      move.these <- head(which(to.assign$cumsum<=max.files),max.datasets)
      chunk.l[[this.chunk]] <- to.assign[move.these,]
      to.assign <- to.assign[-move.these,]
      #Housekeeping
      this.chunk <- this.chunk +1
    }

    #Now perform the search for each of these in turn
    this.fileset <-
      pblapply(chunk.l,
               function(x) {resgf_search_files(dataset_id=x$id)},
               cl=processes)

    #And we're done (with some tidying)
    rtn <-
      this.fileset %>%
      bind_rows() %>%
      new_tibble(class=c("resgfFileset","resgfSearchResult"),
                 nrow=nrow(.),
                 search.cmd=NULL,  #Drop this
                 search.res=NULL)

}

