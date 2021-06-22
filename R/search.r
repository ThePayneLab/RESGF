#========================================================================
# Setup classes ####
#========================================================================

#resgfSearchResult class---------------------

#' @export
is.resgfSearchResult <- function(x) inherits(x, "resgfSearchResult")

#' @export
print.resgfSearchResult <-
  function(object,n=NULL) {
    pretty.hdr(object,NULL,"%-30s : %s\n","Search performed",attr(object,"search.performed"))
    pretty.hdr(object,NULL,"%-30s : %i\n","Number of results",nrow(object))
    pretty.hdr(object,NULL,"%-30s : %s\n","Search command",attr(object,"search.cmd"))
    object %>%
      as_tibble() %>%
      print(n=n)
  }

#' @export
dplyr_reconstruct.resgfSearchResult <- function(data, template){
  # Return a tibble if filename is lost
  if(!all(c("id") %in% colnames(data))) {
    return(as_tibble(data))
  } else {
    return(new_tibble(data,
                      class=class(template)[1],
                      search.performed=attr(template,"search.performed"),
                      search.cmd=attr(template,"search.cmd"),
                      search.res=attr(template,"search.res"),
                      nrow=nrow(data)))
  }
}

#' @export
`[.resgfSearchResult` <- function(x, i, j, drop = FALSE, ...) {
  out <- NextMethod()
  dplyr_reconstruct(out, x)
}

# resgfDataset class-----------------------------------

#' @export
is.resgfDataset <- function(x) inherits(x, "resgfDataset")

#' @export
as.resgfDataset <- function(x) new_tibble(x,nrow=nrow(x),class="resgfDataset")


#' @export
print.resgfDataset <-
  function(object,n=NULL) {
    pretty.hdr(object,NULL,"%-30s : %s\n","Search performed",attr(object,"search.performed"))
    pretty.hdr(object,NULL,"%-30s : %i\n","Number of datasets",nrow(object))
    pretty.hdr(object,"number_of_files","%-30s : %i\n","Number of files",sum(object$number_of_files))
    pretty.hdr(object,"size","%-30s : %s\n","Data size",pretty.filesize(sum(object$size)))
    pretty.hdr(object,"replica","%-30s : %s\n","Proportion replicas",
               format(mean(object$replica),digits=2))
    pretty.hdr(object,NULL,"%-30s : %s\n","Search command",attr(object,"search.cmd"))
    object %>%
      as_tibble() %>%
      print(n=n)
  }

#' @export
dplyr_reconstruct.resgfDataset <- dplyr_reconstruct.resgfSearchResult

#' @export
`[.resgfDataset` <- `[.resgfSearchResult`

# resgfFileset class-----------------------------------

#' @export
as.resgfFileset <- function(x) new_tibble(x,nrow=nrow(x),class="resgfFileset")

#' @export
is.resgfFileset <- function(x) inherits(x, "resgfFileset")


#' @export
print.resgfFileset <-
  function(object,n=NULL) {
    pretty.hdr(object,NULL,"%-30s : %s\n","Search performed",attr(object,"search.performed"))
    pretty.hdr(object,"dataset_id","%-30s : %i\n","Number of datasets",length(unique(object$dataset_id)))
    pretty.hdr(object,NULL,"%-30s : %i\n","Number of files",nrow(object))
    pretty.hdr(object,"size","%-30s : %s\n","Data size",pretty.filesize(sum(object$size)))
    pretty.hdr(object,"replica","%-30s : %s\n","Proportion replicas",
               format(mean(object$replica),digits=2))
    pretty.hdr(object,NULL,"%-30s : %s\n","Search command",attr(object,"search.cmd"))
    object %>%
      as_tibble() %>%
      print(n=n)
  }

#' @export
dplyr_reconstruct.resgfFileset <- dplyr_reconstruct.resgfSearchResult

#' @export
`[.resgfFileset` <- `[.resgfSearchResult`

#Only displays information if key columns exist
pretty.hdr <- function(object,cols=NULL,fmt.str,...) {
  if(all(cols %in% colnames(object)) | is.null(cols)) {
    cat(sprintf(fmt.str,...))
  }
}

pretty.filesize <- function(x) {
  #Get units first
  unit.exp <- floor(log10(x)/3)*3
  unit.txt <- switch(as.character(unit.exp),
                      "0"="B",
                      "3"="kB",
                      "6"="MB",
                      "9"="GB",
                      "12"="TB",
                      "15"="PB",
                      stop("Unknown filesize"))
  #Format text accordingly
  sprintf("%s %s",format(x/10^unit.exp,digits=3),unit.txt)
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
#' @param index.node URL (including "http://") of the ESGF index node to search.  Defaults to global default
#' retrieved by `resgf_get_indexNode()`.
#' @param search.limit Maximum number of values to return in the search. ESGF currently limits this to 10000
#' @param show.all.replicas Show all copies (replicas) of a file or dataset (TRUE) or just the "original". Defaults to FALSE
#' @param show.all.versions Should the result showsall versions of a file or dataset? Defaults to FALSE
#' @param search.local.node.only Should just the local index node be searched (TRUE) or a distributed search across 
#' the entire ESGF be used (FALSE). Defaults to FALSE.
#' @param simplify Should the search function attempt to simplify the results or simply leave them as is?
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
           index.node=resgf_get_indexNode(),
           search.limit=10000,
           show.all.replicas=FALSE,
           show.all.versions=FALSE,
           search.local.node.only=FALSE,
           simplify=TRUE) {
    
    #Check inputs
    assertthat::assert_that(search.limit<=10000,msg = "ESGF currently only supports searchs of up to 10000 items")
    
    #Build the constraints
    facet.constraints <-
      list(...) %>%
      enframe() %>%
      unnest(value) %>%
      mutate(cmd=sprintf("%s=%s",name,value)) %>%
      pull(cmd) %>%
      paste(sep="",collapse="&")
    
    #Build search command
    search.cmd <- sprintf("%s/search?format=application%%2Fsolr%%2Bjson&limit=%i&%s",
                          index.node,
                          search.limit,
                          facet.constraints)
    if(!show.all.replicas) search.cmd <- paste0(search.cmd,'&replica="false"')
    if(!show.all.versions) search.cmd <- paste0(search.cmd,'&latest="true"')
    if(search.local.node.only) search.cmd <- paste0(search.cmd,'&distrib="false"')
    
    #Do first search
    search.res <- fromJSON(search.cmd,flatten=TRUE)

    #Convert to a results table
    if(search.res$response$numFound>0) {
      rtn.tb <-
        search.res$response$docs %>%
        as_tibble()
      if(simplify) {      #Simplify where possible and if asked
        rtn.tb <- resgf_simplify(rtn.tb)
      }
      #Convert to new class
      rtn <-
        rtn.tb %>%
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
      new_tibble(.,
                 class=c("resgfDataset"),
                 nrow=nrow(.))
    
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
      new_tibble(.,
                 class=c("resgfFileset"),
                 nrow=nrow(.))
    
    return(rtn)
  }

#========================================================================
# Helpers ####
#========================================================================
#' Simplify an resgf* object
#' 
#' The datasets returned by searching resgf don't always parse conveniently into a tibble - some columns end up as
#' lists, rather than vectors. This function simplifies and makes that conversion where it can.
#'
#' @param object Raw object to simplify
#'
#' @return
#' @export
resgf_simplify <- function(object) {
  object %>%
    mutate(across(.fns=function(x) {
      simp.res <- simplify(x)
      if(length(simp.res)!=length(x) | is.null(simp.res)) {
        return(x)
      } else {
        simp.res
      }}))
}

#' Get the filelist
#'
#' Gets the list of files associated with the datasets. This is done in a piecewise manner to avoid
#' overloading both the number of return values and the ability to issue the command, and can optionally
#' be done in parallel as well.
#'
#' @param object resgfDataset object from which to retrieve the underlying files
#' @param processes Number of processes to perform in parallel
#' @param index.node URL (including "http://") of the ESGF index node to search. Defaults to global default
#' retrieved by `resgf_get_indexNode()`.' Passed in this instance further to `resgf_search*()`
#' 
#' @details Note that we by default search across all replicas and all versions by default. Each replica and version in ESG has a
#'  unique id that is used as the basis for retrieveal (`object$id`). In principle, it makes sense to always search across the
#'  entire ESGF to find the exact file listed in the dataset - failing to specify these parameters results in zero matches.
#'  But your mileage may vary - please report this as a bug if it is a problem.
#'
#' @return
#' @export
resgf_get_filelist <-
  function(object,
           processes=1,
           index.node=resgf_get_indexNode()) {

    #Require input object to be dataset search result
    assert_that(is.resgfDataset(object),
                msg="Input must be an object returned by resgf_search_datasets().")
    
    #Because we can only receive 10000 results at a time, we need to process the results piecewise
    max.files <- 1000   #Chunking parameters
    max.datasets <- 10
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
    #Explicitly avoid simplification here - we simplify after we have combined.
    this.fileset <-
      pblapply(chunk.l,
               function(x) {resgf_search_files(dataset_id=x$id,
                                               simplify=FALSE,
                                               show.all.replicas=TRUE,
                                               show.all.versions=TRUE,
                                               search.local.node.only=FALSE,
                                               index.node=index.node)},
               cl=processes)

    #And we're done (with some tidying)
    rtn <-
      this.fileset %>%
      bind_rows() %>%
      resgf_simplify() %>%
      new_tibble(class=c("resgfFileset"),
                 nrow=nrow(.),
                 search.cmd=NULL,  #Drop this
                 search.res=NULL)
    
  }

