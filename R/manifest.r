resgf_manifest <- setClass("resgf_manifest",
                           list(node="character",
                                wget.command="character",
                                wget.script="character",
                                manifest="data.frame"))

setMethod("show",signature(object="resgf_manifest"),
          function(object) {
            cat(sprintf("%-20s : %s\n","ESGF node",object@node))
            cat(sprintf("%-20s : %s\n","wget command",object@wget.command))
            cat(sprintf("%-20s :\n","Manifest"))
            print(object@manifest)

          })

#' Extract the manifest from a wget script
#'
#' @param wget.script The contents of the wget script as a single character vector
#'
#' @return A tibble with the manifest
#' @export
resgf_extract_manifest <- function(wget.script) {
  #Write to a tempfile and extract manifest
  wget.fname <- tempfile(fileext = ".sh")
  manifest.fname <- paste(wget.fname,".manifest",sep="")
  writeLines(wget.script,wget.fname)
  system2("bash",args = sprintf("%s -w %s",wget.fname,manifest.fname))
  manifest <- read_delim(manifest.fname,delim=" ",quote="'",col_types="cccc",
                         col_names=c("filename","url","checksum.type","checksum"))
  return(manifest)

}


#' Get the manifest of files from an ESGF search
#'
#' @param ... A list of constraints on which to apply the search. See the ESGF RESTful API documentation for help, https://esgf.github.io/esg-search/ESGF_Search_RESTful_API.html
#' @param node URL (including "http://") of the ESGF index node to search
#'
#' @return An resgf_manifest object containing the manifest
#' @export
#'
#' @examples
#' resgf_get_manifest(variable_id=c("tos","so"),
#'                    experiment_id="historical",
#'                    table_id="Omon",
#'                    project="CMIP6")
resgf_get_manifest <-
  function(...,
           node="http://esgf-node.llnl.gov/esg-search") {

  #Setup return object
  this.manifest <- new("resgf_manifest",node=node)

  #Build search command
  search.cmd <-
    sprintf("wget?limit=10000&%s",
                        resgf_build_constraints(...))
  this.manifest@wget.command <- search.cmd

  #Retrieve wget file
  this.cmd <- sprintf("%s/%s",node,search.cmd)
  this.wget.rtn <- GET(this.cmd)
  assert_that(this.wget.rtn$status_code==200,
              msg=sprintf("Failure on command %s. Try running it again in a browser to see more details.",this.cmd))
  wget.content <- content(this.wget.rtn,"text")
  this.manifest@wget.script <- wget.content

  #Write to a tempfile and extract manifest
  this.manifest@manifest <- resgf_extract_manifest(wget.content)

  return(this.manifest)

  }



