#========================================================================
# Classes ####
#========================================================================


#========================================================================
# Methods ####
#========================================================================


#' Retrieve missing files from ESGF
#'
#' @param object Either a resgf_status or resgf_manifest object detailing what should be retrieved
#' @param node ESGF node to generate wget scripts from
#' @param processes Number of processes to run in parallel
#' @param skip.checksum Stops the wget script from verifying the checksum of the downloaded file. Corresponds to
#' activating the "-p" parameter on the wget script.
#' @param retain.logs Retain download logs in output
#'
#' @details When a resgf_status object is supplied, only files that are not held locally (but are listed in the manifest) are retrieved. For a manifest object, all files are retrieved
#' are
#' @export
resgf_retrieve <-
  function(object,
           node="http://esgf-node.llnl.gov/esg-search",
           processes=1,
           skip.checksum=FALSE,
           retain.logs=TRUE) {

    #Get list to retrieve
    if (attr(object,"checksums.verified")) { #Get missing and checksum failures (if any)
      get.these <-
        object %>%
        as_tibble() %>%
        filter(is.na(local.path) | !checksum.passed)
    } else { #Only get missing
      get.these <-
        object %>%
        as_tibble() %>%
        filter(is.na(local.path))
    }

    #Retrieval function
    retrieve.file <- function(get.this) {
      this.checksum <- get.this$checksum[[1]]
      this.id <- get.this$id
      this.filename <- get.this$filename[[1]]

      #Request a wget script for the file
      search.cmd <- sprintf("%s/wget?checksum=%s",
                            node,
                            this.checksum)
      search.cmd <- sprintf("%s/wget?id=%s",
                            node,
                            this.id)
      this.wget.rtn <- GET(search.cmd)
      wget.script <- content(this.wget.rtn,"text")

      #Write to a tempfile and extract manifest
      this.manifest <- resgf_extract_manifest(wget.script)
      assert_that(nrow(this.manifest)==1,
                  msg=sprintf("Multiple matches found for file %s.",this.filename))

      #Run the script (no authentication)
      local.dir <- attr(object,"local.dir")
      wget.fname <- file.path(local.dir,sprintf("%s.sh",this.filename))
      writeLines(wget.script,wget.fname)
      be.quiet <- processes > 1
      bash.cmd <- sprintf("cd %s && bash %s -s",local.dir,wget.fname)
      if(skip.checksum) bash.cmd <- sprintf("%s -p",bash.cmd)
      retrieval.log <-
        system(bash.cmd,
               intern=TRUE,
               ignore.stderr = be.quiet,ignore.stdout = be.quiet)
      if(retain.logs) {
        get.this$retrieval.log <- list(retrieval.log)
      }

      #Assuming successful completion (how to check?) delete the script
      file.remove(wget.fname)
      return(get.this)

    }

    #Do retrieval
    res <-
      get.these %>%
      split(.,1:nrow(.)) %>%
      pblapply(retrieve.file,cl=processes) %>%
      bind_rows()

    #Return status object
    rtn <- new_tibble(res,
                      nrow=nrow(res),
                      class=c("resgf_status","resgf_download_status"))
    return(rtn)

  }
