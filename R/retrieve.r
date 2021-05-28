#========================================================================
# Classes ####
#========================================================================


#========================================================================
# Methods ####
#========================================================================


#' Retrieve missing files from ESGF
#'
#' @param object Either a resgf_status or resgf_fileset object detailing what should be retrieved
#' @param local.dir The local directory in which to download - overrides the default taken from an resgf_status object.
#' @param node ESGF node to generate wget scripts from
#' @param processes Number of processes to run in parallel
#' @param keep.script Retains the wget download script after completion.
#'
#' @details When a resgf_status object is supplied, only files that are not held locally (but are listed in the manifest) are retrieved. For a manifest object, all files are retrieved
#' are
#' @export
resgf_retrieve <-
  function(object,
           local.dir="missing",
           node="http://esgf-node.llnl.gov/esg-search",
           processes=1,
           keep.script=FALSE) {
    
    #Check inputs
    assert_that(is.resgf_status(object)| is.resgf_fileset(object),
                msg="Object must be of class resgf_status or resgf_fileset.")

    #Get list to retrieve
    if(is.resgf_fileset(object)) {  #Take everything
      get.these <- 
        object %>%
        as_tibble() %>%
        mutate(filename=title)
      assert_that(!missing(local.dir),
                  msg="local.dir argument must be supplied when object is of class resgf_fileset.")
      
    } else if  (attr(object,"checksums.verified")) { #Get missing and checksum failures (if any)
      get.these <-
        object %>%
        as_tibble() %>%
        filter(!file.exists(local.path) | !checksum.passed)
      local.dir <- attr(object,"local.dir")
    } else { #Only get missing
      get.these <-
        object %>%
        as_tibble() %>%
        filter(!file.exists(local.path))
      local.dir <- attr(object,"local.dir")
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

      #Write to a tempfile and check manifest
      wget.fname <- file.path(local.dir,sprintf("%s.sh",this.filename))
      manifest.fname <- tempfile(fileext = ".manifest")
      writeLines(wget.script,wget.fname)
      rtn <- system2("bash",args = sprintf("%s -w %s",wget.fname,manifest.fname))
      if(rtn!=0) {
        stop(sprintf("Cannot extract manifest. Run 'readLines(\"%s\")' to see contents of file.",wget.fname))
      }
      this.manifest <- read_delim(manifest.fname,delim=" ",quote="'",col_types="cccc",
                             col_names=c("filename","url","checksum.type","checksum"))
      assert_that(nrow(this.manifest)==1,
                  msg=sprintf("Multiple matches found for file %s.",this.filename))

      #Run the script (no authentication)
      be.quiet <- processes > 1
      bash.cmd <- sprintf("cd %s && bash %s -s",local.dir,wget.fname)
      get.this$retrieval.status <-
        system(bash.cmd,
               intern=FALSE,
               ignore.stderr = be.quiet,ignore.stdout = be.quiet)

      #Assuming successful completion (how to check?) delete the script
      if(!keep.script) file.remove(wget.fname)
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
