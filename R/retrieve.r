#========================================================================
# Classes ####
#========================================================================


#========================================================================
# Methods ####
#========================================================================


#' Retrieve missing files from ESGF
#'
#' @param object Either a resgfStatus or resgfFileset object detailing what should be retrieved
#' @param local.dir The local directory in which to download - overrides the default taken from an resgfStatus object.
#' @param index.node URL (including "http://") of the ESGF index node to search.  Defaults to global default
#' retrieved by `resgf_get_indexNode()`.
#' @param processes Number of processes to run in parallel
#' @param keep.tempfiles Retains the wget download script and log files after completion.
#' @param connect.insecurely Connect to server insecurely (ignoring certificates), by enabling the "-i" flag
#'
#' @details When a resgfStatus object is supplied, only files that are not held locally or that have failed checksum checks 
#' (if performed)are retrieved. For a resgfFileset object, all files are retrieved.
#' 
#' The download scripts are always written to local.dir with the extension *.sh. If run in parallel, 
#' log files are also written to the local.dir with the extension .sh.log. Normally these are deleted upon a successful 
#' completion, but they can be retained using the keep.tempfiles argument.
#' @export
resgf_retrieve <-
  function(object,
           local.dir="missing",
           index.node=resgf_get_indexNode(),
           processes=1,
           keep.tempfiles=FALSE,
           connect.insecurely=FALSE) {
    
    #Check inputs
    assert_that(is.resgfStatus(object)| is.resgfFileset(object),
                msg="Object must be of class resgfStatus or resgfFileset.")

    #Get list to retrieve
    if(is.resgfFileset(object)) {  #Take everything
      get.these <- 
        object %>%
        as_tibble() %>%
        mutate(filename=title)
      assert_that(!missing(local.dir),
                  msg="local.dir argument must be supplied when object is of class resgfFileset.")
      
    } else if  (is.resgfStatus(object) &attr(object,"checksums.verified")) { #Get missing or checksum failures (if any)
      get.these <-
        object %>%
        as_tibble() %>%
        filter(!locally.valid | !checksum.passed)
      local.dir <- attr(object,"local.dir")
    } else if  (is.resgfStatus(object) & !attr(object,"checksums.verified")) { #Get locally missing or failed  (if any)
      get.these <-
        object %>%
        as_tibble() %>%
        filter(!locally.valid)
      local.dir <- attr(object,"local.dir")
    } else {
      stop("Shouldn't be here")
    }

    #Retrieval function
    retrieve.file <- function(get.this) {
      this.checksum <- get.this$checksum[[1]]
      this.id <- get.this$id
      this.filename <- get.this$filename[[1]]

      #Request a wget script for the file
      search.cmd <- sprintf("%s/wget?id=%s",
                            index.node,
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
      log.this <- processes > 1
      bash.cmd <- sprintf("cd %s && bash %s -s",local.dir,wget.fname)
      if(connect.insecurely) bash.cmd <- paste(bash.cmd,"-i")
      log.file <- sprintf("%s.log",wget.fname)
      if(log.this) bash.cmd <- sprintf("%s > %s 2>&1",bash.cmd,log.file)
      get.this$retrieval.status <- system(bash.cmd,intern=FALSE)

      #Assuming successful completion (how to check?) delete the script
      if(!keep.tempfiles) file.remove(wget.fname)
      if(!keep.tempfiles & log.this) file.remove(log.file)
      
      return(get.this)

    }

    #Do retrieval (or no{t)
    if(nrow(get.these)==0) {
      message("Nothing to do here - all files appear to be up to date.")
      return(NULL)
    } else {
      message(sprintf("Retrieving %i files...",nrow(get.these)))
      res <-
        get.these %>%
        split(.,1:nrow(.)) %>%
        pblapply(retrieve.file,cl=processes) %>%
        bind_rows()
      
    #Return status object
    rtn <- new_tibble(res,
                      nrow=nrow(res),
                      class=c("resgfStatus","resgfDownloadStatus"))
    return(rtn)}

  }
