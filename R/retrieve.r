#' Retrieve missing files from ESGF
#'
#' @param object Either a resgf_status or resgf_manifest object detailing what should be retrieved
#'
#' @details When a resgf_status object is supplied, only files that are not held locally (but are listed in the manifest) are retrieved. For a manifest object, all files are retrieved
#' are
#' @export
resgf_retrieve <-
  function(object) {

  #Get list to retrieve
  get.these <-
    object@status %>%
      filter(!locally.valid,
             in.manifest)

  #Loop
  for(i in seq(nrow(get.these))) {
    this.checksum <- get.these$checksum[i]
    this.filename <- get.these$filename[i]

    #Request a wget script for the file
    search.cmd <- sprintf("%s/wget?checksum=%s",
                          object@remote.manifest@node,
                          this.checksum)
    this.wget.rtn <- GET(search.cmd)
    wget.script <- content(this.wget.rtn,"text")

    #Write to a tempfile and extract manifest
    this.manifest <- resgf_extract_manifest(wget.script)
    assert_that(nrow(this.manifest)==1,
                msg=sprintf("Multiple matches found for file %s.",this.filename))

    #Run the script (no authentication)
    wget.fname <- file.path(object@local.db.dir,sprintf("%s.sh",this.filename))
    writeLines(wget.script,wget.fname)
    system(sprintf("cd %s && bash %s -s",object@local.db.dir,wget.fname))

    #Assuming successful completion (how to check?) delete the script
    file.remove(wget.fname)

  }

  }

