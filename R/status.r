#========================================================================
# Classes ####
#========================================================================

#' @export
print.resgf_status <-
          function(object) {
            cat(sprintf("%-30s : %s\n","Local directory",attr(object,"local.dir")))
            cat(sprintf("%-30s : %s\n","Checksums verified?",attr(object,"checksums.verified")))
            cat(sprintf("%-30s : %i\n","Remote files",nrow(object)))
            cat(sprintf("%-30s : %i\n","Remote files stored locally",
                         sum(file.exists(object$local.path))))
            cat(sprintf("%-30s : %i\n","Local checksums passed",
                        sum(object$checksum.passed,na.rm = TRUE)))
            object %>%
              as_tibble() %>%
              print()
          }

#' @export
is.resgf_status <- function(x) inherits(x, "resgf_status")

#========================================================================
# Perform check ####
#========================================================================

#' Compare local database with ESGF search
#'
#' Compares the list of files generated by an ESGF search with the locally downloaded files
#'
#' @param local.dir Path to the local directory. Files nested in subdirectories are also detected.
#' @param remote.db resgf_fileset object generated previously, which forms the reference database. Alternatively, another
#' resgf_status object that we wish to recheck.
#' @param check.checksums Should the comparison be done using the checksums (slow!)? Or only the filenames.
#' @param processes Number of processes to run in parallel
#'
#' @return An resgf_status object
#' @export
#'
#' @examples
#' resgf_check_status(local.db.dir=pwd(),
#'                    esgf.manifest=this.manifest)
resgf_status_check <-
  function(local.dir,
           remote.db,
           check.checksums=FALSE,
           processes=1) {
    #Check inputs
    assert_that(is.resgf_fileset(remote.db) | is.resgf_status(remote.db),
                msg="'remote.db' argument needs to be of type resgf_fileset or resgf_status.")
    if(is.resgf_status(remote.db)) {
      remote.db <- 
        remote.db %>%
        select(-local.path,-filename,-checksum.passed)
    }

    #Get list of files already existing in database
    local.db <-
      tibble(local.path=dir(local.dir,full.names=TRUE,pattern="*.nc",recursive=TRUE)) %>%
      mutate(filename=basename(local.path)) %>%
      relocate(filename,.before=1)

    #Merge with remote database
    status.db <-
      remote.db %>%
      as_tibble() %>%
      mutate(filename=title)%>%
      right_join(x=local.db,by="filename")

    #Here we would do the checksum check
    if(check.checksums) {
      #Best if we do it via pblapply, to allow parallelisation. This requires
      #first separating out the files to checksum, doing the analysis, and then recombinin
      #Obviously only check the files that we have locally
      check.these <-
        status.db %>%
        filter(!is.na(local.path)) %>%
        select(filename,local.path,id,checksum,checksum_type) %>%
        split(.,seq(nrow(.)))   #Split into individual rows for pbapply

      #Now do the checking
      check.status <-
        pblapply(check.these,cl=processes,FUN=function(d) {
          d$checksum.passed <-
            switch(d$checksum_type,
                 "SHA256"=system(sprintf("echo %s %s | sha256sum --check",d$checksum,d$local.path),
                                 ignore.stderr = TRUE,ignore.stdout = TRUE),
                 stop(sprintf("Unsupport checksum_type %s needed for file %s",d$checksum_type,d$local.path)))
          return(d)
        }) %>%
        bind_rows() %>%
        mutate(checksum.passed=checksum.passed==0)

      #Merge results back into table
      status.db <-
        check.status %>%
        select(id,checksum.passed) %>%
        left_join(x=status.db,by="id")

    } else {
    status.db <-
          mutate(status.db,
                 checksum.passed=FALSE)
    }

    #Return status object
    rtn <- new_tibble(status.db,
                      nrow=nrow(status.db),
                      class="resgf_status",
                      local.dir=local.dir,
                      checksums.verified=check.checksums)

    return(rtn)
  }

#========================================================================
# Helpers ####
#========================================================================

#' Summarise status
#'
#' Summarise the results stored in a resgf_status object in terms of one of the filename fields or the data node
#'
#' @param object An resgf_status object
#' @param field.idx Index of the filename field to summarise over.
#' @param field.sep  Character used to separate fields in the filename. Default is "_"
#' @return A tibble summarising the status accordingly.
#' @name summariseStatus
#' @export
#'
resgf_status_by_field <- function(object,field.idx=1,field.sep="_") {
  #Check inputs
  assert_that(is(object,"resgf_status"),
              msg="Supplied object is not an resgf_status object")
  assert_that(length(field.idx)==1,
              msg="Field argument must be an integer of length 1")

  #Split filenames into fiels
  this.status <-
    object %>%
    as_tibble() %>%
    mutate(noext.fname=gsub("\\..*?$","",filename),
           field=str_split(noext.fname,field.sep,simplify = TRUE))

  #Check that request is ok
  assert_that(ncol(this.status$field) >= field.idx,
              msg=sprintf("Field index requested, %i, is too high. Max value is %i.",
                          field.idx,ncol(this.status$field)))

  this.status %>%
    mutate(field.value=field[,field.idx]) %>%
    group_by(field.value) %>%
    summarise(remote.files=n(),
              local.files=sum(!is.na(local.path)),
              prop=mean(!is.na(local.path)),
              checksums.ok=sum(checksum.passed),
              prop.checksums=mean(checksum.passed),
              .groups="drop")

}

#' @export
#' @name summariseStatus
resgf_status_by_node <- function(object) {
  assert_that(is(object,"resgf_status"),
              msg="Supplied object is not an resgf_status object")
  object %>%
    group_by(data_node) %>%
    summarise(remote.files=n(),
              local.files=sum(!is.na(local.path)),
              prop=mean(!is.na(local.path)),
              checksums.ok=sum(checksum.passed),
              prop.checksums=mean(checksum.passed),
              .groups="drop")

}

