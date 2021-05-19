resgf_status <- setClass("resgf_status",
                         list(local.db.dir="character",
                              remote.manifest="resgf_manifest",
                              checksums.verified="logical",
                              status="data.frame"))

setMethod("show",signature(object="resgf_status"),
          function(object) {
            cat(sprintf("%-30s : %s\n","Local directory",object@local.db.dir))
            cat(sprintf("%-30s : %i\n","Local files",sum(!is.na(object@status$local.path))))
            cat(sprintf("%-30s : %s\n","Checksums verified?",object@checksums.verified))
            cat(sprintf("%-30s : %i\n","Remote files",nrow(object@remote.manifest@manifest)))
            cat(sprintf("%-30s : %i\n","Remote files locally valid",sum(object@status$locally.valid)))
            cat(sprintf("%-30s : %i\n","Invalid/missing locally",
                        sum(!object@status$locally.valid & object@status$in.manifest)))
          })


#' Compare local database with ESGF search
#'
#' Compares the list of files ("manifest") generated by an ESGF search with the locally downloaded files
#'
#' @param local.db.dir Path to the local directory. Files nested in subdirectories are also detected.
#' @param esgf.manifest resgf_manifest object generated previously, which forms the reference database
#' @param check.checksums Should the comparison be done using the checksums (slow!)? Or only the filenames.
#'
#' @return An resgf_status object
#' @export
#'
#' @examples
#' resgf_check_status(local.db.dir=pwd(),
#'                    esgf.manifest=this.manifest)
resgf_check_status <-
  function(local.db.dir,
           esgf.manifest,
           check.checksums=FALSE) {

  #Setup status object
  this.status <- resgf_status(local.db.dir=local.db.dir,
                              remote.manifest=esgf.manifest,
                              checksums.verified=check.checksums)

  #Get list of files already existing in database
  local.db <-
    tibble(local.path=dir(local.db.dir,full.names=TRUE,pattern="*.nc",recursive=TRUE)) %>%
    mutate(filename=basename(local.path))

  #Merge with manifest
  status.db <- full_join(esgf.manifest@manifest,local.db,by="filename")

  #Here we would do the checksum check
  status.db <-
    mutate(status.db,
           locally.valid=!is.na(local.path),
           in.manifest=!is.na(url))

  this.status@status <- status.db

  return(this.status)
  }

