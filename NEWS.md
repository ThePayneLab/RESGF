# RESGF (development version)

* `resgf_search()` results are now simplifed where possible to remove unnecessary lists
* Previous funtionality to work directly with wget scripts and their manifests removed. Instead we focus on using search results as the
  basis for retrieval.
* Print methods for `resgf_*` objects now accept `n=...` argument
* Renamed classes to camelCase
* `resgf_retrieve` now writes log files when running in parallel, as a way to keep track of activity
* `resgf_search()` now has sensible defaults for replicas, versions and distributed search to avoid some easy traps
* Support for `dplyr` methods formalised by addition of `dplyr_reconstruct` methods.
* Add flag to `resgf-retrieve()` to enable insecure connections.
* Use `option("resgf.indexNode")` to set a default index node universally. Add specification of index node to `resgf_get_fileset()`

# RESGF 0.1.0

* First feature complete draft, with basic documentation
