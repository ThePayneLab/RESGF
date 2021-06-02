# RESGF - An R interface to the Earth System Grid Federation (ESGF)

by Mark R. Payne<br>
http://www.staff.dtu.dk/mpay <br>
@MarkPayneAtWork<br>
https://orcid.org/0000-0001-5795-2481

An R package to facilitate searching and downloading data from the Earth System Grid Federation (ESGF).

## Dependencies
RESGF requires that you have a version of wget available in your path.

## Installation

You can install this package directly from the GitHub repository using the following command:

```{R}
devtools::install_github("ThePayneLab/RESGF")
```

## An example

How to search a dataset.

```{R}
resgf_search_datasets(variable_id=c("tos","so"),
                      experiment_id="historical",
                      table_id="Omon",
                      project="CMIP6")
```


## More help

Examples and documentation are still a little thin as yet - if you're serious about using this, I suggest getting in contact directly. Questions, queries, comments or theories are always welcome via the GitHub repository. Push-requests are especially welcome!

Import external resources are:
* Documentation for the ESGF Search API: https://esgf.github.io/esg-search/ESGF_Search_RESTful_API.html
* Documentation describing ESGF metadata fields: https://esgf.github.io/esg-search/index.html 

## Tips and Tricks for Working with ESGF 

* Not all data nodes on the ESGF are equal. If you are having trouble getting getting a particular file, search for replicas of it first, and then trying downloading it from a different data node (see the data_node column of the `resgFileset` object). You'll quickly learn that some nodes are more equal than others. 
* You can view the current status of ESGF data nodes on the webpage: https://esgf-node.llnl.gov/status/ This can be useful for finding alternative sources for files.

## See Also
* The python ESGF client, `esgf-pyclient`, does much the same thing, just in python (but is officially supported by ESGF):  https://github.com/ESGF/esgf-pyclient 