# RESGF - An R interface to the Earth System Grid Federation (ESGF)

by Mark R. Payne<br>
http://www.staff.dtu.dk/mpay <br>
@MarkPayneAtWork

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
