# Use local or online taxon resolution service to check accepted taxon names and update synonyms at once

Names of species can be checked against WCVP database. To verify
individual names, the function wcvp_check_name is used.

## Usage

``` r
check_occ_name(
  occ_import = NA,
  local_taxon_resolution = TRUE,
  threads = 4,
  if_author_fails_try_without_combinations = TRUE
)
```

## Arguments

- occ_import:

  imported GBIF records

- local_taxon_resolution:

  if TRUE, resolve taxon names locally. Otherwise automatically use
  Taxonomic Name Resolution Service(TNRS) instead

- threads:

  your threads requirement, a positive real number, default is 4

- if_author_fails_try_without_combinations:

  option for partial verification of the authorship of the species.
  Remove the authors of combinations, in parentheses

## Value

A list with duration and two data.table: "summary" with species list and
"occ_wcvp_check_name" with WCVP fields

## Details

- [about WCVP database](http://sftp.kew.org/pub/data-repositories/WCVP/)

- [World Checklist of Vascular Plants](https://powo.science.kew.org//)

- [rWCVPdata](https://github.com/matildabrown/rWCVPdata/)
