# Restore duplicate records from voucher, validate coordinates and extract their World Geographical Scheme for Recording Plant Distributions

Restore key usable information for vouchers by consolidating data from
duplicate records belonging to identical collection events, validate
coordinates and extract their World Geographical Scheme for Recording
Plant Distributions.

## Usage

``` r
refine_records(
  voucher = NA,
  threads = 4,
  save_path = NA,
  tests = c("capitals", "centroids", "equal", "gbif", "institutions", "outliers", "seas",
    "zeros")
)
```

## Arguments

- voucher:

  voucher from `set_digital_voucher`

- threads:

  threads requirement, a positive real number, default is 4

- save_path:

  the local path where you want to save the final result

- tests:

  CoordinateCleaner checks. Choose one or more from
  `c("capitals","centroids","equal","gbif","institutions","outliers","seas","zeros")`

## Value

A list with duration and 2 data.table: `all_records` for all refined
records, `native_records` for native records of them.

## Details

It can restore key usable information for vouchers by consolidating data
from duplicate records belonging to identical collection events,
validate coordinates and extract their World Geographical Scheme for
Recording Plant Distributions information

## Examples

``` r
# \donttest{
help(refine_records)
#> ℹ Rendering development documentation for "refine_records"
# }
```
