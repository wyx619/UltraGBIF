# Import GBIF occurrence records

Returns a list contains processed GBIF records and useful issues for
downstream analysis.

## Usage

``` r
import_records(GBIF_file = "", only_PRESERVED_SPECIMEN = F)
```

## Arguments

- GBIF_file:

  GBIF occurrence Darwin Core Archive. See details for more information

- only_PRESERVED_SPECIMEN:

  if TRUE, occurrence records are filtered by
  `basisOfRecord="PRESERVED_SPECIMEN"`

## Value

A list with duration and 3 data.table: "occ" for processed occurrence
data,"occ_gbif_issue" for checked GBIF issues and "summary" for import
summary.

## Details

GBIF_file is a path to your Darwin Core standard file which is
downloaded from GBIF. The Darwin Core Archive (DwC-A) is a compact
package (a ZIP file) contains interconnected text files and enables data
publishers to share their data using a common terminology. GBIF_file
could also be a path to "occurrence.txt" which is decompressed from your
your Darwin Core standard file.

## Examples

``` r
# \donttest{

help(import_records)
#> ℹ Rendering development documentation for "import_records"


# }
```
