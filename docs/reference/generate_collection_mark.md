# Generate unique collection event

It creates a key to identify the physical and digital duplicates of a
given collection event. It combines the last name of primary collector
with the collector number and the botanical family (family +
recordByStandardized + recordNumber_Standard) that groups the duplicates
of the same unique collection event. It also identifies new collectors
to be added to the collector dictionary and that can be reused in the
future.

Include **`recordedByStandardized`** field with verified last name of
the primary collector.

Include **`recordNumber_Standard`** field with only numbers from
**`recordNumber`**.

Create the collection event key to group duplicates in the
**`key_family_recordedBy_recordNumber`** field following the fields:

**`family` + `recordedByStandardized` + `recordNumber_Standard`**.

## Usage

``` r
generate_collection_mark(occ_import = NA, dictionary = NA)
```

## Arguments

- occ_import:

  imported GBIF records

- dictionary:

  your processed dictionary from `prepare_collectors_dictionary`

## Value

A list with duration and 3 data.table: "occ_collectorsDictionary" for
update result fields only, "summary" for summary and
"CollectorsDictionary_add" for new collectors that can be added to the
collector dictionary that can be reused in the future.

## Details

Fields created for each incident record:

`nameRecordedBy_Standard`

`recordNumber_Standard`

`key_family_recordedBy_recordNumber`

`key_year_recordedBy_recordNumber`

\#' A critical step in parsing duplicate records involves generating a
robust key for each unique collecting event (i.e., *gathering*) to
facilitate duplicate recognition. To achieve this, a string is created
by concatenating the **taxon family**, the **last name of the primary
collector**, and the **collection number**.

## Examples

``` r
# \donttest{
help(generate_collection_mark)
#> ℹ Rendering development documentation for "generate_collection_mark"
# }
```
