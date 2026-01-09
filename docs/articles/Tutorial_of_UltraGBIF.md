# Tutorial of UltraGBIF

## Description

This Tutorial parses and merges
[*Saxifraga*](https://doi.org/10.15468/dl.bythb4) occurrence records
from GBIF.

## Installation

You can install UltraGBIF from GitHub. UltraGBIF runs with rWCVPdata, so
install it firstly (We recommend rWCVPdata version 0.6.0 with WCVP
version 14), and the initial installation takes some time.

``` r
options(repos = c(getOption("repos"),"https://wyx619.github.io/Repo"))
options(timeout = 600)
install.packages("rWCVPdata") ## install rWCVPdata [require long time]
install.packages("UltraGBIF") ## install UltraGBIF
```

## Import records

Initially, download occurrence records from GBIF. Filter records online
such as *Basis of record = Preserved specimen, Occurrence status =
present, and Scientific name = Saxifraga*. Get the DwC-A file and
remember its absolute path.

This module also utilizes `EnumOccurrence` Issue values to transform
these issues into a structured format, enabling systematic quality
assessment through individualized issue tracking and standardized
classification. See more
[details](https://gbif.github.io/gbif-api/apidocs/org/gbif/api/vocabulary/OccurrenceIssue.html)
here.

``` r
library(UltraGBIF)
gbif_occurrence_file <- "path/to/the/DwC-A file"
occ_import <- import_records(GBIF_file = gbif_occurrence_file,
                             only_PRESERVED_SPECIMEN = T)
```

`occ_import` includes `occ` for initial occurrence records,
`occ_gbif_issue` for GBIF issues and its `summary`, and time cost.

## Check taxon name

UltraGBIF uses WCVP(Govaerts et al. 2021) as a high-authority taxonomic
backbone, systematically resolving taxons in GBIF occurrence records.
Alternatively, TNRS(Boyle et al. 2013) is supported by setting the
parameter `local_taxon_resolution = FALSE`.

To optimize computational efficiency, this module supports
parallelization through multi-threading by specifying the desired
concurrency level via the `threads` parameter.

``` r
names.checked <- check_occ_name(occ_import = occ_import,
                                threads = 4,
                                local_taxon_resolution = TRUE,
                                if_author_fails_try_without_combinations = TRUE)
```

`names.checked` includes `occ_wcvp_check_name` for records with resolved
taxons and its `summary`, and time cost.

## Check collectors dictionary

This module extracts the primary collector’s surname from the
`recordedBy` field in occurrence records and generates an association
list linking standardized surnames to corresponding records. It assists
to resolve collector identity ambiguities and enforce nomenclatural
consistency. Referred `CollectorDictionary` in this module comes from
ParseGBIF(De Melo et al. 2024).

``` r
dictionary <- prepare_collectors_dictionary(occ_import = occ_import,
                                            surname_selection_type = "largest_string",
                                            max_words_name = 6,
                                            min_characters_in_name = 2)
```

`dictionary` includes `my_dictionary` for checked collectors name and
`ref_dictionary` for referred dictionary, and time cost.

## Generate unique collection mark

To resolve duplicate records in biodiversity datasets, UltraGBIF
generates a standardized composite key for each unique collection event
like ParseGBIF(De Melo et al. 2024) does. This key is constructed by
concatenating three core metadata fields:

- **Plant family** (`family`)

- **Standardized collector surname** (`recordedByStandardized`)

- **Numeric-only collection number** (`recordNumber_Standard`)

The resulting identifier, `key_family_recordedBy_recordNumber`, groups
specimens sharing identical collection events. An optional temporal
variant, `key_year_recordedBy_recordNumber`, incorporates collection
year where available to enhance resolution.

Standardization ensures robust matching: non-numeric characters are
stripped from collection numbers, and collector names are normalized
using predefined dictionaries to reconcile spelling variants.

This module addresses two key challenges:

1.  Disambiguating common collector surnames through taxonomic context.

2.  Enabling automated duplicate detection without reliance on
    often-missing temporal metadata.

``` r
collection_key <- generate_collection_mark(occ_import = occ_import,
                                            dictionary = dictionary)
```

`collection_key` includes `occ_collectorsDictionary` for input records
updated with standardized fields and generated keys and
`CollectorsDictionary_add` for newly encountered collectors for
dictionary expansion and reuseand time costs.

## Set digital voucher

Following generation of a unique collection event identifier
(collection_key), this module resolves duplicate records and outlines
the selection of the master digital voucher and associated integration
to represent each event.

**Duplicate Grouping and Voucher Selection**

- For records possessing complete collection event keys, duplicates are
  identified and grouped based on identical `collection_key` values.
  Within each group, a master digital voucher is selected by evaluating
  a composite score that combines two dimensions: metadata completeness
  and geospatial reliability. Metadata completeness is quantified as the
  sum of binary presence indicators across 10 fields, including
  `recordedBy`, `locality`, and `catalogNumber`, where presence is
  scored as 1 and absence as 0. Geospatial reliability is assigned
  according to GBIF’s `EnumOccurrenceIssue` classification, with values
  of −1 indicating unaffected coordinates, −3 indicating potential
  positional error, and −9 indicating coordinates unsuitable for spatial
  analysis. The record achieving the highest combined score is
  designated the master voucher. Any missing metadata in the master
  record are supplemented using non-redundant values drawn from other
  records within the same group.

- For records with incomplete keys (e.g., missing `recordedBy` or
  `recordNumber`), no grouping is performed, and each is treated as a
  singleton event. Nevertheless, metadata completeness and geospatial
  scores are computed identically to ensure consistent, system-wide data
  quality assessment.

**Taxonomic Assignment**

- For groupable events, the accepted taxon name
  (`UltraGBIF_sample_taxon_name`) is assigned as the most frequently
  occurring species- or subspecies-level identification among
  duplicates; ties are resolved alphabetically. Unidentified events are
  flagged (`UltraGBIF_unidentified_sample = TRUE`).

- Non-groupable records are processed individually with identical
  taxonomic logic.

**Geospatial Integration**

If the master voucher has unusable coordinates (score = -9) or lacks
coordinates entirely, usable coordinates from duplicates (score ≥ -3)
are preferentially retained.

**Standardized Output Fields**

Key output variables include:

- `UltraGBIF_digital_voucher`: identifies master record (logical);

- `UltraGBIF_duplicates`: presence of duplicates (logical);

- `UltraGBIF_num_duplicates`: count of grouped records;

- `UltraGBIF_non_groupable_duplicates`: records with incomplete keys
  (logical);

- `UltraGBIF_duplicates_grouping_status`: grouping feasibility (e.e.,
  “groupable”, “not groupable: no recordedBy”);

- `UltraGBIF_sample_taxon_name_status`: taxonomic resolution
  (“identified”, “divergent”, “not identified”);

- `UltraGBIF_useful_for_spatial_analysis`: coordinate usability
  (logical).

This module ensures robust duplicate resolution, metadata enrichment,
and interoperability with global biodiversity standards.

``` r
voucher <- set_digital_voucher(occ_import = occ_import,
                               names.checked = names.checked,
                               collection_key = collection_key)
```

`voucher` includes the full flagged digital voucher of occurrence
records and its `summary`, and time cost.

## Refine records

When a `voucher` is flagged as usable, its missing information in fields
is restored by consolidating data from duplicate records belonging to
identical collection events, creating one clean record per unique event,
whether the event key is complete or not. It picks the most common
species-level name (using alphabetical order to break ties) or labels
the event “unidentified” if no such name exists; incomplete keys are
treated as separate events. Missing coordinates in the main record are
filled in from duplicates.

For finally complete usable vouchers, their associated occurrence
records are automatically subjected to geospatial validation and precise
WGSRPD attribution. Leveraging the R packages CoordinateCleaner(Zizka et
al. 2019) and terra, this module efficiently performs coordinate
validation and cleaning, targeting common geospatial anomalies such as
misassigned capitals, geographic centroids, duplicate coordinates, GBIF
headquarters artifacts, institution-proximate records, distributional
outliers, marine points in terrestrial datasets, and zero/placeholder
coordinates.

Then each validated record is matched to its corresponding WGSRPD
level-3 Botanical Country classifications with different status
(native/introduced/extinct/location_doubtful), which minimizes
mis-classification risks in cross-regional analyses.

To optimize computational efficiency, this module supports
parallelization through multi-threading by specifying the desired
concurrency level via the `threads` parameter.

``` r
refine <- refine_records(voucher = voucher,
                            threads = 4,
                            save_path = dirname (gbif_occurrence_file),
                            tests = c("capitals", "centroids", "equal", "gbif", "institutions", "outliers", "seas","zeros"))
```

`refine` includes `all_records` for all cleaned records and
`native_records` for native of them, and time costs. If you set
`save_path`, then `all_records` and `native_records` are exported, named
as `usable_refined_records.csv.gz` and `native_refined_records.csv.gz`,
both are potentially meritorious for your future research.

## Map records

This module is implemented optionally, rendering the output from
`coordinate_refine`, employing `geohashTools`(Chirico 2023) to perform
spatial clustering of geo-coordinates. The `precision` parameter
determines grid resolution (e.g. 4 for 20 km×20 km grids, 3 for 156
km×156 km grids). It groups records by `UltraGBIF_wcvp_taxon_name`,
WGSRPD information and `geohash grid` and follows deduplication.

After simplification, dynamic maps with different layers are rendered by
mapview(Appelhans et al. 2023) with WGSRPD information legend. Symbol
size is controlled by the `cex` parameter. Moreover, they supports zoom
functionality for spatial resolution adjustment and enables interactive
selection of individual coordinates to display their full attributes.

``` r
map_records(records_refined = refine,
                     precision = 4, 
                     cex = 4)
```

## Reference

Appelhans, Tim, Florian Detsch, Christoph Reudenbach, and Stefan
Woellauer. 2023. “Mapview: Interactive Viewing of Spatial Data in r.”
<https://CRAN.R-project.org/package=mapview>.

Boyle, Brad, Nicole Hopkins, Zhenyuan Lu, Juan Antonio Raygoza Garay,
Dmitry Mozzherin, Tony Rees, Naim Matasci, et al. 2013. “The Taxonomic
Name Resolution Service: An Online Tool for Automated Standardization of
Plant Names.” *BMC Bioinformatics* 14 (1): 16.
<https://doi.org/10.1186/1471-2105-14-16>.

Chirico, Michael. 2023. “geohashTools: Tools for Working with
Geohashes.” <https://CRAN.R-project.org/package=geohashTools>.

De Melo, Pablo Hendrigo Alves, Nadia Bystriakova, Eve Lucas, and
Alexandre K. Monro. 2024. “A New R Package to Parse Plant Species
Occurrence Records into Unique Collection Events Efficiently Reduces
Data Redundancy.” *Scientific Reports* 14 (1): 5450.
<https://doi.org/10.1038/s41598-024-56158-3>.

Govaerts, Rafaël, Eimear Nic Lughadha, Nicholas Black, Robert Turner,
and Alan Paton. 2021. “The World Checklist of Vascular Plants, a
Continuously Updated Resource for Exploring Global Plant Diversity.”
*Scientific Data* 8 (1): 215.
<https://doi.org/10.1038/s41597-021-00997-6>.

Zizka, Alexander, Daniele Silvestro, Tobias Andermann, Josué Azevedo,
Camila Duarte Ritter, Daniel Edler, Harith Farooq, et al. 2019.
“CoordinateCleaner : Standardized Cleaning of Occurrence Records from
Biological Collection Databases.” Edited by Tiago Quental. *Methods in
Ecology and Evolution* 10 (5): 744–51.
<https://doi.org/10.1111/2041-210X.13152>.
