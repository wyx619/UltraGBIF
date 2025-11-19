# Set master digital voucher

### Grouping Duplicates and Selecting the Digital Voucher

#### 1. Handling Complete Collection Event Keys

- Unique collection events often generate multiple duplicate GBIF
  records. One duplicate is designated as the master digital voucher,
  integrating data from other duplicates.

- When the collection event key is complete, duplicates are grouped and
  parsed based on record completeness and geospatial quality:

  - *Record Completeness* : Assessed using data-quality scores for
    fields: `recordedBy`, `recordNumber`, `year`, `institutionCode`,
    `catalogNumber`, `locality`, `municipality`, `countryCode`,
    `stateProvince`, and `fieldNotes`.

  - *Geospatial Quality* : Ranked via a score derived from geospatial
    issues in `EnumOccurrenceIssue` (GBIF table).

- The duplicate with the highest total score (sum of record
  completeness + geospatial quality) is assigned as the master voucher.
  Missing data from other duplicates are merged into this voucher.

#### 2. Handling Incomplete Collection Event Keys

- When the collection event key is incomplete, duplicates cannot be
  parsed. Each record is treated as a unique collection event, with no
  duplicates identified.

- Record completeness and geospatial quality are still evaluated as
  described below to assess data integrity.

#### 3. Quality Score Calculation

**UltraGBIF_digital_voucher** : The duplicate with the highest total
score, calculated as

### Total Score = Record Completeness + Quality of Geospatial Information

#### 4. Record Completeness Calculation

- Measured as the sum of binary flags (TRUE = 1, FALSE = 0) for the
  following fields:

  - Is `recordedBy` present?

  - Is `recordNumber` present?

  - Is `year` present?

  - Is `institutionCode` present?

  - Is `catalogNumber` present?

  - Is `locality` present?

  - Is `municipality` present?

  - Is `countryCode` present?

  - Is `stateProvince` present?

  - Is `fieldNotes` present?

#### 5. Geospatial Quality Calculation

- Based on GBIF geospatial issues in `EnumOccurrenceIssue`, classified
  into three categories:

  - *No Impact* : Issues not affecting coordinate accuracy
    (`selection_score = -1`).

  - *Potential Impact* : Issues that may affect coordinate accuracy
    (`selection_score = -3`).

  - *Exclusion* : Records with severe issues (`selection_score = -9`)
    are excluded.

**This optimized version eliminates O(n²) complexity by using vectorized
data.table operations.**

## Usage

``` r
set_digital_voucher(occ_import = NA, names.checked = NA, collection_key = NA)
```

## Arguments

- occ_import:

  imported GBIF records

- names.checked:

  your checked taxon names from `check_occ_name`

- collection_key:

  your collection mark from `generate_collection_mark`

## Value

A list with duration and 2 data.table: "occ_digital_voucher" for all
data processing fields and "occ_results" for only result fields.

## Details

`UltraGBIF_duplicates_grouping_status` :

- `groupable`: Complete key, duplicates parsed.

- `not groupable: no recordedBy and no recordNumber`: Missing both
  fields.

- `not groupable: no recordNumber`: Missing recordNumber.

- `not groupable: no recordedBy`: Missing recordedBy.

`UltraGBIF_num_duplicates`: Integer count of duplicate records.

`UltraGBIF_duplicates`: TRUE if duplicates exist, FALSE otherwise.

`UltraGBIF_non_groupable_duplicates`: TRUE if duplicates cannot be
grouped, FALSE otherwise.

## Examples

``` r
# \donttest{
help(set_digital_voucher)
#> ℹ Rendering development documentation for "set_digital_voucher"
# }
```
