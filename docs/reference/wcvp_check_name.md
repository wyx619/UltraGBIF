# Resolve taxons with WCVP database (internal)

Use the [World Checklist of Vascular
Plants](https://powo.science.kew.org//)
[database](https://github.com/matildabrown/rWCVPdata) to check accepted
names and update synonyms.

## Usage

``` r
wcvp_check_name(
  searchedName = "Tephrosia lasiochlaena Cowie",
  wcvp_names_database = NA,
  if_author_fails_try_without_combinations = TRUE
)
```

## Arguments

- searchedName:

  scientific name, with or without author

- wcvp_names_database:

  processed wcvp database

- if_author_fails_try_without_combinations:

  option for partial verification of the authorship of the species.
  Remove the authors of combinations, in parentheses

## Value

data.table with WCVP fields

## Details

About the [World Checklist of Vascular
Plants](https://powo.science.kew.org/about-wcvp) The `searchNotes` field
assigns values based on the following conditions:

- **Accepted**: When a single authorless scientific name exists in the
  `TAXON_name` list with `TAXON_STATUS = "Accepted"`,
  `verified_speciesName = 100`.

- **Accepted among homonyms**: When multiple authorless scientific names
  are present in the `TAXON_name` list, but only one homonym has
  `TAXON_STATUS = "Accepted"`,
  `verified_speciesName = number of matches / 100`.

- **Homonyms**: When multiple authorless scientific names appear in the
  `TAXON_name` list and either multiple or none of the homonyms have
  `TAXON_STATUS = "Accepted"`,
  `verified_speciesName = number of matches / 100`. Prior to homonym
  detection, attempts to match authorless scientific names in
  `TAXON_name` with authors in `TAXON_AUTHORS` failed, resulting in
  `verified_author = 0`.

- **Not Found**: When the authorless scientific name is absent from the
  `TAXON_name` list.

- **Unplaced**: When a single authorless scientific name exists in the
  `TAXON_name` list with `TAXON_STATUS = "Unplaced"`.

- **Updated**: When a single authorless scientific name is present in
  the `TAXON_name` list, and both `ACCEPTED_PLANT_NAME_ID` and the
  species ID differ (i.e.,
  `ACCEPTED_PLANT_NAME_ID != plant_name_id_of_searchedName`), the fields
  `taxon_status_of_searchedName`, `plant_name_id_of_searchedName`, and
  `taxon_authors_of_searchedName` are populated as follows:

- For `searchNotes = "Updated"`: These fields record data from the
  originally queried scientific name.

- For `searchNotes = "Homonyms"`: These fields record data from
  homonymous synonyms, separated by "\|".

The `verified_author` field is assigned as follows:

- **100**: When a direct match exists between the authorless scientific
  name in `TAXON_name` and an author in `TAXON_AUTHORS`.

- **50**: When a partial correspondence exists between the authorless
  scientific name in `TAXON_name` and an author in `TAXON_AUTHORS`,
  without combination.

- **0**: When no author is present in `TAXON_AUTHORS`, irrespective of
  correspondence with the authorless scientific name in `TAXON_name`.
