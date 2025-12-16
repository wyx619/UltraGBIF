# Prepare a list with surname of the main collector

It returns a list with the last name of main collector following the
unique key `recordedBy`.

Consistent recording of the primary collectors and their last name is
thus essential, and a **collector dictionary** is provided for this
purpose. The `prepare_collectors_dictionary` function is utilized to
extract the last name of the primary collector from the `recordedBy`
field and to compile a list linking the last name of the primary
collector with the raw data in `recordedBy`

It is advisable to verify the **last name** of the primary collector in
the `nameRecordedBy_Standard` field. The objective is to standardize the
last name of the primary collector, which is automatically derived from
the `recordedBy` field. This is accomplished by standardizing the text
string to begin with an uppercase character and replacing non-ASCII
characters, ensuring that the primary collector associated with a
collection event is consistently represented by the same character
string.

Once processed, the dictionary can be checked in the future.

## Usage

``` r
prepare_collectors_dictionary(
  occ_import = NA,
  surname_selection_type = "largest_string",
  max_words_name = 6,
  min_characters_in_name = 4
)
```

## Arguments

- occ_import:

  imported GBIF records

- surname_selection_type:

  allows you to select any of two types of the last name:

  **`large_string`** = word with the largest number of characters.

  **`last_name`** = literally the last name of the main collector, with
  more than two characters.

- max_words_name:

  assist in checking the length of name

- min_characters_in_name:

  min characters in name

## Value

A list with duration and 2 data.table: "my_dictionary" for your
processed collectors dictionary and "ref_dictionary" for collectors name
reference dictionary

## Details

If `recordedBy` is present in the ref_dictionary, the function returns
the last name of the primary collector associated with the specific
`recordedBy` key. Otherwise, it returns the last name of the primary
collector, extracted from the `recordedBy` field.

It is recommended to curate the last name of the primary collector,
which is automatically derived from the `recordedBy` field. The goal is
to standardize the last name of the primary collector. The primary
botanical collector of a sample is consistently identified by the same
last name, standardized in capital letters with non-ASCII characters
replaced.

## Examples

``` r
# \donttest{
help(collectors_prepare_dictionary)
#> No documentation for ‘collectors_prepare_dictionary’ in specified packages and libraries:
#> you could try ‘??collectors_prepare_dictionary’
# }
```
