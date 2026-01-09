# Extract last name of the main collector (internal)

Extract last name of the main collector in recordedBy field

## Usage

``` r
get_collectors_name(
  x = NA,
  surname_selection_type = "largest_string",
  max_words_name = 6,
  min_characters_in_name = 2
)
```

## Arguments

- x:

  collectors_name in recordedBy field

- surname_selection_type:

  allows you to select two types of results for the main collector's
  last name:

  **`large_string`** = word with the largest number of characters.

  **`last_name`** = literally the last name of the main collector, with
  more than two characters.

- max_words_name:

  assist in checking the length of name

- min_characters_in_name:

  min characters in name

## Value

last name of the main collector

## Details

Returns the last name
