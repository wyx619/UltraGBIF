# Visualize refined records on a dynamic interactive map

This optional module simplifies and renders refined GBIF occurrence
records on dynamic maps

## Usage

``` r
map_records(records_refined = NA, precision = 4, cex = 4)
```

## Arguments

- records_refined:

  the refined occurrence list from `coordinate_refine`

- precision:

  positive integer scalar controlling the density of your refined GBIF
  occurrence records on dynamic maps. (i.e. 4 for 20 km, 3 for 156 km, 2
  for 1250 km.) Default is 4

- cex:

  the point size of your refined GBIF occurrence records rendering on
  dynamic maps. Default is 4

## Value

A dynamic interactive map with `wcvp_area_status` legend. Note that the
map has 3 different layers for you to choose to show.

## See also

[`gh_encode`](https://rdrr.io/pkg/geohashTools/man/gh_encode.html)

## Examples

``` r
# \donttest{
help(map_refined_records)
#> No documentation for ‘map_refined_records’ in specified packages and libraries:
#> you could try ‘??map_refined_records’
# }
```
