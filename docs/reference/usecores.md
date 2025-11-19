# Detect threads and optimize them (internal)

Input a positive number and return optimized treads requirement.

## Usage

``` r
usecores(x)
```

## Arguments

- x:

  Threads requirement, a positive real number, default is 4

## Value

A positive real number of treads

## Details

If a positive integer is input, return a positive integer not exceeding
the total number of threads. If a non-integer positive number is input,
automatically calculate and return a reasonable number of threads.
