<a href="https://github.com/wyx619/UltraGBIF/"><img src="man/figures/logo.png" align="right" height="70" width="70"/></a> \# [**`UltraGBIF`**](https://github.com/wyx619/UltraGBIF/)

### Fast and easy compliation of GBIF plant occurrence records in one R package

## Introduction

Mapping plant distributions is fundamental to understanding biodiversity patterns, accurate distribution data and such information is necessary for researching plant diversity. Global Biodiversity Information Facility, known as [GBIF](https://www.gbif.org/), is a large repository for plant occurrence records worldwide. It has fueled over 18,200 peer-reviewed journal articles, with ecology (3,769 researches), climate change (2,953), conservation (1,915), and invasive species management (1,840) as of August 2025, supporting global policy frameworks like the Kunming-Montreal Global Biodiversity Framework.

Researchers using GBIF occurrence records usually rely on a suite of packages and scrips, consuming lots of runtime, Such as [`rgbif`](https://doi.org/10.32614/CRAN.package.rgbif), [`TNRS`](https://doi.org/10.32614/CRAN.package.TNRS), [`CoordinateCleaner`](https://doi.org/10.32614/CRAN.package.CoordinateCleaner), [`bdc`](https://doi.org/10.32614/CRAN.package.bdc), [`plantR`](https://doi.org/10.1111/2041-210X.13779), [`NSR`](https://doi.org/10.32614/CRAN.package.NSR) and [`GVS`](https://doi.org/10.32614/CRAN.package.GVS) help to deal with GBIF occurrence records. Moreover, for million records datasets, current methods incur substantial computational overhead through manual chaining of disparate packages, necessitating high-performance infrastructure despite advancing computational capabilities.

To rectify this situation, we introduce UltraGBIF, an efficient R package that unifies taxonomic resolution, spatial validation, duplicate consolidation, and botanical region annotation within a high-performance framework. Its optimized C/CXX- based dependencies and intelligent parallelization enable compiling one million GBIF occurrence records on a laptop within 15 minutes. In a word, UltraGBIF resolves challenges in reproducibility, scalability, and spatial-taxonomic integrity without increasing adoption barriers for biodiversity researchers.

## Workflow

***Three main stages and seven modules of UltraGBIF.** After all stages, generally 35% of the initial occurrence records are retained.* ![Workflow](man/figures/Workflow.png "UltraGBIF workflow")

UltraGBIF provides a reproducible, plant-optimized, and computationally efficient framework for transforming raw GBIF occurrence records into analysis-ready datasets. The package functions are categorized into three main stages and seven distinct modules.

**Stage 1: Data Acquisition**

This stage ensures data accuracy and consistency through three modules:

1.  Import Records: This module receives a user-provided Darwin Core Archive that adheres to GBIF data conventions. The DwC-A is loaded locally (e.g., occurrences.csv/zip) and any extensions described by meta.xml. GBIF-reported issue flags are automatically extracted for downstream quality assessment.

2.  Check Taxon Name: This module implements taxonomic name standardization to resolve and validate plant names by the Taxonomic Name Resolution Service (TNRS, Boyle et al. 2013). This step unifies synonyms and corrects misspellings.

3.  Check Collector Name: This module standardizes collector names to reduce inconsistencies (e.g., "Smith, J." versus "J. Smith") that can fragment single collection events. By preparing a standardized dictionary of primary collector surnames, this step reduces identification errors by over 80% and improves the accuracy of subsequent duplication checks.

**Stage 2: Duplicate Removal and Reliability Filtering**

This stage improves data reliability by identifying high-quality, non-redundant occurrence records.

4.  Generate Unique Collection Mark: This module identifies and consolidates duplicates into unique collection events. A collection event represents a distinct sampling instance (a specific collector at a specific time and place).

5.  Set Digital Voucher: For duplicate entries sharing a collection mark, the record with the highest metadata quality is retained as the "digital voucher." This approach preserves the most geographically informative data while minimizing redundancy, thereby improving the spatially reliability.

**Stage 3: Refine Records**

This stage restores key information, enhances geospatial accuracy, and extracts the native status information of occurrence records.

6.  Refine records: This module validates spatial information and restores detailed metadata for usable vouchers. It performs automated coordinate validation using CoordinateCleaner (Zizka et al., 2019) to flag spatial errors (e.g., centroids, capitals, institutions). It also extracts information from WCVP to annotate records as 'native', 'introduced', or 'doubtful'.

**Optional Stage**

Map records: An optional visualization module that renders verified records onto customizable, dynamic maps, providing an intuitive interface for viewing spatial distributions and data density.

Plot richness: This optional module is useful for creating a simple richness map from your processed occurrence records above. It is based on two functions `lets.presab.points` and `plot.PresenceAbsence` from R package `letsR`(Vilela and Villalobos 2015), but fully leverages vectorization techniques to avoid looping when filling large matrices, thus achieving nearly a hundredfold speedup.

Focused exclusively on GBIF plant occurrence records, UltraGBIF is able to clean one million records within 15 minutes on a laptop, representing 60% memory reduction. In a word, UltraGBIF integrates these components into a unified, automated workflow that enhances data standardization, accuracy, and usability, which enables robust, reproducible, and scalable compiling of GBIF occurrence records for advanced biodiversity research.

## Installation

UltraGBIF will be available on CRAN soon. The initial installation takes time.

``` r
options(repos = c(getOption("repos"),"https://anonymous.4open.science/r/Repo-902F"))
options(timeout = 600)
install.packages("UltraGBIF",type = 'source') ## install UltraGBIF
```

## Tutorial of UltraGBIF

A comprehensive tutorial is available at

``` r
library(UltraGBIF)
vignette("UltraGBIF")
```

## Reference

Appelhans, Tim, Florian Detsch, Christoph Reudenbach, and Stefan Woellauer. 2023. “Mapview: Interactive Viewing of Spatial Data in r.” <https://CRAN.R-project.org/package=mapview>.

Boyle, Brad, Nicole Hopkins, Zhenyuan Lu, Juan Antonio Raygoza Garay, Dmitry Mozzherin, Tony Rees, Naim Matasci, et al. 2013. “The Taxonomic Name Resolution Service: An Online Tool for Automated Standardization of Plant Names.” *BMC Bioinformatics* 14 (1): 16. <https://doi.org/10.1186/1471-2105-14-16>.

Chirico, Michael. 2023. “geohashTools: Tools for Working with Geohashes.” <https://CRAN.R-project.org/package=geohashTools>.

De Melo, Pablo Hendrigo Alves, Nadia Bystriakova, Eve Lucas, and Alexandre K. Monro. 2024. “A New R Package to Parse Plant Species Occurrence Records into Unique Collection Events Efficiently Reduces Data Redundancy.” *Scientific Reports* 14 (1): 5450. <https://doi.org/10.1038/s41598-024-56158-3>.

Vilela, Bruno, and Fabricio Villalobos. 2015. “letsR: A New R Package for Data Handling and Analysis in Macroecology.” Edited by Timothée Poisot. *Methods in Ecology and Evolution* 6 (10): 1229–34. <https://doi.org/10.1111/2041-210x.12401>.

Zizka, Alexander, Daniele Silvestro, Tobias Andermann, Josué Azevedo, Camila Duarte Ritter, Daniel Edler, Harith Farooq, et al. 2019. “CoordinateCleaner : Standardized Cleaning of Occurrence Records from Biological Collection Databases.” Edited by Tiago Quental. *Methods in Ecology and Evolution* 10 (5): 744–51. <https://doi.org/10.1111/2041-210X.13152>.
