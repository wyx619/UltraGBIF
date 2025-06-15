## Introduction of **UltraGBIF** <img src="images/logo.png" style="width: 58px;"/>

Understanding and mapping species distributions is fundamental to biodiversity conservation, as it underpins critical assessments of species vulnerability, habitat integrity, and ecosystem resilience. Precise distribution data enable the identification of biodiversity hotspots, quantification of anthropogenic impacts, and predictive modeling of range shifts under climate change. However, existing biodiversity databases, including GBIF, often contain taxonomic inaccuracies, spatial biases, and incomplete records due to historical inconsistencies, synonymy issues, or uneven sampling efforts. These limitations compromise the reliability of downstream analyses, leading to flawed conservation prioritization, misdirected resource allocation, and ineffective policy decisions.

The goal of UltraGBIF is to parse and merge enormous [Global Biodiversity Information Facility (GBIF)](https://www.gbif.org/) records efficiently. GBIF is the largest biodiversity data repository around the world, hosting over **492 million** occurrence records for [*Tracheophyta*](https://www.gbif.org/occurrence/search?taxon_key=7707728&occurrence_status=present). However, data integration from heterogeneous databases often introduces redundancy and errors, which can compromise the accuracy of species richness measurements and subsequently affect ecological, biogeographic, and evolutionary studies. Moreover, processing massive occurrence datasets typically involves cumbersome workflows and time-consuming operations, posing significant challenges for researchers.

To address these limitations, we present UltraGBIF – an ultrafast and convenient R package designed for efficient parsing and merging GBIF records. Inspired by ParseGBIF(De Melo et al. 2024) and guided by the design principles of accessibility, computational efficiency, and optimal resource utilization, UIltraGBIF implements an integrated workflow comprising 5 key modules as below.

1.  **`Taxon Name Resolution`** Standardize and resolve the taxon name using the World Checklist of Vascular Plants(Govaerts et al. 2021), or using TNRS(Boyle et al. 2013) which has been integrated(Maitner and Boyle 2024).

2.  **`Reduce Duplicate Records`** Collates duplicate records into unique "collection events" while retaining spatial data entries with maximal completeness.

3.  **`Data Quality Evaluation`** Assesses record validity and spatial data quality through GBIF standardized issue flagging system.

4.  **`Coordinate Verification & World Geographical Scheme for Recording Plant Distributions (WGSRPD) information extraction`** Performs automated coordinate validation via CoordinateCleaner(Zizka et al. 2019) integration, complemented by [World Geographical Scheme for Recording Plant Distributions](http://www.tdwg.org/standards/109)(Govaerts et al. 2021) annotations for species occurrence status *(native/introduced/extinct/location_doubtful/unknown)*.

5.  **`Map and Visualization`** This optional module enables the processed species distribution data with WGSRPD information to be mapped onto a highly customizable dynamic map, thereby providing the most intuitive visualization of their spatial distributions and offering a streamlined interface for biodiversity research.

This unified workflow enables rapid processing of enormous records while maintaining rigorous quality control standards.

**Note:** UltraGBIF is currently under **active development**. If you encounter any bugs, please feel free to submit an issue. Your feedback is greatly appreciated!

## Workflow of UltraGBIF

![](images/workflow.jpg)

## Installation

You can install UltraGBIF like so:

``` r
### Fist install rWCVPdata
if (!requireNamespace("rWCVPdata",quietly = TRUE)) {
  install.packages("rWCVPdata", repos=c("https://matildabrown.github.io/drat",getOption("repos")))
}
### Then install UltraGBIF
if (!requireNamespace("remotes",quietly = TRUE)) {
  install.packages("remotes")
  }
remotes::install_github("wyx619/UltraGBIF")
```

## Reference

Boyle, Brad, Nicole Hopkins, Zhenyuan Lu, Juan Antonio Raygoza Garay, Dmitry Mozzherin, Tony Rees, Naim Matasci, et al. 2013. “The Taxonomic Name Resolution Service: An Online Tool for Automated Standardization of Plant Names.” *BMC Bioinformatics* 14 (1): 16. <https://doi.org/10.1186/1471-2105-14-16>.

De Melo, Pablo Hendrigo Alves, Nadia Bystriakova, Eve Lucas, and Alexandre K. Monro. 2024. “A New R Package to Parse Plant Species Occurrence Records into Unique Collection Events Efficiently Reduces Data Redundancy.” *Scientific Reports* 14 (1): 5450. <https://doi.org/10.1038/s41598-024-56158-3>.

Govaerts, Rafaël, Eimear Nic Lughadha, Nicholas Black, Robert Turner, and Alan Paton. 2021. “The World Checklist of Vascular Plants, a Continuously Updated Resource for Exploring Global Plant Diversity.” *Scientific Data* 8 (1): 215. <https://doi.org/10.1038/s41597-021-00997-6>.

Maitner, Brian, and Brad Boyle. 2024. “TNRS: Taxonomic Name Resolution Service.” [https://CRAN.R-project.org/package=TNRS](https://cran.r-project.org/package=TNRS).

Zizka, Alexander, Daniele Silvestro, Tobias Andermann, Josué Azevedo, Camila Duarte Ritter, Daniel Edler, Harith Farooq, et al. 2019. “CoordinateCleaner : Standardized Cleaning of Occurrence Records from Biological Collection Databases.” Edited by Tiago Quental. *Methods in Ecology and Evolution* 10 (5): 744–51. <https://doi.org/10.1111/2041-210X.13152>.
