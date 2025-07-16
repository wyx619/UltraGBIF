## Introduction of **UltraGBIF** <img src="images/logo.png" style="width: 58px;"/>

Mapping plant distributions is essential for understanding biodiversity patterns and advancing conservation efforts, as it provides the foundation for assessing its vulnerability, habitat integrity, and ecosystem resilience. Accurate distribution data are critical for identifying biodiversity hotspots, evaluating the impacts of human activities, and predicting how plants may respond to climate change. Such information is indispensable for informed conservation planning and effective management of plant diversity.

The contribution of public available data to understanding and conserving biodiversity is indispensable in modern ecological research. With the increasing availability of open-access data platforms, scientists and conservation practitioners are now able to overcome traditional limitations in plants distribution knowledge, particularly in data-poor regions. These platforms aggregate occurrence records, trait data, and phylogenetic information from diverse sources, enabling large-scale analyses that were previously unfeasible.

Among the most influential databases, the **Global Biodiversity Information Facility (GBIF)** stands out as the largest and most comprehensive open-access repository for plants occurrence data. GBIF facilitates the integration of millions of georeferenced biodiversity records contributed by museums, herbaria, and individual researchers worldwide. Similarly, the **Global Inventory of Floras and Traits (GIFT)** complements these occurrence-based platforms by offering standardized botanical data for global plants, aiding in the study of floristic patterns and functional diversity.

In parallel, the **Biodiversity Informatics Enablement Network (BIEN)** has emerged as a powerful framework for synthesizing data on plant and animal distributions, particularly for trait-based ecological and evolutionary studies. BIEN integrates data from GBIF, TRY, and other sources to support macroecological analyses and predictive modeling efforts.

These databases are often supported by a suite of open-source analytical tools and software. **R**, with packages such as `rgbif`[@rgbif], `TNRS`[@TNRS] , `sf`[@sf], `terra`[@terra], `BIEN`[@BIEN], `CoordinateCleaner`[@CoordinateCleaner], and `GIFT`[@GIFT], has become the standard for biodiversity data integration, analysis, and modeling. These tools enable researchers to efficiently access, process, and analyze large datasets, facilitating everything from plants distribution modeling to assessments of functional diversity and biogeographic patterns.

Together, public available data and associated tools have transformed biodiversity science by democratizing access to information, enhancing the accuracy of conservation assessments, and enabling predictive modeling under global change scenarios. As data-sharing practices continue to evolve, the integration of open-access biodiversity data will remain central to advancing both theoretical and applied aspects of conservation biology.

GBIF is the world’s largest open-access biodiversity data repository and hosts over 500 million records for [**Tracheophyta**](https://www.gbif.org/occurrence/search?taxon_key=7707728&occurrence_status=present), a foundational group for terrestrial ecosystems and a key focus in ecological, biogeographic, and evolutionary research. However, despite their immense value, biodiversity databases such as GBIF often inherit taxonomic inaccuracies, spatial biases, and incomplete records from their source data—primarily due to historical inconsistencies in specimen collection, unresolved synonymy, and uneven sampling efforts. These limitations can undermine the reliability of downstream analyses, potentially resulting in misallocation of resources, and ineffective policy decisions. Moreover, such issues can distort plants richness estimates and ultimately compromise the accuracy of biodiversity analyses and related scientific studies.

To address these limitations, we present UltraGBIF, an ultrafast and convenient R package designed for efficient parsing and merging GBIF records. Inspired by ParseGBIF(De Melo et al. 2024) and guided by the design principles of accessibility, computational efficiency, and optimal resource utilization.

## Workflow of UltraGBIF

![](images/workflow.jpg)

The pipeline described below is specifically designed to enable rapid processing of large-volume plant occurrence records from GBIF, while maintaining rigorous quality control standards. Focused exclusively on plant data, it streamlines the cleaning, filtering, and standardization of GBIF-derived records, ensuring high data integrity and consistency for downstream analyses such as species distribution modeling, biodiversity assessments, and macro-ecological studies.

1.  **`Taxon Name Resolution`** This section includes a taxonomic standardization step to resolve and validate scientific names of plant taxa. This is achieved by referencing the World Checklist of Vascular Plants, a comprehensive and authoritative global resource. Alternatively, the online Taxonomic Name Resolution Service (TNRS), which has been built in following the protocol of Maitner and Boyle (2024), can be used to automatically match and correct taxon names against multiple taxonomic databases. This ensures consistent and accurate species identification, a critical prerequisite for reliable biodiversity analyses and cross-dataset integration.

2.  **`Reduce Duplicate Records`** To enhance data efficiency and analytical accuracy, this section identifies and consolidates duplicate records into unique collection events, ensuring that each event represents a distinct sampling or observation instance. Among duplicate entries, the record with the highest score is retained. This approach preserves the most geographically informative data while minimizing redundancy, thereby improving the quality and reliability of downstream spatial and ecological analyses.

3.  **`Data Quality Evaluation`** This section evaluates the validity and spatial accuracy of each record using the GBIF standardized issue flagging system. This built-in quality control mechanism identifies potential errors or inconsistencies in occurrence data, such as problematic coordinate formatting, spatial mismatches, or taxonomic discrepancies. Records are flagged based on severity, enabling users to apply customized filtering thresholds depending on the requirements of downstream analyses. This step ensures that only high-quality, spatially reliable data are retained for subsequent processing and interpretation.

4.  **`Coordinate Verification & WGSRPD information extraction`** This section performs automated coordinate validation by CoordinateCleaner(Zizka et al. 2019) and [WGSRPD](http://www.tdwg.org/standards/109) information extraction of taxons *(native/introduced/extinct/location_doubtful)*.

5.  **`Map and Visualization`** This optional section enables these processed distribution data with WGSRPD information to be mapped onto a highly customizable dynamic map, thereby providing the most intuitive visualization of their spatial distributions and offering a streamlined interface for biodiversity research.

**Note:** UltraGBIF is still under **development**. If you encounter any bugs, please feel free to submit an issue. Your feedback is greatly appreciated!

## Installation

You can install UltraGBIF like so:

``` r
if (!requireNamespace("remotes",quietly = TRUE)) {
  install.packages("remotes")
} ##check remotes
if (!requireNamespace("rWCVPdata",quietly = TRUE)) {
  remotes::install_github("matildabrown/rWCVPdata")
} ##install rWCVPdata
remotes::install_github("wyx619/UltraGBIF")##install UltraGBIF
```

## Reference

Boyle, Brad, Nicole Hopkins, Zhenyuan Lu, Juan Antonio Raygoza Garay, Dmitry Mozzherin, Tony Rees, Naim Matasci, et al. 2013. “The Taxonomic Name Resolution Service: An Online Tool for Automated Standardization of Plant Names.” *BMC Bioinformatics* 14 (1): 16. <https://doi.org/10.1186/1471-2105-14-16>.

De Melo, Pablo Hendrigo Alves, Nadia Bystriakova, Eve Lucas, and Alexandre K. Monro. 2024. “A New R Package to Parse Plant Species Occurrence Records into Unique Collection Events Efficiently Reduces Data Redundancy.” *Scientific Reports* 14 (1): 5450. <https://doi.org/10.1038/s41598-024-56158-3>.

Govaerts, Rafaël, Eimear Nic Lughadha, Nicholas Black, Robert Turner, and Alan Paton. 2021. “The World Checklist of Vascular Plants, a Continuously Updated Resource for Exploring Global Plant Diversity.” *Scientific Data* 8 (1): 215. <https://doi.org/10.1038/s41597-021-00997-6>.

Maitner, Brian, and Brad Boyle. 2024. “TNRS: Taxonomic Name Resolution Service.” [https://CRAN.R-project.org/package=TNRS](https://cran.r-project.org/package=TNRS).

Zizka, Alexander, Daniele Silvestro, Tobias Andermann, Josué Azevedo, Camila Duarte Ritter, Daniel Edler, Harith Farooq, et al. 2019. “CoordinateCleaner : Standardized Cleaning of Occurrence Records from Biological Collection Databases.” Edited by Tiago Quental. *Methods in Ecology and Evolution* 10 (5): 744–51. <https://doi.org/10.1111/2041-210X.13152>.
