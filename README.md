## Introduction of **UltraGBIF** <img src="images/logo.png" style="width: 58px;"/>

Mapping plant distributions is fundamental to understanding biodiversity patterns, accurate distribution data and such information is necessary for evidence-based conservation planning and effective management of plant diversity.

Global Biodiversity Information Facility, known as GBIF, is a large and comprehensive open-access repository for plant occurrence records and facilitates the integration of millions of geo-referenced biodiversity records contributed by museums, herbaria, and individual researchers worldwide. Its importance is empirically validated by its extensive adoption in scholarly research: as of August 2025, GBIF has fueled over 18,200 peer-reviewed journal articles, with ecology (3,769 researches), climate change (2,953), conservation (1,915), and invasive species management (1,840) representing the most prominent thematic domains. All of them significantly highlight GBIF\`s indispensable role in fostering interdisciplinary collaboration, enhancing predictive capacity for biodiversity dynamics, and ultimately supporting global policy frameworks like the Kunming-Montreal Global Biodiversity Framework.

Researches using GBIF occurrence records usually rely on a suite of tools. **R**, with packages such as `rgbif`[@rgbif], `TNRS`[@TNRS] , `sf`[@sf], `terra`[@terra], `CoordinateCleaner`[@CoordinateCleaner] and WCVP[@rWCVPdata] enables researchers to deal with GBIF occurrence records. But for million records datasets, current methods incur substantial computational overhead through manual chaining of disparate packages, necessitating high-performance infrastructure despite advancing computational capabilities. Given the rapid advancement of laptops, this situation clearly requires improvement. An efficient R-based pipeline is therefore essential to standardize processing, enhance reproducibility, optimize resource efficiency, and preserve biological integrity for evidence-based conservation.

To rectify this situation, we introduce UltraGBIF, an efficient R package that unifies taxonomic resolution, spatial validation, duplicate consolidation, and botanical region annotation within a single high-performance framework. Building on ParseGBIF's unique collection event methodology[@demelo2024], UltraGBIF incorporates WCVP/TNRS-based taxon name resolution, CoordinateCleaner-enhanced spatial validation, automated WGSRPD botanical region extraction, and dynamic mapping while maintaining backward compatibility. Its optimized C/C++ backend and intelligent parallelization enable processing of million-record datasets on standard laptops within minutes while dramatically reducing manual tool chaining. Crucially, UltraGBIF achieves this expanded functionality through a set of intuitive functions that minimize user inputs, eliminate intermediate files, and support fully offline operation after initial setup, thereby resolving challenges in reproducibility, scalability, and spatial-taxonomic integrity without increasing adoption barriers for biodiversity researchers.

## Workflow of UltraGBIF

![](images/Workflow%20of%20UltraGBIF_01.png)

UltraGBIF comprises a set of functions that constitutes a complete and coherent workflow, which can be categorized into three components and some modules.

1.  The "Preliminary process" component forms the initial stage of the UltraGBIF workflow, ensuring data accuracy and consistency through three core modules: "Import records," which ingests and parses DwC-A files while extracting GBIF issues; "Check taxon name," which resolves plant taxon using WCVP or TNRS; and "Check collectors dictionary," which harmonizes collector name variants to resolve data fragmentation. Together, these modules automate critical data curation tasks, significantly enhancing the reliability of biodiversity datasets prior to analysis.

    -   "Import records" module serves as the primary input point of the UltraGBIF unified workflow, receiving a user-specified DwC-A file from GBIF and automatically extracting GBIF issues. This module is driven by data.table and stringi (both built on C/C++), thereby achieving exceptionally high processing speed.

    -   "Check taxon name" module implements taxonomic standardization to resolve and validate plant taxon. WCVP or TNRS is used to automatically match and correct taxon names against multiple taxonomic databases. This ensures consistent and accurate species identification.

    -   "Check collectors dictionary" module prepare collectors\` dictionary and verifies the primary collector's last name. This resolves critical data fragmentation in legacy biodiversity records where inconsistent name formats (e.g., "Smith, J." vs. "J. Smith") propagate potential errors and spatial biases. By automating nomenclatural standardization, it eliminates manual curation and reduces misidentified records by \>80%, ensuring reliable georeferencing for GBIF-derived biodiversity analyses.

2.  The "Filter usable records" component enhances data reliability by identifying high-quality, non-redundant occurrence records through two key modules: "Generate unique collection mark," which consolidates duplicates into distinct collection events based on standardized collector and numbering information, retaining the highest-quality record per event; and "Set digital voucher," which applies GBIF’s issue flagging system to filter records based on spatial accuracy and data integrity. Together, these modules ensure that only trustworthy, georeferenced records proceed to downstream analyses, significantly improving the robustness of biodiversity studies.

    -   "Generate unique collection mark" module identifies and consolidates duplicate records into unique collection events marked by `family + recordedBy_Standardized + recordNumber_Standardized`, ensuring that each event represents a distinct sampling or observation instance. Among duplicate entries, the record with the highest quality score is retained. This approach preserves the most geographically informative data while minimizing redundancy, thereby improving the quality and reliability of spatial and ecological analyses.

    -   "Set digital voucher" module evaluates the validity and spatial accuracy of each unique collection mark using the GBIF standardized issue flagging system. This quality control mechanism identifies potential errors or inconsistencies in occurrence records. It ensures that only high-quality, spatially reliable data are retained for subsequent processing and interpretation.

3.  The "Refine records" component restores key information, enhances geospatial accuracy and enables visualization through two modules.

    -   "Refine records" module module restores key information for usable vouchers by consolidating data from duplicate records belonging to identical collection events, performs automated coordinate validation by CoordinateCleaner and extracts WGSRPD information for taxa, including native, introduced, extinct, and location_doubtful status classifications. Specifically, UltraGBIF integrates the WGSRPD information from wcvp to directly match the coordinates of occurrence records with regional information, and annotate the corresponding distribution status (native/introduced/extinct, etc.). Although GBIF raw records partially contain country code information, it does not cover all records, and the country code used is the ISO 3166-1 alpha-2 two-letter country code, which cannot be losslessly converted to the Level 3 Botanical Countries code of the World Geographical Scheme for Recording Plant Distributions. Therefore, UltraGBIF operates directly on the coordinates of occurrence records to avoid potential matching issues.

    -   "Map records" optional module enables records with WGSRPD information to be mapped onto dynamic maps, providing intuitive visualization of spatial distributions and offering a streamlined interface for biodiversity research.

Focused exclusively on GBIF plant occurrence records, UltraGBIF is able to clean one million records within 15 minutes on laptops (8 threads, 32 GB RAM), representing 100x faster improvement and 60% memory reduction compared to conventional approaches. By enabling rapid, efficient, and reproducible access to deal with GBIF plants records, UltraGBIF democratizes large-scale plant biodiversity research across macroecology, conservation biology, and global change studies.

UltraGBIF is under development. If you encounter any bugs, please feel free to submit an [issue](https://github.com/wyx619/UltraGBIF/issues/new). Your feedback is greatly appreciated!

## Installation

It\`s easy to install UltraGBIF from GitHub, which ensures access to the latest version and all available features. UltraGBIF is built with rWCVPdata, so it is necessary to install it firstly. We recommend rWCVPdata version 0.6.0 with WCVP version 14 for UltraGBIF, and the initial installation takes some time.

```{r}
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes", dependencies = TRUE)}
remotes::install_github("matildabrown/rWCVPdata", upgrade=F) ## install rWCVPdata
remotes::install_github("wyx619/UltraGBIF", upgrade=F) ## install UltraGBIF
```

If you meet any internet error, download [rWCVPdata](https://wyx619.github.io/UltraGBIF619/src/contrib/rWCVPdata_0.6.0.tar.gz) and install manually. The initial installation also takes some time.

```{r}
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes", dependencies = TRUE)}
remotes::install_local("path/to/your/rWCVPdata_0.6.0.tar.gz", upgrade=F) ## install rWCVPdata manually
remotes::install_github("wyx619/UltraGBIF", upgrade=F) ## install UltraGBIF
```

## Reference

Boyle, Brad, Nicole Hopkins, Zhenyuan Lu, Juan Antonio Raygoza Garay, Dmitry Mozzherin, Tony Rees, Naim Matasci, et al. 2013. “The Taxonomic Name Resolution Service: An Online Tool for Automated Standardization of Plant Names.” *BMC Bioinformatics* 14 (1): 16. <https://doi.org/10.1186/1471-2105-14-16>.

De Melo, Pablo Hendrigo Alves, Nadia Bystriakova, Eve Lucas, and Alexandre K. Monro. 2024. “A New R Package to Parse Plant Species Occurrence Records into Unique Collection Events Efficiently Reduces Data Redundancy.” *Scientific Reports* 14 (1): 5450. <https://doi.org/10.1038/s41598-024-56158-3>.

Govaerts, Rafaël, Eimear Nic Lughadha, Nicholas Black, Robert Turner, and Alan Paton. 2021. “The World Checklist of Vascular Plants, a Continuously Updated Resource for Exploring Global Plant Diversity.” *Scientific Data* 8 (1): 215. <https://doi.org/10.1038/s41597-021-00997-6>.

Maitner, Brian, and Brad Boyle. 2024. “TNRS: Taxonomic Name Resolution Service.” [https://CRAN.R-project.org/package=TNRS](https://cran.r-project.org/package=TNRS).

Zizka, Alexander, Daniele Silvestro, Tobias Andermann, Josué Azevedo, Camila Duarte Ritter, Daniel Edler, Harith Farooq, et al. 2019. “CoordinateCleaner : Standardized Cleaning of Occurrence Records from Biological Collection Databases.” Edited by Tiago Quental. *Methods in Ecology and Evolution* 10 (5): 744–51. <https://doi.org/10.1111/2041-210X.13152>.
