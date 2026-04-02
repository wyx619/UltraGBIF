#' @title Fast and Easy Compilation of GBIF Plant Occurrence Records in One R Package
#' @name UltraGBIF-package
#' @aliases UltraGBIF
#' @description
#'
#' \if{html}{\figure{logo.png}{options: width="120" alt="logo" style="float: right"}}
#'
#' GBIF hosts over 3 billion occurrence records. However, processing them typically
#' involves complex workflows and substantial computational overhead, posing significant
#' challenges for biodiversity researchers. To address these limitations, we present
#' \strong{UltraGBIF}, a fast and easy R package to parse, validate, and merge enormous
#' GBIF plant occurrence records into analysis-ready datasets.
#'
#' UltraGBIF integrates taxonomic resolution, spatial validation, duplicate consolidation,
#' and botanical region annotation within a high-performance framework. Its optimized
#' vectorized programming methods and intelligent parallelization enable compiling one
#' million GBIF occurrence records on a laptop within 15 minutes, without requiring
#' high-memory infrastructure.
#'
#' @details
#' \strong{Getting Started:}
#'
#' UltraGBIF provides a reproducible, plant-optimized, and computationally efficient
#' framework for transforming raw GBIF occurrence records into analysis-ready datasets.
#' The package functions are organized into 3 core stages and 8 distinct modules,
#' designed to be executed sequentially. After the core stages (modules 1-6), typically
#' approximately 35% of the initial occurrence records are retained as high-quality,
#' non-redundant data.
#'
#' \strong{Stage 1: Data Acquisition}
#'
#' This stage ensures data accuracy and consistency through three modules:
#'
#' \itemize{
#'   \item \strong{1. Import Records} (\code{\link{import_records}}): Receives a path to the
#'   GBIF occurrence download zip file. Loads all initial occurrence records and
#'   automatically extracts their GBIF issue flags for downstream quality assessment.
#'
#'   \item \strong{2. Check Taxon Name} (\code{\link{check_occ_taxon}}): Implements taxonomic
#'   name standardization using the Taxonomic Name Resolution Service (TNRS; Boyle et al.,
#'   2013) against the World Checklist of Vascular Plants (WCVP). Resolves synonyms,
#'   corrects misspellings, and assigns accepted names to ensure taxonomic consistency.
#'
#'   \item \strong{3. Check Collector Name} (\code{\link{check_collectors}}): Simplifies and
#'   extracts the primary collector's name from the \code{recordedBy} field, then builds
#'   a standardized collector dictionary. This reduces inconsistencies that can fragment
#'   single collection events and improves the accuracy of subsequent duplicate detection.
#' }
#'
#' \strong{Stage 2: Duplicate Removal and Reliability Filtering}
#'
#' This stage improves data reliability by identifying high-quality, non-redundant
#' occurrence records:
#'
#' \itemize{
#'   \item \strong{4. Generate Unique Collection Mark} (\code{\link{set_collection_mark}}):
#'   Identifies and consolidates duplicates into unique collection events. A collection
#'   event represents a distinct sampling instance (a specific collector at a specific
#'   date or with a specific record number). The collection event key is constructed as
#'   \code{Family + RecordBy + RecordNumber/EventDate}, enabling robust duplicate grouping.
#'
#'   \item \strong{5. Set Digital Voucher} (\code{\link{set_digital_voucher}}): Records
#'   possessing a "full collection mark" are grouped, and those within each group are
#'   scored across multiple dimensions (record completeness and geospatial quality).
#'   The record exhibiting the highest metadata quality is retained as the "digital
#'   voucher." Records lacking any component of this definition are treated as unique
#'   entities and proceed directly to scoring without aggregation. This strategy
#'   preserves the most geographically informative data while minimizing redundancy.
#' }
#'
#' \strong{Stage 3: Refine Records}
#'
#' This stage restores key information for usable records from their duplicates with
#' the same collection mark, enhances geospatial accuracy, and extracts native status:
#'
#' \itemize{
#'   \item \strong{6. Refine Records} (\code{\link{refine_records}}): Validates spatial
#'   information using CoordinateCleaner (Zizka et al., 2019) to flag spatial errors
#'   (e.g., centroids, capitals, institutions, seas, zeros). Restores detailed metadata
#'   from duplicate records to the selected digital vouchers. Extracts native status
#'   information by cross-referencing occurrence coordinates with WCVP distribution
#'   data and WGSRPD Level 3 areas, classifying records as native, introduced, extinct,
#'   location_doubtful, or unknown.
#' }
#'
#' \strong{Optional Stages: Visualization and Analysis}
#'
#' After completing the core pipeline, two optional modules are available for data
#' exploration and visualization:
#'
#' \itemize{
#'   \item \strong{Map Records} (\code{\link{map_records}}): Renders verified records
#'   onto customizable, dynamic interactive maps using \code{mapview}. Employs geohash-based
#'   deduplication to reduce visual clutter and displays records color-coded by native
#'   status classification. Supports multiple basemap layers (OpenStreetMap, Esri World
#'   Imagery, Stadia Stamen Watercolor).
#'
#'   \item \strong{Plot Richness} (\code{\link{plot_richness}}): Generates a species
#'   richness map by aggregating native refined records onto a one-degree geographic grid.
#'   Inspired by the \code{letsR} package (Vilela & Villalobos, 2015), this implementation
#'   fully leverages vectorized \code{terra} operations to achieve nearly a hundredfold
#'   speedup over traditional approaches.
#' }
#'
#' \strong{Quick Start Example:}
#'
#' \preformatted{
#' # Step 1: Import GBIF occurrence records
#' occ_import <- import_records(path = "path/to/gbif_download.zip")
#'
#' # Step 2: Standardize taxonomic names
#' taxa_checked <- check_occ_taxon(occ_import = occ_import, accuracy = 0.9)
#'
#' # Step 3: Standardize collector names
#' collectors_dictionary <- check_collectors(occ_import = occ_import, min_char = 2)
#'
#' # Step 4: Generate collection event keys
#' collection_key <- set_collection_mark(occ_import = occ_import,
#'                                       collectors_dictionary = collectors_dictionary)
#'
#' # Step 5: Select digital vouchers
#' voucher <- set_digital_voucher(occ_import = occ_import,
#'                                taxa_checked = taxa_checked,
#'                                collection_key = collection_key)
#'
#' # Step 6: Refine records (coordinate validation + native status)
#' refined_records <- refine_records(voucher = voucher,
#'                                   threads = 4,
#'                                   save_path = getwd())
#'
#' # Optional: Visualize results
#' map <- map_records(refined_records = refined_records, precision = 4, cex = 4)
#' richness <- plot_richness(refined_records = refined_records,
#'                           main = "Species Richness")
#' }
#'
#' \strong{Performance:}
#'
#' UltraGBIF achieves its outstanding performance through specific technical architectures:
#'
#' \itemize{
#'   \item \strong{C/C++ Backend Integration}: UltraGBIF strategically leverages three
#'   high-performance R packages---\code{data.table}, \code{stringi}, and \code{terra}---all
#'   implemented in C/C++ at their core. This architecture bypasses R's interpretive overhead,
#'   delegating computationally intensive operations to compiled code where type checking,
#'   memory allocation, and function dispatch occur once upon entry rather than per-iteration.
#'
#'   \item \strong{Vectorization Over Explicit Loops}: R, as an interpreted language, imposes
#'   substantial overhead on explicit \code{for} loops: type checking, bounds checking, and
#'   function dispatch execute at each iteration, often exceeding actual computation time.
#'   Furthermore, R's copy-on-modify semantics can trigger repeated memory allocation when
#'   dynamically growing vectors. Vectorized operations circumvent these bottlenecks by
#'   dispatching to pre-compiled C/Fortran routines that process entire vectors in a single
#'   call. The \code{prepare_collectors_dictionary()} function exemplifies this through
#'   \code{Vectorize()}, while \code{set_digital_voucher()} employs vectorized string matching
#'   (\code{stri_detect_fixed()}) and conditional assignment (\code{fcase()}) to eliminate
#'   interpreter overhead.
#'
#'   \item \strong{SIMD Exploitation}: Modern CPUs support Single Instruction Multiple Data
#'   (SIMD) parallelism---executing one instruction across multiple data elements simultaneously.
#'   Traditional scalar computation processes one datum per cycle, whereas SIMD instructions
#'   (e.g., AVX, AVX-512) process 8-16 elements concurrently. Vectorized R functions,
#'   particularly matrix operations in \code{terra::extract()} and string processing in
#'   \code{stringi}, enable compiler-level SIMD auto-vectorization. Explicit R loops cannot
#'   exploit this hardware capability due to interpreter intervention between iterations.
#'   The \code{plot_richness()} function demonstrates this: presence-absence matrix construction
#'   via vectorized cell indexing achieves near-hundredfold speedup over nested-loop alternatives.
#'
#'   \item \strong{Memory-Efficient Design}: Beyond speed, vectorization exhibits superior
#'   memory locality. Contiguous vector access patterns optimize CPU cache utilization---prefetchers
#'   efficiently load sequential memory blocks into L1/L2 cache. Conversely, \code{for} loops
#'   with random or indirect access suffer cache misses, incurring expensive RAM latency.
#'   \code{data.table} further enhances memory efficiency through in-place modification
#'   (\code{set()}, \code{:=}), eliminating intermediate copies during column updates---a
#'   critical optimization for million-record biodiversity datasets.
#'
#'   \item \strong{Chunk-Based Parallelization}: Computational parallelization is selectively
#'   deployed in \code{refine_records()}, where the dataset is partitioned into chunks
#'   proportional to available cores before distributing CoordinateCleaner validation across
#'   workers via \code{foreach}/\code{doParallel}. This hybrid approach---vectorized processing
#'   within chunks, parallel execution across chunks---maximizes throughput while minimizing
#'   inter-process communication overhead.
#' }
#'
#' On a standard laptop, UltraGBIF can compile one million occurrence records within 15 minutes.
#' The core pipeline typically retains approximately 35% of initial records as high-quality,
#' non-redundant data suitable for downstream analyses.
#'
#' @examples
#' \donttest{
#' # Browse the comprehensive tutorial vignette
#' vignette('Tutorial_of_UltraGBIF', package = 'UltraGBIF')
#' }
#'
#' @references
#' \enumerate{
#'   \item Appelhans, Tim, Florian Detsch, Christoph Reudenbach, and Stefan Woellauer. 2023.
#'   "Mapview: Interactive Viewing of Spatial Data in R."
#'   \url{https://CRAN.R-project.org/package=mapview}.
#'
#'   \item Boyle, Brad, Nicole Hopkins, Zhenyuan Lu, Juan Antonio Raygoza Garay, Dmitry Mozzherin,
#'   Tony Rees, Naim Matasci, et al. 2013. "The Taxonomic Name Resolution Service: An Online
#'   Tool for Automated Standardization of Plant Names." \emph{BMC Bioinformatics} 14 (1): 16.
#'   \doi{10.1186/1471-2105-14-16}.
#'
#'   \item Chirico, Michael. 2023. "geohashTools: Tools for Working with Geohashes."
#'   \url{https://CRAN.R-project.org/package=geohashTools}.
#'
#'   \item De Melo, Pablo Hendrigo Alves, Nadia Bystriakova, Eve Lucas, and Alexandre K. Monro. 2024.
#'   "A New R Package to Parse Plant Species Occurrence Records into Unique Collection Events
#'   Efficiently Reduces Data Redundancy." \emph{Scientific Reports} 14 (1): 5450.
#'   \doi{10.1038/s41598-024-56158-3}.
#'
#'   \item Vilela, Bruno, and Fabricio Villalobos. 2015. "letsR: A New R Package for Data Handling
#'   and Analysis in Macroecology." Edited by Timothée Poisot. \emph{Methods in Ecology and
#'   Evolution} 6 (10): 1229-34.
#'   \doi{10.1111/2041-210x.12401}.
#'
#'   \item Zizka, Alexander, Daniele Silvestro, Tobias Andermann, Josue Azevedo, Camila Duarte Ritter,
#'   Daniel Edler, Harith Farooq, et al. 2019. "CoordinateCleaner: Standardized Cleaning of
#'   Occurrence Records from Biological Collection Databases." Edited by Tiago Quental.
#'   \emph{Methods in Ecology and Evolution} 10 (5): 744-51.
#'   \doi{10.1111/2041-210X.13152}.
#' }
#'
"_PACKAGE"
