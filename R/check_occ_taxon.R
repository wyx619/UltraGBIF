#' @title Resolve taxon names using Taxonomic Name Resolution Service against WCVP
#'
#' @name check_occ_taxon
#'
#' @description This function standardizes and validates plant taxonomic names by querying the
#' Taxonomic Name Resolution Service (TNRS) against the World Checklist of Vascular Plants (WCVP).
#' The TNRS corrects spelling errors, resolves alternative spellings, and converts outdated synonyms
#' to their current accepted names, enabling consistent taxonomic identification across the dataset.
#'
#' The function implements the following workflow:
#' \itemize{
#'   \item \strong{Name filtering}: Extracts unique scientific names from occurrence records at
#'   species-level taxonomic ranks (species, variety, subspecies, form) for resolution
#'   \item \strong{TNRS query}: Submits names to the TNRS API with parameters optimized for
#'   botanical nomenclature (sources = "wcvp", classification = "wfo", mode = "resolve",
#'   matches = "best"). Includes automatic retry logic (up to 3 attempts) for API reliability
#'   \item \strong{Name resolution}: For each submitted name, the TNRS performs:
#'   \itemize{
#'     \item \emph{Parsing}: Splits names into component parts (genus, specific epithet, authority,
#'     rank indicators) using the Global Names Biodiversity Name Parser
#'     \item \emph{Matching}: Applies exact and fuzzy matching via the Taxamatch algorithm, which
#'     accelerates searches by navigating within the taxonomic hierarchy
#'     \item \emph{Correction}: Identifies the accepted name for any matched synonyms according
#'     to WCVP taxonomy
#'     \item \emph{Best match selection}: Ranks multiple potential matches and selects the
#'     highest-quality resolution
#'   }
#'   \item \strong{Result integration}: Merges resolved taxonomic information back to the original
#'   occurrence records, including WCVP plant name IDs, accepted names, taxonomic status, and
#'   family assignments
#' }
#'
#' @param occ_import imported GBIF records from \code{\link{import_records}}
#' @param accuracy numeric threshold between 0 and 1 specifying the minimum match score required
#'   for a name resolution to be accepted. Names with match scores below this threshold are not
#'   resolved. Default is 0.9. See \code{\link[TNRS]{TNRS}} for details.
#'
#' @details
#' \strong{Taxonomic Name Resolution Service (TNRS):}
#'
#' The TNRS is a computational tool for standardizing plant scientific names. It corrects spelling
#' errors and alternative spellings to a standard list of names, and converts outdated synonyms to
#' current accepted names. The TNRS can process many names simultaneously, eliminating the need for
#' manual name correction. For comprehensive documentation, see
#' \url{https://tnrs.biendata.org/about/}.
#'
#' \strong{Taxonomic Source - World Checklist of Vascular Plants (WCVP):}
#'
#' WCVP (\url{https://powo.science.kew.org/}) is an expert-curated taxonomic
#' database maintained by the Royal Botanic Gardens, Kew. WCVP provides authoritative taxonomic
#' opinions on vascular plant nomenclature, including synonymy relationships and accepted name
#' designations. The TNRS does not provide its own taxonomic opinions; it accelerates the process
#' of researching name status according to these authoritative sources.
#'
#' \strong{Resolution Process:}
#'
#' The TNRS resolves names through four sequential steps:
#' \enumerate{
#'   \item \strong{Parse}: Names are split into component parts (genus, specific epithet, authority,
#'   rank indicators such as "var.", "subsp.", etc.). Standard botanical annotations (e.g., "sp. nov.",
#'   "ined.", "cf.", "aff.") are detected and separated to improve matching accuracy
#'   \item \strong{Match}: Parsed components are matched against the TNRS database using both exact
#'   matching and the Taxamatch fuzzy matching algorithm. Taxamatch accelerates searches by navigating
#'   within the taxonomic hierarchy (e.g., once a genus is identified, only species within that genus
#'   are searched)
#'   \item \strong{Correct}: The taxonomic status of matched names is examined. Outdated synonyms
#'   are converted to their accepted names according to WCVP. For erroneous names where no accepted
#'   name exists (e.g., invalid or illegitimate names), only the matched name is returned
#'   \item \strong{Select Best Match}: When multiple matches have identical scores, the Best Match
#'   Algorithm ranks candidates by match quality, preferring corrected synonyms over names already
#'   marked as accepted
#' }
#'
#' \strong{Output Fields:}
#'
#' The returned \code{occ_wcvp_check_name} table includes the following WCVP-derived fields:
#' \itemize{
#'   \item \code{wcvp_plant_name_id}: WCVP identifier for the matched name
#'   \item \code{wcvp_taxon_name}: Accepted taxon name
#'   \item \code{wcvp_taxon_status}: Taxonomic status ("Accepted" or \code{NA})
#'   \item \code{wcvp_family}: Accepted family name
#'   \item \code{wcvp_taxon_rank}: Taxonomic rank of the accepted name
#'   \item \code{wcvp_searchNotes}: Resolution outcome ("Accepted", "Updated", or "Not found")
#'   \item \code{wcvp_reviewed}: Manual review flag ("N" indicates not yet reviewed)
#' }
#'
#' \strong{Retry Mechanism:}
#'
#' The function implements automatic retry logic (up to 3 attempts with 5-second intervals) to
#' handle potential API connectivity issues or empty responses from the TNRS service.
#'
#' @return UltraGBIF_taxa_checked list containing:
#'   \itemize{
#'     \item \code{occ_wcvp_check_name}: A data.table containing the original occurrence records
#'     merged with WCVP taxonomic resolution results, including matched names, accepted names,
#'     taxonomic status, and family assignments
#'     \item \code{summary}: A data.table containing the unique taxonomic resolution results
#'     (one row per submitted name), suitable for reviewing resolution outcomes and identifying
#'     names requiring manual curation
#'     \item \code{runtime}: Execution time of the function
#'   }
#'
#' @references
#' \itemize{
#'   \item Boyle, B. et al. (2013). The taxonomic name resolution service: an online tool for
#'   automated standardization of plant names. \emph{BMC Bioinformatics}, 14, 16.
#'   \doi{10.1186/1471-2105-14-16}
#'   \item Govaerts, R. et al. (2023). World Checklist of Vascular Plants. \emph{Royal Botanic Gardens, Kew}.
#'   \url{https://powo.science.kew.org/}
#' }
#'
#' @seealso \code{\link[TNRS]{TNRS}}
#'
#'
#' @importFrom dplyr %>% case_when if_else select
#' @import data.table
#' @import stringi
#' @import TNRS
#' @examples
#' \dontrun{
#' taxa_checked <- check_occ_taxon(occ_import = occ_import,accuracy = 0.9)
#' }
#' @export
check_occ_taxon <- function(occ_import = NA,accuracy = 0.9){
  start=Sys.time()

  occ=occ_import$occ[, .(Ctrl_scientificName)]

  wcvp_na <- data.table(wcvp_plant_name_id  = NA_integer_,
                        wcvp_taxon_rank = NA_character_,
                        wcvp_taxon_status = NA_character_,
                        wcvp_family = NA_character_,
                        wcvp_taxon_name = NA_character_,
                        wcvp_taxon_authors = NA_character_,
                        wcvp_accepted_plant_name_id = NA_integer_,
                        wcvp_reviewed = NA_character_,
                        wcvp_taxon_status_of_searchedName = NA_character_,
                        wcvp_plant_name_id_of_searchedName = NA_character_,
                        wcvp_taxon_authors_of_searchedName = NA_character_,
                        wcvp_verified_author = NA_real_,
                        wcvp_verified_speciesName = NA_real_,
                        wcvp_searchNotes = NA_character_)

  taxon_levels <- c('SPECIES', 'VARIETY', 'SUBSPECIES', 'FORM')
  occ_all <- cbind(occ[, .(Ctrl_gbifID,wcvp_searchedName = Ctrl_scientificName)], wcvp_na)
  name_search_wcvp <- occ_all[stri_trans_toupper(occ[,Ctrl_taxonRank]) %in% taxon_levels, wcvp_searchedName]%>%unique()


  check_initial <- data.frame(ID = 1:length(name_search_wcvp),
                              taxon = name_search_wcvp)

  chunk_list <- split(check_initial, ceiling(seq_len(nrow(check_initial)) / 4000))
  n_chunks <- length(chunk_list)
  check_result_list <- vector("list", n_chunks)

  for (i in seq_len(n_chunks)) {
    chunk <- chunk_list[[i]]
    message(paste("Processing chunk", i, "of", n_chunks, "(rows", nrow(chunk), ")"))

    chunk_result <- data.frame()
    attempt <- 1
    max_attempts <- 3

    while (attempt <= max_attempts && nrow(chunk_result) == 0) {
      tryCatch({
        if (attempt > 1) {
          message(paste("Retry attempt", attempt, "of", max_attempts))
        }

        chunk_result <- TNRS(chunk,
                             sources = "wcvp",
                             classification = "wfo",
                             mode = "resolve",
                             matches = "best",
                             accuracy = 0.9,
                             skip_internet_check = TRUE) %>%
          data.table::setDT()

        if (nrow(chunk_result) == 0) {
          message("Query succeeded but returned empty result. Retrying...")
        }
      }, error = function(e) {
        message(paste("TNRS query failed:", conditionMessage(e)))
        chunk_result <<- data.frame()
      })

      attempt <- attempt + 1
      if (nrow(chunk_result) == 0 && attempt <= max_attempts) {
        Sys.sleep(5)
      }
    }

    if (nrow(chunk_result) == 0) {
      stop("Network error: TNRS API is unreachable for chunk", i, ". Please try again later.")
    }

    check_result_list[[i]] <- chunk_result
  }

  check_result <- data.table::rbindlist(check_result_list)

  check_temp <- check_result[,.(ori_sp_name=Name_submitted,
                                wcvp_plant_name_id_of_searchedName=Name_matched_id,
                                wcvp_searchedName=Name_matched,
                                wcvp_taxon_authors_of_searchedName=Author_matched,
                                wcvp_taxon_status_of_searchedName=Taxonomic_status,
                                wcvp_plant_name_id=Accepted_name_id,
                                wcvp_taxon_name=Accepted_name,
                                wcvp_taxon_authors=Accepted_name_author,
                                wcvp_accepted_plant_name_id=Accepted_name_id,
                                wcvp_taxon_rank=Accepted_name_rank,
                                wcvp_family=Accepted_family)]
  for (col in names(check_temp)) {
    set(check_temp,
        i = which(check_temp[[col]] == ""),
        j = col,
        value = NA_character_
    )
  }
  check_temp[,`:=`(wcvp_verified_author=NA_real_,
                   wcvp_verified_speciesName=NA_real_,
                   wcvp_searchNotes=NA_character_,
                   wcvp_reviewed=NA_character_)]
  check_temp[,wcvp_reviewed:=fifelse(is.na(wcvp_plant_name_id),NA_character_,"N")]
  check_temp[,wcvp_taxon_status:=fifelse(is.na(wcvp_plant_name_id),NA_character_,"Accepted")]
  check_temp[,wcvp_verified_author:=fifelse(is.na(wcvp_taxon_authors_of_searchedName)&is.na(wcvp_taxon_authors),0L,100L)]
  check_temp[,wcvp_verified_speciesName:=fifelse(is.na(wcvp_accepted_plant_name_id),0L,100L)]
  check_temp[,wcvp_searchNotes:=fcase(wcvp_verified_speciesName==0L,"Not found",
                                      wcvp_taxon_status_of_searchedName!="Accepted","Updated",
                                      default="Accepted")]
  check_temp[wcvp_taxon_rank=="genus",`:=`(wcvp_verified_speciesName=0L,wcvp_searchNotes="Not found",
                                           wcvp_taxon_status=NA_character_,wcvp_reviewed=NA_character_)]
  result <- check_temp


  occ_all=merge(occ_all[,.(Ctrl_gbifID,wcvp_searchedName)],
                result%>%select(-wcvp_searchedName),
                by.x = "wcvp_searchedName",
                by.y = "ori_sp_name",all.x = T)

  summary = unique(result[,ori_sp_name:=NULL])

  end=Sys.time()
  used=end-start
  message(paste('used',used%>%round(1),attributes(used)$units))
  Taxa_checked <- list(occ_wcvp_check_name = occ_all,
                       summary = summary,
                       runtime=used)
  class(Taxa_checked) <- 'UltraGBIF_taxa_checked'
  return(Taxa_checked)
}
