#' @title Using integrated Taxonomic Name Resolution Service to check taxon names and update synonyms under World Checklist of Vascular Plants
#'
#' @name check_occ_name
#'
#' @description Names of taxa are checked using integrated Taxonomic Name Resolution Service (TNRS)
#'
#' @param occ_import imported GBIF records
#' @param accuracy numeric. If specified, only matches with a score greater than or equal to the supplied accuracy level will be returned. If left, the default threshold will be 0.9.
#'
#' @details
#' * [About TNRS](https://tnrs.biendata.org/about/)
#' * [World Checklist of Vascular Plants](https://powo.science.kew.org//)
#'
#' @return A list with duration and two data.table: "summary" with taxa list and "occ_wcvp_check_name" with WCVP fields
#' @examples
#'\dontrun{
#' taxa_checked <- check_occ_name(occ_import = occ_import,accuracy = 0.9)
#'}
#' @seealso \code{\link[TNRS]{TNRS}}
#'
#'
#' @importFrom dplyr %>% case_when if_else select
#' @import data.table
#' @import stringi
#' @import TNRS
#'
#' @export
check_occ_name <- function(occ_import = NA,accuracy = 0.9){
  start=Sys.time()

  occ=occ_import$occ%>%setDT()

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
  check_result <- NULL
  attempt <- 1
  max_attempts <- 3

  while(attempt <= max_attempts && (is.null(check_result) || nrow(check_result) == 0)) {
    try({
      if (attempt > 0) {
        message(paste("Attempt", attempt, "of", max_attempts))
      }

      check_result <- TNRS(check_initial,
                           sources = "wcvp",
                           classification = "wfo",
                           mode = "resolve",
                           matches = "best",
                           accuracy = 0.9,
                           skip_internet_check = TRUE) %>%
        data.table::setDT()

      if (nrow(check_result) == 0) {
        message("Query succeeded but returned empty result. Retrying...")
      }
    }, silent = TRUE)

    attempt <- attempt + 1
    if (nrow(check_result) == 0) {
      Sys.sleep(5)
    }
  }

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
                       duration=used)
  class(Taxa_checked) <- 'UltraGBIF_taxa_checked'
  return(Taxa_checked)
}
