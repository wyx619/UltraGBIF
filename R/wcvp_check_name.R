#' @title Resolve taxons with WCVP database (internal)
#' @name wcvp_check_name
#' @description Use the [World Checklist of Vascular Plants](https://powo.science.kew.org//)
#' [database](https://github.com/matildabrown/rWCVPdata) to check accepted names and update synonyms.
#'
#' @param searchedName scientific name, with or without author
#' @param wcvp_names_database processed wcvp database
#' @param if_author_fails_try_without_combinations option for partial verification of the authorship of the species. Remove the authors of combinations, in parentheses
#'
#' @details About the [World Checklist of Vascular Plants](https://powo.science.kew.org/about-wcvp)
#' The `searchNotes` field assigns values based on the following conditions:
#' - **Accepted**: When a single authorless scientific name exists in the `TAXON_name` list with `TAXON_STATUS = "Accepted"`, `verified_speciesName = 100`.
#' - **Accepted among homonyms**: When multiple authorless scientific names are present in the `TAXON_name` list, but only one homonym has `TAXON_STATUS = "Accepted"`, `verified_speciesName = number of matches / 100`.
#' - **Homonyms**: When multiple authorless scientific names appear in the `TAXON_name` list and either multiple or none of the homonyms have `TAXON_STATUS = "Accepted"`, `verified_speciesName = number of matches / 100`. Prior to homonym detection, attempts to match authorless scientific names in `TAXON_name` with authors in `TAXON_AUTHORS` failed, resulting in `verified_author = 0`.
#' - **Not Found**: When the authorless scientific name is absent from the `TAXON_name` list.
#' - **Unplaced**: When a single authorless scientific name exists in the `TAXON_name` list with `TAXON_STATUS = "Unplaced"`.
#' - **Updated**: When a single authorless scientific name is present in the `TAXON_name` list, and both `ACCEPTED_PLANT_NAME_ID` and the species ID differ (i.e., `ACCEPTED_PLANT_NAME_ID != plant_name_id_of_searchedName`), the fields `taxon_status_of_searchedName`, `plant_name_id_of_searchedName`, and `taxon_authors_of_searchedName` are populated as follows:
#'  - For `searchNotes = "Updated"`: These fields record data from the originally queried scientific name.
#'  - For `searchNotes = "Homonyms"`: These fields record data from homonymous synonyms, separated by "|".
#'
#' The `verified_author` field is assigned as follows:
#' - **100**: When a direct match exists between the authorless scientific name in `TAXON_name` and an author in `TAXON_AUTHORS`.
#' - **50**: When a partial correspondence exists between the authorless scientific name in `TAXON_name` and an author in `TAXON_AUTHORS`, without combination.
#' - **0**: When no author is present in `TAXON_AUTHORS`, irrespective of correspondence with the authorless scientific name in `TAXON_name`.
#'
#' @return data.table with WCVP fields
#'
#'
#'
#' @importFrom dplyr %>% filter mutate select distinct case_when if_else
#' @import stringi
#' @import data.table
#'
#' @export
wcvp_check_name <- function (searchedName = "Tephrosia lasiochlaena Cowie",
                             wcvp_names_database = NA,
                             if_author_fails_try_without_combinations = TRUE)
{
  if (is.na(searchedName) || searchedName == "") {
    stop("invalid searchedName!")
  }

  sp_wcvp <- standardize_name(searchedName)

  if (sp_wcvp$taxonAuthors != "") {

    index_author <- 100
    index <- which(
      wcvp_names_database$TAXON_NAME_U %in% stri_trans_toupper(sp_wcvp$standardizeName) &
        wcvp_names_database$TAXON_AUTHORS_U %in% stri_trans_toupper(stri_replace_all_regex(sp_wcvp$taxonAuthors, "\\s+", ""))
    )
    ntaxa <- length(index)

    if (ntaxa == 0 & if_author_fails_try_without_combinations ==TRUE) {
      index_author <- 50
      index <- which(wcvp_names_database$TAXON_NAME_U == stri_trans_toupper(sp_wcvp$standardizeName) &
                       wcvp_names_database$TAXON_AUTHORS_U == stri_trans_toupper(stri_replace_all_regex(sp_wcvp$taxonAuthors_last, "\\s+", "")))
      ntaxa <- length(index)
    }
    if (ntaxa == 0) {
      index_author <- 0
      index <- which(wcvp_names_database$TAXON_NAME_U %in% stri_trans_toupper(sp_wcvp$standardizeName))
      ntaxa <- length(index)
    }
  } else {
    index_author <- 0
    index <- which(wcvp_names_database$TAXON_NAME_U %in% stri_trans_toupper(sp_wcvp$standardizeName))
    ntaxa <- length(index)
  }

  if (ntaxa == 0) {
    x= data.table(searchedName = sp_wcvp$standardizeName,
                  taxon_status_of_searchedName = NA_character_,
                  plant_name_id_of_searchedName = NA_integer_,
                  taxon_authors_of_searchedName = NA_character_,
                  verified_author = index_author,
                  verified_speciesName = 0L,
                  searchNotes = "Not found")%>%
      rbind(wcvp_names_database[index],fill=T)
  }
  if (ntaxa == 1) {
    verified_speciesName <- 100
    x <- wcvp_names_database[index]
    teste_plant_name_id <- is.na(x$accepted_plant_name_id)
    id_accept <- ifelse(teste_plant_name_id == TRUE, "", x$accepted_plant_name_id)
    if ((!teste_plant_name_id) & (x$plant_name_id != id_accept)) {
      taxon_status_of_searchedName <- x$taxon_status
      plant_name_id_of_searchedName <- x$plant_name_id
      taxon_authors_of_searchedName <- x$taxon_authors
      index_synonym <- which(wcvp_names_database$plant_name_id == x$accepted_plant_name_id)
      x <- wcvp_names_database[index_synonym]
      x[,`:=`(searchedName = sp_wcvp$standardizeName,
              taxon_status_of_searchedName = taxon_status_of_searchedName,
              plant_name_id_of_searchedName = plant_name_id_of_searchedName,
              taxon_authors_of_searchedName = taxon_authors_of_searchedName,
              verified_author = index_author,
              verified_speciesName = verified_speciesName,
              searchNotes = "Updated")]
    }
    else {
      x <- x[,`:=`(searchedName = sp_wcvp$standardizeName,
                  taxon_status_of_searchedName = NA_character_,
                  plant_name_id_of_searchedName = NA_integer_,
                  taxon_authors_of_searchedName = NA_character_,
                  verified_author = index_author,
                  verified_speciesName = verified_speciesName,
                  searchNotes = taxon_status)]
    }
  }

  if (ntaxa > 1) {
    taxon_status_of_searchedName <- paste(wcvp_names_database[index,taxon_status], collapse = "|")
    plant_name_id_of_searchedName <- paste(wcvp_names_database[index,plant_name_id], collapse = "|")
    taxon_authors_of_searchedName <- paste(wcvp_names_database[index,taxon_authors], collapse = "|")

    index_status <- wcvp_names_database[TAXON_NAME_U == stri_trans_toupper(sp_wcvp$standardizeName) &
                            wcvp_names_database$taxon_status == "Accepted",]
    ntaxa_status <- nrow(index_status)

    if (ntaxa_status == 1) {
      x <- index_status
      x <- x[, `:=`(searchedName = sp_wcvp$standardizeName,
                    taxon_status_of_searchedName = taxon_status_of_searchedName,
                    plant_name_id_of_searchedName = plant_name_id_of_searchedName,
                    taxon_authors_of_searchedName = taxon_authors_of_searchedName,
                    verified_author = index_author,
                    verified_speciesName = 100/ntaxa,
                    searchNotes = "Accepted among homonyms")]
      } else {
        x = data.table(searchedName = stri_trans_toupper(sp_wcvp$standardizeName),
                       taxon_status_of_searchedName = taxon_status_of_searchedName,
                       plant_name_id_of_searchedName = plant_name_id_of_searchedName,
                       taxon_authors_of_searchedName = taxon_authors_of_searchedName,
                       verified_author = index_author,
                       verified_speciesName = 100/ntaxa,
                       searchNotes = "Homonyms")%>%
          rbind(wcvp_names_database[0,],fill = T)
    }
  }

  x %>% setnames(paste0("wcvp_", names(.)))

  return(x)
}
