#' @title (Copilot) Check species names against WCVP database
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
#' @examples
#' \donttest{
#' require(data.table)
#' ## It might take a little time
#' wcvp_names_database = setDT(rWCVPdata::wcvp_names)[,`:=`(TAXON_NAME_U=toupper(taxon_name),
#' TAXON_AUTHORS_U=toupper(taxon_authors))]
#'
#' # 1) Updated
#' wcvp_check_name(searchedName = 'Hemistylus brasiliensis Wedd.',
#'                wcvp_names_database = wcvp_names_database,
#'                if_author_fails_try_without_combinations = TRUE)
#'
#' # 2) Accepted
#' wcvp_check_name(searchedName = 'Hemistylus boehmerioides Wedd. ex Warm.',
#'                wcvp_names_database = wcvp_names_database,
#'                if_author_fails_try_without_combinations = TRUE)
#'
#' # 3) Unplaced - taxon_status = Unplaced
#' wcvp_check_name(searchedName = 'Leucosyke australis Unruh',
#'                wcvp_names_database = wcvp_names_database,
#'                if_author_fails_try_without_combinations = TRUE)
#'
#' # 4) Accepted among homonyms - When author is not informed. In this case, one of
#' # the homonyms, taxon_status is accepted
#' wcvp_check_name(searchedName = 'Parietaria cretica',
#'                wcvp_names_database = wcvp_names_database,
#'                if_author_fails_try_without_combinations = TRUE)
#'
#' # When author is informed
#' wcvp_check_name(searchedName = 'Parietaria cretica L.',
#'                wcvp_names_database = wcvp_names_database,
#'                if_author_fails_try_without_combinations = TRUE)
#'
#' # When author is informed
#' wcvp_check_name(searchedName = 'Parietaria cretica Moris',
#'                wcvp_names_database = wcvp_names_database,
#'                if_author_fails_try_without_combinations = TRUE)
#'
#' # 5) Homonyms - When author is not informed. In this case, none of the homonyms,
#' # taxon_status is Accepted
#' wcvp_check_name(searchedName = 'Laportea peltata',
#'                wcvp_names_database = wcvp_names_database,
#'                if_author_fails_try_without_combinations = TRUE)
#'
#' # When author is informed
#' wcvp_check_name(searchedName = 'Laportea peltata Gaudich. & Decne.',
#'                wcvp_names_database = wcvp_names_database,
#'                if_author_fails_try_without_combinations = TRUE)
#'
#' # When author is informed
#' wcvp_check_name(searchedName = 'Laportea peltata (Blume) Gaudich.',
#'                wcvp_names_database = wcvp_names_database,
#'                if_author_fails_try_without_combinations = TRUE)
#' }
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

  standardize_name <- function (searchedName = "Lupinus polyphyllus var. pallidipes (A.Heller) C.P.Sm.")
  {
    if (is.na(searchedName)) {
      return(list(searchedName = "",
                  standardizeName = "",
                  taxonAuthors = "",
                  taxonAuthors_last = ""))
    }

    x <- {}
    infrataxa = ""
    searchedName_raw <- searchedName
    searchedName_ori <- searchedName
    taxon_authors_last <- ""
    sp <- stringi::stri_split_fixed(searchedName, " ", simplify = TRUE)

    padrao <- c("var.", "nothosubsp.", "subsp.", " f. ")

    if (length(sp) > 1) {
      if ((stri_detect_fixed(searchedName, padrao[1]) & !stri_detect_fixed(searchedName, "Sivar.")) | any(stri_detect_fixed(searchedName, padrao[-1]))) {
        ip <- 1
        for(ip in 1:length(padrao))
        {
          if(stri_detect_fixed(searchedName, padrao[ip]))
          {
            indx <- sp == padrao[ip]
            infrataxa <- ""
            for(i in seq_along(indx)) {
              if(indx[i] && length(sp) > i) {
                infrataxa <- sp[i+1]
                break
              }
            }
            searchedName <- if (stri_detect_fixed(searchedName_raw, "<U+00D7>")) {
              paste0(sp[1], " <U+00D7> ", sp[3], ifelse(infrataxa == "", "", paste0(" ", padrao[ip], " ", infrataxa)))
            } else {
              paste0(sp[1], " ", sp[2], " ", padrao[ip], " ", infrataxa)}
            break
          }
        }
      }else {
        if (stri_detect(searchedName_raw, fixed = "<U+00D7>")) {
          if (stri_detect(sp[2], fixed = "<U+00D7>")) {
            searchedName <- stri_replace_first_fixed(searchedName, "<U+00D7>", "<U+00D7> ")
            sp <- stri_split_fixed(searchedName, " ") %>%
              unlist()
          }
          searchedName <- paste0(sp[1], " <U+00D7> ", sp[3])
          if (stri_sub(sp[1], 1, 1) == "<U+00D7>") {
            searchedName <- stri_sub(sp[1], 2)
            if (length(sp) > 1) {
              searchedName <- paste0(stri_sub(sp[1], 2), " ", sp[2])
            }
          }
        } else {
          if ((stri_sub(sp[2], 1, 1) %in% c(stri_sub(sp[2], 1, 1) %>% stri_trans_toupper(), "("))) {
            searchedName <- sp[1]
          } else {
            searchedName <- paste0(sp[1], " ", sp[2])
          }
        }
      }
    }else {
      searchedName <- sp[1]
    }

    searchedName <- searchedName%>%stri_replace_all_fixed("  "," ")%>%stri_trim()
    sp2 <- stri_split_fixed(searchedName, " ", simplify = TRUE)
    taxon_authors <- ""
    try({
      taxon_authors <- stri_sub(searchedName_ori,
                                stri_locate_first_fixed(searchedName_ori, sp2[length(sp2)])[2] + 2,
                                stri_length(searchedName_ori))
    }, silent = TRUE)

    if (length(sp2) == 4 & !is.na(taxon_authors)) {
      if (paste0(sp2[3], " ", sp2[4]) == taxon_authors) {
        taxon_authors <- ""
      }
    }

    xi <- stri_locate_first_fixed(taxon_authors, "(")
    xf <- stri_locate_first_fixed(taxon_authors, ")")

    if (!is.na(xi)[1] & stri_length(taxon_authors) > 0) {
      if (xi[1] == 1) {
        next_char <- stri_sub(taxon_authors, xf[2] + 1, xf[2] + 1)
        taxon_authors_last <- stri_sub(taxon_authors,
                                       xf[2] + ifelse(next_char == " ", 2, 1),
                                       stri_length(taxon_authors))
      }
    } else {
      taxon_authors_last <- ""
    }

    return(list(searchedName = searchedName_raw,
                standardizeName = searchedName,
                taxonAuthors = taxon_authors,
                taxonAuthors_last = taxon_authors_last))
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
