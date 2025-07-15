#' @title Standardize scientific name (internal)
#'
#' @name standardize_name
#'
#' @description Standardize scientific name of taxons
#' @param searchedName the name of taxon you want to standardize
#'
#' @details See help(standardize_name)
#' @seealso \code{\link[UltraGBIF]{wcvp_check_name}}
#' @return A list with species name you input, standardized scientific name of it,
#' taxon authors of it and the last taxon author of it
#'
#'
#'
#' @importFrom dplyr %>%
#' @import stringi
#'
#' @export
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
