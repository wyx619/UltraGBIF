#' @title Simplify and extract the name of collectors
#' @name get_collectors_name
#'
#' @description This function simplifies and extracts the name of one collector
#'
#' @param x name of the collectors
#' @param min_char min characters in name
#'
#' @return
#' Standard name of the main collector
#' @examples
#' \donttest{
#' # Normal conditions
#' get_collectors_name('Anderberg, Arne & Anderberg, Anna-Lena')
#' get_collectors_name('Schlegel, Henrik Adolf Leonard & Arnell, Hampus Wilrshelm')
#' get_collectors_name('Tomitarô-Makino, John')
#'
#' # Edge cases
#' get_collectors_name(NA)
#' get_collectors_name('')
#' get_collectors_name('et al.')
#'
#' }
#'
#'
#' @import stringi data.table tokenizers
#' @importFrom dplyr %>%
#'
#' @export
get_collectors_name <- function (x = '', min_char = 2)
{
  if (is.na(x) | x == "?., ?." | x == "?" | x == "") {return("UNKNOWN")}
  no_name = c("AL", "ALLI", "JR", "ET", "TEAM", "JUNIOR",
              "FILHO", "NETO", "SOBRINHO", "RESEARCH", "BY", "IN",
              "FROM", "DE", "STAFF", "EXPED", "EXP", "DEPARTMENT",
              "OF", "CENTER", "COLLECTION", "UNIVERSITY", "COLLEGE",
              "EX", "HERB", "SCHOOL", "TECH", "DEPT", "II", "III",
              "AND", "COL", "COLL", "WITH", "DEN", "VAN", "CLUB",
              "BOTANICAL", "GARDEN", "GARDENS", "SOCIETY", "HERBARIUM",
              "SECTION", "FIELD", "TRIP", "CLASS", "BOTANY", "EXPEDITION",
              "TRANSECT", "COLLECTOR",'JUNIOR', 'JR', 'FILHO', 'NETO',
              'SOBRINHO','COLLECTORS')

  clean_text <- function(x){
    x <- x %>%
      stri_trans_general("Russian-Latin/BGN")%>%
      stri_trans_general("Latin-ASCII") %>%
      stri_replace_all_fixed("|", " ") %>%
      stri_replace_all_fixed("], [", "") %>%
      stri_replace_first_fixed("&", ",") %>%
      stri_replace_all_fixed("?", "") %>%
      stri_replace_all_fixed("*", "") %>%
      stri_replace_all_fixed("\u00a4", "") %>%
      stri_replace_all_fixed("\ufffd", "") %>%
      stri_replace_all_fixed("<TD>", "") %>%
      stri_replace_all_fixed("<td>", "") %>%
      stri_replace_first_fixed("'", "") %>%
      stri_replace_first_fixed("!", "") %>%
      stri_replace_all_fixed(".", " ") %>%
      stri_replace_all_fixed("_", " ") %>%
      stri_trans_toupper() %>%
      stri_replace_all_fixed(' AND ',' ')%>%
      stri_replace_all_fixed(' WITH ',' ')%>%
      stri_replace_all_fixed(' ET ',' ')%>%
      stri_replace_all_fixed(' TO ',' ')%>%
      stri_replace_all_fixed(' IN ',' ')%>%
      stri_replace_all_fixed("COLLECTOR(S): ", "") %>%
      stri_replace_all_fixed("COLLECTOR(S):", "") %>%
      stri_replace_all_fixed("COLLECTORS: ", "") %>%
      stri_replace_all_fixed("COLLECTORS:", "") %>%
      stri_replace_all_fixed("COLLABORATION; ", "") %>%
      stri_replace_all_fixed("COLLABORATION;", "") %>%
      stri_replace_all_fixed("[], ", "") %>%
      stri_replace_all_fixed("[AND]", "")%>%
      stri_replace_all_fixed("("," ")%>%
      stri_replace_all_fixed(")]"," ") %>%
      stri_replace_all_fixed("PROJETO FLORA CRISTALINO; ", "") %>%
      stri_replace_all_fixed("PROJETO FLORA CRISTALINO;", "") %>%
      stri_replace_all_fixed("ET AL", "") %>%
      stri_replace_all_fixed("\u00A1", "I") %>%
      stri_replace_all_fixed("\u00A2", "O") %>%
      stri_replace_all_fixed("\u00B4", "") %>%
      stri_replace_all_fixed("`", "") %>%
      stri_replace_all_fixed("'", "") %>%
      stri_replace_all_fixed("\u2019", "") %>%
      stri_replace_all_regex("[^\\u0000-\\u007F\\u4E00-\\u9FFF]", "")%>%
      stri_replace_all_regex("[\\(\\)]", " ") %>%
      stri_replace_all_regex("[\\[\\]]", " ") %>%
      stri_replace_all_regex("[\"]", " ") %>%
      stri_replace_all_regex("[\\{\\}]", " ") %>%
      stri_replace_all_fixed("\"", " ") %>%
      stri_replace_all_fixed("#", " ") %>%
      stri_replace_all_fixed("/", "") %>%
      stri_replace_all_fixed("=", "") %>%
      stri_replace_all_fixed(';',' ')%>%
      stri_replace_all_fixed("&",' ')%>%
      stri_replace_all_fixed(',', ' ')%>%
      stri_replace_all_regex("-", " ") %>%
      stri_replace_all_regex("~", " ") %>%
      stri_replace_all_regex("\\s+", " ") %>%
      stri_trim_both()%>%
      ifelse(stri_length(.) == 0,"UNKNOWN",.)

    return(x)
  }

  max_foo <- function(x) {
      words <- tokenize_words(x, simplify = TRUE, lowercase = FALSE)
      words[which.max(nchar(words))]
    }

  x <- clean_text(x)%>%max_foo()

  if (x %chin% no_name | stri_length(x) < min_char) 'UNKNOWN' else x

}
