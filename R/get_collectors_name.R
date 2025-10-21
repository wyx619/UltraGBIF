#' @title Extract last name of the main collector (internal)
#' @name get_collectors_name
#'
#' @description Extract last name of the main collector in recordedBy field
#'
#' @param x collectors_name in recordedBy field
#' @param surname_selection_type allows you to select two types of results
#' for the main collector's last name:
#'
#' **`large_string`** = word with the largest number of characters.
#'
#' **`last_name`** = literally the last name of the main collector, with more than two characters.
#' @param min_characters_in_name min characters in name
#' @param max_words_name assist in checking the length of name
#'
#' @details Returns the last name
#'
#' @return
#' last name of the main collector
#'
#'
#' @import stringi data.table
#' @importFrom dplyr %>%
#' @importFrom utils tail head
#'
#' @export
get_collectors_name<-function (x = NA, surname_selection_type = "largest_string",
                               max_words_name = 6, min_characters_in_name = 2)
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
              "TRANSECT", "COLLECTOR")
  check_date_num <- function(x_t, no_name) {
    x_r <- FALSE
    x_t_1 <- suppressWarnings(lubridate::ymd(x_t, locale = "en_US.UTF-8"))
    if (!is.na(x_t_1)) {
      x_r <- lubridate::is.Date(x_t_1)
    }
    if (is.na(x_t_1) & !x_r) {
      x_t_1 <- suppressWarnings(as.numeric(x_t))
      if (!is.na(x_t_1)) {
        x_r <- is.numeric(x_t_1)
      }
    }
    if (!x_r) {
      x_t_1 <- stri_replace_all_regex(x_t, "[^A-Z]", "")
      x_r <- is.na(x_t_1) | x_t_1 == ""
    }
    if (!x_r) {
      x_r <- x_t %chin% no_name
    }
    return(x_r)
  }
  clean_text <- function(x){
    if (stri_detect_fixed(x, "|")) {
      x <- stri_split_fixed(x, "|") %>% unlist() %>% head(1)
    }
    x <- x %>%
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
      stri_replace_all_fixed("COLLECTOR(S): ", "") %>%
      stri_replace_all_fixed("COLLECTOR(S):", "") %>%
      stri_replace_all_fixed("COLLECTORS: ", "") %>%
      stri_replace_all_fixed("COLLECTORS:", "") %>%
      stri_replace_all_fixed("COLLABORATION; ", "") %>%
      stri_replace_all_fixed("COLLABORATION;", "") %>%
      stri_replace_all_fixed("[], ", "") %>%
      stri_replace_all_fixed("[AND]", "")

    if (stri_length(x) == 0) x <- "UNKNOWN-COLLECTOR"

    x_s <- stri_locate_first_fixed(x, "(")[, "end"]
    x_e <- stri_locate_first_fixed(x, ")]")[, "end"]

    if (!is.na(x_s) && !is.na(x_e) && x_s != 1 && x_s > 0 && x_e > 0) {
      x <- stri_sub(x, 1, x_s - 1)
      if (stri_sub(x, -1, -1) == " ") {
        x <- stri_sub(x, 1, stri_length(x) - 1)
      }
    }

    x <- x %>%
      stri_replace_all_fixed("PROJETO FLORA CRISTALINO; ", "") %>%
      stri_replace_all_fixed("PROJETO FLORA CRISTALINO;", "") %>%
      stri_replace_all_fixed("ET AL", "") %>%
      stri_trans_general("Russian-Latin/BGN")%>%
      stri_trans_general("Latin-ASCII") %>%
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
      stri_replace_all_fixed("  ", " ") %>%
      stri_replace_all_fixed("/", "") %>%
      stri_replace_all_fixed("=", "") %>%
      stri_trim_both()

    if (stri_length(x) == 0) x <- "UNKNOWN-COLLECTOR"

    parse_chr=function(x=NA,chr=""){
      if (stri_detect_fixed(x,chr)) {
        x=stri_split_fixed(x,chr)%>%
        unlist() %>% head(1)}
      return(x)}

    x <- x %>%
      parse_chr(chr="&") %>%
      parse_chr(chr=" AND ") %>%
      parse_chr(chr=" WITH ") %>%
      parse_chr(chr=" ET ") %>%
      parse_chr(chr=" TO ") %>%
      parse_chr(chr=" IN ") %>%
      parse_chr(chr=" , ")

    if (x == ";") x <- "UNKNOWN-COLLECTOR"

    if (stri_detect_fixed(x, ";")) {
      x_t <- stri_split_fixed(x, ";")%>%
        unlist() %>% head(1) %>% stri_trim()
      if (stri_length(x_t) == 0) {
        x_t <- unlist(stri_split_fixed(x, ";"))[2]%>%stri_trim()
        if (is.na(x_t)) {x_t <- ""}
      }
      if (stri_length(x_t) > 0) {x <- x_t} else {x <- ""}
    }
    return(x)
  }
  surname_check=function(xx=NA,
                         surname_selection_type=NA,
                         max_words_name=NA,
                         min_characters_in_name=NA,
                         no_name=NA){

    if (surname_selection_type == "largest_string") {
      vll = which(nchar(xx)==max(nchar(xx)))
      sobren <- ifelse(length(vll) > 1,xx[tail(vll, 1)],tail(xx,1))
      return(sobren)
    }
    if (surname_selection_type != "largest_string") {
      sobren = ""
      ind_name =  stri_length(xx) > 1 & stri_detect_regex(xx, "[AEIOUY]") & stri_count_fixed(xx, "-") < 2
      if (sum(stri_detect_regex(xx, "[\\u4E00-\\u9FFF]"))==0 & any(ind_name)) {
        for (i2 in length(xx[ind_name]):1) {
          if (i2 > max_words_name) {
            next
          }
          if (stri_length(xx[ind_name][i2]) >= min_characters_in_name &
              !check_date_num(xx[ind_name][i2],no_name = no_name)) {
            sobren = xx[ind_name][i2]
            break
          }
        }
      } else {
        sobren=xx[stri_detect_regex(xx, "[\\u4E00-\\u9FFF]")][1]
        if (is.na(sobren)) {sobren=xx%>%tail(1)}
        }
      return(sobren)
    }
  }
  x=clean_text(x)

  if (stri_detect_regex(x, ",| ")) {

    xx <- stri_split_fixed(x, ",") %>% unlist()
    xx <- stri_split_fixed(xx, " ") %>% unlist()
    xx = xx[xx != ""]
    xx <- xx[stri_length(xx)>=min_characters_in_name]
    xx <- xx[!xx %chin% no_name]
    xx <- xx[!stri_detect_regex(xx, "^[0-9]+$") & xx != ""]
    if (length(xx)==0) xx<-""

    if (max(stri_length(xx)) > 2) {
      sobren=surname_check(xx,
                           surname_selection_type=surname_selection_type,
                           max_words_name=max_words_name,
                           min_characters_in_name=min_characters_in_name,
                           no_name=no_name)
    } else {
      sobren <- xx %>% unlist() %>% stri_trim_both() %>%
        {.[tail(which.max(stri_length(.)), 1)]}
    }
  } else {
    xx = strsplit(x, " ") %>% unlist()
    xx <- xx[stri_length(xx)>=min_characters_in_name]
    xx <- xx[!xx %chin% no_name]
    xx <- xx[!stri_detect_regex(xx, "^[0-9]+$") & xx != ""]
    if(length(xx)==0) xx=""
    sobren=surname_check(xx,
                         surname_selection_type=surname_selection_type,
                         max_words_name=max_words_name,
                         min_characters_in_name=min_characters_in_name,
                         no_name=no_name)
  }

  sobren <- sobren %>%
    stri_trim_both() %>%
    stri_replace_all_fixed("?", "") %>%
    stri_join(sep="-") %>%
    stri_trans_toupper()

  if(!is.na(sobren) & stri_length(sobren) >= min_characters_in_name){
    sobren <- stri_split_fixed(sobren, "|")%>%unlist()%>%head(1)
    if (stri_startswith_fixed(sobren, "-")) sobren <- stri_sub(sobren, 2)
    if (stri_endswith_fixed(sobren, "-")) sobren <- stri_sub(sobren, 1, -2)
    if (stri_detect_fixed(sobren, "-") & stri_count_fixed(sobren, "-") == 1){
      x_t <- stri_split_fixed(sobren, "-")%>%unlist()
      ind_name <- !x_t %chin% c('JUNIOR', 'JR', 'FILHO', 'NETO', 'SOBRINHO') & stri_length(x_t) > 1
      if (sum(ind_name) >= 1) sobren <- x_t[ind_name][1]
      if (sum(ind_name) == 0) sobren <- ''
    }
    return(sobren)
  }else{return("")}
}
