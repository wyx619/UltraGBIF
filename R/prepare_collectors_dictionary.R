#' @title Prepare a list with surname of the main collector
#' @name prepare_collectors_dictionary
#'
#' @description It returns a list with the last name of main collector following the unique key `recordedBy`.
#'
#'
#' Consistent recording of the primary collectors and their last name is thus essential, and a **collector dictionary**
#' is provided for this purpose. The `prepare_collectors_dictionary` function is utilized to extract the last
#' name of the primary collector from the `recordedBy` field and to compile a list linking the last name
#' of the primary collector with the raw data in `recordedBy`
#'
#' It is advisable to verify the **last name** of the primary collector in the `nameRecordedBy_Standard`
#' field. The objective is to standardize the last name of the primary collector, which is automatically
#' derived from the `recordedBy` field. This is accomplished by standardizing the text string to begin with
#' an uppercase character and replacing non-ASCII characters, ensuring that the primary collector associated
#' with a collection event is consistently represented by the same character string.
#'
#' Once processed, the dictionary can be checked in the future.
#'
#' @param occ_import imported GBIF records
#' @param surname_selection_type allows you to select any of two types of the last name:
#'
#' **`large_string`** = word with the largest number of characters.
#'
#' **`last_name`** = literally the last name of the main collector, with more than two characters.
#'
#' @param min_characters_in_name min characters in name
#' @param max_words_name assist in checking the length of name
#'
#' @details
#' If `recordedBy` is present in the ref_dictionary, the function returns the last name of
#' the primary collector associated with the specific `recordedBy` key. Otherwise, it
#' returns the last name of the primary collector, extracted from the `recordedBy` field.
#'
#' It is recommended to curate the last name of the primary collector, which is automatically
#' derived from the `recordedBy` field. The goal is to standardize the last name of the primary
#' collector. The primary botanical collector of a sample is consistently identified by the
#' same last name, standardized in capital letters with non-ASCII characters replaced.
#'
#' @return A list with duration and 2 data.table: "my_dictionary" for your processed
#' collectors dictionary and "ref_dictionary" for collectors name reference dictionary
#'
#' @importFrom dplyr %>% filter mutate select distinct case_when if_else
#' @import foreach
#' @import doParallel
#' @import stringi
#' @import data.table
#'
#' @examples
#' \donttest{
#' help(collectors_prepare_dictionary)
#'}
#' @export
prepare_collectors_dictionary <- function(occ_import = NA,
                                          surname_selection_type = "largest_string",
                                          max_words_name = 6,
                                          min_characters_in_name = 4)
{
  start=Sys.time()

  occ=occ_import$occ%>%setDT()

  if(NROW(occ)==0){stop("Occurrence is empty!")}
  ref_dictionary <- ref_dictionary

  if("Ctrl_nameRecordedBy_Standard" %in% names(ref_dictionary)){
    ref_dictionary[, Ctrl_recordedBy:=stri_trans_toupper(Ctrl_recordedBy)]%>%
      setnames(old = "Ctrl_nameRecordedBy_Standard",
               new = "Ctrl_nameRecordedBy_Standard_x")
    }

  get_collectors_name_vec <- Vectorize(get_collectors_name, vectorize.args = "x")
  result_dt <- occ[,.(Ctrl_recordedBy)] %>% unique()
  result_dt[, Ctrl_nameRecordedBy_Standard := get_collectors_name_vec(
    x = Ctrl_recordedBy,
    surname_selection_type = surname_selection_type,
    max_words_name = max_words_name,
    min_characters_in_name = min_characters_in_name
  ) %>% stri_trans_toupper()]

  recordedBy_Standart <- ref_dictionary[result_dt, on = .(Ctrl_recordedBy)
  ][, ref_dictionary := fifelse(
    !is.na(Ctrl_nameRecordedBy_Standard_x), "checked", ""
  )][, Ctrl_nameRecordedBy_Standard := fifelse(
    ref_dictionary == "checked", Ctrl_nameRecordedBy_Standard_x, Ctrl_nameRecordedBy_Standard
  )][, Ctrl_nameRecordedBy_Standard_x := NULL] %>%
    setorder(ref_dictionary, Ctrl_nameRecordedBy_Standard, Ctrl_recordedBy)

  end=Sys.time()
  print(end-start)
  return(list(my_dictionary=recordedBy_Standart,
              ref_dictionary=ref_dictionary,
              used_time=end-start))
}
