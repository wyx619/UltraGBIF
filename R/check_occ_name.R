#' @title Use local or online taxon resolution service to check accepted taxon names and update synonyms at once
#'
#' @name check_occ_name
#'
#' @description Names of species can be checked against WCVP database.
#' To verify individual names, the function wcvp_check_name is used.
#'
#' @param occ_import imported GBIF records
#' @param local_taxon_resolution if TRUE, resolve taxon names locally. Otherwise automatically use Taxonomic Name Resolution Service(TNRS) instead
#' @param threads your threads requirement, a positive real number, default is 4
#' @param if_author_fails_try_without_combinations option for partial verification of the authorship of the species.
#' Remove the authors of combinations, in parentheses
#'
#' @details
#' * [about WCVP database](http://sftp.kew.org/pub/data-repositories/WCVP/)
#' * [World Checklist of Vascular Plants](https://powo.science.kew.org//)
#' * [rWCVPdata](https://github.com/matildabrown/rWCVPdata/)
#'
#' @return A list with duration and two data.table: "summary" with species list and "occ_wcvp_check_name" with WCVP fields
#'
#'
#'
#' @importFrom dplyr %>% case_when if_else select
#' @import data.table
#' @import stringi
#' @import foreach
#' @import doParallel
#' @import TNRS
#'
#' @export
check_occ_name <- function(occ_import = NA,
                           local_taxon_resolution = TRUE,
                           threads = 4,
                           if_author_fails_try_without_combinations = TRUE)
{ start=Sys.time()

  occ=occ_import$occ%>%setDT()

  wcvp_names_database = rWCVPdata::wcvp_names
  setDT(wcvp_names_database)
  wcvp_names_database[,`:=`(TAXON_NAME_U=stri_trans_toupper(taxon_name),
                            TAXON_AUTHORS_U = taxon_authors %>%
                              stri_trans_toupper() %>%
                              stri_replace_all_regex("\\s+", ""))]

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

  if(local_taxon_resolution){
  genus_ref <- occ_all[,wcvp_searchedName]%>%unique()%>%stri_extract_first_regex("^\\S+")%>%unique()
  family_ref <- occ[,Ctrl_family] %>% unique()
  wcvp_names_database1 <- wcvp_names_database[family %in% family_ref,]
  wcvp_names_database2 <- wcvp_names_database[genus %in% genus_ref,]
  wcvp_names_database <- rbind(wcvp_names_database1,
                                wcvp_names_database2) %>% unique(by="plant_name_id")

  numCores <- threads%>%usecores()
  cl <- parallel::makeCluster(numCores)
  registerDoParallel(cl)

  sppp=function(sp) {
    x_tmp <- wcvp_check_name(
      searchedName = sp,
      if_author_fails_try_without_combinations = TRUE,
      wcvp_names_database = wcvp_names_database
    )

    summary <- x_tmp[, .(
      wcvp_searchedName,
      wcvp_plant_name_id,
      wcvp_taxon_rank,
      wcvp_taxon_status,
      wcvp_family,
      wcvp_taxon_name,
      wcvp_taxon_authors,
      wcvp_accepted_plant_name_id,
      wcvp_reviewed,
      wcvp_taxon_status_of_searchedName,
      wcvp_plant_name_id_of_searchedName,
      wcvp_taxon_authors_of_searchedName,
      wcvp_verified_author,
      wcvp_verified_speciesName,
      wcvp_searchNotes
    )][, ori_sp_name := sp]

    return(summary)
  }
  result <- foreach(
    sp = name_search_wcvp,
    .packages = c("data.table","stringi","dplyr"),
    .multicombine = T,
    .errorhandling = "remove",
    .export = c("wcvp_check_name","standardize_name"),
    .inorder = F) %dopar% {sppp(sp)}

  parallel::stopCluster(cl)

  result <- rbindlist(result,fill = T)%>%unique()}else{
    check_initial <- data.frame(ID = 1:length(name_search_wcvp),
                                taxon = name_search_wcvp%>%
                                  stri_replace_all_regex( "<U+00D7>\\s*(\\S)", "<U+00D7> $1")%>%
                                  stri_replace_all_regex(" +", " "))
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
        Sys.sleep(2)
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
  }

  occ_all=merge(occ_all[,.(Ctrl_gbifID,wcvp_searchedName)],
                 result%>%select(-wcvp_searchedName),
                 by.x = "wcvp_searchedName",
                 by.y = "ori_sp_name",all.x = T)

  summary = unique(result[,ori_sp_name:=NULL])

  end=Sys.time()
  used=end-start
  print(used)

  return(list(occ_wcvp_check_name = occ_all,
              summary = summary,
              used_time=used))
  }
