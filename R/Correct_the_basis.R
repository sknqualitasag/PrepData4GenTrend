#' @title Correct with the breed constant the basis of the extracted genTrend log-info
#'
#' @description
#' Correct with the breed constant the basis of the extracted genTrend log-info
#'
#' @param ps_input_tibble resulting tibble from the function extract4genTrend
#' @param ps_par_default path to the parameter file
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr inner_join
#' @importFrom dplyr select
#' @importFrom dplyr left_join
#'
#' @return tbl_result resulting tibble with information extracted from genTrend log-file
#' @export correctBasis4genTrend
correctBasis4genTrend <- function(ps_input_tibble,
                                  ps_par_bdiff,
                                  ps_output_csvfile){

  if(is.null(ps_input_tibble)){
    stop(" * ERROR: Required tibble is empty!")
  }
  if(!file.exists(ps_par_bdiff)){
    stop(" * ERROR: Required bdiff parameter-file  is not existing!")
  }


  # Read bdiffZuBsBasis
  tbl_bdiff <- readr::read_delim(file = ps_par_bdiff, delim = ";")


  # Join the ps_input_tibble tibble with tbl_bdiff tibble
  tbl_res_diff <- ps_input_tibble %>% left_join(tbl_bdiff, by = c("Breed" = "rasse", "Trait" = "merkmal"))
  # correct mean_zw column
  tbl_res_diff <- tbl_res_diff %>% mutate(mean_zw = case_when(!is.na(tbl_res_diff$basisdifferenz) ~ tbl_res_diff$mean_zw + tbl_res_diff$basisdifferenz,
                                                              is.na(tbl_res_diff$basisdifferenz) ~ tbl_res_diff$mean_zw))


  # change after correcting mean_zw the "Basis"
  tbl_res_diff <- tbl_res_diff %>% mutate(Basis = case_when(!is.na(tbl_res_diff$basisdifferenz) ~ substr(tbl_res_diff$basisbezeichnung,1,2),
                                                            is.na(tbl_res_diff$basisdifferenz) ~ tbl_res_diff$Basis))


  # select only the column for RShiny
  tbl_res_diff <- tbl_res_diff %>% select("Geburtsjahr","Basis","mean_zw","Breed","Trait")


  # Write output
  readr::write_delim(tbl_res_diff, delim =";",path = ps_output_csvfile)

}

