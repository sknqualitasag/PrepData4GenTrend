#' @title Build up Index for calving survival
#'
#' @description
#' Build up Index for calving survival to a csv-file
#'
#' @param ps_input_csvfile path to the input csv-file coming from function correctBasis4genTrend
#' @param ps_evaluation type of evaluation by Qualitas AG (bv, rh)
#' @param ps_weighingfile file which contains the constant for weighing and factor depending of evaluation type
#' @param ps_output_csvfile resulting csv-file
#'
#' @importFrom magrittr %>%
#' @importFrom tidyr pivot_wider
#' @importFrom dplyr filter
#' @importFrom dplyr select
#'
#' @return tbl_index resulting index tibble
#' @export build_calvingsurvival_index
build_calvingsurvival_index <- function(ps_input_csvfile,
                                        ps_evaluation,
                                        ps_weighingfile = system.file("extdata","weighing_factor2buildIndex.csv",package = "PrepData4GenTrend"),
                                        ps_output_csvfile){

  if(!file.exists(ps_input_csvfile)){
    stop(" * ERROR: Required input-csv-file is not existing!")
  }
  if(is.null(ps_evaluation)){
    stop(" * ERROR: Required evalution information is empty!")
  }
  if(!file.exists(ps_weighingfile)){
    stop(" * ERROR: Required constant-csv-file with weighings and factors is not existing!")
  }



  # Read single ebvs to build cas-index
  tbl_cas_ebv <- readr::read_delim(file = ps_input_csvfile, delim = ";")
  # Transform the dataframe from long to wide
  tbl_cas_ebv <- tbl_cas_ebv %>% tidyr::pivot_wider(names_from = Trait, values_from = mean_zw)


  # Read constant file containing the weight and factor
  tbl_weighing <- readr::read_delim(file = ps_weighingfile, delim = ";")


  # Weighing of the single ebvs to build cas-index
  tbl_cas_ebv <- tbl_cas_ebv %>% mutate(mean_zw = (tbl_weighing[[ps_evaluation]][1] * (tbl_cas_ebv$p1 - 100) + tbl_weighing[[ps_evaluation]][2] * (tbl_cas_ebv$hp2 - 100) + tbl_weighing[[ps_evaluation]][3] * (tbl_cas_ebv$bp2 - 100)) * tbl_weighing[[ps_evaluation]][4] + 100,
                                     Trait = "cas")


  # select only the column for RShiny
  tbl_index <- tbl_cas_ebv %>% select("Geburtsjahr","Basis","mean_zw","Breed","Trait")


  # Write output
  readr::write_delim(tbl_index, delim =";",path = ps_output_csvfile)

}


#' @title Build up Index for fertility
#'
#' @description
#' Build up Index fertility to a csv-file
#'
#' @param ps_input_csvfile path to the input csv-file coming from function correctBasis4genTrend
#' @param ps_evaluation type of evaluation by Qualitas AG (bv, rh)
#' @param ps_weighingfile file which contains the constant for weighing and factor depending of evaluation type
#' @param ps_output_csvfile resulting csv-file
#'
#' @importFrom magrittr %>%
#' @importFrom tidyr pivot_wider
#' @importFrom dplyr filter
#' @importFrom dplyr select
#'
#' @return tbl_index resulting index tibble
#' @export build_fertility_index
build_fertility_index <- function(ps_input_csvfile,
                                  ps_evaluation,
                                  ps_weighingfile = system.file("extdata","weighing_factor2buildIndex.csv",package = "PrepData4GenTrend"),
                                  ps_output_csvfile){

  if(!file.exists(ps_input_csvfile)){
    stop(" * ERROR: Required input-csv-file is not existing!")
  }
  if(is.null(ps_evaluation)){
    stop(" * ERROR: Required evalution information is empty!")
  }
  if(!file.exists(ps_weighingfile)){
    stop(" * ERROR: Required constant-csv-file with weighings and factors is not existing!")
  }



  # Read single ebvs to build fertility-index
  tbl_fbk_ebv <- readr::read_delim(file = ps_input_csvfile, delim = ";")
  # Transform the dataframe from long to wide
  tbl_fbk_ebv <- tbl_fbk_ebv %>% tidyr::pivot_wider(names_from = Trait, values_from = mean_zw)


  # Read constant file containing the weight and factor
  tbl_weighing <- readr::read_delim(file = ps_weighingfile, delim = ";")


  # Weighing of the single ebvs to build fertility-index
  tbl_fbk_ebv <- tbl_fbk_ebv %>% mutate(mean_zw = (tbl_weighing[[ps_evaluation]][5] * (tbl_fbk_ebv$nrr - 100) + tbl_weighing[[ps_evaluation]][6] * (tbl_fbk_ebv$vzr - 100) + tbl_weighing[[ps_evaluation]][7] * (tbl_fbk_ebv$nrk - 100) + tbl_weighing[[ps_evaluation]][8] * (tbl_fbk_ebv$vzk - 100) + tbl_weighing[[ps_evaluation]][9] * (tbl_fbk_ebv$raz - 100)) * tbl_weighing[[ps_evaluation]][10] + 100,
                                        Trait = "fbk")


  # select only the column for RShiny
  tbl_index <- tbl_fbk_ebv %>% select("Geburtsjahr","Basis","mean_zw","Breed","Trait")


  # Write output
  readr::write_delim(tbl_index, delim =";",path = ps_output_csvfile)


}

