#' ---
#' title: Extract information from genTrend log-file
#' date:  "`r Sys.Date()`"
#' ---
#' 

ps_genTrend_log_file <- "genTrend.log"
ps_parameter_to_search <- "DefaultParameterGeneticTrend_2204r_bv.txt"


#' @title Extract information from genTrend log-file
#'
#' @description
#' Extracting information coming  from genTrend-log-file in a tibble file.
#'
#' @param ps_genTrend_log_file log-file resulting from genTrend.f90
#' @param ps_parameter_to_search parameter with taggs to know the starting and ending point to search
#' 
#' @export extract_genTrend_log
extract_genTrend_log(ps_genTrend_log_file,
                     ps_parameter_to_search){
  
  # Check if input-files are existing
  if(!file.exists(ps_genTrend_log_file)){
    stop(" * ERROR: Required log file from genTrend.f90 is not existing!")
  }
  if(!file.exists(ps_parameter_to_search)){
    stop(" * ERROR: Required parameter file 'DefaultParameterGeneticTrend_{YYMMr}_{auswertungsrasse}.txt' is not existing!")
  }
  
  # Read the file ps_genTrend_log_file line-by-line
  con <- file(description = ps_genTrend_log_file)
  vec_log_result <- readLines(con = con)
  close(con)
  
  # Extract from the log-file info about "merkmale" and "auswertungsrasse"
  # Info to extract about "merkmale" is always in the log-file on line 14
  s_merkmale <- vec_log_result[14]
  vec_merkmale <- unlist(strsplit(s_merkmale, split = "\\s+"))
  if(vec_merkmale[2]=="merkmale:"){
    vec_merkmale <- vec_merkmale[3:length(vec_merkmale)]
    vec_merkmale <- unlist(strsplit(vec_merkmale, split = ","))
  }
  # Info to extract about "merkmale" is always in the log-file on line 19
  s_auswertungsrasse <- vec_log_result[19]
  vec_auswertungsrasse <- unlist(strsplit(s_auswertungsrasse, split = "\\s+"))
  if(vec_auswertungsrasse[2]=="auswertungsrasse:"){
    vec_auswertungsrasse <- vec_auswertungsrasse[3:length(vec_auswertungsrasse)]
  }
  
  # Extract part with specific info about genetic trend in the log-file
  s_result_upper <- "Start: Genetische Trends nach Basisdefinitionen (ohne Geburtsjahre) berechnen"
  s_result_lower <- "Ende: Genetische Trends nach Basisdefinitionen (ohne Geburtsjahre) berechnen"
  n_index_upper <- grep(pattern = s_result_upper, vec_log_result, fixed = TRUE)
  n_index_lower <- grep(pattern = s_result_lower, vec_log_result, fixed = TRUE)
  # Exclude everything outside of result area
  vec_log_result <- vec_log_result[n_index_upper:n_index_lower]
  
  # Read default parameter to search taggs
  # DefaultParameter.txt has to be manually updated: especially SearchPatternStart, SearchPatternEnd, minEbvToPlot, maxEbvToPlot
  s_par <- "DefaultParameterGeneticTrend_2204r_bv.txt"
  tbl_par <- readr::read_delim(file = s_par, delim = ";")
  # Check "auswertungsrasse" which are in log-file and extract only the specific info from defalut parameter file
  tbl_par <- tbl_par[tbl_par[["Auswertungsrasse"]] == vec_auswertungsrasse,]
  # Check "merkmale" which are in log-file and extract only the specific info from defalut parameter file
  tbl_search <- NULL
  for(mrkml in vec_merkmale){
    tbl_tmp <- tbl_par[tbl_par[["ExtractForTrait"]] == mrkml,]
    tbl_search <- dplyr::bind_rows(tbl_search,tbl_tmp)
    rm(tbl_tmp)
  }
  
  # Extract the pattern in a result tibble
  tbl_result <- NULL
  for (idx in 1:nrow(tbl_search)){
    
    n_cur_trait_idx <- grep(pattern = tbl_search$SearchPatternStart[idx], vec_log_result, fixed = TRUE) + tbl_search$IndexOffsetStart[idx]
    n_end_cur_trait_idx <- grep(pattern = tbl_search$SearchPatternEnd[idx], vec_log_result, fixed = TRUE) + tbl_search$IndexOffSetEnd[idx]
    
    # Surch header
    s_header_result <- vec_log_result[n_cur_trait_idx-1]
    vec_header_result <- unlist(strsplit(s_header_result, split = "\\s+"))
    
    # Surch Breed
    vec_breed_result <- tbl_search$ExtractForBreed[idx]
    
    # Surch Trait
    vec_trait_result <- tbl_search$ExtractForTrait[idx]
    
    # Extract pattern with start and end from the tbl_search information
    for(lidx in n_cur_trait_idx:n_end_cur_trait_idx){
      
      s_cur_result <- vec_log_result[n_cur_trait_idx]
      vec_cur_result <- unlist(strsplit(s_cur_result, split = "\\s+"))
      # If available, remove empty vector at the beginning
      if(vec_cur_result[1]==""){
        vec_cur_result <- vec_cur_result[2:(length(vec_cur_result))]
      }
      
      # Vector to matrix
      mat_cur_result <- matrix(vec_cur_result, nrow = 1)
      colnames(mat_cur_result) <- vec_header_result
      
      # Matrix to tibble
      tbl_cur_result <- tibble::as_tibble(mat_cur_result)
      tbl_cur_result$Breed <- vec_breed_result
      tbl_cur_result$Trait <- vec_trait_result
      
      # Transform the typ
      tbl_cur_result[,"Geburtsjahr"] <- as.numeric(tbl_cur_result[,"Geburtsjahr"])
      tbl_cur_result[,"n_zw"] <- as.numeric(tbl_cur_result[,"n_zw"])
      tbl_cur_result[,"mean_zw"] <- as.numeric(tbl_cur_result[,"mean_zw"])
      
      
      # Extension
      if (is.null(tbl_result)){
        tbl_result <- tbl_cur_result
      } else {
        tbl_result <- dplyr::bind_rows(tbl_result, tbl_cur_result)
      }
      
      # Next step in the loop
      n_cur_trait_idx <- n_cur_trait_idx + 1
      
    }
    
  }
  
}