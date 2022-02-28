### #
### # Read log-file genTrend and extract information
### #
### # -------------------------------------------------------------------------

# Load requiered packages
library(dplyr)
library(ggplot2)


### # -------------------------------------------------------------------------
### # Extract info from genTrend log-file
### # -------------------------------------------------------------------------

# Path to the log-file to be readen
#s_log_path <- "genTrend_scs_ym0.log"
s_log_path <- "inst/extdata/genTrend.log"
### # ??? Build a function with file-path as parameter

# Read the file line-by-line
con <- file(description = s_log_path)
vec_log_result <- readLines(con = con)
close(con)


# Extract from the log-file info about parameter like "merkmale" and "auswertungsrasse"
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
s_par <- "inst/extdata/DefaultParameterGeneticTrend_2204r_bv.csv"
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


### # -------------------------------------------------------------------------
### # Correct the basis
### # -------------------------------------------------------------------------

# Read bdiffZuBsBasis
s_bdiffZuBs <- "inst/extdata/bdiffZuBsBasisAlleMerkmale_ym0.csv"
tbl_bdiff <- readr::read_delim(file = s_bdiffZuBs, delim = ";")

## Join the tbl_result tibble with tbl_bdiff tibble
tbl_res_diff <- tbl_result %>% left_join(tbl_bdiff, by = c("Breed" = "rasse", "Trait" = "merkmal"))
# correct mean_zw column
tbl_res_diff <- tbl_res_diff %>% mutate(mean_zw = case_when(!is.na(tbl_res_diff$basisdifferenz) ~ tbl_res_diff$mean_zw + tbl_res_diff$basisdifferenz,
                                                                  is.na(tbl_res_diff$basisdifferenz) ~ tbl_res_diff$mean_zw))
# change after correcting mean_zw the "Basis"
tbl_res_diff <- tbl_res_diff %>% mutate(Basis = case_when(!is.na(tbl_res_diff$basisdifferenz) ~ substr(tbl_res_diff$basisbezeichnung,1,2),
                                                          is.na(tbl_res_diff$basisdifferenz) ~ tbl_res_diff$Basis))

# select only the column for RShiny
tbl_res_diff <- tbl_res_diff %>% select("Geburtsjahr","Basis","mean_zw","Breed","Trait")

# Write output
readr::write_delim(tbl_res_diff, delim =";",path = paste(Sys.Date(),sep = "_","GeneticTrendForRShiny.csv"))


### # -------------------------------------------------------------------------
### # Plot
### # -------------------------------------------------------------------------

### # First Alternative
s_default <- "inst/extdata/DefaultParameterGeneticTrendRShiny.csv"
tbl_default <- readr::read_delim(file = s_default, delim = ";")

tbl_BS_nrr <- tbl_res_diff %>% filter(Breed == "BV/BS") %>% filter(Trait == "nrr")
maxBY<-max(tbl_BS_nrr[,"Geburtsjahr"])

ggplot(data = tbl_BS_nrr, mapping = aes(Geburtsjahr, y = mean_zw)) +
  geom_line(aes(color=Trait), size = 1.2) +
  coord_cartesian(xlim = c(maxBY-20, maxBY-4),
                  ylim = c(90,110)) +
  labs(x = "Geburtsjahr",
       y = "Zuchtwert",
       title = "Genetische Trends",
       subtitle = paste("Zuchwerte sind auf der Rasse Basis",sep = " ",format(Sys.Date(), "%Y")),
       caption = "(c) Braunvieh Schweiz") +
  theme(legend.title = element_blank(),
        #legend.position = "top",
        axis.title = element_text(size = 18),
        plot.title = element_text(size = 18),
        plot.caption = element_text(hjust = 0)) +
  scale_x_continuous(breaks = seq(maxBY-20,maxBY-4, by = 1))

### # Second Alternative
# Merge Info
tbl_BS_nrr <- tbl_BS_nrr %>% inner_join(tbl_default, by = c("Trait" = "Trait"))
tbl_BS_nrr <- tbl_BS_nrr %>% select(-c("FirstYearToPlot_ym","LastYearToPlot_ym","minEbvToPlot","maxEbvToPlot"))

ggplot(data = tbl_BS_nrr, mapping = aes(Geburtsjahr, y = mean_zw)) +
  geom_line(aes(color=Designation), size = 1.2) +
  coord_cartesian(xlim = c(maxBY-20, maxBY-4),
                  ylim = c(90,110)) +
  labs(x = "Geburtsjahr",
       y = "Zuchtwert",
       title = "Genetische Trends",
       subtitle = paste("Zuchwerte sind auf der Rasse Basis",sep = " ",format(Sys.Date(), "%Y")),
       caption = "(c) Braunvieh Schweiz") +
  theme(legend.title = element_blank(),
        #legend.position = "top",
        axis.title = element_text(size = 18),
        plot.title = element_text(size = 18),
        plot.caption = element_text(hjust = 0)) +
  scale_x_continuous(breaks = seq(maxBY-20,maxBY-4, by = 1))

### # Third Alternative
# per TraitGroup
tbl_res_diff_TraitGroup <- tbl_res_diff %>% filter(Breed == "BV/BS")
tbl_res_diff_TraitGroup <- tbl_res_diff %>% inner_join(tbl_default, by = c("Trait" = "Trait"))
tbl_res_diff_TraitGroup <- tbl_res_diff_TraitGroup %>% select(-c("FirstYearToPlot_ym","LastYearToPlot_ym","minEbvToPlot","maxEbvToPlot"))


ggplot(data = tbl_res_diff_TraitGroup, mapping = aes(Geburtsjahr, y = mean_zw)) +
  geom_line(aes(color=Designation), size = 1.2) +
  coord_cartesian(xlim = c(maxBY-20, maxBY-4),
                  ylim = c(90,110)) +
  labs(x = "Geburtsjahr",
       y = "Zuchtwert",
       title = "Genetische Trends",
       subtitle = paste("Zuchwerte sind auf der Rasse Basis",sep = " ",format(Sys.Date(), "%Y")),
       caption = "(c) Braunvieh Schweiz") +
  theme(legend.title = element_blank(),
        #legend.position = "top",
        axis.title = element_text(size = 18),
        plot.title = element_text(size = 18),
        plot.caption = element_text(hjust = 0)) +
  scale_x_continuous(breaks = seq(maxBY-20,maxBY-4, by = 1))

