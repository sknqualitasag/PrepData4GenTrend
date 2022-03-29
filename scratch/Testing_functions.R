s_input_logfile <- "inst/extdata/genTrend.log"
s_par_default <- "inst/extdata/DefaultParameterGeneticTrend_2204r_bv.csv"

tbl_test <- PrepData4GenTrend::extract4genTrend(ps_input_logfile = s_input_logfile,
                            ps_par_default = s_par_default)



s_par_bdiff <- "inst/extdata/bdiffZuBsBasisAlleMerkmale_ym0.csv"
s_output_csvfile <- "inst/extdata/GeneticTrendForRShiny_fruchtbarkeit.csv"
PrepData4GenTrend::correctBasis4genTrend(ps_input_tibble = tbl_test,
                                         ps_par_bdiff = s_par_bdiff,
                                         ps_output_csvfile = s_output_csvfile)


s_input_csvfile <- "scratch/extdata/GeneticTrendForRShiny_cas.csv"
s_evaluation <- "bv"
s_output_csvfile <- "scratch/extdata/GeneticTrendForRShiny_cas_index.csv"

build_calvingsurvival_index(ps_input_csvfile = s_input_csvfile,
                            ps_evaluation = s_evaluation,
                            ps_weighingfile = file.path(here::here(),"inst","extdata","weighing_factor2buildIndex.csv"),
                            ps_output_csvfile = s_output_csvfile)


s_input_csvfile <- "scratch/extdata/GeneticTrendForRShiny_fruchtbarkeit.csv"
s_evaluation <- "bv"
s_output_csvfile <- "scratch/extdata/GeneticTrendForRShiny_fbk_index.csv"

build_fertility_index <- function(ps_input_csvfile = s_input_csvfile,
                                  ps_evaluation = s_evaluation,
                                  ps_weighingfile = file.path(here::here(),"inst","extdata","weighing_factor2buildIndex.csv"),
                                  ps_output_csvfile = s_output_csvfile)
