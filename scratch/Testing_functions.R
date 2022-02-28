s_input_logfile <- "inst/extdata/genTrend.log"
s_par_default <- "inst/extdata/DefaultParameterGeneticTrend_2204r_bv.csv"

tbl_test <- PrepData4GenTrend::extract4genTrend(ps_input_logfile = s_input_logfile,
                            ps_par_default = s_par_default)



s_par_bdiff <- "inst/extdata/bdiffZuBsBasisAlleMerkmale_ym0.csv"
s_output_csvfile <- "inst/extdata/GeneticTrendForRShiny_fruchtbarkeit.csv"
PrepData4GenTrend::correctBasis4genTrend(ps_input_tibble = tbl_test,
                                         ps_par_bdiff = s_par_bdiff,
                                         ps_output_csvfile = s_output_csvfile)

