### #
### # Prepare data to plot the genTrend in RShiny
### #
### # -------------------------------------------------------------------------


cat(" ### # Start to Prepare data to plot the genTrend in RShiny")

cat(" ### # Get overall parameters")

s_par_default <- "inst/extdata/DefaultParameterGeneticTrend_2204r_bv.csv"
s_par_bdiff <- "inst/extdata/bdiffZuBsBasisAlleMerkmale_ym0.csv"

cat("-- Fruchtbarkeit --")

s_input_logfile_fbk <- "inst/extdata/genTrend.log"
s_output_csvfile_fbk <- "inst/extdata/GeneticTrendForRShiny_fruchtbarkeit.csv"


tbl_fbk <- PrepData4GenTrend::extract4genTrend(ps_input_logfile = s_input_logfile_fbk,
                                                ps_par_default = s_par_default)

PrepData4GenTrend::correctBasis4genTrend(ps_input_tibble = tbl_fbk,
                                         ps_par_bdiff = s_par_bdiff,
                                         ps_output_csvfile = s_output_csvfile_fbk)

cat("-- Zellzahl --")

s_input_logfile_scs <- "inst/extdata/genTrend_scs_ym0.log"
s_output_csvfile_scs <- "inst/extdata/GeneticTrendForRShiny_scs.csv"


tbl_scs <- PrepData4GenTrend::extract4genTrend(ps_input_logfile = s_input_logfile_scs,
                                               ps_par_default = s_par_default)

PrepData4GenTrend::correctBasis4genTrend(ps_input_tibble = tbl_scs,
                                         ps_par_bdiff = s_par_bdiff,
                                         ps_output_csvfile = s_output_csvfile_scs)

cat(" ### # Finish to Prepare data to plot the genTrend in RShiny")


