% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Index.R
\name{build_fertility_index}
\alias{build_fertility_index}
\title{Build up Index for fertility}
\usage{
build_fertility_index(
  ps_input_csvfile,
  ps_evaluation,
  ps_weighingfile = file.path(here::here(), "inst", "extdata",
    "weighing_factor2buildIndex.csv"),
  ps_output_csvfile
)
}
\arguments{
\item{ps_input_csvfile}{path to the input csv-file coming from function correctBasis4genTrend}

\item{ps_evaluation}{type of evaluation by Qualitas AG (bv, rh)}

\item{ps_weighingfile}{file which contains the constant for weighing and factor depending of evaluation type}

\item{ps_output_csvfile}{resulting csv-file}
}
\value{
tbl_index resulting index tibble
}
\description{
Build up Index fertility to a csv-file
}
