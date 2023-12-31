% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/discard_redundant_cols.R
\name{discard_redundant_cols}
\alias{discard_redundant_cols}
\title{Get rid of all metadata colmuns that show duplicated information or are uninformative (only-1 or all-different variable values)}
\usage{
discard_redundant_cols(df)
}
\value{
A dataframe object
}
\description{
Leave only unique-information columns. Generally metadata includes the same variable information expressed in different ways through different columns. This functions detects an eliminates all of those.
}
