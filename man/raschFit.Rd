% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/raschFit.R
\name{raschFit}
\alias{raschFit}
\title{Fits Rasch models for the reference group 0 and the focal group 1}
\usage{
raschFit(data, resp.mat.name = "i", group.name = "groups")
}
\arguments{
\item{data}{\itemize{
\item data.frame - simulated or a real data. Must contain:
\item response item matrix (matrix), binary (0/1) input.
\item group (vector), the group of the test takers.
}}

\item{resp.mat.name}{string vector, the name of the response matrix in 'data' input with 'i' as a default (as dgp).}

\item{group.name}{string vector, the group name in the data frame 'data' (as dgp).}
}
\value{
two objects of class "raschmodel", produced by function \code{RaschModel.fit} of the package \pkg{psychotools}.
}
\description{
Fits Rasch models for the reference group 0 and the focal group 1
}
\examples{
# Load the SPISA data set (general knowledge quiz - more information at ?SPISA)
library("psychotree")
data("SPISA")

# Fit the Rasch Models for the two groups females and males
fit <- raschFit(SPISA, resp.mat.name='spisa', group.name='gender')
}
