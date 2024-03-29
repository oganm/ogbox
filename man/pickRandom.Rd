% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bioSpecific.R
\name{pickRandom}
\alias{pickRandom}
\title{Select random elements based on a criteria}
\usage{
pickRandom(
  labels,
  allValues,
  tolerance,
  allowSelf = TRUE,
  invalids = c(),
  n = 500
)
}
\arguments{
\item{labels}{Character vector. Labels to randomize}

\item{allValues}{Named numeric vector. Randomization candidates}

\item{tolerance}{Percentage tolerance for randomization}

\item{allowSelf}{Logical, is self a viable candidate for randomization}

\item{invalids}{Specific cases to exclude from randomization}

\item{n}{How many times randomization should happen}
}
\description{
Select random elements based on a criteria
}
