% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/textstat_polarity.R
\name{get_polarity_dictionary}
\alias{get_polarity_dictionary}
\title{Get a standard polarity dictionary for sentiment analysis}
\usage{
get_polarity_dictionary(dictionary)
}
\arguments{
\item{dictionary}{a \pkg{quanteda} \link{dictionary}}
}
\value{
a single-level \link{dictionary} with keys \code{pos}, \code{neg}, and (optionally)
\code{neut}.
}
\description{
Checks and standardizes a \link{dictionary} object with its \link{polarity} set, so
that the polarity categories are standardized into the keys \code{pos}, \code{neg}, and
(optionally) \code{neut}.  Also checks that the dictionary contains all of the
keys named in the polarity object.  (It is necessary to check here since the
dictionary could have been subset after creation.)
}
\keyword{internal}
