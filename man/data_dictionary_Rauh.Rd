% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data-documentation.R
\docType{data}
\name{data_dictionary_Rauh}
\alias{data_dictionary_Rauh}
\title{Rauh's German Political Sentiment Dictionary}
\format{
The dictionary has four keys.
\describe{
\item{\code{negative}}{19,750 terms indicating negative sentiment}
\item{\code{positive}}{17,330 terms indicating positive sentiment}
\item{\code{neg_positive}}{17,330 terms indicating a positive word preceded
by a negation (used to convey negative sentiment)}
\item{\code{neg_negative}}{19,750 terms indicating a negative word preceded
by a negation (used to convey positive sentiment)}
}
}
\source{
\doi{10.7910/DVN/BKBXWD}
}
\usage{
data_dictionary_Rauh
}
\description{
A \pkg{quanteda} \link[quanteda:dictionary]{dictionary} object containing the
dictionaries provided in Rauh (forthcoming). Rauh assesses its performance
against human intuition of sentiment in German political language
(parliamentary speeches, party manifestos, and media coverage). The resource
builds on, harmonizes and extends the SentiWS (Remus et al. 2010) and
GermanPolarityClues (Waltinger 2010) dictionaries. In order to use the
negation correction provided by the dictionary, currently a combination of
\link[quanteda:tokens_replace]{tokens_replace} and \link[quanteda:tokens_compound]{tokens_compound} is
required to harmonize the five covered bi-gram patterns prior to scoring. The
example below shows how to conduct this transformation. Note that the process
changes the terms "nicht|nichts|kein|keine|keinen" to a joint term altering
some of the features of the original corpus.
}
\examples{
\donttest{
# tokenize example text
toks <- tokens("nicht schlecht dieses wunderschöne Wörterbuch")
# replace negation markers with "not"
toks1 <- tokens_replace(toks, pattern = c("nicht", "nichts", "kein",
                                                    "keine", "keinen"),
                                  replacement = rep("not", 5))
# compound bi-gram negation patterns
toks2 <- tokens_compound(toks1, data_dictionary_Rauh, concatenator = " ")

# apply dictionary
tokens_lookup(toks2, dictionary = data_dictionary_Rauh) |>
  dfm()
}
}
\references{
Rauh, C. (2018). Validating a Sentiment Dictionary for German Political
Language: A Workbench Note.
\emph{Journal of Information Technology & Politics}, 15(4), 319--343.
\doi{10.1080/19331681.2018.1485608}

Remus, R., Quasthoff U., & Heyer, G. (2010). "\href{http://www.lrec-conf.org/proceedings/lrec2010/pdf/490_Paper.pdf}{SentiWS - a Publicly Available German-language Resource for Sentiment Analysis.}"
In \emph{Proceedings of the 7th International Language Resources and Evaluation
(LREC'10)}, 1168--1171.

Waltinger, U. (2010). "\href{http://www.ulliwaltinger.de/pdf/91_Paper.pdf}{GermanPolarityClues: A Lexical Resource for German Sentiment Analysis}." In
\emph{International Conference on Language Resources and Evaluation}, 17--23 May
2010 LREC'10.
}
\keyword{data}
