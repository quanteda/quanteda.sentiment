# AFINN Dictionary

library("quanteda")

afinn111 <- read.delim("AFINN-111.txt", header = FALSE, col.names = c("word", "valence"))
afinn96 <- read.delim("AFINN-96.txt", header = FALSE, col.names = c("word", "valence"))

afinn111 <- dplyr::arrange(afinn111, word)
afinn96 <- dplyr::arrange(afinn96, word)
dplyr::filter(afinn96, duplicated(afinn96$word))

data_dictionary_AFINN <- dictionary(list("AFINN" = afinn111$word))
valence(data_dictionary_AFINN) <- list("AFINN" = afinn111$valence)

meta(data_dictionary_AFINN) <- list(
  title = "Finn Årup Nielsen's (2011) 'new ANEW' valenced word list",
  description = "AFINN is a list of English words rated for valence with an integer between minus five (negative) and plus five (positive), manually labeled by Finn Årup Nielsen in 2009-2011.  This dictionary is the newer AFINN-111 version with 2,477 words and phrases.",
  url = "http://www2.imm.dtu.dk/pubdb/views/publication_details.php?id=6010",
  reference = "Nielsen, F. Å. (2011). A new ANEW: Evaluation of a Word List for Sentiment Analysis in Microblogs. In Proceedings of the ESWC2011 Workshop on 'Making Sense of Microposts': Big Things Come in Small Packages, 93--98.",
  license = "This database of words is copyright protected and distributed under the Open Database License (ODbL) v1.0, http://www.opendatacommons.org/licenses/odbl/1.0/"
)

usethis::use_data(data_dictionary_AFINN, overwrite = TRUE)

