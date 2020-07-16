# ANEW

library("quanteda")

anew <- read.delim(url("https://bit.ly/2zZ44w0"))
anew <- anew[!duplicated(anew$Word), ] # because some words repeat
data_dictionary_ANEW <- dictionary(list(pleasure = anew$Word, 
                                        arousal = anew$Word, 
                                        dominance = anew$Word))
valence(data_dictionary_ANEW) <- list(pleasure = anew$ValMn, 
                                      arousal = anew$AroMn, 
                                      dominance = anew$DomMn)

meta(data_dictionary_ANEW) <- 
    list(
        title = "Affective Norms for English Words (ANEW)",
        description = "A quanteda dictionary object containing the ANEW, or Affective Norms for English Words (Bradley and Lang 2017) valenced lexicon.  The ANEW provides a lexicon of 2,471 distinct fixed word matches that are associated with three valenced categories: pleasure, arousal, and dominance.",
        url = "https://csea.phhp.ufl.edu/media.html#bottommedia",
        reference = "Bradley, M.M. & Lang, P.J. (2017). Affective Norms for English Words (ANEW): Instruction manual and affective ratings. Technical Report C-3. Gainesville, FL:UF Center for the Study of Emotion and Attention.",
        license = "For non-profit academic research purposes."
    )

usethis::use_data(data_dictionary_ANEW, overwrite = TRUE)

