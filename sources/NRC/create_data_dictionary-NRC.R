library("quanteda")

class(data_dictionary_NRC) <- "dictionary2"
data_dictionary_NRC <- as.dictionary(data_dictionary_NRC)

meta(data_dictionary_NRC) <- 
  list(
    title = "NRC Word-Emotion Association Lexicon",
    description = "A quanteda dictionary object containing Mohammad and Charron's (2010, 2013) English version of the NRC Word-Emotion Association Lexicon (aka NRC Emotion Lexicon aka EmoLex): association of words with eight emotions (anger, fear, anticipation, trust, surprise, sadness, joy, and disgust) and two sentiments (negative and positive) manually annotated on Amazon's Mechanical Turk. Available in 40 different languages.",
    url = "http://saifmohammad.com/WebPages/AccessResource.htm",
    description = "A quanteda dictionary object containing 2,006 positive and 4,783 negative words from Hu and Liu (2004, 2005).",
    license = "Open, or for commercial for CAD $975.",
    reference = "Mohammad, S. & Turney, P. (2013). Crowdsourcing a Word-Emotion Association Lexicon. _Computational Intelligence_, 29(3), 436--465. https://arxiv.org/abs/1308.6297

Mohammad, S. & Turney, P. (2010). Emotions Evoked by Common Words and Phrases: Using Mechanical Turk to Create an Emotion Lexicon. In _Proceedings of the NAACL-HLT 2010 Workshop on Computational Approaches to Analysis and Generation of Emotion in Text_, June 2010, LA, California. https://dl.acm.org/doi/10.5555/1860631.1860635"
    )

polarity(data_dictionary_NRC) <- list(pos = c("positive"), neg = c("negative"))

usethis::use_data(data_dictionary_NRC, overwrite = TRUE)
