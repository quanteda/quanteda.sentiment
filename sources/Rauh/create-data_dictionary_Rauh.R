#' Rauh's German Political Sentiment Dictionary

library("quanteda")
library("dplyr")

# load dictionary dataframes (downloaded here: https://doi.org/10.7910/DVN/BKBX)
load("sources/Rauh/Rauh_SentDictionaryGerman_Negation.Rdata")
load("sources/Rauh/Rauh_SentDictionaryGerman.Rdata")

# new column where NOT and word are divided with a space
neg.sent.dictionary <- neg.sent.dictionary %>% 
    mutate(word = gsub("NOT_", "NOT ", feature)) %>% 
    mutate(sentiment = ifelse(sentiment == 1, "neg_negative", "neg_positive"))

sent.dictionary <- sent.dictionary %>% 
    mutate(word = feature) %>% 
    mutate(sentiment = ifelse(sentiment == -1, "negative", "positive"))

# bind both dataframes
sent_dictionary_rauh <- bind_rows(sent.dictionary, neg.sent.dictionary)

# save as quanteda dictionary (word and sentiment column)
data_dictionary_Rauh <- quanteda::as.dictionary(sent_dictionary_rauh)

data_dictionary_Rauh <- as.dictionary(data_dictionary_Rauh)
meta(data_dictionary_Rauh) <- 
  list(
    title = "Rauh's German Political Sentiment Dictionary",
    description = "A quanteda dictionary object containing the dictionaries provided in Rauh (forthcoming). Rauh assesses its performance against human intuition of sentiment in German political language (parliamentary speeches, party manifestos, and media coverage). The resource builds on, harmonizes and extends the SentiWS (Remus et al. 2010) and GermanPolarityClues (Waltinger 2010) dictionaries. In order to use the negation correction provided by the dictionary, currently a combination of tokens_replace and tokens_compound is required to harmonize the five covered bi-gram patterns prior to scoring. The example below shows how to conduct this transformation. Note that the process changes the terms 'nicht|nichts|kein|keine|keinen' to a joint term altering some of the features of the original corpus.",
    url = "https://doi.org/10.7910/DVN/BKBXWD",
    reference = "Rauh, C. (2018). Validating a Sentiment Dictionary for German Political Language: A Workbench Note. Journal of Information Technology & Politics, 15(4), 319–343.

Remus, R., Quasthoff U., & Heyer, G. (2010). \"SentiWS - a Publicly Available German-language Resource for Sentiment Analysis.\" In Proceedings of the 7th International Language Resources and Evaluation (LREC'10), 1168–1171.

Waltinger, U. (2010). \"GermanPolarityClues: A Lexical Resource for German Sentiment Analysis.\" In International Conference on Language Resources and Evaluation, 17–23 May 2010 LREC'10.",
    license = "Unknown"
    )


polarity(data_dictionary_Rauh) <- 
  list(pos = c("positive", "neg_negative"), neg = c("negative", "neg_positive"))

usethis::use_data(data_dictionary_Rauh, overwrite = TRUE)
