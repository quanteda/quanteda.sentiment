# Regressive Imagery Dictionary

library("quanteda")

data_dictionary_RID <-
    dictionary(file = "RID/RID.CAT")
devtools::use_data(data_dictionary_RID, overwrite = TRUE)
