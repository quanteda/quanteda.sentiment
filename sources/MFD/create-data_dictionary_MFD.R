# Moral Foundations Dictionary

library("quanteda")

#data_dictionary_MFD <- dictionary(file = "sources/MFD/moral_foundations_dictionary.dic")
data_dictionary_MFD <- dictionary(file = "MFD/mfd2.0.dic")

usethis::use_data(data_dictionary_MFD, overwrite = TRUE)
