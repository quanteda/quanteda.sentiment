# Laver and Garry Dictionary of Policy Positions

library("quanteda")

data_dictionary_LaverGarry  <- dictionary(file = "Laver-Garry/Laver_and_Garry_2000.cat")

usethis::use_data(data_dictionary_LaverGarry, overwrite = TRUE)
