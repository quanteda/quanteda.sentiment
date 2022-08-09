## (re)make all sentiment dictionaries

library("quanteda")

source("AFINN/create-data_dictionary_AFINN.R")
source("ANEW/create-data_dictionary_ANEW.R")
source("geninquirer/create-data_dictionary_geninquirer.R")
source("Hu-Liu/create_data_dictionary-HuLiu.R")
source("Loughran-McDonald/create-data_dictionary_LoughranMcDonald.R")
source("NRC/create_data_dictionary-NRC.R")
source("Rauh/create-data_dictionary_Rauh.R")
source("sentiws/create-data_dictionary_sentiws.R")

## not sentiment dictionaries
# source("Laver-Garry/create-data_dictionary_LaverGarry.R")
# source("MFD/create-data_dictionary_MFD.R")
# source("RID/create-data_dictionary_RID.R")

# LSD
data("data_dictionary_LSD2015", package = "quanteda")
polarity(data_dictionary_LSD2015) <- 
  list(pos = c("positive", "neg_negative"), neg = c("negative", "neg_positive"))
names(meta(data_dictionary_LSD2015))[which(names(meta(data_dictionary_LSD2015)) == "source")] <- "reference"
usethis::use_data(data_dictionary_LSD2015, overwrite = TRUE)
