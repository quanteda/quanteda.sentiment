library("quanteda")

geninquirer <- read.csv("inquireraugmented.csv",
                        stringsAsFactors = FALSE, comment.char = "")
GIpos <-
    c(geninquirer$Entry[geninquirer$Positiv == "Positiv"],
      geninquirer$Entry[geninquirer$Yes == "Yes"]) %>%
    char_tolower %>%
    stringi::stri_replace_all_regex("#\\w+$", "") %>%
    unique
GIneg <-
    c(geninquirer$Entry[geninquirer$Negativ == "Negativ"],
      geninquirer$Entry[geninquirer$No == "No"]) %>%
    char_tolower %>%
    stringi::stri_replace_all_regex("#\\w+$", "") %>%
    unique
data_dictionary_geninqposneg <-
    dictionary(list(positive = GIpos, negative = GIneg))

meta(data_dictionary_geninqposneg) <- 
  list(
    title = "Augmented General Inquirer Positiv and Negativ dictionary",
    url = "http://www.wjh.harvard.edu/~inquirer/spreadsheet_guide.htm",
    description = "A lexicon containing the Positiv and Negativ dictionary entries from the augmented
      General Inquirer. These are new valence categories described at
      http://www.wjh.harvard.edu/~inquirer/homecat.htm but also include the
      terms from the http://www.wjh.harvard.edu/~inquirer/Yes.html 'yes' and
      http://www.wjh.harvard.edu/~inquirer/No.html 'no' dictionary entries.",
    url = "http://www.wjh.harvard.edu/~inquirer/spreadsheet_guide.htm",
    license = "Open, but email the creators for commercial use. Many more categories are available.",
    reference = "Stone, P.J., Dunphy, C.D., & Smith, M.S. (1966). _The General Inquirer: A Computer Approach to Content Analysis._ Cambridge, MA: MIT Press."
  )

polarity(data_dictionary_geninqposneg) <- 
  list(pos = "positive", neg = "negative")


usethis::use_data(data_dictionary_geninqposneg, overwrite = TRUE)
