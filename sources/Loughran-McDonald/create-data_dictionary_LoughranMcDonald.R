# Loughran and McDonald Sentiment Word Lists

library("quanteda")

data_dictionary_LoughranMcDonald  <- dictionary(file = "sources/Loughran-McDonald/Loughran_and_McDonald_2014.cat")

polarity(data_dictionary_LoughranMcDonald) <- 
  list(pos = c("POSITIVE"), neg = c("NEGATIVE"))

meta(data_dictionary_LoughranMcDonald) <- 
  list(
    title = "Loughran and McDonald Sentiment Word Lists",
    description = "A quanteda dictionary object containing the 2014 version of the Loughran and McDonald Sentiment Word Lists. The categories are 'negative' (2355 features), 'positive' (354), 'uncertainty' (297), 'litigious' (903), 'constraining' (184), 'superfluous' (56), 'interesting' (68), 'modal words strong' (68) and 'modal words weak' (0).",
    url = "http://sraf.nd.edu/textual-analysis/resources/",
    reference = "Loughran, T. & McDonald, B. (2011). When is a Liability not a Liability? Textual Analysis, Dictionaries, and 10-Ks. Journal of Finance, 66(1), 35–65.",
    license = "The data compilations provided on this website are for use by individual researchers. For commercial licenses please contact mcdonald.1@nd.edu."
  )

usethis::use_data(data_dictionary_LoughranMcDonald, overwrite = TRUE)
