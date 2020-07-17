context("test data")

data("data_dictionary_LSD2015", package = "quanteda.sentiment")

test_that("dictionaries have polarities and valences set", {
  expect_output(
    print(data_dictionary_AFINN, 0, 0),
    "Dictionary object with 1 key entry.\nValences set for keys: AFINN."
  )
  
  expect_output(
    print(data_dictionary_ANEW, 0, 0),
    "Dictionary object with 3 key entries.\nValences set for keys: pleasure, arousal, dominance."
  )

  expect_output(
    print(data_dictionary_geninqposneg, 0, 0),
    'Dictionary object with 2 key entries.\nPolarities: pos = "positive"; neg = "negative".'
  )

  expect_output(
    print(data_dictionary_HuLiu, 0, 0),
    'Dictionary object with 2 key entries.\nPolarities: pos = "positive"; neg = "negative".'
  )
  
  expect_output(
    print(data_dictionary_LoughranMcDonald, 0, 0),
    'Dictionary object with 9 key entries.\nPolarities: pos = "POSITIVE"; neg = "NEGATIVE".'
  )
  
  expect_output(
    print(data_dictionary_LSD2015, 0, 0),
    'Dictionary object with 4 key entries.\nPolarities: pos = "positive", "neg_negative"; neg = "negative", "neg_positive".'
  )
  
  expect_output(
    print(data_dictionary_NRC, 0, 0),
    'Dictionary object with 10 key entries.\nPolarities: pos = "positive"; neg = "negative"'
  )

  expect_output(
    print(data_dictionary_Rauh, 0, 0),
    'Dictionary object with 4 key entries.\nPolarities: pos = "positive", "neg_negative"; neg = "negative", "neg_positive"'
  )

  expect_output(
    print(data_dictionary_sentiws, 0, 0),
    'Dictionary object with 2 key entries.\nPolarities: pos = "positive"; neg = "negative" \nValences set for keys: positive, negative',
    fixed = TRUE
  )
})

test_that("dictionaries have metadata set", {
  meta_ok <- function(d) {
    fields <- c("title", "description", "url", "reference", "license") 
    tmp <- fields %in% names(meta(d))
    if (all(tmp)) {
      TRUE
    } else {
      warning("MISSING: ", paste(fields[!tmp], collapse = " "), call. = FALSE)
      FALSE
    }
  }
  expect_true(meta_ok(data_dictionary_AFINN))
  expect_true(meta_ok(data_dictionary_ANEW))
  expect_true(meta_ok(data_dictionary_geninqposneg))
  expect_true(meta_ok(data_dictionary_HuLiu))
  expect_true(meta_ok(data_dictionary_LoughranMcDonald))
  expect_true(meta_ok(data_dictionary_LSD2015))
  expect_true(meta_ok(data_dictionary_NRC))
  expect_true(meta_ok(data_dictionary_Rauh))
  expect_true(meta_ok(data_dictionary_sentiws))
})

