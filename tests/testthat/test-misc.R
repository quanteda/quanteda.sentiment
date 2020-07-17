context("test misc aspects")

test_that("printing dictionary3 works", {
  expect_output(
    print(data_dictionary_AFINN, 0, 0),
    "Dictionary object with 1 key entry.\nValences set for keys: AFINN ",
    fixed = TRUE
  )
  
  dict <- quanteda::dictionary(list(one = list(oneA = c("a", "b"),
                                               oneB = "d"),
                                    two = c("x", "y")))
  polarity(dict) <- list(pos = "one", neg = "two")
  expect_output(
    print(dict, 0, 0),
    'Dictionary object with 2 primary key entries and 2 nested levels.\nPolarities: pos = "one"; neg = "two" ',
    fixed = TRUE
  )
})

test_that("friendly error messages work", {
  expect_error(
    textstat_polarity(0),
    "textstat_polarity() only works on character, corpus, dfm, tokens objects.",
    fixed = TRUE
  )
  expect_error(
    textstat_valence(0),
    "textstat_valence() only works on character, corpus, dfm, tokens objects.",
    fixed = TRUE
  )
})
