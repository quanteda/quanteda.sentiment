test_that("investigate digits problem", {
  skip("skip until digits issue can be solved")
  
  data("data_dictionary_LSD2015", package = "quanteda")
  expect_output(print(data_dictionary_LSD2015, max_nkey = 0, max_nval = 0),
                "Dictionary object with 4 key entries.",
                fixed = TRUE)

  expect_output(print(data_dictionary_geninqposneg, 0, 0),
               "Dictionary object with 2 key entries.",
               fixed = TRUE)
})
