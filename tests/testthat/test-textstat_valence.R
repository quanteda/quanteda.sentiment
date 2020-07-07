context("test textstat_valence")

test_that("textstat_valence works for uniform valences within key", {
  dict <- dictionary(list(positive = c("good", "great"),
                          negative = c("bad"),
                          neg_positive = "not good",
                          neg_negative = "not bad"))
  txt <- c(d1 = "good good bad bad good word1 word1 word1 word2 word2",
           d2 = "good",
           d3 = "notsentiment",
           d4 = "Great! Not bad.",
           d5 = "good good not good bad")

  # for two categories
  valence(dict) <- list(positive = 1, negative = -1)

  corp <- corpus(txt)
  toks <- tokens(corp)
  dfmat <- dfm(toks)
  
  expect_identical(
    textstat_valence(corp, dict),
    textstat_valence(toks, dict)
  )
  expect_identical(
    textstat_valence(corp, dict),
    textstat_valence(dfmat, dict)
  )
  
  expect_identical(
    textstat_valence(corp, dict)$sentiment,
    c((3 * 1 + 2 * -1) / (3 + 2),
      (1 * 1 + 0 * -1) / (1 + 0),
      (0 * 1 + 0 * -1) / (1),
      (1 * 1 + 1 * -1) / (1 + 1),
      (3 * 1 + 1 * -1) / (3 + 1))
  )
  
  # for multiple categories within one polarity
  valence(dict) <- list(positive = 1, negative = -1, 
                        neg_negative = 1, neg_positive = -1)
  expect_identical(
    textstat_valence(corp, dict),
    textstat_valence(toks, dict)
  )
  expect_equal(
    all.equal(textstat_valence(corp, dict)$sentiment,
              textstat_valence(dfmat, dict)$sentiment),
    "Mean relative difference: 1.5"
  )
  expect_identical(
    textstat_valence(corp, dict)$sentiment,
    c((3 * 1 + 2 * -1) / (5),
      (1 * 1 + 0 * -1) / (1),
      (0 * 1 + 0 * -1) / (1),
      (2 * 1 + 0 * -1) / (2),
      (2 * 1 + 2 * -1) / (4))
  )
})

test_that("textstat_valence with individual value scores works", {
  dict <- dictionary(list(
    happy = c("happy", "jubilant", "exuberant"),
    sad = c("sad", "morose", "down"),
    okay = c("just okay", "okay")
  ))
  valence(dict) <- list(
    happy = c("happy" = 1, "jubilant" = 2, "exuberant" = 2),
    sad = c("sad" = -1, "morose" = -2, "down" = -1),
    okay = c("just okay" = 0.5, "okay" = 5)
  )
  txt <- c(d1 = "sad word happy word exuberant",
           d2 = "down sad just okay",
           d3 = "sad happy word word")
  
  corp <- corpus(txt)
  toks <- tokens(corp) %>%
    tokens_compound(dict, concatenator = " ")
  dfmat <- dfm(toks)
    
  expect_identical(
    textstat_valence(corp, dict),
    textstat_valence(toks, dict)
  )
  expect_identical(
    textstat_valence(corp, dict),
    textstat_valence(dfmat, dict)
  )
  
  sent <- c((-1 + 1 + 2)   / 3, # 5
            (-1 - 1 + 0.5) / 3,
            (-1 + 1)       / 2) # 4
  expect_identical(
    textstat_valence(txt, dict),
    data.frame(doc_id = docnames(dfmat),
               sentiment = sent)
  )
})

test_that("textstat_valence error conditions work", {
  dict <- dictionary(list(
    happy = c("happy", "jubilant", "exuberant"),
    sad = c("sad", "morose", "down"),
    okay = "just okay"
  ))
  expect_error(
    textstat_valence("Happy, sad, neutral.", dictionary = dict),
    "no valenced keys found"
  )
})

test_that("valence assignment functions work", {
  dict <- dictionary(list(
    happy = c("happy", "jubilant", "exuberant"),
    sad = c("sad", "morose", "down"),
    okay = "just okay"
  ))
  
  expect_equal(valence(dict), NULL)
  
  expect_error(
    valence(dict) <- list(happy = "a", sad = -1),
    "valence values must be numeric"
  )
  
  valence(dict) <- list(happy = 1, sad = -1, okay = 0)
  expect_identical(
    valence(dict),
    list(happy = c(happy = 1, jubilant = 1, exuberant = 1), 
         sad = c(sad = -1, morose = -1, down = -1), 
         okay = c(`just okay` = 0))
  )
})

test_that("valence error checks work", {
  dict <- dictionary(list(top = c("top1", "top2"),
                          nested = list(nest1 = c("a", "one"),
                                        nest2 = c("b", "two"))))
  expect_error(
    valence(dict) <- list(top = c(1, 2), nested = -5),
    "valenced dictionaries cannot be nested"
  )
})

test_that("dictionary print method shows valence and polarity", {
  dict <- dictionary(list(
    happy = c("happy", "jubilant", "exuberant"),
    sad = c("sad", "morose", "down")
  ))
  valence(dict) <- c(happy = 1, sad = -1)
  expect_output(print(dict),
                "Dictionary object with 2 key entries.
Valences set for keys: happy, sad \b.
- [happy]:
  - happy, jubilant, exuberant
- [sad]:
  - sad, morose, down", fixed = TRUE)
  
  dict <- dictionary(list(
    happiness = c("happy", "jubilant", "exuberant", "content"),
    anger = c("mad", "peeved", "irate", "furious", "livid")
  ))
  valence(dict) <- list(happiness = c(3, 4, 5, 2),
                        anger = c(3.1, 2.4, 2.9, 4.1, 5.0))
  expect_output(print(dict),
                "Dictionary object with 2 key entries.
Valences set for keys: happiness, anger \b.
- [happiness]:
  - happy, jubilant, exuberant, content
- [anger]:
  - mad, peeved, irate, furious, livid", fixed = TRUE)
})

test_that("overlapping values work as expected", {
  dict <- dictionary(list(
    happy = c("okay", "exuberant"),
    sad = c("okay", "depressed")
  ))
  valence(dict) <- list(happy = c(okay = 1, exuberant = 3),
                        sad = c(depressed = -4, okay = -2))
  expect_identical(
    textstat_valence("Depressed not okay", dict)$sentiment,
    (-4 + 1 - 2) / 3
  )
  expect_identical(
    textstat_valence("Depressed not okay", dict)$sentiment,
    textstat_valence(dfm("Depressed not okay"), dict)$sentiment
  )
})
