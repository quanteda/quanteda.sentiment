library("quanteda")
test_that("textstat_polarity works on all object types", {
    txt <- c(d1 = "good good bad bad good word1 word1 word1 word2 word2",
             d2 = "good",
             d3 = "notsentiment",
             d4 = "Great!",
             d5 = "good good")

    smooth <- 0.5
    logit <- c(log(3 + smooth) - log(2 + smooth),
               log(1 + smooth) - log(0 + smooth),
               log(0 + smooth) - log(0 + smooth),
               log(1 + smooth) - log(0 + smooth),
               log(2 + smooth) - log(0 + smooth))

    data(data_dictionary_LSD2015, package = "quanteda.sentiment")
    
    expect_equivalent(
        textstat_polarity(txt, dictionary = data_dictionary_LSD2015),
        data.frame(doc_id = names(txt), sentiment = logit, stringsAsFactors = FALSE)
    )
    expect_identical(
        textstat_polarity(txt, dictionary = data_dictionary_LSD2015),
        textstat_polarity(corpus(txt), dictionary = data_dictionary_LSD2015)
    )
    expect_identical(
        textstat_polarity(txt, dictionary = data_dictionary_LSD2015),
        textstat_polarity(tokens(txt), dictionary = data_dictionary_LSD2015)
    )
    expect_identical(
        textstat_polarity(txt, dictionary = data_dictionary_LSD2015),
        textstat_polarity(dfm(tokens(txt)), dictionary = data_dictionary_LSD2015)
    )
})

test_that("different sentiment functions work as expected", {
    txt <- c(d1 = "good good bad bad good word1 word1 word1 word2 word2",
             d2 = "good",
             d3 = "notsentiment",
             d4 = "Great!",
             d5 = "good good")

    # logit scale
    smooth <- 0.5
    logit <- c(log(3 + smooth) - log(2 + smooth),
               log(1 + smooth) - log(0 + smooth),
               log(0 + smooth) - log(0 + smooth),
               log(1 + smooth) - log(0 + smooth),
               log(2 + smooth) - log(0 + smooth))
    data(data_dictionary_LSD2015, package = "quanteda.sentiment")
    expect_equal(
        logit,
        textstat_polarity(txt, dictionary = data_dictionary_LSD2015)$sentiment
    )

    # relative proportional difference
    rpd <- c(3 - 2,
             1 - 0,
             0 - 0,
             1 - 0,
             2 - 0) / c(5, 1, 0, 1, 2)
    expect_equal(
        rpd,
        textstat_polarity(txt, dictionary = data_dictionary_LSD2015,
                           fun = sent_relpropdiff)$sentiment
    )

    # absolute proportional difference
    apd <- c(3 - 2,
             1 - 0,
             0 - 0,
             1 - 0,
             2 - 0) / unname(ntoken(txt))
    expect_equal(
        apd,
        textstat_polarity(txt, dictionary = data_dictionary_LSD2015,
                           fun = sent_abspropdiff)$sentiment
    )
})

test_that("textstat_polarity error conditions work", {
    dict <- dictionary(list(
        happy = c("happy", "jubilant", "exuberant"),
        sad = c("sad", "morose", "down"),
        okay = "just okay"
    ))
    expect_error(
        textstat_polarity("Happy, sad, neutral.", dictionary = dict),
        "polarity is not set for this dictionary; see ?polarity", 
        fixed = TRUE
    )
    
})

test_that("polarity functions work", {
    dict <- dictionary(list(
        happy = c("happy", "jubilant", "exuberant"),
        sad = c("sad", "morose", "down"),
        okay = "just okay"
    ))

    expect_equal(polarity(dict), NULL)

    polarity(dict) <- list(pos = "happy", neg = "sad")
    expect_identical(
        polarity(dict),
        list(pos = "happy", neg = "sad")
    )

    polarity(dict) <- list(pos = "happy", neg = "sad", neut = "okay")
    expect_identical(
        polarity(dict),
        list(pos = "happy", neg = "sad", neut = "okay")
    )

    polarity(dict) <- list(pos = c("happy", "okay"), neg = "sad")
    expect_identical(
        polarity(dict),
        list(pos = c("happy", "okay"), neg = "sad")
    )

    expect_error(
        polarity(dict) <- list(blank = "happy", neg = "sad"),
        "value must be a list of 'pos', 'neg', and (optionally) 'neut'",
        fixed = TRUE
    )
    expect_error(
        polarity(dict) <- list(pos = "happy", neg = "sad", neutr = "okay"),
        "value must be a list of 'pos', 'neg', and (optionally) 'neut'",
        fixed = TRUE
    )
    
    # this should generate an error
    expect_error(
        polarity(dict) <- list(pos = "notfound", neg = "sad"),
        "'notfound' key not found in this dictionary"
    )

    # should test that both pos and neg are assigned ?

})

test_that("get_polarity_dictionary() works", {
    dict <- dictionary(list(
        happy = c("happy", "jubilant", "exuberant"),
        sad = c("sad", "morose", "down"),
        okay = "just okay"
    ))
    expect_equal(polarity(dict), NULL)
    
    polarity(dict) <- list(pos = "happy", neg = "sad", neut = "okay")

    expect_identical(
        quanteda.sentiment:::get_polarity_dictionary(dict) %>% 
            quanteda::as.list(),
        list(pos = c("happy", "jubilant", "exuberant"),
             neg = c("sad", "morose", "down"),
             neut = "just okay")
    )

    expect_identical(
        quanteda.sentiment:::get_polarity_dictionary(dict) %>% polarity(),
        list(pos = "pos", neg = "neg", neut = "neut")
    )
    
    polarity(dict) <- list(pos = "happy", neg = "sad", neut = "okay")
    dict["okay"] <- NULL
    expect_error(
        quanteda.sentiment:::get_polarity_dictionary(dict),
        "'okay' key not found in this dictionary"
    )
})

test_that("nested scope works for textstatpolarity on tokens", {
  dict <- dictionary(list(positive = "good", negative = "not good"))
  polarity(dict) <- list(pos = "positive", neg = "negative")
  valence(dict) <- c(positive = 1, negative = -1)
  toks <- tokens("The test is not good")
  
  expect_equivalent(
    textstat_polarity(toks, dictionary = dict, fun = sent_abspropdiff),
    data.frame(doc_id = "text1", sentiment = -0.25, row.names = NULL)
  )
})
