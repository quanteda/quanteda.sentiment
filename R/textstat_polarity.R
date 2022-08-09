# textstat_polarity ----------------

#' Compute sentiment from key polarities
#'
#' Compute sentiment scores using a polarity approach, based on assigned
#' categories (types or features) of positive, negative, and neutral sentiment.
#' Several formulas for combining the polar categories are available, or the
#' user can supply a custom function.
#' @param x a character, [corpus], [tokens], or [dfm] object containing
#'   text, tokens, or features whose sentiment will be scored
#' @param dictionary a [dictionary] that has [polarity] set, indicating which
#'   keys are associated with positive, negative, and (optionally) neutral
#'   sentiment
#' @param fun function; the formula for computing sentiment, which must refer to
#'   `pos`, `neg`, and (optionally) `neut`.  The default is the "logit" scale
#'   (Lowe et al 2011) which is the log of (positive / negative) counts.  See
#'   [sentiment-functions] for details and for additional available functions,
#'   as well as details on how to supply custom functions.
#' @param ... additional arguments passed to `fun`
#' @return a [data.frame] of sentiment scores
#' @export
#' @references  Lowe, W., Benoit, K. R., Mikhaylov, S., & Laver, M. (2011).
#'   Scaling Policy Preferences from Coded Political Texts. _Legislative Studies
#'   Quarterly_, 36(1), 123–155.
#'   <http://doi.org/10.1111/j.1939-9162.2010.00006.x>
#' @examples
#' library("quanteda")
#' corp <- tail(data_corpus_inaugural, n = 5)
#' toks <- tokens(corp)
#' dfmat <- dfm(toks)
#' polar1 <- list(pos = "positive", neg = "negative")
#' polar2 <- list(pos = c("positive", "neg_negative"),
#'                neg = c("negative", "neg_positive"))
#'
#' polarity(data_dictionary_LSD2015) <- polar1
#' textstat_polarity(corp, dictionary = data_dictionary_LSD2015)
#' textstat_polarity(toks, dictionary = data_dictionary_LSD2015)
#' textstat_polarity(dfmat, dictionary = data_dictionary_LSD2015)
#'
#' polarity(data_dictionary_LSD2015) <- polar2
#' textstat_polarity(corp, dictionary = data_dictionary_LSD2015)
#' textstat_polarity(toks, dictionary = data_dictionary_LSD2015)
#' textstat_polarity(corp, dictionary = data_dictionary_LSD2015)
#' textstat_polarity(dfmat, dictionary = data_dictionary_LSD2015)
#'
#' # with a user-supplied function
#' sent_fn <- function(x) (x[, "pos"] - x[, "neg"]) / rowSums(x) * 100
#' textstat_polarity(toks, data_dictionary_LSD2015, fun = sent_fn)
textstat_polarity <- function(x, dictionary, fun = sent_logit, ...) {
    UseMethod("textstat_polarity")
}

#' @export
textstat_polarity.default <-  function(x, dictionary, fun = sent_logit, ...) {
    stop(friendly_class_undefined_message(class(x), "textstat_polarity"))
}

#' @importFrom quanteda corpus
#' @export
textstat_polarity.character <- function(x, ...) {
    textstat_polarity(corpus(x), ...)
}

#' @importFrom quanteda tokens
#' @export
textstat_polarity.corpus <- function(x, ...) {
    textstat_polarity(tokens(x), ...)
}

#' @importFrom quanteda dictionary tokens_lookup dfm
#' @export
textstat_polarity.tokens <- function(x, dictionary, ...) {
    dict <- get_polarity_dictionary(dictionary)
    poldict <- dictionary(polarity(dict))
    polarity(poldict) <- polarity(dict)

    tokens(x) |>
        tokens_lookup(dictionary = dict, nomatch = "other", nested_scope = "dictionary") |>
        dfm() |>
        textstat_polarity(dictionary = poldict, ...)
}

#' @importFrom quanteda convert dfm_lookup as.dfm
#' @export
textstat_polarity.dfm <- function(x, dictionary, fun = sent_logit, ...) {
    dict <- get_polarity_dictionary(dictionary)

    result <- fun(dfm_lookup(x, dict, nomatch = "other"), ...)
    result <- convert(as.dfm(result), to = "data.frame")
    names(result)[2] <- "sentiment"

    class(result) <- c("sentiment", "textstat", "data.frame")
    attr(result, "fun") <- fun
    attr(result, "fun_name") <- as.character(substitute(fun))

    result
}


# polarity setting and checking functions --------------

#' Set or get the sentiment polarity of a dictionary
#'
#' Set or retrieve the polarity of a [dictionary] object for the purposes of
#' sentiment analysis.  Polarity consists of a set of dictionary keys that are
#' associated with positive, negative, and (optionally) neutral categories for
#' use in [textstat_polarity()].
#'
#' A dictionary may have only one set of polarities at a time, but may be
#' changed as needed.
#' @param x a [dictionary] object
#' @return `polarity()` returns the polarity as a list.
#' @keywords dictionary textstat utility
#' @export
#'
#' @examples
#' library("quanteda")
#' simpledict <- dictionary(list(
#'     happy = c("happy", "jubilant", "exuberant"),
#'     sad = c("sad", "morose", "down")
#' ))
#' polarity(simpledict)
#' polarity(simpledict) <- list(pos = "happy", neg = "sad")
#' polarity(simpledict)
#'
#' # can list multiple keys
#' polarity(data_dictionary_LSD2015) <- list(
#'     pos = c("positive", "neg_negative"),
#'     neg = c("negative", "neg_positive")
#' )
#' polarity(data_dictionary_LSD2015)
polarity <- function(x) {
    UseMethod("polarity")
}

#' @export
polarity.dictionary2 <- function(x) {
    x@meta$object$polarity
}

#' @rdname polarity
#' @param value list consisting of named character vectors `pos`, `neg`, and
#'   (optionally) `neut` corresponding to positive, negative, and neutral
#'   sentiment categories respectively.  Each element may contain multiple
#'   key names.  The `neut` category is optional but `pos` and `neg` must be
#'   supplied.
#' @return `polarity<-` sets the dictionary's polarity.
#' @export
"polarity<-" <- function(x, value) {
    UseMethod("polarity<-")
}

#' @export
"polarity<-.dictionary2" <- function(x, value) {
    if (!is.null(value)) {
        if (!setequal(union(c("pos", "neg", "neut"), names(value)),
                      c("pos", "neg", "neut")) ||
            !is.list(value)) {
            stop("value must be a list of 'pos', 'neg', and (optionally) 'neut'",
                 call. = FALSE)
        }
        check_that_poles_exist(x, value)
    } else {
        if (is.null(valence(x))) class(x) <- "dictionary2"
    }
        
    x@meta$object$polarity <- value
    x
}

#' Get a standard polarity dictionary for sentiment analysis
#'
#' Checks and standardizes a [dictionary] object with its [polarity] set, so
#' that the polarity categories are standardized into the keys `pos`, `neg`, and
#' (optionally) `neut`.  Also checks that the dictionary contains all of the
#' keys named in the polarity object.  (It is necessary to check here since the
#' dictionary could have been subset after creation.)
#' @param dictionary a \pkg{quanteda} [dictionary]
#' @return a single-level [dictionary] with keys `pos`, `neg`, and (optionally)
#'   `neut`.
#' @keywords internal
get_polarity_dictionary <- function(dictionary) {
    poles <- polarity(dictionary)

    # check the poles
    if (is.null(poles)) {
        stop("polarity is not set for this dictionary; see ?polarity",
             call. = FALSE)
    }
    check_that_poles_exist(dictionary, poles)

    # standardize the dictionary
    dictlist <- list(
        pos = unlist(dictionary[poles$pos], use.names = FALSE),
        neg = unlist(dictionary[poles$neg], use.names = FALSE),
        neut = unlist(dictionary[poles$neut], use.names = FALSE)
    )
    dict <- dictionary(dictlist[!sapply(dictlist, is.null)])

    # set the polarity to the keys
    newpoles <- list(pos = "pos", neg = "neg")
    if (!is.null(dictlist$neut)) newpoles <- c(newpoles, list(neut = "neut"))
    polarity(dict) <- newpoles

    return(dict)
}


check_that_poles_exist <- function(dictionary, poles) {
    poles <- unlist(poles, use.names = FALSE)
    polematch <- poles %in% names(dictionary)
    if (!all(polematch)) {
        stop("'", poles[!polematch], "' key not found in this dictionary",
             call. = FALSE)
    }
}

# sentiment formula functions --------------

#' Sentiment functions
#'
#' Functions for computing sentiment, for [textstat_polarity()].  Each function
#' takes an input [dfm] with fixed feature names (see Details), and returns a
#' sparse Matrix with a single column representing the results of the sentiment
#' calculation.
#'
#' @details
#' User supplied functions must take `x` and optional additional arguments, such
#' as `smooth` for a smoothing constant for the logit scaling function. feature
#' names for the sentiment categories `pos`, `neg`, `neut`, and `other`.  (The
#' `other` category is only required when a scaling function needs the count of
#' non-sentiment associated features.)
#'
#' Additional arguments may be passed via `...`, such as `smooth` for the logit
#' scale.
#'
#' @param x a [dfm] that has the following required feature names: `pos`,
#' `neg`, `neut`, and `other`
#' @return a sparse \pkg{Matrix} object of documents by sentiment score, where
#'   the sentiment score is the only column.  (Its name is unimportant as this
#'   will not be used by [textstat_polarity()].)
#' @keywords textstat internal
#' @references  Lowe, W., Benoit, K. R., Mikhaylov, S., & Laver, M. (2011).
#'   Scaling Policy Preferences from Coded Political Texts. _Legislative Studies
#'   Quarterly_, 36(1), 123–155.
#'   <http://doi.org/10.1111/j.1939-9162.2010.00006.x>
#' @name sentiment-functions
#' @examples
#' library("quanteda")
#' dfmat <- dfm(c("pos pos pos neg pos pos", "neg neg pos pos pos"))
#' sent_logit(dfmat)
#' sent_abspropdiff(dfmat)
#'
#' # user-supplied function
#' my_sent_fn <- function(x) (x[, "pos"] - x[, "neg"]) / rowSums(x) * 100
#' my_sent_fn(dfmat)
#'
#' # user supplied function with fixed weights and using neutral category
#' dfmat2 <- dfm(c("pos pos neut neg neut pos", "neg neg neut neut pos"))
#' my_sent_fn2 <- function(x) (x[, "pos"]*3 + x[, "neut"]*2 + x[, "neg"]*1)/3
#' my_sent_fn2(dfmat2)
NULL

#' @description `sent_logit` is \eqn{log(\frac{pos}{neg})}.
#' @rdname sentiment-functions
#' @param smooth additional smoothing function added to `pos` and `neg` before
#'   logarithmic transformation
#' @export
sent_logit <- function(x, smooth = 0.5) {
    log(x[, "pos"] + smooth) - log(x[, "neg"] + smooth)
}

#' @description `sent_abspropdiff` is \eqn{\frac{pos - neg}{N}}, where \eqn{N}
#'   is the total number of all features in a document.
#' @rdname sentiment-functions
#' @importFrom Matrix rowSums
#' @export
sent_abspropdiff <- function(x) {
    (x[, "pos"] - x[, "neg"]) / Matrix::rowSums(x)
}

#' @description `sent_relpropdiff` is \eqn{\frac{pos - neg}{pos + neg}}.
#' @rdname sentiment-functions
#' @export
sent_relpropdiff <- function(x) {
    (x[, "pos"] - x[, "neg"]) / (x[, "pos"] + x[, "neg"])
}
