# textstat_valence ----------------

#' Compute sentiment using a dictionary method
#'
#' Compute sentiment scores from tokens or document-feature matrices, based on
#' the valences of dictionary keys and values.
#' @param x a character, [corpus], [tokens], or [dfm] object containing
#'   text, tokens, or features whose sentiment will be scored.
#' @param dictionary a \pkg{quanteda} [dictionary] that has [valence] set, in
#'   the form of numerical valences associated with sentiment
#' @param ... not used here
#' @return a data.frame of sentiment scores
#' @note 
#' If the input item is a [dfm], then multi-word values will not be matched 
#' unless the features of the [dfm] have been compounded previously.  The input
#' objects should not have had dictionaries applied previously.
#' @export
#' @references  
#'   For a discussion of how to aggregate sentiment scores to the document
#'   level, see:
#'   
#'   Lowe, W., Benoit, K. R., Mikhaylov, S., & Laver, M. (2011).
#'   Scaling Policy Preferences from Coded Political Texts. _Legislative Studies
#'   Quarterly_, 36(1), 123–155.
#'   <http://doi.org/10.1111/j.1939-9162.2010.00006.x>
#' @seealso [valence()]
#' @examples
#' library("quanteda")
#' corp <- tail(data_corpus_inaugural, n = 5)
#' toks <- tokens(corp)
#' dfmat <- dfm(toks)
#'
#' valence(data_dictionary_LSD2015) <- list(positive = 1, neg_negative = 1, 
#'                                          negative = -1, neg_positive = -1)
#' textstat_valence(toks, dictionary = data_dictionary_LSD2015)
#' # slightly different because does not account for phrases
#' textstat_valence(dfmat, dictionary = data_dictionary_LSD2015)
#' 
#' # Lowe et al (2011) log(pos / neg)
#' as.dfm(log(dfmat + 0.5)) %>%
#'     textstat_valence(dictionary = data_dictionary_LSD2015)
#'
#' \dontrun{
#' 
#' # AFINN
#' afinn <- tidytext::get_sentiments(lexicon = c("afinn"))
#' data_dictionary_afinn <- dictionary(list(afinn = afinn$word))
#' valence(data_dictionary_afinn) <- list(afinn = afinn$value)
#' textstat_valence(toks, dictionary = data_dictionary_afinn)
#' 
#' # ANEW
#' anew <- read.delim(url("https://bit.ly/2zZ44w0"))
#' anew <- anew[!duplicated(anew$Word), ] # because some words repeat
#' data_dictionary_anew <- dictionary(list(pleasure = anew$Word, 
#'                                         arousal = anew$Word, 
#'                                         dominance = anew$Word))
#' valence(data_dictionary_anew) <- list(pleasure = anew$ValMn, 
#'                                       arousal = anew$AroMn, 
#'                                       dominance = anew$DomMn)
#' textstat_valence(toks, data_dictionary_anew["pleasure"])
#' textstat_valence(toks, data_dictionary_anew["arousal"])}
#'
textstat_valence <- function(x, dictionary, ...) {
  UseMethod("textstat_valence")
}

#' @export
textstat_valence.default <-  function(x, dictionary, ...) {
  stop(friendly_class_undefined_message(class(x), "textstat_valence"))
}

#' @export
textstat_valence.character <- function(x, ...) {
  textstat_valence(corpus(x), ...)
}

#' @export
textstat_valence.corpus <- function(x, ...) {
  textstat_valence(tokens(x), ...)
}

#' @export
textstat_valence.tokens <- function(x, dictionary, ...) {
  valence(dictionary) <- set_valences(dictionary, valence(dictionary))
  numdict <- dictionary(as.list(flip_valence(dictionary)))
  as.tokens(x) %>%
    tokens_lookup(dictionary = numdict, nomatch = "other",
                  nested_scope = "dictionary") %>%
    dfm() %>%
    aggregate_valence()
}

#' @export
textstat_valence.dfm <- function(x, dictionary, ...) {
  valence(dictionary) <- set_valences(dictionary, valence(dictionary))
  numdict <- dictionary(as.list(flip_valence(dictionary)))
  as.dfm(x) %>%
    dfm_lookup(dictionary = numdict, nomatch = "other") %>%
    aggregate_valence()
}

# internal sentiment calculation functions -----------

# uses Kohei's approach to make the valence values into the keys, and 
# then groups all values together under that score
flip_valence <- function(dictionary) {
  v <- valence(dictionary)
  if (is.null(v)) stop("valence not set")
  
  structure(unlist(sapply(v, names), use.names = FALSE), 
            names = unlist(v, use.names = FALSE))
}

aggregate_valence <- function(x, normalize = TRUE) {
  other_index <- match("other", colnames(x))
  other <- as.vector(x[, other_index])
  x <- x[, -other_index]
  result <- data.frame(doc_id = quanteda::docnames(x),
                       sentiment = as.vector(x %*% as.numeric(colnames(x))
                                             / (if (normalize) rowSums(x) else 1)))
  result$sentiment[is.na(result$sentiment)] <- 0
  result
}

# valence setting and checking functions --------------

#' Set or get the valences of dictionary values or keys
#'
#' Set or retrieve the valences of a [dictionary] object for the purposes of
#' sentiment analysis.  Valences consist of numerical values attached to each
#' dictionary "value".  For dictionaries with a more "polarity"-based approach,
#' see [textstat_polarity()]
#' 
#' Valences are used only in [textstat_valence()].
#'
#' A dictionary may have only one set of valences at a time, but may be
#' changed as needed.
#' @param x a \pkg{quanteda} [dictionary][quanteda::dictionary] object
#' @return `valences()` returns the valences as a list named numeric vectors,
#'   where each list element corresponds to a key in the dictionary, and each
#'   numeric element matches a value within that key.
#' @keywords dictionary textstat utility
#' @seealso [textstat_valence()], [valence()]
#' @export
#'
#' @examples
#' library("quanteda")
#' 
#' # setting valences
#' dict <- dictionary(list(
#'     happiness = c("happy", "jubilant", "exuberant", "content"),
#'     anger = c("mad", "peeved", "irate", "furious", "livid")
#' ))
#' valence(dict)
#' # using a 5-point scale: 1:1 match
#' valence(dict) <- list(happiness = c(3, 4, 5, 2),
#'                       anger = c(3.1, 2.4, 2.9, 4.1, 5.0))
#' valence(dict)
#' # with single valences applied to all values within the keys
#' valence(dict) <- c(happiness = 1, anger = -1)
#' valence(dict)
#' # with named elements - order does not matter
#' valence(dict) <- list(
#'     happiness = c(exuberant = 5, jubilant = 4, happy = 3, content = 2)
#' ) 
#' valence(dict) 
#' 
valence <- function(x) {
  UseMethod("valence")
}

#' @export
valence.dictionary2 <- function(x) {
    x@meta$object$valence
}

#' @rdname valence
#' @param value named list consisting of numerical value.  The names of the
#'   elements must correspond to a dictionary key. Each element must be:
#'   * a single numeric value that will be applied to all of the dictionary
#'   values in that key; or
#'   * a vector of numeric values that matches the length and order of the
#'   dictionary values in that key; or
#'   * a named numeric vector where each element name matches dictionary values
#'   in the key.
#' @return `valence<-` sets the dictionary's valences.
#' @export
"valence<-" <- function(x, value) {
  UseMethod("valence<-")
}

#' @export
"valence<-.dictionary2" <- function(x, value) {
  value <- as.list(value)
  check_valences(x, value)
  x@meta$object$valence <- set_valences(x, value)
  class(x) <- "dictionary3"
  x
}    

check_valences <- function(dictionary, valences) {
  if (dictionary_depth(dictionary) > 1)
    stop("valenced dictionaries cannot be nested", call. = FALSE)
  if (!is.list(valences) || any(names(valences) == ""))
    stop("valence must be a fully named list", call. = FALSE)
  for (key in names(valences)) {
    if (!key %in% names(dictionary))
      stop("'", key, "' is not a dictionary key", call. = FALSE)
    if (!is.numeric(valences[[key]]))
      stop("valence values must be numeric", call. = FALSE)
    if (length(valences[[key]]) != 1 &&
        length(valences[[key]]) != length(dictionary[[key]]))
      stop("valence value length not equal to number of values for key '",
           key, "'", call. = FALSE)
  }
}    

set_valences <- function(dictionary, valences) {
  # only use valences for keys in dictionary
  valences <- valences[names(valences) %in% names(dictionary)]
  if (!length(valences))
    stop("no valenced keys found")
  
  for (key in names(valences)) {
    # repeat valences if only a single value is supplied
    if (length(valences[[key]]) == 1)
      valences[[key]] <- rep(valences[[key]], length(dictionary[[key]]))
    # use dictionary values as names if none supplied
    if (length(names(valences[[key]])) != length(valences[[key]]))
      names(valences[[key]]) <- dictionary[[key]]
  }
  valences
}
