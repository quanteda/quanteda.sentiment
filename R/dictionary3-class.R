print_dictionary <- quanteda:::print_dictionary
field_object <- quanteda:::field_object
dictionary_depth <- quanteda:::dictionary_depth
build_dictionary2 <- quanteda:::build_dictionary2
validate_dictionary <- quanteda:::validate_dictionary

# class definition and class functions --------
#' dictionary3 class
#'
#' This extends the \pkg{quanteda} `dictionary2` class with a version that 
#' recognizes [valence] and [polarity] meta-data for sentiment analysis.
#' @rdname dictionary3
#' @export
#' @keywords internal dictionary
#' @importFrom quanteda meta<- quanteda_options as.dictionary as.tokens
#' @slot .Data named list of mode character, where each element name is a
#'   dictionary "key" and each element is one or more dictionary entry "values"
#'   consisting of a pattern match
#' @slot meta list of object metadata
setClass("dictionary3", contains = "dictionary2")

#' @rdname dictionary3
#' @aliases print.dictionary3
#' @param max_nkey max number of keys to print; default is from the
#'   `print_dictionary_max_max_nkey` setting of [quanteda_options()]
#' @param max_nval max number of values to print; default is from the
#'   `print_dictionary_max_nval` setting of [quanteda_options()]
#' @export
setMethod("print", signature(x = "dictionary3"),
          function(x,
                   max_nkey = quanteda_options("print_dictionary_max_nkey"),
                   max_nval = quanteda_options("print_dictionary_max_nval"),
                   show_summary = quanteda_options("print_dictionary_summary"),
                   ...) {
            x <- as.dictionary(x)
            if (show_summary) {
              depth <- dictionary_depth(x)
              lev <- if (depth > 1L) " primary" else ""
              nkey <- length(names(x))
              cat("Dictionary object with ", nkey, lev, " key entr",
                  if (nkey == 1L) "y" else "ies", sep = "")
              if (lev != "") cat(" and ", depth, " nested levels", sep = "")
              cat(".\n")
              if (!is.null(polarity(x))) {
                cat("Polarities: ")
                poles <- lapply(polarity(x), function(y) paste0("\"", y, "\""))
                cat(mapply(paste, names(poles), "=",
                           unname(sapply(poles, paste, collapse = ", "))) %>%
                      paste(collapse = "; "), 
                    "\b.\n")
              } else if (!is.null(valence(x))) {
                cat("Valences set for keys: ")
                cat(paste(names(valence(x)), collapse = ", "), "\b.\n")
              }
            }
            print_dictionary(x, 1, max_nkey, max_nval, ...)
          })

# # @param object the dictionary to be extracted
# # @param i index for entries
# # @rdname dictionary3
# # @export
# setMethod("[",
#           signature = c("dictionary3", i = "index"),
#           function(x, i) {
#             x <- as.dictionary(x)
#             x <- unclass(x)
#             attrs <- attributes(x)
#             is_category <- vapply(x[i], function(y) is.list(y), logical(1))
#             result <- build_dictionary2(x[i][is_category],
#                                         separator = field_object(attrs, "separator"),
#                                         valuetype = field_object(attrs, "valuetype"))
#             meta(result) <- attrs[["meta"]][["user"]]
#             result@meta$object <- attrs[["meta"]][["object"]]
#             # subset valence on top-level keys
#             if (!is.null(valence(result)))
#               valence(result) <- valence(result)[names(result)]
#             # subset valence on top-level keys
#             if (!is.null(polarity(result)))
#               polarity(result) <- polarity(result)[names(polarity(result)) %in% names(result)]
#             result
#           })
