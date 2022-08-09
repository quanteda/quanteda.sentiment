print_dictionary <- quanteda:::print_dictionary

# new method for printing sentiment dictionaries
#' @import methods
setMethod("print", signature(x = "dictionary2"),
          function(x,
                   max_nkey = quanteda::quanteda_options("print_dictionary_max_nkey"),
                   max_nval = quanteda::quanteda_options("print_dictionary_max_nval"),
                   show_summary = quanteda::quanteda_options("print_dictionary_summary"),
                   ...) {
            x <- quanteda::as.dictionary(x)
            if (show_summary) {
              depth <- dictionary_depth(x)
              lev <- if (depth > 1L) " primary" else ""
              nkey <- length(names(x))
              cat("Dictionary object with ", as.character(nkey), lev, " key entr",
                  if (nkey == 1L) "y" else "ies", sep = "")
              if (lev != "") cat(" and ", as.character(depth), " nested levels", sep = "")
              cat(".\n")
              if (!is.null(polarity(x))) {
                cat("Polarities: ")
                poles <- lapply(polarity(x), function(y) paste0("\"", y, "\""))
                cat(mapply(paste, names(poles), "=",
                           unname(sapply(poles, paste, collapse = ", "))) |>
                      paste(collapse = "; "),
                    "\n")
              } 
              if (!is.null(valence(x))) {
                cat("Valences set for keys: ")
                cat(paste(names(valence(x)), collapse = ", "), "\n")
              }
            }
            invisible(print_dictionary(x, 1, max_nkey, max_nval, ...))
          })
