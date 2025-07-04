# Script for helper functions                            #
# Autor : G. Mulier                                      #
# Created the 02/07/2025, modified the 02/07/2025        #


# Check the validity of the supplied thresholds
CheckThresh <- function(Thresholds) {
  Thresholds <- gsub(",", "\\.", Thresholds)
  Thresholds <- gsub("^ +", "", Thresholds)
  Thresholds <- gsub(" +$", "", Thresholds)
  if (grepl(" ", Thresholds)) { # Several thresholds
    Thresholds <- strsplit(Thresholds, " ")[[1]]
    if (any(vapply(Thresholds, \(x) !substr(x, 1, 1) %in% c(">", "<"), logical(1)))) {
      return(list(FALSE, "All thresholds separated by spaces should start with > or <."))
    } else if (any(vapply(Thresholds, \(x) nchar(x) == 1 | grepl("[^0-9\\.-]", substr(x, 2, nchar(x))), logical(1)))) {
      return(list(FALSE, "All thresholds separated by spaces should be of the form > or < followed by a numeric."))
    } else {
      return(list(TRUE,
                  data.frame(
                    "operator" = vapply(Thresholds, \(x) substr(x, 1, 1), character(1)),
                    "threshold" = vapply(Thresholds, \(x) as.numeric(substr(x, 2, nchar(x))), numeric(1))
                  )))
    }
  } else { # Only 1 threshold
    if (!substr(Thresholds, 1, 1) %in% c(">", "<")) {
      return(list(FALSE, "The threshold should start with > or <."))
    } else if (nchar(Thresholds) == 1 | grepl("[^0-9\\.-]", substr(Thresholds, 2, nchar(Thresholds)))) {
      return(list(FALSE, "The threshold should be of the form > or < followed by a numeric."))
    } else {
      return(list(TRUE, data.frame("operator" = substr(Thresholds, 1, 1), "threshold" = as.numeric(substr(Thresholds, 2, nchar(Thresholds))))))
    }
  }
}

# Process the functions
ProcessFunctions <- function(FctTxt) {
  if (!grepl("//", FctTxt)) {
    return(list(NULL, NULL, FctTxt))
  } else {
    SplitSplit <- strsplit(FctTxt, "//\\n")[[1]]
    Pkg <- if (any(grepl("library", SplitSplit))) SplitSplit[grepl("library", SplitSplit)] else NULL
    Definition <- if (any(grepl("function", SplitSplit))) SplitSplit[grepl("function", SplitSplit)] else NULL
    Corps <- SplitSplit[length(SplitSplit)]
    return(list(Pkg, Definition, Corps))
  }
}


