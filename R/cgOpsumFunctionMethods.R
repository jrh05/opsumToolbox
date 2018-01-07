# Create cgOpsum Functions and Methods

#' @importFrom methods findFunction show
#' @importFrom dplyr %>%
#' @importFrom plyr ldply
#'
NULL

#' Converts a character textfile to semi-structured list by numbered paragraphs.
#'
#' @param dat A character vector.
#' @return A list of class cgOpsum
#' @keywords internal
splitParagraph <- function (dat) {
  tmp <- dat
  tmp <- unlist(strsplit(tmp, "BT", fixed = TRUE))
  tmp[1] <- gsub("\n(INFO|TO|FM)", "\n0. \\1:", tmp[1])
  hasDTG <- grepl("[0-9]{6}[A-Z] ?[A-Z]{3}", tmp[1])
  tmp <- paste(tmp, collapse = "BT")
  hasSubj <- grepl("SUBJ:", tmp)
  tmp <- gsub("SUBJ: ([[:print:]]+)\n", "0. SUBJ: \\1\n", tmp)
  if (!hasSubj) tmp <- gsub("UNCLAS\n","0. PERIOD COVERED:", tmp)
  if (!hasSubj|!hasDTG) tmp <- gsub("^FM","0. FM:", tmp)
  tmp <- gsub("([0-9]+[.] [A-Za-z -]+[:])", "#PAR\\1#PAR", tmp)
  tmp <- unlist(strsplit(tmp, "#PAR"))
  ind <- seq(from = 1, to = length(tmp), by = 2)
  nms <- gsub("[0-9]+. ([A-Za-z ]+):", "\\1", tmp[ind-1])
  tmp <- tmp[ind]
  names(tmp) <- c("MSG DTG", nms)
  tmp <- trimws(tmp)
  tmp <- gsub("\n\n", "\n", tmp)
  tmp <- gsub("[\t]+", "\t", tmp)
  tmp <- strsplit(tmp, "\n")
  tmp <- sapply(tmp, trimws)
  class(tmp) <- "cgOpsum"
  attr(tmp, "text") <- dat
  tmp
}

#' Convert character vector of opsums to cgOpsum class
#'
#' @param x A character vector.
#' @return A list of class cgOpsum
#' @examples
#' file <- system.file("extdata", "07JAN15_OPSUM.txt", package = "opsumToolbox")
#' x <- readtext::readtext(file)
#' opsum <- cgOpsum(x$text)
#' @export
cgOpsum <- function (x) {
  if (!all(grepl("USCGC|SUM", x))) stop("File is not a USCG OPSUM")
  x <- lapply(x, splitParagraph)
  if (class(x) == "list") class(x) <- "cgOpsum.list" #x <- as.cgOpsum.list(x)
  return(x)
}

#' show Method for cgOpsum
#'
#' @param obj A cgOpsum object
#' @param i An integer vector indicating which opsum to display
#' @export
show.cgOpsum <- function (obj, i = 1L) {
  tmp <- c(A = "cgOpsum", `msg dtg` = obj$`MSG DTG`,
           `from` = paste0(obj$FM, "."),
           `SUBJ:` = obj$SUBJ)
  cat(paste0("[", i, "]"), paste(names(tmp), tmp, collapse = " "), "\n")
}

#' print Method for cgOpsum
#'
#' @param x A cgOpsum object.
#' @param ... further arguments passed to or from other methods.
#' @export
print.cgOpsum.list <- function (x, ...) {
  if (class(x) == "cgOpsum.list") {
    for (i in seq_along(x)) show.cgOpsum(x[[i]], i)
  } else {
    cat(attr(x, "text"))
  }
}

#' subset Method for cgOpsum.list
#' @param x A cgOpsum.list object.
#' @param subset A vector of names or indices to subset.
#' @param ... further arguments passed to or from other methods.
#' @return A cgOpsum.list object
#' @export
`[.cgOpsum.list` <- function (x, subset, ...) {
  out <- `[`(x, subset, ... = ...)
  class(out) <- "cgOpsum.list"
  out
}

#' slice Method for cgOpsum.list
#' @param object A cgOpsum.list object.
#' @param slice A vector of names or indices to subset
#' @return A list
#' @export
slice.cgOpsum.list <- function (object, slice) {
  lapply(object, `[`, slice)
}
