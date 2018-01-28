# Create cgOpsum Functions and Methods

#' @importFrom methods findFunction show
#' @importFrom dplyr %>% slice
#' @importFrom plyr ldply
#' @importFrom utils read.table
#'
NULL

#' Identify available start and stop paragraph heading keywords from a corpus
#'
#' @param x A corpus.
#' @return A character vector of paragraph names
#' @export
getStartStopPairs <- function (x) {
  docs <- x$documents # Get raw text of each document
  docs$texts <- toupper(docs$texts)
  byline <- sapply(docs$texts, strsplit, "\n")
  parname <- lapply(byline, grep,
                    pattern = "^[[:blank:]]*[0-9]{1,2}[.]?[A-Z]? [A-Z ,]+:",
                    value = TRUE)
  parname <- lapply(parname, gsub,
                    pattern = "^[0-9]{1,2}[.]? ([A-Z ,]+):.*",
                    replacement = "\\1")
  parname <- lapply(parname, function (x) paste(x, stop = c(x[-1], "BT"), sep = "|"))
  parname <- as.data.frame(table(unlist(parname)), stringAsFactors = FALSE)
  parname$pair <- as.character(parname$Var1)
  parname$start <- gsub("\\|.*$", "", parname$pair)
  parname$stop <- gsub("^[A-Z ]+\\|", "", parname$pair)
  parname <- parname[, c("start", "stop", "pair", "Freq")]
  return(parname)
}

#' Extract a paragraph of text from a corpus using start and end keywords.
#'
#' @param x A corpus
#' @param start A character vector length 1 for the section start keyword
#' @param stop A character vector length 1 for the section end keyword
#' @return A dataframe with doc_id and text columns
#' @export
extractParagraph <- function (x, start, stop) {
  # Add error handler if object not a corpus
    docs <- x$documents # Get raw text of each document
    docs$texts <- toupper(docs$texts)
    pre.text <- "\n[[:digit:][:space:].,:]+" # Padding for start/stop phrases
    post.text <- "[[:space:][:punct:]]+"
    textforsplit <- paste0(pre.text, toupper(start), post.text, "|",
                           pre.text, toupper(stop), post.text)
    # Split each document and extract the middle if both phrases appeared or NA
    info <- lapply(docs$text, strsplit, textforsplit)
    info <- lapply(info, function (x) ifelse(length(x[[1]])==3, x[[1]][2], NA))
    info <- unlist(info)
    info <- data.frame(text = info, doc_id = docs$doc_id, stringsAsFactors = FALSE)
    return(info)
}

#' Create a text dataframe from a corpus using multiple start and end keywords.
#'
#' @param mycorpus A corpus
#' @param pair A dataframe generated using the getStartStopPairs function
#' @return A dataframe
#' @export
mergeData <- function (mycorpus, pair) {
  x <- mapply(extractParagraph, start = pair$start, stop = pair$stop,
              MoreArgs = list(x = mycorpus), SIMPLIFY = FALSE)
  varnames <- paste(pair$start, pair$stop, sep = ",")
  x <- Map(function (y, varnames) {
    names(y)[1] <- varnames
    y
  }, y = x, varnames = varnames)
  mymerge <- function (x, y, by.x = "doc_id", by.y = "doc_id") {
    merge(x, y, by.x = by.x, by.y = by.y)
  }
  y <- Reduce(mymerge, x)
}

#' Extract List of labeled text from a Corpus
#'
#' @param mycorpus A corpus object
#' @param label A regular expression to pass to grep
#'
#' The function searches for data that matches the search pattern given by
#' label after splitting the corpus by lines of text.
#' @examples
#' file <- system.file("extdata", package = "opsumToolbox")
#' x <- readtext::readtext(file)
#' opsum <- quanteda::corpus(x$text)
#' quanteda::docnames(opsum) <- x$doc_id
#' y <- getLabeledData(opsum, label = "SUBJ:")
#' @export
getLabeledData <- function (mycorpus, label) {
  docs <- mycorpus$documents # Get raw text of each document
  docs$texts <- toupper(docs$texts)
  docsbyline <- strsplit(docs$texts, "\n")
  out <- lapply(docsbyline, function (x) {
    grep(pattern = label, x = x, ignore.case = TRUE, value = TRUE)
  })
  out <- gsub(pattern = paste0("^(.*)", label, "(.*)$"), replacement = "\\2",
              out)
  out <- lapply(out, trimws)
  names(out) <- docs$`_document`
  return(out)
}

#' Convert Text String Vector stored in a Table Format to a list
#'
#' @param x text string containing table like data for conversion using read.table
#' @param cols Integer representing number of columns in table, default value is 2
#' @param id vector of column indices, usually x$doc_id if converted from a corpus
#' @param mynames Vector of column names for converted tabular data
#' @export
textTableToList <- function (x, cols = 2, id = NULL, mynames = NULL) {
  if (is.na(x)) return (c(doc_id = id))
  y <- gsub("([A-Z0-9]):?[ \t]{2,}([^\n])|(: ) *", "\\1%\\2", x) #
  z <- t(read.table(text=y, sep="%", header = FALSE, fill = NA))
  #if (nrow(z) == 3 & nchar(paste(z[1,], collapse = "") > 20)) z <- z[-1,]
  if (cols <= 2) {
    myinfo <- z[2,]
    infonames <- trimws(z[1,])
    infonames <- gsub("^\\([0-9]+\\) |^[A-Z][.] ", "", infonames)
    myinfo <- sapply(myinfo, trimws)
  } else {
    rowsOfNAs <- function (z) apply(z, 1, function (x) all(is.na(x)))
    colsOfBlanks <- function (z) apply(z, 2, function (x) sum(x==""))
    z <- z[!rowsOfNAs(z), colsOfBlanks(z) < nrow(z)-1]
    z <- apply(z, 1, trimws)
    colnames(z) <- z[1,]
    rownames(z) <- z[,1]
    z <- z[-1,-1]
    infonames <- expand.grid(rownames(z), colnames(z))
    infonames <- paste(infonames$Var1, infonames$Var2, sep = ".")
    myinfo <- as.vector(z)
  }
  names(myinfo) <- infonames
  myinfo <- myinfo[myinfo != ""]
  if (!is.null(mynames)) names(myinfo) <- mynames[1:length(myinfo)]
  myinfo <- c(doc_id = id, myinfo)
  myinfo <- list(myinfo)
  return(myinfo)
}

#' Convert Text Strings Vector written in a Table Format to a new Data Frame
#'
#' @param x A dataframe containing vectors of text strings and an index vector
#' @param colname Name of column containing table-like text strings
#' @param cols Integer representing number of columns in table, default value is 2
#' @param idcolname Name of column containing indices, usually doc_id if converted from a corpus
#' @param mynames Vector of column names for converted tabular data to pass to textTableToList function
#' @export
convertToDf <- function (x, colname, cols = 2, idcolname, mynames = NULL) {
  current.opt <- default.stringsAsFactors()
  options(stringsAsFactors = FALSE)
  y <- mapply(textTableToList, cols = cols, id = x[,idcolname],
              x = x[, colname],
              MoreArgs = list(mynames = mynames))
  out <- ldply(y, rbind, .id = NULL)
  options(stringsAsFactors = current.opt)
  return(out)
}

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
