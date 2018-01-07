# Defines the DTG class and defines function to convert to POSIXct time

#' Change character string DTG to an intermediate object.
#'
#' @param x A character vector.
#' @return A list with values and DTG character representation
#' @keywords internal
splitDTG <- function (x) {
  y <- gsub("([0-3][0-9]) ?([0-2][0-9][0-5][0-9])([A-Z]+) ?([A-Z]{3}) ?([0-9]{2,})",
            "\\1,\\2,\\3,\\4,\\5", x)
  y <- unlist(strsplit(y, ","))
  x <- list(day = y[1], time4 = y[2], tz = y[3], month = y[4],
            year = y[5])
  return (list(values = x,
               DTG = paste0(x$day, x$time4, x$tz, " ", x$month, " ", x$year)))
}

#' Checks validity of DTG object.
#'
#' @param object A suspected DTG object.
#' @return A logical vector
#' @keywords internal
validDTG <- function (object) {
  object <- splitDTG(object)$values
  return (!(any(as.integer(object$day) > 31,
                as.integer(object$day) < 1,
                as.integer(object$time4) < 1,
                as.integer(object$time4) > 2359,
                !(object$tz %in% LETTERS[-10]),
                !(object$month %in% toupper(month.abb)))))
}

#' Change character vector to a DTG vector.
#'
#' @param x A character vector.
#' @return A DTG object
#' @examples
#' DTG("220815ZDEC17")
#' DTG(c(dtg1 = "181918BDEC17", dtg2 = "182318BDEC17"))
#' \dontrun{
#' DTG("182918BDEC17") # Returns Warning
#' }
#'
#' @export
DTG <- function (x) {
  out <- sapply(x, function (y) {
    if (!validDTG(y)) {
      warning (paste(y, "is not a valid DTG"), immediate. = TRUE)
      return (NA)
    }
    splitDTG(y)$DTG
  }, simplify = TRUE)
  class(out) <- "DTG"
  out
}

#' Show Method for DTG
#' @param x A DTG object.
#' @export
show.DTG <- function (x) {
  cat(x@.Data, "\n")
}

#' Convert DTG class object to POSIXct class object
#'
#' @param x A DTG object.
#' @param tz A string representing timezone, e.g. "EST"
#' @param ... further arguments passed to or from other methods.
#' @examples
#' x <- DTG("220815ZDEC17")
#' as.POSIXct(x)
#' as.POSIXct(x, tz = "PDT")
#' @export
as.POSIXct.DTG <- function (x, tz = "", ...) {
  x <- lapply(x, splitDTG)
  convertDTG <- function (y) {
    y <- y$values
    if (is.na(y$day)) return (NA)
    tzs <- c(1:12,1*1:12,0)
    #tzs <- sapply(tzs, sprintf, fmt="%04d")
    tzs <- paste0("Etc/GMT", c(rep("+",12),rep("-",12),"+"), tzs)
    names(tzs) <- LETTERS[-10]
    .d <- y$day
    .H.M <- y$time4
    .z <- tzs[y$tz]
    .m <- grep(y$month, toupper(month.abb))
    .Y <- if (as.integer(y$year) < 100) 2000+as.integer(y$year)
    y1 <- paste(paste(c(.Y, .m, .d), collapse = "-"),
                sub("(..)(..)", "\\1:\\2", .H.M), .z)
    y1 <- as.POSIXct(y1, "%Y-%m-%d %H:%M", tz = .z)
    y1
  }
  as.POSIXct(sapply(x, convertDTG), origin = "1970-01-01 00:00.00 UTC", tz = tz, ... = ...)
}
