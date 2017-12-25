# Defines the DTG class and defines function to convert to POSIXct time

#' An S4 class to represent a date-time-group (DTG)
#' @keywords internal
#' @export
DTG <- setClass("DTG", contains = "character",
                slots = c(".day",".time4",".tz",".month",".year"))

#' Change character string to a DTG object.
#'
#' @param x A character vector.
#' @return A DTG object
#' @examples
#' DTG("220815ZDEC17")
#' DTG("181918BDEC17")
#' \dontrun{
#' DTG("182918BDEC17") # Returns Error
#' }
#'
#' @export
DTG <- function (x) {
  y <- gsub("([0-3][0-9]) ?([0-2][0-9][0-5][0-9])([A-Z]+) ?([A-Z]{3}) ?([0-9]{2,})",
            "\\1,\\2,\\3,\\4,\\5", x)
  if (y==x) {
    warning ("Not a valid DTG")
    return(NA)
  }
  y <- unlist(strsplit(y, ","))
  x <- new("DTG", .Data = x, .day=y[1], .time4=y[2], .tz=y[3], .month=y[4],
           .year=y[5])
}

#' Checks validity of DTG object.
#'
#' @param object A suspected DTG object.
#' @return A logical vector
#' @keywords internal
validDTG <- function (object) {
  if (any(as.integer(object@.day) > 31,
          as.integer(object@.day) < 1,
          as.integer(object@.time4) < 1,
          as.integer(object@.time4) > 2359,
          !(object@.month %in% toupper(month.abb)))) {
    paste(object@.Data, "is not a valid DTG")
  } else {
    TRUE
  }
}

#' Sets validity function for DTG class.
#' @importFrom methods setValidity
#' @keywords internal
isvalidDTG <- setValidity("DTG", validDTG)

#' Print Method for DTG
#' @param x A DTG object.
#' @export
print.DTG <- function (x) {
  cat(x@.Data)
}

#' Show Method for DTG
#' @param x A DTG object.
#' @export
show.DTG <- function (x) {
  cat(x@.Data)
}

#' @export
setMethod("show", signature(object="DTG"), function (object) {
  show.DTG(object)
})


#' Convert DTG class object to POSIXct class object
#'
#' @param x A DTG object.
#' @examples
#' x <- DTG("220815ZDEC17")
#' as.POSIXct(x)
#' @export
as.POSIXct.DTG <- function (x) {
  tzs <- c(1:12,1*1:12,0)
  #tzs <- sapply(tzs, sprintf, fmt="%04d")
  tzs <- paste0("Etc/GMT", c(rep("+",12),rep("-",12),"+"), tzs)
  names(tzs) <- LETTERS[-10]
  .d <- x@.day
  .H.M <- x@.time4
  .z <- tzs[x@.tz]
  .m <- grep(x@.month, toupper(month.abb))
  .Y <- if (as.integer(x@.year) < 100) 2000+as.integer(x@.year)
  y1 <- paste(paste(c(.Y, .m, .d), collapse = "-"),
             sub("(..)(..)", "\\1:\\2", .H.M), .z)
  y1 <- as.POSIXct(y1, "%Y-%m-%d %H:%M", tz = .z)
  y1
}
