# Create Opsum Data Extractor Functions

#' Default Function to convert a  to .
#'
#' @param x A character vector.
#' @return Named character vector
#' @keywords internal
#' @export
getDTG <- function (x) {
  if (!any(grepl("[0-9 ][A-Z]+ [A-Z]{3} ?[0-9]{2}", x))) return(x)
  x <- gsub("(ARR|EDT)","\\1:", x)
  x <- gsub("([0-9]):([0-9])", "\\1-\\2", x)
  x <- gsub("^(.*[0-9]) +TO +([0-9].*)$","START: \\1 : END: \\2", x)
  x <- gsub("^.*POSIT:", "POSIT:", x)
  x <- lapply(strsplit(x, ":"), trimws)
  x <- unlist(x)
  x[x=="POSIT"] <- paste0("POSIT", 1:(sum(x=="POSIT")))
  x <- as.vector(x)
  y <- x[seq(1, length(x), by = 2)]
  x <- x[seq(2, length(x), by = 2)]
  names(x) <- y
  x
}


#' Default Function to convert a  to .
#'
#' @param x A character vector.
#' @return Named character vector
#' @keywords internal
#' @export
getBinary <- function (x) {
  x <- trimws(strsplit(x, ":"))
  x <- sapply(x, function (y) {
    if (length(y) == 2) {
      z <- y[1]
      y <- y[2]
      names(y) <- z
    }
    y
  })
  x
}

#' Default Function to convert a  to .
#'
#' @param x A character vector.
#' @return Named character vector
#' @keywords internal
#' @export
getWeather <- function (x) {
  a <- "DESC"
  x <- gsub("ICE: [^)]*[)]:?", "ICE: ", x)
  x <- trimws(strsplit(x, ":"))
  if (is.list(x) & length(x[[1]])==1) x[[1]] <- c(a, x[[1]])
  x <- matrix(unlist(x), nrow = 2, byrow = FALSE)
  colnames(x) <- x[1,]
  x[-1,]
}

#' Default Function to convert a  to .
#'
#' @param x A character vector.
#' @return Named character vector
#' @keywords internal
#' @export
getComms <- function (x) {
  x <- strsplit(x, "[[:space:]]{2,}")
  y <- x[length(x)+-1:0]
  x <- x[sapply(x, length)!=1]
  x <- c(x, list(unlist(y)))
  y <- sapply(x, `[[`, 1)
  y <- gsub("^[^ ]* ([[:print:]]+):?$", "\\1", y)
  x <- sapply(x, `[[`, 2)
  names(x) <- y
  x
}

#' Default Function to convert a  to .
#'
#' @param x A character vector.
#' @return Named character vector
#' @keywords internal
#' @export
getLiquid <- function (x) {
  x <- strsplit(x, "[[:space:]]+")
  width <- max(sapply(x, length))
  x <- lapply(x, function (y) c(y, rep(NA, width-length(y))))
  z <- paste0(x[[1]][1], x[[1]][2:width])
  x <- lapply(x, function (y) {
    if (grepl("PERC", y[2]) & is.na(y[3])) {
      names(y)[2:width] <- "POTABLE WATER-PERCENT OF CAPACITY"
    } else {
      names(y)[2:width] <- paste(z, y[1], sep="-")
    }
    y[-1]
  })
  x <- unlist(x)
  x <- x[grepl("[0-9]", x)]
  x
}

#' Default Function to convert a  to .
#'
#' @param x A character vector.
#' @return Named character vector
#' @keywords internal
#' @export
getEngCas <- function (x) {
  y <- unlist(strsplit(x[1], "/"))
  x <- x[-1]
  z <- unlist(strsplit(paste(x[seq(2,6,by=2)], collapse = "   "), "[[:space:]]+"))
  names(z) <- unlist(strsplit(paste(x[seq(1,5,by=2)], collapse = "  "), "[[:space:]]+"))
  z <- strsplit(z, "/")
  z <- lapply(z, function (a) {
    names(a) <- paste0(names(a), y)
    a
  })
  unlist(z)
}

#' Default Function to convert a  to .
#'
#' @param x A character vector.
#' @return Named character vector
#' @keywords internal
#' @export
getSmallBoat <- function (x) {
  x <- gsub("([0-9()]{3})( .+)) {2,}", "\\1\\2), STATUS\\1: ", x)
  x <- gsub(") ", "): ", x)
  x <- lapply(strsplit(x, ",|: "), trimws)
  ct <- grep(" STATUS|HOURS|TIME", x) #which(sapply(x, length) == 1)
  labs <- unlist(c("SMALL BOAT STATUS", sapply(x[ct], function (y) y[1])))
  names(x) <- rep(labs, times = c(ct, length(x)+1) - c(1,ct))
  names(x) <- gsub(":", "", names(x))
  x <- x[which(sapply(x, length) != 1)]
  x <- sapply(x, function (y) {
    ind <- seq(2, length(y), by = 2)
    z <- y[ind]
    names(z) <- y[ind-1]
    z
  })
  unlist(x)
}

#' Default Function to convert a  to .
#'
#' @param x A character vector.
#' @return Named character vector
#' @keywords internal
#' @export
asText <- function (x) paste(x, collapse = "/n")

#' Default Function to convert a  to .
#'
#' @param x A character vector.
#' @return Named character vector
#' @keywords internal
#' @export
noChange <- function (x) x

#' Default Function to produce function map a  to .
#'
#' @param x A Named character vector.
#' @return Named character vector
#' @export
extractFuncs <- function (x = NULL) {
  return(c(`MSG DTG` = "asText",
    `FM` = "noChange",
    `TO` = "asText",
    `INFO` = "asText",
    `SUBJ` = "asText",
    `PERIOD COVERED` = "getDTG",
    `SMALL BOAT STATUS` = "getSmallBoat",
    `LIQUID LOAD` = "getLiquid",
    `CURRENT WX DESCRIPTION` = "asText",
    `MISSION CRITICAL CASUALTIES` = "asText",
    `ENGINEERING CASUALTIES` = "getEngCas",
    `PATROL EFFORTS` = "asText",
    `COMMS SYSTEM` = "getComms",
    `PATROL INTENTIONS` = "asText",
    `CO COMMENTS` = "asText",
    `POC` = "asText",
    `DATE DPT HP` = "noChange",
    `FOREIGN FISHING VESSELS SIGHTED` = "noChange",
    `FOREIGN VESSELS SIGHTED` = "noChange",
    `VESSEL SIGHTINGS` = "noChange",
    `VESSELS SIGHTED` = "noChange",
    x))
}

#' data frame Method for cgOpsum list
#'
#' @param object A cgOpsum.list object.
#' @export
data.frame.cgOpsum.list <- function (object) {
  #dat2 <- as.list(object@.Data)
  dat2 <- as.list(object)
  dat2 <- plyr::llply(dat2, function (x) {
    z <- names(x)
    #x <- as.list(x@.Data)
    funcs <- get("extractFuncs", findFunction("extractFuncs")[[1]])
    funcs <- funcs()[z]
    out <- Map(function (a, b) try(do.call(a, list(b))), a=funcs, b=x[])
    unlist(out)
  })
  dat2 <- plyr::ldply(dat2, rbind)
  dat2 <- apply(dat2, 2, as.character)
  dat2 <- as.data.frame(dat2, stringsAsFactors = FALSE)
}
