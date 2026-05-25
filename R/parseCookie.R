#' Parse a Cookie header
#' @export
parseCookie <- function(str, options = NULL) {
  if (is.null(options)) {
    options <- decode
  }

  obj <- list()

  len <- nchar(str)

  # RFC 6265 sec 4.1.1, RFC 2616 2.2 defines a cookie name consists of one char minimum, plus '='.
  if (len < 2) {
    return(obj)
  }

  dec <- options
  index <- 1
  while (index < len) {
    eqIdx <- eqIndex(str, index, len)
    if (eqIdx == -1) {
      break
    }

    endIdx <- endIndex(str, index, len)

    if (eqIdx > endIdx) {
      # backtrack on prior semicolon
      m <- gregexpr(";", substring(str, 1, eqIdx))[[1]]
      index <- m[length(m)] + 1
      next
    }

    key <- valueSlice(str, index, eqIdx)

    # only assign once
    if (is.null(obj[[key]])) {
      obj[[key]] <- dec(valueSlice(str, eqIdx + 1, endIdx))
    }
    index <- endIdx + 1
  }

  obj
}


indexOf <- function(str, pattern, min = 1) {
  stopifnot(length(str) == 1)
  m <- gregexpr(pattern, str)[[1]]
  m <- m[m >= min]
  if (!length(m)) -1L else m[1]
}

endIndex <- function(str, min, len) {
  index <- indexOf(str, ";", min)
  # R also represent -1 as no pattern match!
  if (index == -1L) len + 1L else index
}

eqIndex <- function(str, min, max) {
  index <- indexOf(str, "=", min)
  if (index <= max) index else -1L
}

valueSlice <- function(str, min, max) {
  if (min == max) {
    return("")
  }
  start <- min
  end <- max

  while (start < end) {
    # ascii code
    code <- utf8ToInt(substring(str, start, start))
    if (code != 32 && code != 9) {
      break
    }
    start <- start + 1
  }

  while (end > start) {
    code <- utf8ToInt(substring(str, end - 1L, end - 1L))
    if (code != 32 && code != 9) {
      break
    }
    end <- end - 1L
  }

  substring(str, start, end - 1L)
}

decode <- function(str) {
  if (regexpr("%", str) == -1) {
    return(str)
  }

  # As far as I know this function doesn't return an error
  utils::URLdecode(str)
}
