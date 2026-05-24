stringifySetCookie <- function(name, val = NULL, ...) {
  opts <- list(...)

  if (is.list(name)) {
    val <- name$value
    name <- name$name
  }

  enc <- opts$encode %||% urlEncode

  val <- if (!nzchar(val)) {
    val
  } else {
    enc(val)
  }

  if (!grepl(cookieNameRegExp, name)) {
    stop("Argument name is invalid", call. = FALSE)
  }

  if (!grepl(cookieValueRegExp, val)) {
    stop("Argument val is invalid", call. = FALSE)
  }

  str <- paste0(name, "=", val)
  if (!length(opts)) {
    return(str)
  }

  if (!is.null(opts$maxAge)) {
    stopifnot(is.integer(opts$maxAge))
    str <- paste0(str, "; Max-Age=", opts$maxAge)
  }

  if (!is.null(opts$domain)) {
    stopifnot(grepl(domainValueRegExp, opts$domain))

    str <- paste0(str, "; Domain=", opts$domain)
  }

  if (!is.null(opts$path)) {
    stopifnot(grepl(pathValueRegExp, opts$path))

    str <- paste0(str, "; Path=", opts$path)
  }

  if (!is.null(opts$expires)) {
    stopifnot(
      # In R, date stuff are just double vectors with an additional class!
      is.finite(unclass(opts$expires)),
      inherits(opts$expires, c("Date", "POSIXct", "POSIXt"))
    )
    str <- paste0(str, "; Expires=", http_date_string(opts$expires))
  }

  if (isTRUE(opts$httpOnly)) {
    str <- paste0(str, "; HttpOnly")
  }

  if (isTRUE(opts$secure)) {
    str <- paste0(str, "; Secure")
  }

  if (isTRUE(opts$partitioned)) {
    str <- paste0(str, "; Partitioned")
  }

  if (!is.null(opts$priority)) {
    priority <- tolower(opts$priority)
    switch(
      priority,
      "low" = {
        str <- paste0(str, "; Priority=Low")
      },
      "medium" = {
        str <- paste0(str, "; Priority=Medium")
      },
      "high" = {
        str <- paste0(str, "; Priority=High")
      },
      stop("option priority is invalid: ", opts$priority, call. = FALSE)
    )
  }

  if (!is.null(opts$sameSite)) {
    sameSite <- opts$sameSite
    # If the value of EXPR is not a character string it is coerced to integer, from ?switch
    # so FALSE -> 0, no corresponding element, also we want to coerce  TRUE to
    # character.

    sameSite <- if (!isFALSE(sameSite)) tolower(sameSite) else sameSite

    switch(
      sameSite,
      "true" = ,
      "strict" = {
        str <- paste0(str, "; SameSite=Strict")
      },
      "lax" = {
        str <- paste0(str, "; SameSite=Lax")
      },
      "none" = {
        str <- paste0(str, "; SameSite=None")
      },

      stop("option sameSite is invalid: ", opts$sameSite, call. = FALSE)
    )
  }

  str
}
