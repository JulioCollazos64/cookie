#' Cookie Parser Middleware
#'
#' Creates a middleware function that parses the `Cookie` HTTP header and
#' populates `req$cookies`. When a `secret` is provided, signed cookies are
#' verified and exposed on `req$signedCookies`.
#'
#' @param secret A character vector or list of strings used to sign
#'   and verify cookies. Optional.
#' @param options A function used to decode cookie values. Defaults to
#'   [utils::URLdecode()].
#'
#' @return A middleware function that sets `req$cookies`,
#'   `req$signedCookies`, and `req$secret`, then calls `forward()`.
#'
#' @export
cookieParser <- function(secret = NULL, options = NULL) {
  secrets <- as.list(secret %||% list())

  function(req, res) {
    if (!is.null(req$cookies)) {
      return(forward())
    }

    cookies <- req$HTTP_COOKIE

    req$secret <- secret[[1]]
    req$cookies <- list()
    req$signedCookies <- list()

    if (is.null(cookies)) {
      return(forward())
    }

    req$cookies <- parseCookie(cookies, options)

    if (length(secrets)) {
      req$signedCookies <- signedCookies(req$cookies, secrets)
      req$cookies[names(req$signedCookies)] <- NULL
    }

    forward()
  }
}


signedCookie <- function(str, secrets) {
  if (!is.character(str)) {
    return(NULL)
  }

  if (substr(str, 1, 2) != "s:") {
    return(str)
  }

  for (secret in secrets) {
    val <- unsign(substr(str, 3, nchar(str)), secret)

    if (!isFALSE(val)) {
      return(val)
    }
  }

  FALSE
}


signedCookies <- function(obj, secret) {
  cookies <- names(obj)
  ret <- list()

  for (key in cookies) {
    val <- obj[[key]]
    dec <- signedCookie(val, secret)

    if (!identical(val, dec)) {
      ret[[key]] <- dec
    }
  }

  ret
}


# To keep R CMD check happy
utils::globalVariables(
  "forward"
)
