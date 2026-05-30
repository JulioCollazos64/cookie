#' Sign a Cookie Value
#'
#' Sign the given `val` with `secret`.
#'
#' @param val A string. The cookie value to sign.
#' @param secret The secret key used to generate the signature.
#'
#' @return A string of the form `"<val>.<signature>"`.
#'
#' @examples
#' sign("hello", "tobiiscool")
#'
#' @export
sign <- function(val, secret) {
  stopifnot("Cookie value must be a string" = is.character(val))

  rand <- secretbase::sha256(val, key = secret, convert = FALSE)
  rand <- secretbase::base64enc(rand)
  rand <- sub("\\=+$", "", rand)
  paste0(val, ".", rand)
}

#' Unsign a Cookie Value
#'
#' Verifies the signature of a signed cookie value and returns the original
#' value if valid, or `FALSE` if the signature does not match.
#'
#' @param input A string. A signed cookie value produced by [sign()].
#' @param secret The secret key to verify against.
#'
#' @return The original unsigned value if verification succeeds, `FALSE`
#'   otherwise.
#'
#' @examples
#' input <- sign("hello", "tobiiscool")
#' unsign(input, "tobiiscool")
#' unsign(input, "luna")
#'
#' @export
unsign <- function(input, secret) {
  stopifnot("Signed cookie must be a string" = is.character(input))
  tentativeValue <- strsplit(input, split = ".", fixed = TRUE)[[1]][1]
  expectedInput <- sign(tentativeValue, secret)
  expectedBuffer <- charToRaw(expectedInput)
  inputBuffer <- charToRaw(input)

  return(
    if (
      length(expectedBuffer) == length(inputBuffer) &&
        identical(expectedBuffer, inputBuffer)
    ) {
      tentativeValue
    } else {
      FALSE
    }
  )
}
