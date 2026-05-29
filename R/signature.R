#' Sign
#' @examples
#' sign("hello","tobiiscool")
#' @export
sign <- function(val, secret) {
  stopifnot("Cookie value must be a string" = is.character(val))

  rand <- secretbase::sha256(val, key = secret, convert = FALSE)
  rand <- secretbase::base64enc(rand)
  rand <- sub("\\=+$", "", rand)
  paste0(val, ".", rand)
}

#' Unsign
#' @examples
#' input <- sign("hello","tobiiscool")
#' unsign(input,"tobiiscool")
#' unsign(input,"luna")
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
