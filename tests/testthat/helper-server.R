Response <- R6::R6Class(
  "Response",
  public = list(
    status = NULL,
    headers = list(),
    body = NULL,
    send = function(body = NULL) {
      list(
        status = self$status,
        headers = self$headers,
        body = body
      )
    },
    json = function(body = NULL) {
      if (!length(body)) {
        body <- setNames(list(), character(0))
      }
      self$send(yyjsonr::write_json_str(
        body,
        opts = yyjsonr::opts_write_json(auto_unbox = TRUE)
      ))
    }
  )
)

createServer <- function(secret = NULL) {
  router <- routing::Router$new()
  router$use(cookieParser(secret))
  router$get("/", \(req, res) {
    res$status <- 200L
    res$json(req$cookies)
  })
  router$get("/signed", \(req, res) {
    res$status <- 200L
    res$json(req$signedCookies)
  })

  httpuv::startServer(
    "127.0.0.1",
    httpuv::randomPort(),
    list(
      call = function(req) {
        res <- Response$new()
        router$handle(req, res, routing::finalHandler(req, res))
      }
    )
  )
}

fetch <- function(server, path, method = "GET", headers = NULL) {
  url <- paste0("http://127.0.0.1:", server$getPort(), path)
  handle <- curl::new_handle(customrequest = toupper(method))
  if (!is.null(headers)) {
    curl::handle_setheaders(handle, .list = headers)
  }
  pool <- curl::new_pool()
  result <- NULL
  error <- NULL

  curl::curl_fetch_multi(
    url,
    done = function(r) result <<- r,
    fail = function(e) error <<- e,
    handle = handle,
    pool = pool
  )

  while (is.null(result) && is.null(error)) {
    later::run_now(0)
    curl::multi_run(timeout = 0.01, pool = pool)
  }

  if (!is.null(error)) {
    stop(error)
  }

  list(
    status = result$status_code,
    body = rawToChar(result$content),
    headers = curl::parse_headers_list(result$headers)
  )
}
