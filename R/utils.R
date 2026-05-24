'%||%' <- function(x, y) {
  if (is.null(x)) y else x
}

urlEncode <- function(URL) {
  utils::URLencode(URL, reserved = TRUE)
}

# Shamelessly copied from:
# https://github.com/rstudio/httpuv/blob/main/tests/testthat/helper-app.R#L131

http_date_string <- function(time) {
  weekday_names <- c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")
  weekday_num <- as.integer(strftime(time, format = "%w", tz = "GMT"))
  weekday_name <- weekday_names[weekday_num + 1]

  month_names <- c(
    "Jan",
    "Feb",
    "Mar",
    "Apr",
    "May",
    "Jun",
    "Jul",
    "Aug",
    "Sep",
    "Oct",
    "Nov",
    "Dec"
  )
  month_num <- as.integer(strftime(time, format = "%m", tz = "GMT"))
  month_name <- month_names[month_num]

  strftime(
    time,
    paste0(weekday_name, ", %d ", month_name, " %Y %H:%M:%S GMT"),
    tz = "GMT"
  )
}


# https://github.com/jshttp/cookie/blob/e8db32e04f7aa5affff00724c8a28dd0d17fc397/src/index.ts#L2

cookieNameRegExp <- "^[\u0021-\u003A\u003C\u003E-\u007E]+$"

# https://github.com/jshttp/cookie/blob/e8db32e04f7aa5affff00724c8a28dd0d17fc397/src/index.ts#L18

cookieValueRegExp <- "^[\u0021-\u003A\u003C-\u007E]*$"

# https://github.com/jshttp/cookie/blob/e8db32e04f7aa5affff00724c8a28dd0d17fc397/src/index.ts#L32

domainValueRegExp <- "^(?i)([.]?[a-z0-9]([a-z0-9-]{0,61}[a-z0-9])?)([.][a-z0-9]([a-z0-9-]{0,61}[a-z0-9])?)*$"

# https://github.com/jshttp/cookie/blob/e8db32e04f7aa5affff00724c8a28dd0d17fc397/src/index.ts#L58

pathValueRegExp <- "^[\u0020-\u003A\u003D-\u007E]*$"
