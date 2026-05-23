'%||%' <- function(x, y) {
  if (is.null(x)) y else x
}

# Shamelessly copied from:
# https://github.com/jshttp/cookie/blob/e8db32e04f7aa5affff00724c8a28dd0d17fc397/src/index.ts#L2

cookieNameRegExp <- "^[\u0021-\u003A\u003C\u003E-\u007E]+$"

# https://github.com/jshttp/cookie/blob/e8db32e04f7aa5affff00724c8a28dd0d17fc397/src/index.ts#L18

cookieValueRegExp <- "^[\u0021-\u003A\u003C-\u007E]*$"

# https://github.com/jshttp/cookie/blob/e8db32e04f7aa5affff00724c8a28dd0d17fc397/src/index.ts#L32

domainValueRegExp <- "^(?i)([.]?[a-z0-9]([a-z0-9-]{0,61}[a-z0-9])?)([.][a-z0-9]([a-z0-9-]{0,61}[a-z0-9])?)*$"

# https://github.com/jshttp/cookie/blob/e8db32e04f7aa5affff00724c8a28dd0d17fc397/src/index.ts#L58

pathValueRegExp <- "^[\u0020-\u003A\u003D-\u007E]*$"
