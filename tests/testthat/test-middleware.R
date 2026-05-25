describe("cookieParser()", {
  on.exit(
    {
      httpuv::stopAllServers()
    },
    add = TRUE
  )

  describe("when no cookies are sent", {
    it("should default req$cookies to {}", {
      r <- fetch(createServer("keyboard cat"), "/")

      expect_identical(
        list(
          r$status,
          r$body
        ),
        list(
          200L,
          "{}"
        )
      )
    })

    it("should default req$signedCookies to {}", {
      r <- fetch(createServer("keyboard cat"), "/signed")

      expect_identical(
        list(
          r$status,
          r$body
        ),
        list(
          200L,
          "{}"
        )
      )
    })
  })

  describe("when cookies are sent", {
    it("should populate req$cookies", {
      r <- fetch(
        createServer("keyboard cat"),
        path = "/",
        headers = list(
          "Cookie" = "foo=bar; bar=baz"
        )
      )

      expect_identical(
        list(
          r$status,
          r$body
        ),
        list(
          200L,
          '{"foo":"bar","bar":"baz"}'
        )
      )
    })
  })

  describe("when a secret is given", {
    val <- sign("foobarbaz", "keyboard cat")

    it("should populate req$signedCookies", {
      r <- fetch(
        createServer("keyboard cat"),
        path = "/signed",
        headers = list(
          "Cookie" = paste0("foo=s:", val)
        )
      )

      expect_identical(
        list(
          r$status,
          r$body
        ),
        list(
          200L,
          '{"foo":"foobarbaz"}'
        )
      )
    })

    it("should remove the signed value from req$cookies", {
      r <- fetch(
        createServer("keyboard cat"),
        path = "/",
        headers = list(
          "Cookie" = paste0("foo=s:", val)
        )
      )

      expect_identical(
        list(
          r$status,
          r$body
        ),
        list(
          200L,
          "{}"
        )
      )
    })

    it("should omit invalid signature", {
      server <- createServer("keyboard cat")

      r <- fetch(
        server,
        path = "/signed",
        headers = list(
          "Cookie" = paste0("foo=", val, "3")
        )
      )

      expect_identical(
        list(
          r$status,
          r$body
        ),
        list(
          200L,
          "{}"
        )
      )

      r <- fetch(
        server,
        path = "/",
        headers = list(
          "Cookie" = paste0("foo=", val, "3")
        )
      )

      expect_identical(
        list(
          r$status,
          r$body
        ),
        list(
          200L,
          '{"foo":"foobarbaz.CP7AWaXDfAKIRfH49dQzKJx7sKzzSoPq7/AcBBRVwlI3"}'
        )
      )
    })
  })

  describe("when multiple secrets are given", {
    it("should populate req$signedCookies", {
      r <- fetch(
        createServer(list("keyboard cat", "nyan cat")),
        "/signed",
        headers = list(
          Cookie = 'buzz=s:foobar.N5r0C3M8W+IPpzyAJaIddMWbTGfDSO+bfKlZErJ+MeE; fizz=s:foobar.JTCAgiMWsnuZpN3mrYnEUjXlGxmDi4POCBnWbRxse88'
        )
      )

      expect_identical(
        list(
          r$status,
          r$body
        ),
        list(
          200L,
          '{"buzz":"foobar","fizz":"foobar"}'
        )
      )
    })
  })

  describe("when no secret is given", {
    server <- createServer()

    it("should populate req$cookies", {
      r <- fetch(
        server,
        "/",
        headers = list(
          Cookie = 'foo=bar; bar=baz'
        )
      )

      expect_identical(
        list(
          r$status,
          r$body
        ),
        list(
          200L,
          '{"foo":"bar","bar":"baz"}'
        )
      )
    })

    it("should not populate req$signedCookies", {
      val <- sign("foobarbaz", "keyboard cat")

      r <- fetch(
        server,
        "/signed",
        headers = list(
          Cookie = paste0("foo=s:", val)
        )
      )

      expect_identical(
        list(
          r$status,
          r$body
        ),
        list(
          200L,
          "{}"
        )
      )
    })
  })

  describe("signedCookie", {
    it("should return NULL for non-string arguments", {
      expect_null(signedCookie(NULL, "keyboard cat"))
      expect_null(signedCookie(42, "keyboard cat"))
      expect_null(signedCookie(list(), "keyboard cat"))
      expect_null(signedCookie(function(x) {}, "keyboard cat"))
    })

    it("should pass through non-signed strings", {
      expect_identical(signedCookie("", "keyboard cat"), "")
      expect_identical(signedCookie("foo", "keyboard cat"), "foo")
    })

    it("should return FALSE for tampered signed strings", {
      expect_false(
        signedCookie(
          's:foobaz.N5r0C3M8W+IPpzyAJaIddMWbTGfDSO+bfKlZErJ+MeE',
          "keyboard cat"
        )
      )
    })

    it("should return unsigned value for signed strings", {
      expect_identical(
        signedCookie(
          's:foobar.N5r0C3M8W+IPpzyAJaIddMWbTGfDSO+bfKlZErJ+MeE',
          "keyboard cat"
        ),
        "foobar"
      )
    })

    describe("when secret is a list", {
      it("should return FALSE for tampered signed string", {
        expect_false(
          signedCookie(
            's:foobaz.N5r0C3M8W+IPpzyAJaIddMWbTGfDSO+bfKlZErJ+MeE',
            list(
              "keyboard cat",
              "nyan cat"
            )
          )
        )
      })

      it("should return unsigned value for first secret", {
        expect_identical(
          signedCookie(
            's:foobar.N5r0C3M8W+IPpzyAJaIddMWbTGfDSO+bfKlZErJ+MeE',
            list(
              "keyboard cat",
              "nyan cat"
            )
          ),
          "foobar"
        )
      })

      it("should return unsigned value for second secret", {
        expect_identical(
          signedCookie(
            's:foobar.JTCAgiMWsnuZpN3mrYnEUjXlGxmDi4POCBnWbRxse88',
            list(
              "keyboard cat",
              "nyan cat"
            )
          ),
          "foobar"
        )
      })
    })
  })

  describe("signedCookies(obj, secret)", {
    it("should ignore non-signed strings", {
      expect_identical(signedCookies(list(), "keyboard cat"), list())
      expect_identical(signedCookies(list(foo = "bar")), list())
    })

    it("should include tampered strings as false", {
      expect_identical(
        signedCookies(
          list(
            foo = 's:foobaz.N5r0C3M8W+IPpzyAJaIddMWbTGfDSO+bfKlZErJ+MeE'
          ),
          "keyboard cat"
        ),
        list(
          foo = FALSE
        )
      )
    })

    it("should include unsigned strings", {
      expect_identical(
        signedCookies(
          list(
            foo = 's:foobar.N5r0C3M8W+IPpzyAJaIddMWbTGfDSO+bfKlZErJ+MeE'
          ),
          "keyboard cat"
        ),
        list(
          foo = "foobar"
        )
      )
    })

    describe("when secret is a list", {
      it("should include unsigned strings for matching secrets", {
        obj <- list(
          buzz = 's:foobar.N5r0C3M8W+IPpzyAJaIddMWbTGfDSO+bfKlZErJ+MeE',
          fizz = 's:foobar.JTCAgiMWsnuZpN3mrYnEUjXlGxmDi4POCBnWbRxse88'
        )

        expect_identical(
          signedCookies(obj, list("keyboard cat")),
          list(
            buzz = "foobar",
            fizz = FALSE
          )
        )
      })

      it("should include unsigned strings for all secrets", {
        obj <- list(
          buzz = 's:foobar.N5r0C3M8W+IPpzyAJaIddMWbTGfDSO+bfKlZErJ+MeE',
          fizz = 's:foobar.JTCAgiMWsnuZpN3mrYnEUjXlGxmDi4POCBnWbRxse88'
        )

        expect_identical(
          signedCookies(obj, list("keyboard cat", "nyan cat")),
          list(
            buzz = "foobar",
            fizz = "foobar"
          )
        )
      })
    })
  })
})
