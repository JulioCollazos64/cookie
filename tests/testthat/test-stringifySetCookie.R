describe("stringifySetCookie", {
  it("should serialize name and value", {
    expect_identical(
      stringifySetCookie("foo", "bar"),
      "foo=bar"
    )
  })

  it("should URL-encode value", {
    expect_identical(
      stringifySetCookie("foo", "bar +baz"),
      "foo=bar%20%2Bbaz"
    )
  })

  it("should serialize empty value", {
    expect_identical(
      stringifySetCookie("foo", ""),
      "foo="
    )
  })

  it("should serialize a list", {
    expect_identical(
      stringifySetCookie(
        list(name = "foo", value = "bar +baz")
      ),
      "foo=bar%20%2Bbaz"
    )
  })

  it("should serialize a list with options", {
    expect_identical(
      stringifySetCookie(
        list(name = "foo", value = "bar+baz"),
        encode = identity
      ),
      "foo=bar+baz"
    )
  })

  names <- c(
    "foo",
    "foo,bar",
    "foo!bar",
    "foo#bar",
    "foo$bar",
    "foo'bar",
    "foo*bar",
    "foo+bar",
    "foo-bar",
    "foo.bar",
    "foo^bar",
    "foo_bar",
    "foo`bar",
    "foo|bar",
    "foo~bar",
    "foo7bar",
    "foo/bar",
    "foo@bar",
    "foo[bar",
    "foo]bar",
    "foo:bar",
    "foo{bar",
    "foo}bar",
    'foo"bar',
    "foo<bar",
    "foo>bar",
    "foo?bar",
    "foo\\bar"
  )

  for (name in names) {
    it(sprintf("should serialize name: %s", name), {
      expect_identical(
        stringifySetCookie(name, "baz"),
        paste0(name, "=baz")
      )
    })
  }

  names <- c(
    "foo\n",
    "foo\u280a",
    "foo=bar",
    "foo;bar",
    "foo bar",
    "foo\tbar"
  )
  for (name in names) {
    it(sprintf("should throw for invalid name: %s", name), {
      expect_error(
        stringifySetCookie(name, "bar"),
        regexp = "Argument name is invalid"
      )
    })
  }

  describe('with "domain" option', {
    domains <- c(
      "examples.com",
      "sub.example.com",
      ".example.com",
      "localhost",
      ".localhost",
      "my-site.org"
    )

    for (domain in domains) {
      it(sprintf("should serialize domain: %s", domain), {
        expect_identical(
          stringifySetCookie("foo", "bar", domain = domain),
          sprintf('foo=bar; Domain=%s', domain)
        )
      })
    }

    domains <- c(
      "example.com\n",
      # "sub.example.com\u0000", Doesn't work in R...
      "my site.org",
      "domain..com",
      "example.com; Path=/",
      'example.com /* inject a comment */'
    )

    for (domain in domains) {
      it(sprintf("should throw for invalid domain: %s", domain), {
        expect_error(stringifySetCookie("foo", "bar", domain = domain))
      })
    }
  })

  describe('with "expires" option', {
    it("should throw on invalid date", {
      expect_error(stringifySetCookie("foo", "bar", expires = as.Date(NA)))
    })

    it("should set expires to given date", {
      expect_identical(
        stringifySetCookie(
          "foo",
          "bar",
          expires = as.POSIXct("2000-12-24 10:30:59", tz = "UTC")
        ),
        "foo=bar; Expires=Sun, 24 Dec 2000 10:30:59 GMT"
      )
    })
  })

  describe('with "httpOnly" option', {
    it("should include httpOnly flag when true", {
      expect_identical(
        stringifySetCookie("foo", "bar", httpOnly = TRUE),
        "foo=bar; HttpOnly"
      )
    })

    it("should not include httpOnly flag when false", {
      expect_identical(
        stringifySetCookie("foo", "bar", httpOnly = FALSE),
        "foo=bar"
      )
    })
  })

  describe('with "maxAge" option', {
    it("should throw when not a number", {
      expect_error(stringifySetCookie("foo", "bar", maxAge = "buzz"))
    })

    it("should throw when Infinity", {
      expect_error(stringifySetCookie("foo", "bar", maxAge = Inf))
    })

    it("should throw when max-age is not an integer", {
      expect_error(stringifySetCookie("foo", "bar", maxAge = 3.14))
    })

    it("should set max-age to value", {
      expect_identical(
        stringifySetCookie("foo", "bar", maxAge = 1000L),
        "foo=bar; Max-Age=1000"
      )

      expect_identical(
        stringifySetCookie("foo", "bar", maxAge = 0L),
        "foo=bar; Max-Age=0"
      )
    })

    it("should not set when undefined", {
      expect_identical(
        stringifySetCookie("foo", "bar", maxAge = NULL),
        "foo=bar"
      )
    })
  })

  describe('with "partitioned" option', {
    it("should include partitioned flag when true", {
      expect_identical(
        stringifySetCookie("foo", "bar", partitioned = TRUE),
        "foo=bar; Partitioned"
      )
    })

    it("should not include partitioned flag when false", {
      expect_identical(
        stringifySetCookie("foo", "bar", partitioned = FALSE),
        "foo=bar"
      )
    })

    it("should not include partitioned flag when not defined", {
      expect_identical(
        stringifySetCookie("foo", "bar"),
        "foo=bar"
      )
    })
  })

  describe('with "path" option', {
    it("should serialize path", {
      validPaths <- c(
        "/",
        "/login",
        "/foo.bar/baz",
        "/foo-bar",
        "/foo=bar?baz",
        '/foo"bar"',
        "/../foo/bar",
        "../foo/",
        "./"
      )

      for (path in validPaths) {
        expect_identical(
          stringifySetCookie("foo", "bar", path = path),
          paste0("foo=bar; Path=", path)
        )
      }
    })

    it("should throw for invalid value", {
      invalidPaths <- c(
        "/\n",
        # "/foo\u0000",
        "/path/with\rnewline",
        "/; Path=/sensitive-data",
        '/login"><script>alert(1)</script>'
      )

      for (path in invalidPaths) {
        expect_error(stringifySetCookie("foo", "bar", path = path))
      }
    })
  })

  describe('with "priority" option', {
    it("should throw on invalid priority", {
      expect_error(
        stringifySetCookie("foo", "bar", priority = "foo"),
        regexp = "option priority is invalid"
      )
    })

    it("should throw on non-string", {
      expect_error(
        stringifySetCookie("foo", "bar", priority = 42),
        regexp = "option priority is invalid"
      )
    })

    it("should set priority low", {
      expect_identical(
        stringifySetCookie("foo", "bar", priority = "low"),
        "foo=bar; Priority=Low"
      )
    })

    it("should set priority medium", {
      expect_identical(
        stringifySetCookie("foo", "bar", priority = "medium"),
        "foo=bar; Priority=Medium"
      )
    })

    it("should set priority high", {
      expect_identical(
        stringifySetCookie("foo", "bar", priority = "high"),
        "foo=bar; Priority=High"
      )
    })

    it("should set priority case insensitive", {
      expect_identical(
        stringifySetCookie("foo", "bar", priority = "High"),
        "foo=bar; Priority=High"
      )
    })
  })

  describe('with "sameSite" option', {
    it("should throw on invalid sameSite", {
      expect_error(stringifySetCookie("foo", "bar", sameSite = "foo"))
    })

    it("should set sameSite strict", {
      expect_identical(
        stringifySetCookie("foo", "bar", sameSite = "strict"),
        "foo=bar; SameSite=Strict"
      )
    })

    it("should set sameSite lax", {
      expect_identical(
        stringifySetCookie("foo", "bar", sameSite = "lax"),
        "foo=bar; SameSite=Lax"
      )
    })

    it("should set sameSite none", {
      expect_identical(
        stringifySetCookie("foo", "bar", sameSite = "none"),
        "foo=bar; SameSite=None"
      )
    })

    it("should set sameSite strict when true", {
      expect_identical(
        stringifySetCookie("foo", "bar", sameSite = TRUE),
        "foo=bar; SameSite=Strict"
      )
    })

    it("should not set sameSite strict when false", {
      expect_identical(
        stringifySetCookie("foo", "bar", sameSite = FALSE),
        "foo=bar"
      )
    })

    it("should set sameSite case insensitive", {
      expect_identical(
        stringifySetCookie("foo", "bar", sameSite = "Lax"),
        "foo=bar; SameSite=Lax"
      )
    })
  })

  describe('with "secure" option', {
    it("should include secure flag when true", {
      expect_identical(
        stringifySetCookie("foo", "bar", secure = TRUE),
        "foo=bar; Secure"
      )
    })

    it("should not include secure flag when false", {
      expect_identical(
        stringifySetCookie("foo", "bar", secure = FALSE),
        "foo=bar"
      )
    })
  })
})
