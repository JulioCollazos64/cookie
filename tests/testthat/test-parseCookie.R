describe("parseCookie", {
  it("should parse cookie string to object", {
    expect_identical(
      parseCookie("foo=bar"),
      list(foo = "bar")
    )
    expect_identical(
      parseCookie("foo=123"),
      list(foo = "123")
    )
  })

  it("should ignore OWS", {
    expect_identical(
      parseCookie("FOO    = bar;   baz  =   raz"),
      list(
        FOO = "bar",
        baz = "raz"
      )
    )
  })

  it("should return empty object", {
    expect_identical(
      parseCookie(""),
      list()
    )

    expect_identical(
      parseCookie(" \t "),
      list()
    )
  })

  it("should parse cookie with empty value", {
    expect_identical(
      parseCookie("foo=; bar="),
      list(
        foo = "",
        bar = ""
      )
    )
  })

  it("should parse cookie with minimum length", {
    expect_identical(
      parseCookie("f="),
      list(
        f = ""
      )
    )
    expect_identical(
      parseCookie("f=;b="),
      list(
        f = "",
        b = ""
      )
    )
  })

  it("should URL-decode value", {
    expect_identical(
      parseCookie('foo="bar=123456789&name=Magic+Mouse"'),
      list(
        foo = '"bar=123456789&name=Magic+Mouse"'
      )
    )

    expect_identical(
      parseCookie("email=%20%22%2c%3b%2f"),
      list(
        email = ' ",;/'
      )
    )
  })

  it("should trim whitespace around key and value", {
    expect_identical(
      parseCookie('  foo  =  "bar"  '),
      list(
        foo = '"bar"'
      )
    )

    expect_identical(
      parseCookie("  foo  =  bar  ;  fizz  =  buzz  "),
      list(
        foo = "bar",
        fizz = "buzz"
      )
    )

    expect_identical(
      parseCookie(' foo = " a b c " '),
      list(foo = '" a b c "')
    )

    # Not allowed in R
    # expect_identical(
    #   parseCookie(" = bar "), # This one should result in an eror I think
    # )

    expect_identical(
      parseCookie(" foo = "),
      list(foo = "")
    )

    # expect_identical(
    #   parseCookie(" = ") # Should return an error...
    # )

    expect_identical(
      parseCookie("\tfoo\t=\tbar\t"),
      list(
        foo = "bar"
      )
    )
  })

  # Doesn't work as utils::URLdecode doesn't return errors...

  # it("should return original value on escape error", {
  #   expect_identical(
  #     parseCookie("foo=%1;bar=bar"),
  #     list(
  #       foo = "%1",
  #       bar = "bar"
  #     )
  #   )
  # })

  it("should ignore cookie without value", {
    expect_identical(
      parseCookie("foo=bar; fizz  ; buzz"),
      list(
        foo = "bar"
      )
    )

    expect_identical(
      parseCookie("  fizz;foo= bar"),
      list(
        foo = "bar"
      )
    )
  })

  it("should ignore duplicate cookies", {
    expect_identical(
      parseCookie("foo=false;bar=bar;foo=true"),
      list(
        foo = "false",
        bar = "bar"
      )
    )

    expect_identical(
      parseCookie("foo=;bar=bar;foo=boo"),
      list(
        foo = "",
        bar = "bar"
      )
    )
  })

  it("should parse native properties", {
    expect_identical(
      parseCookie("toString=foo;valueOf=bar"),
      list(
        toString = "foo",
        valueOf = "bar"
      )
    )
  })
})
