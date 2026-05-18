describe("sign", {
  it("should sign the cookie", {
    val <- sign("hello", "tobiiscool")
    expect_identical(val, "hello.DGDUkGlIkCzPz+C0B064FNgHdEjox7ch8tOBGslZ5QI")

    val <- sign("hello", "luna")
    expect_false(identical(
      val,
      "hello.DGDUkGlIkCzPz+C0B064FNgHdEjox7ch8tOBGslZ5QI"
    ))
  })
  it("should accept appropiately non-string secrets", {
    key <- as.raw(c(0xA0, 0xAB, 0xBC, 0x0C))
    val <- sign("hello", key)
    expect_identical(val, 'hello.hIvljrKw5oOZtHHSq5u+MlL27cgnPKX77y7F+x5r1to')
  })
})

describe("unsign", {
  it("should unsign the cookie", {
    val <- sign("hello", "tobiiscool")

    expect_identical(unsign(val, "tobiiscool"), "hello")
    expect_false(unsign(val, "luna"))
  })

  it("should reject malformed cookies", {
    pwd <- "actual sekrit password"

    expect_false(unsign("fake unsigned data", pwd))

    val <- sign("real data", pwd)
    expect_false(unsign(paste0("garbage", val), pwd))
    expect_false(unsign(paste0("garbage.", val), pwd))
    expect_false(unsign(paste0(val, ".garbage"), pwd))
    expect_false(unsign(paste0(val, "garbage"), pwd))
  })

  it("should accept non-string secrets", {
    key <- as.raw(c(0xA0, 0xAB, 0xBC, 0x0C))
    val <- unsign('hello.hIvljrKw5oOZtHHSq5u+MlL27cgnPKX77y7F+x5r1to', key)
    expect_identical(val, "hello")
  })
})
