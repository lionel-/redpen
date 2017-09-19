context("node-match")

test_that("mend_ellipsis() supports corner cases", {
  expect_identical(mend_ellipsis(pairlist(quote(...))), list(TRUE, NULL))
  expect_identical(mend_ellipsis(pairlist(1, quote(...))), list(TRUE, pairlist(1)))
  expect_identical(mend_ellipsis(pairlist(quote(...), 1)), list(TRUE, pairlist(1)))
  expect_identical(mend_ellipsis(pairlist(1)), list(FALSE, pairlist(1)))
})

test_that("can match by position", {
  x <- quote(lang(foo, bar))

  expect_true(node_match(x, lang(foo, bar) := TRUE))
  expect_null(node_match(x, lang(bar, foo) := TRUE))

  expect_null(node_match(x, lang(foo) := TRUE))
  expect_null(node_match(x, lang(bar) := TRUE))

  expect_null(node_match(x, lang(bar, ...) := TRUE))
  expect_true(node_match(x, lang(foo, ...) := TRUE))
  expect_true(node_match(x, lang(...) := TRUE))


  x <- quote(lang(foo, tag = bar))

  expect_null(node_match(x, lang(foo) := TRUE))
  expect_true(node_match(x, lang(foo, ...) := TRUE))


  x <- quote(1 + 2)

  expect_null(node_match(x, 1 + 0 := TRUE))
  expect_true(node_match(x, 1 + 2 := TRUE))
})

test_that("can match by tag", {
  x <- quote(lang(foo, tag = bar))

  expect_null(node_match(x, lang(tag = bar) := TRUE))
  expect_true(node_match(x, lang(tag = bar, ...) := TRUE))

  x <- quote(lang(tag = foo, tag = bar))

  expect_true(node_match(x, lang(tag = foo, tag = bar) := TRUE))
  expect_true(node_match(x, lang(tag = bar, tag = foo) := TRUE))
})

test_that("can match by tag and position", {
  x <- quote(lang(foo, tag = bar))

  expect_true(node_match(x, lang(foo, tag = bar) := TRUE))
  expect_true(node_match(x, lang(tag = bar, foo) := TRUE))
})

test_that("can match wildcard", {
  expect_true(node_match(quote(1 + 2), 1 + . := TRUE))
  expect_true(node_match(quote(call(arg = 1)), call(arg = .) := TRUE))

  expect_null(node_match(quote(call(arg = 1)), call(.) := TRUE))
  expect_null(node_match(quote(call(arg = 1)), call(wrong = .) := TRUE))
})

test_that("can match and bind", {
  expect_identical(node_match(quote(1 + call()), 1 + .(foo) := foo), quote(call()))
  expect_identical(node_match(quote(call(tag = arg)), call(. = .(foo)) := foo), quote(arg))
  expect_null(node_match(quote(call(tag = arg)), call(.(foo)) := foo))
})

test_that("can match and eval-bind", {
  call <- function() "foobar"
  expect_identical(node_match(quote(1 + call()), 1 + ..(foo) := foo), "foobar")
})

test_that("bind input node on `.`", {
  x <- quote(foo(bar))
  match <- node_match(x, foo(bar) := .)
  expect_true(is_reference(x, match))
})

test_that("matching processes patterns sequentially", {
  match <- node_match(quote(1 + 2),
    1 + 1 := "foo",
    1 + 2 := "bar"
  )
  expect_identical(match, "bar")
})
