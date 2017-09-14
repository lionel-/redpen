context("node-match")

test_that("can match against simple expressions", {
  foo <- "match"
  x <- quote(1 + 2)

  expect_null(node_match(x, 1 + 0 := foo))
  expect_identical(node_match(x, 1 + 2 := foo), foo)

  x <- quote(call(arg = 1))
  expect_null(node_match(x, call(1) := foo))
  expect_null(node_match(x, call(wrong = 1) := foo))
  expect_identical(node_match(x, call(arg = 1) := foo), foo)
})

test_that("can match wildcard", {
  expect_identical(node_match(quote(1 + 2), 1 + . := "match"), "match")
  expect_identical(node_match(quote(call(arg = 1)), call(arg = .) := "match"), "match")

  expect_null(node_match(quote(call(arg = 1)), call(.) := "match"), "match")
  expect_null(node_match(quote(call(arg = 1)), call(wrong = .) := "match"), "match")
})

test_that("can match and bind", {
  expect_identical(node_match(quote(1 + call()), 1 + .(foo) := foo), quote(call()))

  expect_null(node_match(quote(call(param = arg)), call(.(foo)) := foo))
  expect_identical(node_match(quote(call(param = arg)), call(. = .(foo)) := foo), quote(arg))
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
