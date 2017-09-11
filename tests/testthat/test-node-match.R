context("node-match")

test_that("can match against simple expressions", {
  foo <- "match"
  x <- quote(1 + 2)

  expect_null(node_match(x, match_pattern(1 + 0, foo)))
  expect_identical(node_match(x, match_pattern(1 + 2, foo)), foo)

  x <- quote(call(arg = 1))
  expect_null(node_match(x, match_pattern(call(1), foo)))
  expect_null(node_match(x, match_pattern(call(wrong = 1), foo)))
  expect_identical(node_match(x, match_pattern(call(arg = 1), foo)), foo)
})

test_that("can match wildcard", {
  expect_identical(node_match(quote(1 + 2), match_pattern(1 + ., "match")), "match")
  expect_identical(node_match(quote(call(arg = 1)), match_pattern(call(arg = .), "match")), "match")

  expect_null(node_match(quote(call(arg = 1)), match_pattern(call(.), "match")), "match")
  expect_null(node_match(quote(call(arg = 1)), match_pattern(call(wrong = .), "match")), "match")
})
