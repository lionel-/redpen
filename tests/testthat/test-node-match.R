context("node-match")

test_that("can match against simple expressions", {
  foo <- "foo"
  x <- quote(1 + 2)

  expect_null(node_match(x, node_pattern(1 + 0, foo)))
  expect_identical(node_match(x, node_pattern(1 + 2, foo)), foo)

  x <- quote(call(arg = 1))
  expect_null(node_match(x, node_pattern(call(1), foo)))
  expect_null(node_match(x, node_pattern(call(wrong = 1), foo)))
  expect_identical(node_match(x, node_pattern(call(arg = 1), foo)), foo)
})
