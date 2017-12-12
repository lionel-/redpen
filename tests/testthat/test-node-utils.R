context("node-utils")

test_that("node_walk2() walks function over two pairlists", {
  x <- pairlist(1, 2)
  y <- pairlist(3, 4)

  out <- dbl()
  fn <- function(x, y) out <<- c(out, node_car(x), node_car(y))

  node_walk2(x, y, fn)
  expect_identical(out, c(1, 3, 2, 4))

  expect_error(node_walk2(x, pairlist(5), fn), "different size")
})

test_that("node_replicate() reproduces node list in CARs", {
  node <- pairlist(quote(bar), tag = quote(baz))
  rep <- node_replicate(node)

  expect_identical(node_car(rep), node)
  expect_identical(node_cadr(rep), node_cdr(node))
})

test_that("node_list_discard() splices node lists", {
  splice_strings <- function(x) {
    node_list_discard_car(x, is_string)
  }

  x <- pairlist(1, "a", 2, "b")
  expect_identical(splice_strings(x), pairlist(1, 2))

  x <- pairlist("a", 1, "b", 2)
  expect_identical(splice_strings(x), pairlist(1, 2))

  x <- pairlist(1)
  expect_identical(splice_strings(x), pairlist(1))

  x <- pairlist("a")
  expect_identical(splice_strings(x), NULL)
})

test_that("can find parent of node that match a predicate", {
  # No match + No parent
  x <- pairlist(1)
  expect_null(node_find_parent_car(x, is_string))

  # No match
  x <- pairlist(1, 2)
  expect_null(node_find_parent_car(x, is_string))

  # No parent
  x <- pairlist("a", 2)
  expect_true(is_null_node(node_find_parent_car(x, is_string)))

  x <- pairlist(1, "a", 2)
  expect_true(is_reference(node_find_parent_car(x, is_string), x))

  x <- pairlist(1, 2, "a")
  expect_true(is_reference(node_find_parent_car(x, is_string), node_cdr(x)))
})
