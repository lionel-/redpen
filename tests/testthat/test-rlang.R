
test_that("fn_swap() swaps two arguments", {
  expect_error(fn_swap(function(x) NULL), "fewer than two arguments")

  swapped <- fn_swap(function(x, y, z) list(x, y, z))
  expect_identical(swapped(1, 2, 3), list(2, 1, 3))
  expect_identical(swapped(y = 2, 3, x = 1), list(2, 1, 3))

  swapped_defaults <- fn_swap(function(x = 1, y = 2) list(x, y))
  expect_identical(swapped_defaults(), list(2, 1))
})

test_that("dots patterns check input types", {
  expect_error(dots_patterns(foo()), "`...` can only contain two-sided formulas")
  expect_error(dots_patterns(foo(), .operator = ":="), "`...` can only contain `:=` definitions")
  expect_error(dots_patterns(foo(), .operator = "foo"), "`...` can only contain calls to `foo` with two arguments")
})

test_that("dots patterns accept arbitrary binary functions", {
  pat <- list(A = list(lhs = quo(a), rhs = quo(b)))
  expect_identical(dots_patterns(A = a + b, .operator = "+"), pat)
  expect_identical(dots_patterns(A = foo(a, b), .operator = "foo"), pat)
})
