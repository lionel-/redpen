#' Quote patterns, definitions or formulas
#'
#' @description
#'
#' These quoting functions take dots and expect two-sided patterns
#' such as `a ~ b` or `a := b`. The LHS and RHS of each input are
#' extracted in a list containing two quosures.
#'
#' * `dots_patterns()` expects two-sided formulas by default. If
#'   `.operator` is set to `":="`, it expects definitions.
#'
#' * `dots_definitions()` extracts `:=` definitions in a sublist named
#'   `defs`. All other types of inputs are captured in quosures within
#'   `dots`.
#'
#'
#' @section Life cycle:
#'
#' These functions are [experimental][lifecycle].
#'
#' @param .operator The name of a binary operator from which to
#'   extract an LHS and a RHS.
#' @return A named list of lists of extracted LHS and RHS
#'   components. The inner lists contain two quosures named `lhs` and
#'   `rhs`. The outer list is named after the dots inputs.
#' @seealso [quos()]
#' @noRd
#' @examples
#' dots_patterns(
#'   A = a ~ b,
#'   c ~ d
#' )
#'
#' # You can specify the binary operator:
#' dots_patterns(a := b, .operator = ":=")
#' dots_patterns(a / b, .operator = "/")
dots_patterns <- function(...,
                          .named = FALSE,
                          .ignore_empty = c("trailing", "none", "all"),
                          .operator = "~") {
  dots <- quos(...,
    .named = .named,
    .ignore_empty = .ignore_empty,
    .unquote_names = FALSE
  )

  stopifnot(is_string(.operator))
  predicate <- switch(.operator,
    `~` = function(x) quo_is_formula(x, lhs = TRUE),
    `:=` = quo_is_definition,
    function(x) is_lang(quo_get_expr(x), .operator, n = 2)
  )
  if (!every(dots, predicate)) {
    type_name <- switch(.operator,
      `~` = "two-sided formulas",
      `:=` = "`:=` definitions",
      paste0("calls to `", .operator, "` with two arguments")
    )
    abort(paste0("`...` can only contain ", type_name))
  }

  map(dots, pattern_quos)
}
pattern_quos <- function(x, default_env = NULL) {
  # The pattern may be wrapped in a quosure
  if (is_quosure(x)) {
    env <- quo_get_env(x)
    x <- quo_get_expr(x)
  } else {
    env <- default_env
  }
  stopifnot(is_lang(x, n = 2))
  stopifnot(is_env(env))

  list(
    lhs = new_quosure(node_cadr(x), env),
    rhs = new_quosure(node_cadr(node_cdr(x)), env)
  )
}

#' Swap the first two arguments of a function
#'
#' This function operator is similar in spirit to the `swap()` Haskell
#' function. It changes the order of the first two arguments.
#'
#' @param fn A closure.
#'
#' @noRd
#' @examples
#' fn <- function(x, y, z) list(x, y, z)
#'
#' fn(1, 2, 3)
#' fn_swap(fn)(1, 2, 3)
fn_swap <- function(fn) {
  fmls <- formals(fn)
  if (length(fmls) < 2) {
    abort("Can't swap arguments of functions with fewer than two arguments")
  }

  fmls_syms <- fn_fmls_syms(fn)
  fmls_names <- names(fmls_syms)
  names(fmls_syms) <- fmls_names[c(2, 1, seq2(3, length(fmls_names)))]

  new_function(fmls, expr({
    fn(!!! fmls_syms)
  }))
}

node_list_along <- function(.x) {
  vector("pairlist", length(.x))
}
node_list_len <- function(.n) {
  vector("pairlist", .n)
}

quo_is_formula <- function(x, scoped = NULL, lhs = NULL) {
  is_formula(quo_get_expr(x), scoped = scoped, lhs = lhs)
}
quo_is_definition <- function(x) {
  is_definition(quo_get_expr(x))
}
