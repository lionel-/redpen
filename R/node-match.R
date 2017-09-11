
node_match <- function(.node, ..., .env = caller_env()) {
  patterns <- dots_list(...)
  stopifnot(every(patterns, is_node_pattern))

  match <- detect_value(patterns, node_match_pattern, negate(is_null),
    node = .node,
    env = .env
  )
  if (is_null(match)) {
    return(NULL)
  }

  expr <- match$pattern$expr
  eval_tidy(expr, data = match$bindings)
}

# Returns pattern and a list of bindings in case of match, NULL otherwise
node_match_pattern <- function(node, node_pattern, env) {
  if (is_quosure(node)) {
    env <- get_env(node)
    node <- get_expr(node)
  }
  pattern <- node_pattern$pattern

  if (typeof(node) != typeof(pattern)) {
    return(NULL)
  }
  if (!is_node(node)) {
    if (identical(node, pattern)) {
      out <- node
    } else {
      out <- NULL
    }
    return(out)
  }

  node_rest <- node
  pattern_rest <- pattern

  while (!is_null(node_rest) && !is_null(pattern_rest)) {
    if (!is_identical_node_data(node_rest, pattern_rest)) {
      return(NULL)
    }
    node_rest <- node_cdr(node_rest)
    pattern_rest <- node_cdr(pattern_rest)
  }

  list(pattern = node_pattern, bindings = list())
}

# Checks both CAR and TAG
is_identical_node_data <- function(x, y) {
  if (!identical(node_car(x), node_car(y))) {
    return(FALSE)
  }
  identical(node_tag(x), node_tag(y))
}

node_pattern <- function(pattern, expr) {
  new_node_pattern(enexpr(pattern), enquo(expr))
}
new_node_pattern <- function(pattern, expr) {
  stopifnot(is_quosure(expr))
  pattern <- list(
    pattern = pattern,
    expr = expr
  )
  set_attrs(pattern, class = "node_pattern")
}
is_node_pattern <- function(x) {
  inherits(x, "node_pattern")
}
