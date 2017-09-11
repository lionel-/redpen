
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

  bindings <- list()
  if (!sxp_match(node, pattern, bindings)) {
    return(NULL)
  }

  list(pattern = node_pattern, bindings = bindings)
}

sxp_match <- function(x, y, bindings) {
  switch_type(x,
    language = ,
    pairlist = {
      matched_data <- is_identical_node_data(x, y)
      matched_data && sxp_match(node_cdr(x), node_cdr(y), bindings)
    },
    identical(x, y)
  )
}


# Checks both CAR and TAG
is_identical_node_data <- function(x, y) {
  if (!identical(node_car(x), node_car(y))) {
    return(FALSE)
  }
  identical(node_tag(x), node_tag(y))
}

match_pattern <- function(pattern, expr) {
  new_match_pattern(enexpr(pattern), enquo(expr))
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
