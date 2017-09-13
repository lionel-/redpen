
node_match <- function(.node, ..., .env = caller_env()) {
  patterns <- dots_list(...)
  stopifnot(every(patterns, is_match_pattern))

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
node_match_pattern <- function(node, match_pattern, env) {
  if (is_quosure(node)) {
    env <- get_env(node)
    node <- get_expr(node)
  }
  pattern <- match_pattern$pattern

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

  bindings <- new_environment()
  if (!sxp_match(node, pattern, bindings)) {
    return(NULL)
  }

  list(pattern = match_pattern, bindings = bindings)
}

sxp_match <- function(x, y, bindings) {
  switch_type(x,
    symbol =
      is_match_identical(x, y),
    language = ,
    pairlist = {
      matched_data <- is_identical_node_data(x, y, bindings)
      matched_data && sxp_match(node_cdr(x), node_cdr(y), bindings)
    },
    identical(x, y)
  )
}

is_match_identical <- function(x, y) {
  is_wildcard(y) || identical(x, y)
}
is_wildcard <- function(x) {
  identical(x, dot_sym)
}
is_bind_operator <- function(x) {
  is_language(x) && identical(node_car(x), dot_sym)
}

# Checks both CAR and TAG. Supports wildcards and bindings.
is_identical_node_data <- function(x, y, bindings) {
  if (!is_match_identical(node_tag(x), node_tag(y))) {
    return(FALSE)
  }

  if (is_bind_operator(node_car(y))) {
    binding <- node_cadr(node_car(y))
    if (!is_symbol(binding)) {
      abort("Binding must be a symbol")
    }
    env_poke(bindings, as_string(binding), node_car(x))
    return(TRUE)
  }

  is_match_identical(node_car(x), node_car(y))
}

match_pattern <- function(pattern, expr) {
  new_match_pattern(enexpr(pattern), enquo(expr))
}
new_match_pattern <- function(pattern, expr) {
  stopifnot(is_quosure(expr))
  pattern <- list(
    pattern = pattern,
    expr = expr
  )
  set_attrs(pattern, class = "match_pattern")
}
is_match_pattern <- function(x) {
  inherits(x, "match_pattern")
}

dot_sym <- quote(.)

env_poke <- env_set
is_language <- is_lang
