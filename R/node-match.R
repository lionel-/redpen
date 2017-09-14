
node_match <- function(.node, ..., .env = caller_env()) {
  dots <- dots_match_patterns(...)

  match <- detect_value(dots, node_match_pattern, negate(is_null),
    node = .node,
    env = .env
  )
  if (is_null(match)) {
    return(NULL)
  }

  expr <- match$pattern$expr
  eval_tidy(expr, data = match$bindings)
}

dots_match_patterns <- function(...) {
  dots <- dots_patterns(...)
  dots <- map(dots, set_names, "pattern", "expr")

  # Extract quosures from patterns RHS
  dots <- map(dots, function(input) {
    input$pattern <- get_expr(input$pattern)
    input
  })

  dots
}

# Returns pattern and a list of bindings in case of match, NULL otherwise
node_match_pattern <- function(node, input, env) {
  if (is_quosure(node)) {
    env <- get_env(node)
    node <- get_expr(node)
  }
  pattern <- input$pattern

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
  if (!sxp_match(node, pattern, bindings, env)) {
    return(NULL)
  }

  list(pattern = input, bindings = bindings)
}

sxp_match <- function(x, y, bindings, env) {
  switch_type(x,
    symbol =
      is_symbol_match(x, y),
    language = ,
    pairlist = {
      matched_data <- is_node_match(x, y, bindings, env)
      matched_data && sxp_match(node_cdr(x), node_cdr(y), bindings, env)
    },
    identical(x, y)
  )
}

is_symbol_match <- function(x, y) {
  is_wildcard(y) || identical(x, y)
}

# Checks both CAR and TAG. Supports wildcards and bindings.
is_node_match <- function(x, y, bindings, env) {
  if (!is_symbol_match(node_tag(x), node_tag(y))) {
    return(FALSE)
  }

  if (is_bind_operator(node_car(y))) {
    binding <- node_cadr(node_car(y))
    if (!is_symbol(binding)) {
      abort("Binding must be a symbol")
    }

    if (is_eval_operator(node_car(y))) {
      value <- eval_bare(node_car(x), env)
    } else {
      value <- node_car(x)
    }
    env_poke(bindings, as_string(binding), value)
    return(TRUE)
  }

  is_symbol_match(node_car(x), node_car(y))
}

is_wildcard <- function(x) {
  identical(x, dot_sym)
}
is_bind_operator <- function(x) {
  is_language(x, list(bind_sym, eval_sym))
}
is_eval_operator <- function(x) {
  is_language(x, eval_sym)
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
bind_sym <- quote(.)
eval_sym <- quote(..)

env_poke <- env_set
is_language <- is_lang
