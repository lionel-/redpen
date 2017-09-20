
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
  bindings <- match$bindings
  bindings$. <- .node

  eval_tidy(expr, data = bindings)
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
  if (!sxp_match(node, pattern, env, bindings)) {
    return(NULL)
  }

  list(pattern = input, bindings = bindings)
}

sxp_match <- function(input, pattern, env, bindings) {
  switch_type(input,
    symbol = {
      is_symbol_match(input, pattern)
    },
    language = {
      matched_first <- sxp_match(node_car(input), node_car(pattern), env, bindings)
      matched_first && sxp_match(node_cdr(input), node_cdr(pattern), env, bindings)
    },
    pairlist = {
      input_replicate <- node_replicate(input)
      pattern_duplicate <- duplicate(pattern)

      mended <- mend_ellipsis(pattern_duplicate)
      partial <- mended[[1]]
      pattern_mended <- mended[[2]]

      is_node_list_match(input_replicate, pattern_mended, env, bindings, partial)
    },
    identical(input, pattern)
  )
}

is_symbol_match <- function(x, y) {
  is_wildcard(x) || is_wildcard(y) || identical(x, y)
}
mend_ellipsis <- function(node) {
  parent <- node_find_parent_car(node, identical, dots_sym)

  if (is_null(parent)) {
    return(list(FALSE, node))
  }

  if (is_null_node(parent)) {
    list(TRUE, node_cdr(node))
  } else {
    node_poke_cdr(parent, node_cddr(parent))
    list(TRUE, parent)
  }
}

is_node_list_match <- function(input, pattern, env, bindings, partial) {
  if (is_null(input) || is_null(pattern)) {
    return(check_match(input, pattern, partial))
  }
  if (is_ordered_pattern(pattern)) {
    return(is_ordered_match(input, pattern, env, bindings, partial))
  }

  matches_one <- function(x) {
    parent <- node_find_parent(pattern, fn_swap(is_node_match), x, env, bindings)
    if (is_null(parent)) {
      return(FALSE)
    }

    # Remove parent element from `pattern` node list
    if (is_null_node(parent)) {
      pattern <<- node_cdr(pattern)
    } else {
      node_poke_cdr(parent, node_cddr(parent))
    }

    # Since we return TRUE, `x` will be removed from `input`
    TRUE
  }

  input <- node_list_discard_car(input, matches_one)
  check_match(input, pattern, partial)
}

check_match <- function(input, pattern, partial) {
  # We didn't exhaust pattern elements
  if (!is_null(pattern)) {
    return(FALSE)
  }

  # Allow input to include more elements than the pattern if `partial` is TRUE
  if (!is_null(input)) {
    return(partial)
  }

  TRUE
}
is_ordered_pattern <- function(node) {
  is_null(node_tag(node))
}
is_ordered_match <- function(input, pattern, env, bindings, partial) {
  matched <- is_node_match(node_car(input), pattern, env, bindings)
  if (matched) {
    is_node_list_match(node_cdr(input), node_cdr(pattern), env, bindings, partial)
  } else {
    FALSE
  }
}

# Checks CAR and TAG, not CDR. Supports wildcards and bindings.
is_node_match <- function(input, pattern, env, bindings) {
  if (!is_symbol_match(node_tag(input), node_tag(pattern))) {
    return(FALSE)
  }

  if (is_bind_operator(node_car(pattern))) {
    push_binding(node_car(pattern), node_car(input), env, bindings)
    TRUE
  } else {
    is_symbol_match(node_car(input), node_car(pattern))
  }
}
push_binding <- function(pattern, input, env, bindings) {
  binding <- node_cadr(pattern)
  if (!is_symbol(binding)) {
    abort("Binding must be a symbol")
  }

  if (is_eval_operator(pattern)) {
    value <- eval_bare(input, env)
  } else {
    value <- input
  }
  env_poke(bindings, as_string(binding), value)
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
dots_sym <- quote(...)
