#' Match patterns to a call
#'
#' @description
#'
#' `node_match()` provides a flexible way of checking whether a quoted
#' call conforms to a pattern. It compares the call to a set of
#' `pattern ~ expression` pairs. If the LHS of a formula (e.g. a
#' pattern) matches, the RHS of the formula is evaluated; otherwise
#' the next formula is checked. Patterns support named and positional
#' arguments, and can involve wildcards that match anything.
#'
#' If the wildcards are named, the part of the call that matched the
#' wildcard will be assigned under that name in the environment where
#' the RHS is evaluated. This technique makes it easy to provide
#' specific responses to different inputs.
#'
#'
#' @section Pattern rules:
#'
#' A pattern typically involves one call (e.g. `fn(1, 2)`), and
#' possibly subcalls (as arguments of an outer call, e.g. `fn(other(),
#' 2)`). The arguments of a call in the pattern are checked using the
#' following rules:
#'
#' * __Named arguments__ will match even if not in order. The patterns
#'   `call(foo = 1, bar = 2)` and `call(bar = 2, foo = 1)` are thus
#'   equivalent.
#'
#' * __Unnamed arguments__ will match only in order. The patterns
#'   `call(foo, bar)` and `call(bar, foo)` are thus not equivalent.
#'
#' * __NULL-named arguments__ signal an unnamed argument that should
#'   match even if not in order. The patterns `call(NULL = foo, NULL =
#'   bar)` and `call(NULL = bar, NULL = foo)` will both match the call
#'   `call(foo, bar)`.
#'
#'
#' In addition, patterns can contain wildcards that will match
#' anything:
#'
#' * The `.` wildcard will match anything. It can appear on the LHS or
#'   the RHS of an argument. For instance `call(. = .)` will match any
#'   argument no matter its name. `call(.)` will match any unnamed
#'   argument.
#'
#' * The `...` wildcard matches all remaining unmatched arguments.
#'   `call(...)` will match any calls to `call()`, including
#'   `call(foo)` or `call(foo, bar())`. This is in contrast to the
#'   default where matching must be exhaustive, i.e. `call(.)` will
#'   match `call(foo)` but won't `call(foo, bar)`.
#'
#'   If you need to match an argument regardless of whether it has a
#'   name, use the ellipsis as LHS: `call(... = .)`.
#'
#' * Binding wildcards with `.(name)`. Binding wildcards match
#'   anything and create a reference to the matched code that you can
#'   later refer to for further checking. For instance if you are
#'   matching against the call `call(foo(bar))` with the pattern
#'   `call(.(arg))`, the `arg` object will contain `foo(bar)`. If
#'   several wildcards are given the same name, they override each
#'   other and the last one prevails.
#'
#' * Eval-binding wildcards with `..(name)`. These wildcards work just
#'   like binding wildcards but evaluate the matched code before
#'   binding it to a symbol.
#'
#' Patterns are supplied as formulas with the pattern on the left-hand
#' side and an expression of your choosing on the right-hand side. The
#' expression is evaluated only if the pattern match. It can evaluate
#' to a sentinel value (so you know which expression matched) or to
#' some checking code. If no pattern matches, the return value of is
#' `NULL`. For example the following returns `NULL`:
#'
#' ```
#' expr <- quote(call(foo, bar))
#' node_match(expr,
#'   call(baz) ~ 1,
#'   call(.) ~ 2
#' )
#' ```
#'
#' While this returns `2`:
#'
#' ```
#' node_match(expr,
#'   call(foo, baz) ~ 1,
#'   call(foo, .) ~ 2
#' )
#' ```
#'
#'
#' @section `call_match()` versus `node_match()`:
#'
#' `call_match()` is just like `node_match()` except it standardises
#' the `.x` call and pattern calls with [rlang::call_standardise()].
#' Standardisation ensures that arguments are matched by position
#' rather than by name. Note that only function arguments positioned
#' before `...` are normalised.
#'
#'
#' @section Named patterns:
#'
#' Argument names are parsed to R code. This makes it easy to supply
#' binding wildcards as names, e.g.
#'
#' ```
#' call(`.(arg_name)` = .)
#' ```
#'
#' On the other hand it makes it more difficult to supply
#' non-syntactic names as you have to double quote. Either one of the
#' following types of double-quotes will work:
#'
#' ```
#' call("`non-syntactic`" = .)
#' call(`\`non-syntactic\`` = .)
#' ```
#'
#'
#' @section Evaluated bindings:
#'
#' It is often useful to check the value of an argument rather than
#' its symbolic form. The eval-bind wildcard `..()` makes it easy
#' because it evaluates the matched argument and binds it to a
#' symbol. You can then refer to that symbol in the right-hand side:
#'
#' ```
#' call(. = ..(foo)) ~ is.numeric(foo)
#' ```
#'
#' However you need to be a bit careful when evaluating R code this way.
#'
#' - First, the code should be evaluated in the right environment. If
#'   you supplied a quosure, then it is evaluated in the quosure
#'   environment. Otherwise, it is evaluated in `.env` (the caller
#'   environment by default).
#'
#' - Secondly, the evaluated code might produce side effects, such as
#'   a warning or an error. You might want to wrap your pattern
#'   matching code with e.g. `purrr::safely()`.
#'
#' @param .x An expression to match.
#' @param ... Two sided formulas with patterns on the LHS and
#'   expressions on the RHS. If the LHS matches `.x`, the RHS is
#'   evaluated.
#' @param .env An environment in which to evaluate the RHS. By default
#'   the current environment.
#' @return The result of evaluating the RHS of a matching pattern.
#'   `NULL` if no pattern has matched `.x`
#' @export
#' @examples
#'
#' # Let's create a call to dplyr::mutate(). We'll try to match this code
#' # with various patterns:
#' call <- quote(mutate(df, weight_sq = weight^2))
#'
#' # A pattern is an expression supplied as a formula. The pattern is on
#' # the LHS and the expression on the RHS is evaluated only if the LHS
#' # matches. Here the second call matches and the RHS evaluates to `2`:
#' node_match(call,
#'   mutate(df)                       ~ 1,
#'   mutate(df, weight_sq = weight^2) ~ 2
#' )
#'
#' # If further arguments do not matter, use `...` in the pattern:
#' node_match(call,
#'   mutate(df)      ~ 1,
#'   mutate(df, ...) ~ 2
#' )
#'
#' # The following patterns do not match because the data frame goes by
#' # another name. Instead of returning the RHS of a matching pattern,
#' # node_match() returns `NULL`:
#' node_match(call,
#'   mutate(x, ...)             ~  1,
#'   mutate(my_data_frame, ...) ~  2
#' )
#'
#' # Let's use a wildcard so the data frame doesn't count. Since we have
#' # a match, node_match() returns the pattern RHS, `2`:
#' node_match(call,
#'   mutate(x, ...) ~ 1,
#'   mutate(., ...) ~ 2
#' )
#'
#' # Wildcards also apply to names:
#' node_match(call,
#'   mutate(., weight^2)         ~ 1,
#'   mutate(., wrong = weight^2) ~ 2,
#'   mutate(., . = weight^2)     ~ 3
#' )
#'
#' # If you want to match an argument regardless of whether it has a
#' # name, use the ellipsis wildcard instead:
#' node_match(quote(call(arg)),
#'   call(. = arg)   ~ 1,
#'   call(... = arg) ~ 2
#' )
#'
#' # The RHS is a handy way of providing custom error messages:
#' fail_unnamed <- function() {
#'   stop("You should provide a named argument")
#' }
#' check_sq_suffix <- function(nm) {
#'   if (!grepl(".*_sq$", nm)) {
#'     stop("The new variable must end with `_sq`")
#'   }
#'   message("Alright!")
#' }
#' node_match(call,
#'   mutate(., .)             ~ fail_unnamed(),
#'   mutate(., `.(nm)` = .^2) ~ check_sq_suffix(nm),
#'   .                        ~ message("Try again")
#' )
call_match <- function(.x, ..., .env = caller_env()) {
  .x <- call_standardise(.x, env = .env)

  dots <- dots_patterns(...)
  dots <- map(dots, function(x) {
    x$lhs <- call_standardise(x$lhs)
    x
  })
  dots <- patterns_recompose(dots)

  node_match(.x = .x, !!! dots, .env = .env)
}

patterns_recompose <- function(patterns, new_pattern = new_formula) {
  map(patterns, pattern_recompose, new_pattern)
}

is_pattern <- function(x) {
  if (!identical(names(x), c("lhs", "rhs"))) {
    return(FALSE)
  }
  if (!every(x, is_quosure)) {
    return(FALSE)
  }
  if (!identical(get_env(x$lhs), get_env(x$rhs))) {
    return(FALSE)
  }
  TRUE
}
pattern_recompose <- function(pattern, new_pattern) {
  if (!is_pattern(pattern)) {
    abort("A pattern must be a list of `lhs` and `rhs` quosures")
  }
  env <- get_env(pattern$lhs)
  exprs <- map(pattern, get_expr)
  new_pattern(exprs$lhs, exprs$rhs, env = env)
}

#' @rdname call_match
#' @export
node_match <- function(.x, ..., .env = caller_env()) {
  dots <- dots_match_patterns(...)

  match <- detect_value(dots, node_match_pattern, negate(is_null),
    node = .x,
    env = .env
  )
  if (is_null(match)) {
    return(NULL)
  }

  expr <- match$pattern$expr
  bindings <- match$bindings
  bindings$. <- .x

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

node_match_pattern <- function(node, input, env) {
  if (is_quosure(node)) {
    env <- get_env(node)
    node <- get_expr(node)
  }
  pattern <- input$pattern

  bindings <- new_environment()
  if (sxp_match(node, pattern, env, bindings)) {
    list(pattern = input, bindings = bindings)
  } else {
    NULL
  }
}

sxp_match <- function(input, pattern, env, bindings) {
  if (is_wildcard(pattern)) {
    return(TRUE)
  }
  if (is_bind_operator(pattern)) {
    push_binding(pattern, input, env, bindings)
    return(TRUE)
  }

  switch_type(input,
    symbol = {
      is_symbol_match(input, pattern)
    },
    language = {
      if (!is_call(pattern)) {
        return(FALSE)
      }
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
is_tag_match <- function(tag, pattern) {
  if (identical(tag, pattern)) {
    TRUE
  } else if (is_null(tag)) {
    is_ellipsis(pattern)
  } else {
    is_wildcard(pattern)
  }
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
is_node_match <- function(input_node, pattern_node, env, bindings) {
  input_tag <- node_tag(input_node)
  parsed_tag <- sym_parse(node_tag(pattern_node))

  if (is_call(parsed_tag)) {
    if (!is_bind_operator(parsed_tag)) {
      abort("Unexpected argument name in pattern. Do you need to double-quote?")
    }
    push_binding(parsed_tag, as_string(input_tag), env, bindings)
  } else if (!is_tag_match(input_tag, parsed_tag)) {
    return(FALSE)
  }

  input <- node_car(input_node)
  pattern <- node_car(pattern_node)
  if (is_bind_operator(pattern)) {
    push_binding(pattern, input, env, bindings)
    TRUE
  } else if (is_call(pattern)) {
    sxp_match(input, pattern, env, bindings)
  } else {
    is_symbol_match(input, pattern)
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
sym_parse <- function(sym) {
  if (is_null(sym)) {
    NULL
  } else {
    parse_expr(as_string(sym))
  }
}


is_wildcard <- function(x) {
  identical(x, dot_sym)
}
is_ellipsis <- function(x) {
  identical(x, dots_sym)
}
is_bind_operator <- function(x) {
  is_call(x, list(bind_sym, eval_sym))
}
is_eval_operator <- function(x) {
  is_call(x, eval_sym)
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
