
node_find <- function(.x, .p, ..., .matcher = base::`!`) {
  rest <- .x

  while (!is_null(rest) && .matcher(.p(rest, ...))) {
    rest <- node_cdr(rest)
  }

  rest
}
node_find_car <- function(.x, .p, ...) {
  p <- function(node) .p(node_car(node), ...)
  node_find(.x, p)
}

# Returns NULL if there is no parent (first element matches)
# Returns `.sentinel` if there is no match
node_find_parent <- function(.x, .p, ..., .missing = null_node()) {
  rest <- .x
  parent <- .missing

  while (!is_null(rest) && !.p(rest, ...)) {
    parent <- rest
    rest <- node_cdr(rest)
  }

  if (is_null(rest)) {
    NULL
  } else {
    parent
  }
}
node_find_parent_car <- function(.x, .p, ...) {
  p <- function(node) .p(node_car(node), ...)
  node_find_parent(.x, p)
}


node_walk2 <- function(.x, .y, .f, ...) {
  x_cur <- .x
  y_cur <- .y

  while (!is_null(x_cur) && !is_null(y_cur)) {
    .f(x_cur, y_cur, ...)
    x_cur <- node_cdr(x_cur)
    y_cur <- node_cdr(y_cur)
  }

  if (!is_null(x_cur) || !is_null(y_cur)) {
    abort("Can't walk over two node lists of different size")
  }

  invisible(.x)
}

node_replicate <- function(node) {
  rep <- node_list_along(node)
  node_walk2(rep, node, node_poke_car)
  rep
}


node_list_discard <- function(.x, .f, ...) {
  current <- .x

  while (!is_null(current)) {
    parent <- node_find_parent(current, .f, ...)

    if (is_null(parent)) {
      break
    }

    if (is_null_node(parent)) {
      current <- node_cdr(.x)
      .x <- current
    } else {
      current <- node_cddr(parent)
      node_poke_cdr(parent, current)
    }
  }

  .x
}
node_list_discard_car <- function(.x, .f, ...) {
  f <- function(node) .f(node_car(node), ...)
  node_list_discard(.x, f)
}


null_node_sym <- quote(`__rlang_null_node`)
null_node <- function() {
  node(null_node_sym, NULL)
}
is_null_node <- function(x) {
  identical(x, null_node())
}
