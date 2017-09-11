
node_find <- function(.x, .p, ...) {
  rest <- .x
  while (!is_null(rest) && !.p(.x, node_car(rest))) {
    rest <- node_cdr(.x)
  }
  rest
}
