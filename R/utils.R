
detect <- function(.x, .f, ..., .right = FALSE, .p = is_true) {
  for (i in index(.x, .right)) {
    if (.p(.f(.x[[i]], ...))) {
      return(.x[[i]])
    }
  }
  NULL
}
detect_index <- function(.x, .f, ..., .right = FALSE, .p = is_true) {
  for (i in index(.x, .right)) {
    if (.p(.f(.x[[i]], ...))) {
      return(i)
    }
  }
  0L
}
index <- function(x, right = FALSE) {
  idx <- seq_along(x)
  if (right) {
    idx <- rev(idx)
  }
  idx
}

detect_value <- function(.x, .f, .p, ..., .right = FALSE) {
  for (i in index(.x, .right)) {
    value <- .f(.x[[i]], ...)
    if (.p(value)) {
      return(value)
    }
  }
  NULL
}
