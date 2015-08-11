#' @rdname is_leaf
#' @export
assert_is_leaf <- function(x)
{                                                         
  assert_engine(is_leaf, x, .xname = get_name_in_parent(x))       
}

#' @rdname is_ts
#' @export
assert_is_mts <- function(x)
{                                                         
  assert_engine(is_mts, x, .xname = get_name_in_parent(x))       
}

#' @rdname is_function
#' @export
assert_is_stepfun <- function(x)
{                                                         
  assert_engine(is_stepfun, x, .xname = get_name_in_parent(x))
}

#' @rdname is_ts
#' @export
assert_is_ts <- function(x)
{                                                         
  assert_engine(is_ts, x, .xname = get_name_in_parent(x))       
}

#' @rdname is_ts
#' @export
assert_is_tskernel <- function(x)
{                                                         
  assert_engine(is_tskernel, x, .xname = get_name_in_parent(x))       
}
