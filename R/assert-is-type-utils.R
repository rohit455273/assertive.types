#' @rdname is_relistable
#' @export
assert_is_relistable <- function(x)
{                                                         
  assert_engine(is_relistable, x, .xname = get_name_in_parent(x))
}

