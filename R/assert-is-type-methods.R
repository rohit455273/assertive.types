#' @rdname is_class
#' @export
assert_all_are_classes <- function(x)
{                                                         
  assert_engine(is_class, x, .xname = get_name_in_parent(x))   
}

#' @rdname is_class
#' @export
assert_any_are_classes <- function(x)
{                                                         
  assert_engine(is_class, x, .xname = get_name_in_parent(x), what = "any")   
}
