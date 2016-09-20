#' @rdname is_class
#' @export
assert_all_are_classes <- function(x, 
  severity = getOption("assertive.severity", "stop"))
{                                                         
  assert_engine(
    is_class, 
    x, 
    .xname = get_name_in_parent(x),
    severity = severity
  )
}

#' @rdname is_class
#' @export
assert_any_are_classes <- function(x, 
  severity = getOption("assertive.severity", "stop"))
{                                                         
  assert_engine(
    is_class, 
    x, 
    .xname = get_name_in_parent(x),
    what = "any",
    severity = severity
  )
}

#' @rdname is_s4
#' @export
assert_is_ref_class_generator <- function(x, 
  severity = getOption("assertive.severity", "stop"))
{                                                         
  assert_engine(
    is_ref_class_generator, 
    x, 
    .xname = get_name_in_parent(x),
    severity = severity
  )
}

#' @rdname is_s4
#' @export
assert_is_ref_class_object <- function(x, 
  severity = getOption("assertive.severity", "stop"))
{                                                         
  assert_engine(
    is_ref_class_object, 
    x, 
    .xname = get_name_in_parent(x),
    severity = severity
  )
}
