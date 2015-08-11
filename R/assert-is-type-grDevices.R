#' @rdname is_raster
#' @export
assert_is_raster <- function(x)
{                                                         
  assert_engine(is_raster, x, .xname = get_name_in_parent(x))
}
