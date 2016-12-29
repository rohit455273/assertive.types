
# Constants ---------------------------------------------------------------

S3_PRIMITIVE_GENERICS <- mget(.S3PrimitiveGenerics, envir = baseenv())

# Listed in tools:::.get_S3_primitive_generics
# Same as S4 group generics, but with extra ! and without log2 and log10
S3_GROUP_GENERICS <- mget(
  c(
  "abs", "sign", "sqrt", "floor", "ceiling", "trunc", "round", "signif", "exp", 
  "log", "expm1", "log1p", "cos", "sin", "tan", "acos", "asin", "atan", "cosh", 
  "sinh", "tanh", "acosh", "asinh", "atanh", "lgamma", "gamma", "digamma", 
  "trigamma", "cumsum", "cumprod", "cummax", "cummin", "+", "-", "*", "/", "^", 
  "%%", "%/%", "&", "|", "!", "==", "!=", "<", "<=", ">=", ">", "all", "any", 
  "sum", "prod", "max", "min", "range", "Arg", "Conj", "Im", "Mod", "Re"
  ),
  envir = baseenv()
)

S3_NON_PRIMITIVE_NON_GROUP_GENERICS <- mget(
  c("[", "[[", "$", "[<-", "[[<-", "$<-", "as.vector", "unlist"),
  envir = baseenv()
)

#' @importFrom methods getGeneric
S4_GROUP_GENERICS <- local(
  {
    groups <- c("Arith", "Compare", "Logic", "Math", "Math2", "Summary", "Complex")
    names(groups) <- groups
    lapply(
      groups, 
      function(group)
      {
        # simplified version of methods::getGroupMembers(group)
        mget(unlist(getGeneric(group)@groupMembers), envir = baseenv())
      }
    )
  }
)

BASE_S3_METHODS <- as.list(baseenv()$.__S3MethodsTable__.)

# Utilities ---------------------------------------------------------------

#' Does the function call another function?
#' 
#' Operator wrapper to \code{\link[codetools]{findGlobals}}.
#' @param caller_fn A closure function.
#' @param callee_fn_name A character vector of function names.
#' @return \code{TRUE} if \code{caller_fn} calls any the the functions named in
#' \code{callee_fn_name}.
#' @details The LHS is a function and the RHS is a character vector because 
#' that is how \code{\link[codetools]{findGlobals}} works.
#' @importFrom codetools findGlobals
#' @noRd
`%calls%` <- function(caller_fn, callee_fn_name)
{
  fns_called <- codetools::findGlobals(caller_fn, merge = FALSE)$functions
  any(fns_called %in% callee_fn_name)
}

`%fn_in%` <- function(x, y)
{
  any(vapply(y, identical, logical(1), x, ignore.environment = TRUE))
}

# Types: closure/builtin/special ------------------------------------------

is_typeof_function <- function(x, type, .xname = get_name_in_parent(x))
{
  if(!(ok <- is_function(x)))
  {
    return(ok)
  }
  typeof_x <- typeof(x)
  if(typeof_x != type)
  {
    return(
      false("%s is not a %s function; it is a %s function.", .xname, type, typeof_x)
    )
  }
  TRUE
}

#' Is the input a closure, builtin or special function?
#' 
#' Checks to see if the input is a closure, builtin or special function. 
#' @param x Input to check.
#' @param .xname Not intended to be used directly.
#' @param severity How severe should the consequences of the assertion be?  
#' Either \code{"stop"}, \code{"warning"}, \code{"message"}, or \code{"none"}.
#' @return \code{is_internal_function} returns \code{TRUE} when the input is a 
#' closure function that calls \code{\link[base]{.Internal}}.  The 
#' \code{assert_*} function returns nothing but throw an error if the 
#' corresponding \code{is_*} function returns \code{FALSE}.
#' @references There is some discussion of closure vs. builtin vs. special
#' functions in the Argument Evaluation section of R-internals.
#' \url{https://cran.r-project.org/doc/manuals/r-devel/R-ints.html#Argument-evaluation}
#' @seealso \code{\link[base]{is.function}} and its assertive wrapper
#' \code{\link{is_function}}.
#' \code{\link[base]{typeof}} is used to distinguish the three types 
#' of function.
#' @examples 
#' # most functions are closures
#' is_closure_function(mean)
#' is_closure_function(lm)
#' is_closure_function(summary)
#' 
#' # builtin functions are typically math operators, low level math functions
#' # and commonly used functions
#' is_builtin_function(`*`)
#' is_builtin_function(cumsum)
#' is_builtin_function(is.numeric)
#' 
#' # special functions are mostly language features 
#' is_special_function(`if`)
#' is_special_function(`return`)
#' is_special_function(`~`)
#' 
#' # some failure messages
#' assertive.base::dont_stop({
#' assert_is_builtin_function(mean)
#' assert_is_builtin_function("mean")
#' })
#' @export
is_closure_function <- function(x, .xname = get_name_in_parent(x))
{
  is_typeof_function(x, "closure", .xname)
}

#' @rdname is_closure_function
#' @export
is_builtin_function <- function(x, .xname = get_name_in_parent(x))
{
  is_typeof_function(x, "builtin", .xname)
}

#' @rdname is_closure_function
#' @export
is_special_function <- function(x, .xname = get_name_in_parent(x))
{
  is_typeof_function(x, "special", .xname)
}


# Internal functions ------------------------------------------------------

#' Is the input an internal function?
#' 
#' Checks to see if the input is an internal function. That is, it is a 
#' non-primitive function that calls C-code via \code{\link[base]{.Internal}}.
#' @param x Input to check.
#' @param .xname Not intended to be used directly.
#' @param severity How severe should the consequences of the assertion be?  
#' Either \code{"stop"}, \code{"warning"}, \code{"message"}, or \code{"none"}.
#' @return \code{is_internal_function} returns \code{TRUE} when the input is a 
#' closure function that calls \code{\link[base]{.Internal}}.  The 
#' \code{assert_*} function returns nothing but throw an error if the 
#' corresponding \code{is_*} function returns \code{FALSE}.
#' @seealso \code{\link[base]{is.function}} and its assertive wrapper
#' \code{\link{is_function}}.
#' @references This function is modeled upon \code{is_internal}, internal to the
#' \code{pryr} package.
#' The differences between the \code{.Internal} and \code{.Primitive} interfaces
#' to C code are discussed in R-Internals, in the chapter Internal vs. Primitive.
#' \url{https://cran.r-project.org/doc/manuals/r-devel/R-ints.html#g_t_002eInternal-vs-_002ePrimitive}
#' @examples
#' # Some common fns calling .Internal
#' is_internal_function(unlist)
#' is_internal_function(cbind)
#' 
#' # Some failures
#' assertive.base::dont_stop({
#' assert_is_internal_function("unlist")
#' assert_is_internal_function(sqrt)
#' assert_is_internal_function(function(){})
#' })
#' @export
is_internal_function <- function(x, .xname = get_name_in_parent(x))
{
  if(!(ok <- is_function(x)))
  {
    return(ok)
  }
  if(is.primitive(x))
  {
    return(
      false("%s is a primitive function, thus cannot be internal.", .xname)
    )
  }
  # should be guaranteed typeof(x) == "closure" now
  if(!x %calls% ".Internal")
  {
    return(
      false("%s does not call .Internal.", .xname)
    )
  }
  TRUE
}

# S3 Generics and methods -------------------------------------------------

#' Is the input an S3 generic or method?
#' 
#' Checks whether the input is an S3 generic or method.
#' @param x Input to check.
#' @param groups A character vector of S4 group generic groups.
#' @param .xname Not intended to be used directly.
#' @param severity How severe should the consequences of the assertion be?  
#' Either \code{"stop"}, \code{"warning"}, \code{"message"}, or \code{"none"}.
#' @return \code{is_internal_function} returns \code{TRUE} when the input is a 
#' closure function that calls \code{\link[base]{.Internal}}.  The 
#' \code{assert_*} function returns nothing but throw an error if the 
#' corresponding \code{is_*} function returns \code{FALSE}.
#' @seealso \code{\link[base]{is.function}} and its assertive wrapper
#' \code{\link{is_function}}.
#' \code{\link{is_closure_function}} to check for closures/builtin and
#' special functions.
#' \code{\link{is_internal_function}} to check for functions that use the
#' \code{\link[base]{.Internal}} interface to C code.
#' @references \code{is_s3_generic} is based upon
#' \code{\link[pryr]{is_s3_generic}}. Similarly, \code{is_s3_method} is based
#' upon \code{find_generic}, internal to \code{pryr}, with some ideas from
#' \code{\link[utils]{isS3method}}.
#' \code{is_primitive_generic} checks for the functions listed by
#' \code{\link[base]{.S3PrimitiveGenerics}}.
#' \code{is_s3_group_generic} checks for the functions listed by 
#' \code{.get_internal_S3_generics}, internal to the \code{tools} package.
#' \code{is_s4_group_generic} checks for the functions listed by
#' \code{\link[methods]{getGroupMembers}}. S4 group generics are mostly the same
#' as S3 group generics, except that the not operator, \code{!}, is S3 group 
#' generic but not S4, and \code{log2} and \code{log10} are S4
#' group generic but not S3.
#' \code{is_s3_internal_generic} checks for the functions listed by
#' \code{.get_internal_S3_generics}, internal to the \code{tools} package.
#' \code{internal_generics}, internal to \code{pryr} works similarly, though
#' checks for S4 group generics rather than S3 group generics.
#' There is some discussion of group generics scatterd throughout R-internals.
#' In particular, see the section on the Mechanics of S4 Dispatch.
#' \url{https://cran.r-project.org/doc/manuals/r-devel/R-ints.html#Mechanics-of-S4-dispatch}
#' @examples
#' # General check for S3 generics and methods
#' is_s3_generic(is.na)
#' is_s3_method(is.na.data.frame)
#' 
#' # More specific types of S3 generic
#' is_s3_primitive_generic(c)
#' is_s3_group_generic(abs)
#' is_s3_internal_generic(unlist)
#' 
#' # S4 group generics are mostly the same as S3 group generics
#' is_s4_group_generic(cosh)
#' 
#' # Renaming functions is fine
#' not <- `!`
#' is_s3_group_generic(not)
#' 
#' # Some failures
#' assertive.base::dont_stop({
#' assert_is_s3_primitive_generic(exp)
#' assert_is_s4_group_generic(`!`)
#' })
#' @export
is_s3_generic <- function(x, .xname = get_name_in_parent(x))
{
  if(!(ok <- is_function(x)))
  {
    return(ok)
  }
  if(is.primitive(x) || is_internal_function(x))
  {
    if(!is_s3_internal_generic(x, .xname = .xname))
    {
      return(false("%s is not an S3 generic function.", .xname))
    }
  } else 
  {
    if(!x %calls% "UseMethod")
    {
      return(false("%s is not an S3 generic function.", .xname))
    }
  }
  TRUE
}

#' @rdname is_s3_generic
#' @export
is_s3_method <- function(x, .xname = get_name_in_parent(x))
{
  # Loosely modelled on pryr:::find_generic with ideas from
  # utils::isS3method.
  if(!(ok <- is_function(x)))
  {
    return(ok)
  }
  # For speed, rule out some common choices
  if(x %fn_in% BASE_S3_METHODS)
  {
    return(TRUE)
  }
  splits <- strsplit(.xname, ".", fixed = TRUE)[[1]]
  n <- length(splits)
  if(n < 2L)
  {
    return(false("%s has no '.' in its name, so cannot be an S3 method.", .xname))
  }
  for(i in seq.int(1L, length(splits) - 1))
  {
    generic_name <- paste(splits[seq_len(i)], collapse = ".")
    generic <- mget(generic_name, ifnotfound = NA, inherits = TRUE)[[1]]
    # don't use is.na as it gives warning for function input
    if(identical(generic, NA)) 
    {
      next
    }
    if(is_s3_generic(generic, .xname = generic_name))
    {
      return(TRUE)
    }
  }
  return(false("%s is not an S3 method.", .xname))
}

#' @rdname is_s3_generic
#' @export
is_s3_primitive_generic <- function(x, .xname = get_name_in_parent(x))
{
  if(!(ok <- is_function(x)))
  {
    return(ok)
  }
  if(!(x %fn_in% S3_PRIMITIVE_GENERICS))
  {
    return(false("%s is not an S3 primitive generic function.", .xname))
  }
  TRUE
} 

#' @rdname is_s3_generic
#' @export
is_s3_group_generic <- function(x, .xname = get_name_in_parent(x))
{
  if(!(ok <- is_function(x)))
  {
    return(ok)
  }
  if(!x %fn_in% S3_GROUP_GENERICS)
  {
    return(false("%s is not an S3 group generic function.", .xname))
  }
  TRUE
}

#' @rdname is_s3_generic
#' @export
is_s4_group_generic <- function(x, 
  groups = c("Arith", "Compare", "Ops", "Logic", "Math", "Math2", "Summary", "Complex"), 
  .xname = get_name_in_parent(x))
{
  if(!(ok <- is_function(x)))
  {
    return(ok)
  }
  groups <- match.arg(groups, several.ok = TRUE)
  if("Ops" %in% groups) 
  {
    groups <- unique(c(groups[groups != "Ops"], "Arith", "Compare", "Logic"))
  }
  fns <- unlist(
    S4_GROUP_GENERICS[groups], 
    recursive = FALSE,
    use.names = FALSE
  )
  if(!x %fn_in% fns)
  {
    return(false("%s is not an internal group generic function.", .xname))
  }
  TRUE
}

#' @rdname is_s3_generic
#' @export
is_s3_internal_generic <- function(x, .xname = get_name_in_parent(x))
{
  # logic as per tools:::.get_internal_S3_generics
  if(!(ok <- is_function(x)))
  {
    return(ok)
  }
  if(!x %fn_in% c(S3_PRIMITIVE_GENERICS, S3_GROUP_GENERICS, S3_NON_PRIMITIVE_NON_GROUP_GENERICS))
  {
    return(false("%s is not an S3 internal generic function.", .xname))
  }
  TRUE
}
