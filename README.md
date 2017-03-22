[![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/0.1.0/active.svg)](http://www.repostatus.org/#active)
[![Is the package on CRAN?](http://www.r-pkg.org/badges/version/assertive.types)](http://www.r-pkg.org/pkg/assertive.types)
[![SemaphoreCI Build Status](https://semaphoreci.com/api/v1/projects/4bd0f182-fbc5-4c70-876b-e6f9570fdf3c/635195/badge.svg)](https://semaphoreci.com/richierocks/assertive-types)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/fb50ck21nm0os9rd?svg=true)](https://ci.appveyor.com/project/richierocks/assertive-types)
[![Research software impact](http://depsy.org/api/package/cran/assertive.types/badge.svg)](http://depsy.org/package/r/assertive.types)

# assertive.types

A set of predicates and assertions for checking the types of variables.  Most of the documentation is on the *[assertive](https://bitbucket.org/richierocks/assertive)* page.  End-users will usually want to use *assertive* directly.


### Installation

To install the stable version, type:

```{r}
install.packages("assertive.types")
```

To install the development version, you first need the *devtools* package.

```{r}
install.packages("devtools")
```

Then you can install the *assertive.types* package using

```{r}
library(devtools)
install_bitbucket("richierocks/assertive.types")
```

### Predicates

All the predicates in this package check for objects of specific types.

Wrappers to functions in `base`:

`is_array`, `is_call`, `is_character`, `is_complex`, `is_data.frame`, `is_environment`, `is_expression`, `is_factor`, `is_function`, `is_integer`, `is_language`, `is_list`, `is_logical`, `is_matrix`, `is_name`/`is_symbol`, `is_numeric`, `is_ordered`, `is_primitive`, `is_qr`, `is_raw`, `is_s4`, and  `is_table`.

Wrappers to functions in `grDevices`:

`is_raster`.

Wrappers to functions in `methods`:

`is_class`.

Wrappers to functions in `stats`:

`is_leaf`, `is_mts`, `is_stepfun`, `is_ts`, `is_tskernel`.

Wrappers to functions in `utils`:

`is_relistable`.

### Assertions

Predicates that return a vector have two corresponding assertions.  For example,
`is_class` has `assert_all_are_classes` and `assert_any_are_classes`.

Predicates returning a single logical value have one corresponding assertion.
For example, `is_array` has `assert_is_array`.