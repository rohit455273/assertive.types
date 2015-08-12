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

Predicates return a single logical value have one corresponding assertion.
For example, `is_array` has `assert_is_array`.

