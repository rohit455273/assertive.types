library(testthat)
library(devtools)
library(assertive.types)

with_envvar(
  c(LANG = "en_US"),
  test_check("assertive.types")
)
