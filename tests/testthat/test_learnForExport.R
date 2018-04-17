library("regressoR")
context("regressoR.learnForExport")

.check <- function(result) {
  expect_true(!(is.null(result)));
  expect_is(result, "RegressionResult");
  expect_true(is.function(result@result@f));
  expect_true(is.numeric(result@result@quality));
  expect_true(is.integer(result@result@size));
  expect_true(is.numeric(result@time));
  expect_true(is.character(result@name));
  if(!(is.null(result@metric))) {
    expect_identical(result@metric@quality(result@result@f), result@result@quality);
  }
}

slow.tests <- is.na(Sys.getenv("TRAVIS", unset=NA))

test_that("Test regressoR.learnForExport I", {
  if(slow.tests) {
    n <- 100L;
    dx <- rnorm(n);
    dy <- rnorm(n=n, mean=50*dx*dx-33);
    .check(regressoR.learnForExport(x=dx, y=dy));
  }
})

test_that("Test regressoR.learnForExport II", {
  if(slow.tests) {
    n <- 100L;
    dx <- runif(n) + 0.1
    dy.raw <- 1 + 0.4* exp(5 - 3*dx + 0.6*dx*dx)
    dy <- rnorm(n=n, mean=dy.raw)
    .check(regressoR.learnForExport(x=dx, y=dy));
  }
})
