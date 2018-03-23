library("regressoR")
context("regressoR.learn")

test_that("Test regressoR.learn I", {
  dx <- rnorm(100);
  dy <- rnorm(n=100, mean=50*dx*dx-33);
  result <- regressoR.learn(x=dx, y=dy);
  expect_true(!(is.null(result)));
  expect_is(result, "FittedModel");
  validObject(result);
})

test_that("Test regressoR.learn II", {
  dx <- runif(100) + 0.1
  dy.raw <- 1 + 0.4* exp(5 - 3*dx + 0.6*dx*dx)
  dy <- rnorm(n=100, mean=dy.raw)
  result <- regressoR.learn(x=dx, y=dy);
  expect_true(!(is.null(result)));
  expect_is(result, "FittedModel");
  validObject(result);
})
