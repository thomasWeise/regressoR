library("regressoR")
context("regressoR.learn")

need.fast.tests <- (!(is.na(Sys.getenv("TRAVIS", unset=NA))));

test_that("Test regressoR.learn I", {
  if(need.fast.tests) { n <- 40; }
  else                { n <- 100; }
  dx <- rnorm(n);
  dy <- rnorm(n=n, mean=50*dx*dx-33);
  result <- regressoR.learn(x=dx, y=dy);
  expect_true(!(is.null(result)));
  expect_is(result, "FittedModel");
  validObject(result);
})

test_that("Test regressoR.learn II", {
  if(need.fast.tests) { n <- 40; }
  else                { n <- 100; }
  dx <- runif(n) + 0.1
  dy.raw <- 1 + 0.4* exp(5 - 3*dx + 0.6*dx*dx)
  dy <- rnorm(n=n, mean=dy.raw)
  result <- regressoR.learn(x=dx, y=dy);
  expect_true(!(is.null(result)));
  expect_is(result, "FittedModel");
  validObject(result);
})
