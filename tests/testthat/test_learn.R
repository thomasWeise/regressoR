library("regressoR")
context("regressoR.learn")

slow.tests <- (is.na(Sys.getenv("TRAVIS", unset=NA)))
regressoR.learn.choice <- as.integer(runif(n=1, min=1, max=3))

test_that("Test regressoR.learn I", {
  if(slow.tests || (regressoR.learn.choice == 1L)) {
    if(slow.tests) { n <- 100L; }
    else           { n <- 20L;  }
    dx <- rnorm(n);
    dy <- rnorm(n=n, mean=50*dx*dx-33);
    result <- regressoR.learn(x=dx, y=dy);
    expect_true(!(is.null(result)));
    expect_is(result, "FittedModel");
    validObject(result);
  }
})

test_that("Test regressoR.learn II", {
  if(slow.tests || (regressoR.learn.choice == 2L)) {
    if(slow.tests) { n <- 100L; }
    else           { n <- 20L;  }
    dx <- runif(n) + 0.1
    dy.raw <- 1 + 0.4* exp(5 - 3*dx + 0.6*dx*dx)
    dy <- rnorm(n=n, mean=dy.raw)
    result <- regressoR.learn(x=dx, y=dy);
    expect_true(!(is.null(result)));
    expect_is(result, "FittedModel");
    validObject(result);
  }
})
