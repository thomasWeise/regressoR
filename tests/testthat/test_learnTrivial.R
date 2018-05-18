library("regressoR")
context(".regressoR.learnTrivial")


test_that("Test .regressoR.learnTrivial single point", {
  res <- .regressoR.learnTrivial(c(1), c(2));
  expect_is(res, "FittedModel");
  expect_identical(res@f(1), 2);
  expect_identical(res@f(2), 2);
  expect_identical(res@f(3), 2);

  res <- .regressoR.learnTrivial(c(1, 1), c(2, 2));
  expect_is(res, "FittedModel");
  expect_identical(res@f(1), 2);
  expect_identical(res@f(2), 2);
  expect_identical(res@f(3), 2);


  res <- .regressoR.learnTrivial(c(1, 1, 1), c(2, 2, 2));
  expect_is(res, "FittedModel");
  expect_identical(res@f(1), 2);
  expect_identical(res@f(2), 2);
  expect_identical(res@f(3), 2);
})


test_that("Test .regressoR.learnTrivial two points", {
  f <- function(x) 2 * x + 3

  res <- .regressoR.learnTrivial(c(1, 2), c(f(1), f(2)));
  expect_is(res, "FittedModel");
  expect_identical(res@f(1), f(1));
  expect_identical(res@f(2), f(2));
  expect_identical(res@f(3), f(3));
  expect_identical(res@f(4), f(4));

  res <- .regressoR.learnTrivial(c(1, 2, 1), c(f(1), f(2), f(1)));
  expect_is(res, "FittedModel");
  expect_identical(res@f(1), f(1));
  expect_identical(res@f(2), f(2));
  expect_identical(res@f(3), f(3));
  expect_identical(res@f(4), f(4));

  res <- .regressoR.learnTrivial(c(1, 2, 2, 1), c(f(1), f(2), f(2), f(1)));
  expect_is(res, "FittedModel");
  expect_identical(res@f(1), f(1));
  expect_identical(res@f(2), f(2));
  expect_identical(res@f(3), f(3));
  expect_identical(res@f(4), f(4));
})


test_that("Test .regressoR.learnTrivial three points", {
  f <- function(x) 2 * x + 3 + 5*x*x

  res <- .regressoR.learnTrivial(c(1, 2, 3), c(f(1), f(2), f(3)));
  if(!is.null(res)) {
    expect_is(res, "FittedModel");
    expect_identical(res@f(1), f(1));
    expect_identical(res@f(2), f(2));
    expect_identical(res@f(3), f(3));
    expect_identical(res@f(4), f(4));
    expect_identical(res@f(5), f(5));
  }

  res <- .regressoR.learnTrivial(c(1, 2, 1, 3), c(f(1), f(2), f(1), f(3)));
  if(!(is.null)) {
    expect_is(res, "FittedModel");
    expect_identical(res@f(1), f(1));
    expect_identical(res@f(2), f(2));
    expect_identical(res@f(3), f(3));
    expect_identical(res@f(4), f(4));
    expect_identical(res@f(5), f(5));
  }

})
