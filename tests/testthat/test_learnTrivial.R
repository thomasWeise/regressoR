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
