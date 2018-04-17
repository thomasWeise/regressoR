library("regressoR")
context("regressoR.RegressionResults")

library(regressoR)

test_that("Test RegressionResults", {
  res <- new("RegressionResults",
            names=c("a", "b"),
            results=list(
              RegressionResult.new(12),
              RegressionResult.new(13)
            ));
  expect_true(!is.null(res));
  expect_is(res, "RegressionResults");
  validObject(res);
})


