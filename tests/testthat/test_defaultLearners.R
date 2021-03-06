library("regressoR")
context("regressoR.defaultLearners")

test_that("Test regressoR.defaultLearners()", {
  learners <- regressoR.defaultLearners();
  expect_true(!is.null(learners));
  expect_true(is.list(learners));
  expect_gt(length(learners), 0L);

  x <- runif(n=10, min=1, max=10);
  params <- c(5, 7);
  func <- function(x) { params[1] + params[2]*x };
  y <- func(x);

  metric <- regressoR.quality::RegressionQualityMetric.default(x, y);

  for(learner in learners) {
    expect_true(is.function(learner));
    result <- learner(metric, NULL, NULL, NULL, q=0.76);
    expect_true(!is.null(result));
    expect_is(result, "FittedModel");
    validObject(result);
  }
})


test_that("Test regressoR.monotonousLearners()", {
  learners <- regressoR.monotonousLearners();
  expect_true(!is.null(learners));
  expect_true(is.list(learners));
  expect_gt(length(learners), 0L);

  x <- runif(n=10, min=1, max=10);
  params <- c(5, 7);
  func <- function(x) { params[1] + params[2]*x };
  y <- func(x);

  metric <- regressoR.quality::RegressionQualityMetric.default(x, y);

  for(learner in learners) {
    expect_true(is.function(learner));
    result <- learner(metric, NULL, NULL, NULL, q=0.76);
    expect_true(!is.null(result));
    expect_is(result, "FittedModel");
    validObject(result);
  }
})
