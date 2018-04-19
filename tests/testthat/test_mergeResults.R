library("regressoR")
context("regressoR.mergeResults")

library(regressoR)

test_that("Test regressoR.mergeResults", {
  lst <- list(RegressionResult.new(12),
              RegressionResult.new(13));
  results <- c(RegressionResults.new(
                names=c("a", "1", "Z"),
                results=lst),
               RegressionResults.new(
                 names=c("b", "1", "Z"),
                 results=lst),
               RegressionResults.new(
                 names=c("c", "1", "Z"),
                 results=lst),
               RegressionResults.new(
                 names=c("a", "2", "Z"),
                 results=lst),
               RegressionResults.new(
                 names=c("b", "2", "Z"),
                 results=lst),
               RegressionResults.new(
                 names=c("c", "2", "Z"),
                 results=lst),
               RegressionResults.new(
                 names=c("a", "1", "Y"),
                 results=lst),
               RegressionResults.new(
                 names=c("b", "1", "Y"),
                 results=lst),
               RegressionResults.new(
                 names=c("c", "1", "Y"),
                 results=lst),
               RegressionResults.new(
                 names=c("a", "2", "Y"),
                 results=lst),
               RegressionResults.new(
                 names=c("b", "2", "Y"),
                 results=lst),
               RegressionResults.new(
                 names=c("c", "2", "Y"),
                 results=lst),
               RegressionResults.new(
                   names=c("a", "1", "X"),
                   results=lst),
               RegressionResults.new(
                 names=c("b", "1", "X"),
                 results=lst),
               RegressionResults.new(
                 names=c("c", "1", "X"),
                 results=lst),
               RegressionResults.new(
                 names=c("a", "2", "X"),
                 results=lst),
               RegressionResults.new(
                 names=c("b", "2", "X"),
                 results=lst),
               RegressionResults.new(
                 names=c("c", "2", "X"),
                 results=lst));

  expect_identical(regressoR.mergeResults(results, identity), results);

  r <- regressoR.mergeResults(results, function(n) n[[1]]);
  expect_length(r, 3);
  expect_identical(r[[1]]@names, "a");
  expect_length(r[[1]]@results, 2*6);
  expect_identical(r[[2]]@names, "b");
  expect_length(r[[2]]@results, 2*6);
  expect_identical(r[[3]]@names, "c");
  expect_length(r[[3]]@results, 2*6);

  r <- regressoR.mergeResults(results, function(n) n[[2]]);
  expect_length(r, 2);
  expect_identical(r[[1]]@names, "1");
  expect_length(r[[1]]@results, 2*9);
  expect_identical(r[[2]]@names, "2");
  expect_length(r[[2]]@results, 2*9);

  r <- regressoR.mergeResults(results, function(n) n[[3]]);
  expect_length(r, 3);
  expect_identical(r[[1]]@names, "Z");
  expect_length(r[[1]]@results, 2*6);
  expect_identical(r[[2]]@names, "Y");
  expect_length(r[[2]]@results, 2*6);
  expect_identical(r[[3]]@names, "X");
  expect_length(r[[3]]@results, 2*6);

  r <- regressoR.mergeResults(results, function(n) n[c(1,2)]);
  expect_length(r, 6);
  expect_identical(r[[1]]@names, c("a", "1"));
  expect_length(r[[1]]@results, 2*3);
  expect_identical(r[[2]]@names, c("b", "1"));
  expect_length(r[[2]]@results, 2*3);
  expect_identical(r[[3]]@names, c("c", "1"));
  expect_length(r[[3]]@results, 2*3);
  expect_identical(r[[4]]@names, c("a", "2"));
  expect_length(r[[4]]@results, 2*3);
  expect_identical(r[[5]]@names, c("b", "2"));
  expect_length(r[[5]]@results, 2*3);
  expect_identical(r[[6]]@names, c("c", "2"));
  expect_length(r[[6]]@results, 2*3);

  r <- regressoR.mergeResults(results, function(n) n[c(1,3)]);
  expect_length(r, 9);
  expect_identical(r[[1]]@names, c("a", "Z"));
  expect_length(r[[1]]@results, 2*2);
  expect_identical(r[[2]]@names, c("b", "Z"));
  expect_length(r[[2]]@results, 2*2);
  expect_identical(r[[3]]@names, c("c", "Z"));
  expect_length(r[[3]]@results, 2*2);
  expect_identical(r[[4]]@names, c("a", "Y"));
  expect_length(r[[4]]@results, 2*2);
  expect_identical(r[[5]]@names, c("b", "Y"));
  expect_length(r[[5]]@results, 2*2);
  expect_identical(r[[6]]@names, c("c", "Y"));
  expect_length(r[[6]]@results, 2*2);
  expect_identical(r[[7]]@names, c("a", "X"));
  expect_length(r[[7]]@results, 2*2);
  expect_identical(r[[8]]@names, c("b", "X"));
  expect_length(r[[8]]@results, 2*2);
  expect_identical(r[[9]]@names, c("c", "X"));
  expect_length(r[[9]]@results, 2*2);

  r <- regressoR.mergeResults(results, function(n) n[c(3,1)]);
  expect_length(r, 9);
  expect_identical(r[[1]]@names, c("Z", "a"));
  expect_length(r[[1]]@results, 2*2);
  expect_identical(r[[2]]@names, c("Z", "b"));
  expect_length(r[[2]]@results, 2*2);
  expect_identical(r[[3]]@names, c("Z", "c"));
  expect_length(r[[3]]@results, 2*2);
  expect_identical(r[[4]]@names, c("Y", "a"));
  expect_length(r[[4]]@results, 2*2);
  expect_identical(r[[5]]@names, c("Y", "b"));
  expect_length(r[[5]]@results, 2*2);
  expect_identical(r[[6]]@names, c("Y", "c"));
  expect_length(r[[6]]@results, 2*2);
  expect_identical(r[[7]]@names, c("X", "a"));
  expect_length(r[[7]]@results, 2*2);
  expect_identical(r[[8]]@names, c("X", "b"));
  expect_length(r[[8]]@results, 2*2);
  expect_identical(r[[9]]@names, c("X", "c"));
  expect_length(r[[9]]@results, 2*2);

  r <- regressoR.mergeResults(results, function(n) n[c(1,2,3)]);
  expect_equal(results, r);
})


