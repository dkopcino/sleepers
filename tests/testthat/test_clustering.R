#library(sleepers)
context("Testing clustering helpers")

test_that("elbow_method", {
  elbow_method(data.frame(x = sample(1:100, 50), y = sample(1:100, 100), z = sample(1:100, 100)),
               k = 5,
               method = "kmeans")
  elbow_method(data.frame(x = sample(1:100, 50), y = sample(1:100, 100), z = sample(1:100, 100)),
               k = 5,
               method = "hclust")
  elbow_method(data.frame(x = sample(1:100, 50), y = sample(1:100, 100), z = sample(1:100, 100)),
               k = 5,
               method = "blabla")
  expect_error(
    elbow_method(data.frame(),
                 k = 5,
                 method = "kmeans")
  )
  expect_error(
    elbow_method(data.frame(),
                 k = 5,
                 method = "hclust")
  )
  expect_error(
    elbow_method(data.frame(x = sample(1:100, 50), y = sample(1:100, 100), z = sample(1:100, 100)),
                 k = 0,
                 method = "kmeans")
  )
})

