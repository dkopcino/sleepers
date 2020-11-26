#library(sleepers)
context("Testing polr helpers")

test_that("inverse logit", {
  expect_equal(round(invlogit(2.3), 3), 0.909)
  expect_equal(invlogit(0), 0.5)
  expect_equal(length(invlogit(c(0, 2.3))), 2)
})

library(MASS)
test_that("toprobabilities", {
  house.plr <- polr(Sat ~ Infl + Type + Cont, weights = Freq, data = housing)
  tps = round(toprobabilities(invlogit(house.plr$zeta-coefficients(house.plr)["InflMedium"]), levels(housing$Sat)), 3)
  expect_equal(sum(tps == c(0.257, 0.274, 0.469)), 3)
})

