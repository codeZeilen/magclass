
p <- maxample("pop")

test_that("clean_magpie works", {
  expect_identical(clean_magpie(p), p)
  expect_error(clean_magpie(p, what = "themess"), "Unknown setting")
  names(dimnames(p)) <- NULL
  expect_identical(getSets(clean_magpie(p)), c(d1.1 = "region", d2.1 = "year", d3.1 = "data"))
  p0 <- dimSums(maxample("pop"), dim = 1)
  expect_silent(clean_magpie(p0))
  p <- maxample("pop")
  names(dimnames(p)[[1]]) <- paste0("n", 1:10)
  expect_silent(clean_magpie(p))
})

test_that("clean_magpie 'cells' produces a clean set name", {
  x <- new.magpie(paste0(c("AFR", "CPA", "EUR"), ".", 1:3), 2000, "a")
  getSets(x)[1:2] <- c("region", "cell")
  expected <- new.magpie(c("AFR", "CPA", "EUR"), 2000, "a")
  expect_identical(getItems(clean_magpie(x, "cells"), dim = 1), c("AFR", "CPA", "EUR"))
  expect_identical(getSets(clean_magpie(x, "cells")), c(d1.1 = "region", d2.1 = "year", d3.1 = "data"))
  expect_identical(getSets(clean_magpie(x, "all")), c(d1.1 = "region", d2.1 = "year", d3.1 = "data"))
})

test_that("clean_magpie 'cells' leaves non-regional objects untouched", {
  x <- new.magpie(c("AFR.1", "AFR.2"), 2000, "a")
  expect_identical(clean_magpie(x, "cells"), x)
})

test_that("clean_magpie 'items' still fills empty subdimensions", {
  x <- new.magpie("GLO..x", 2000, c("a..b", ".c", "d."))
  y <- clean_magpie(x, "items")
  expect_identical(getItems(y, dim = 1), "GLO. .x")
  expect_identical(getItems(y, dim = 3), c("a. .b", " .c", "d. "))
})
