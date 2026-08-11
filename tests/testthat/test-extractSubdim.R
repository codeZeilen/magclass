test_that("extractSubdim extracts the requested subdimension", {
  expect_identical(extractSubdim(c("a.b.c", "x.y.z"), 1), c("a", "x"))
  expect_identical(extractSubdim(c("a.b.c", "x.y.z"), 2), c("b", "y"))
  expect_identical(extractSubdim(c("a.b.c", "x.y.z"), 3), c("c", "z"))
})

test_that("extractSubdim leaves elements with too few subdimensions unchanged", {
  expect_identical(extractSubdim(c("a.b", "c"), 2), c("b", "c"))
  expect_identical(extractSubdim(c("a.b", "c"), 3), c("a.b", "c"))
})

test_that("extractSubdim handles empty subdimensions and trailing dots", {
  expect_identical(extractSubdim("a..b", 1), "a")
  expect_identical(extractSubdim("a..b", 2), "")
  expect_identical(extractSubdim("a..b", 3), "b")
  expect_identical(extractSubdim(c("a.b.", "x.y.z"), 3), c("", "z"))
})

test_that("extractSubdim handles edge cases", {
  expect_identical(extractSubdim(character(0), 1), character(0))
  expect_identical(extractSubdim("", 1), "")
  expect_identical(extractSubdim(NA_character_, 1), NA_character_)
  expect_identical(extractSubdim(c(NA, "a.b"), 2), c(NA, "b"))
})
