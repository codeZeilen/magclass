a <- maxample("animal")
attr(a, "Metadata") <- NULL

test_that("addDim works", {
  aMini <- a[1, 1:3, 1:2]
  attr(aMini, ".internal.selfref") <- NULL
  ref1 <- new("magpie",
              .Data = structure(c(0, 0, 0, 6, 7, 9),
                                .Dim = c(1L, 3L, 2L),
                                .Dimnames = list(x.y.country.cell = "5p75.53p25.NLD.14084",
                                                 year.month.day = c("y2000.april.20", "y2000.may.20", "y2000.june.20"),
                                                 new.type.species.color = c("dummy.animal.rabbit.black",
                                                                            "dummy.animal.rabbit.white"))))
  expect_identical(addDim(aMini), ref1)
  ref2 <- new("magpie",
              .Data = structure(c(0, 0, 0, 6, 7, 9),
                                .Dim = c(1L, 3L, 2L),
                                .Dimnames = list(x.y.country.cell = "5p75.53p25.NLD.14084",
                                                 year.new.month.day = c("y2000.dummy.april.20", "y2000.dummy.may.20",
                                                                        "y2000.dummy.june.20"),
                                                 type.species.color = c("animal.rabbit.black",
                                                                        "animal.rabbit.white"))))
  expect_identical(addDim(aMini, dim = 2.2), ref2)
  ref3 <- new("magpie",
              .Data = structure(c(0, 0, 0, 0, 0, 0, 6, 7, 9, 6, 7, 9),
                                .Dim = c(1L, 6L, 2L),
                                .Dimnames = list(x.y.country.cell = "5p75.53p25.NLD.14084",
                                                 year.new.month.day = c("y2000.d1.april.20", "y2000.d1.may.20",
                                                                        "y2000.d1.june.20", "y2000.d2.april.20",
                                                                        "y2000.d2.may.20",  "y2000.d2.june.20"),
                                                 type.species.color = c("animal.rabbit.black",
                                                                        "animal.rabbit.white"))))
  expect_identical(addDim(aMini, dim = 2.2, item = paste0("d", 1:2)), ref3)

  a0 <- dimSums(a[, 1, 1], dim = 1)
  ref4 <- new("magpie", .Data = structure(190, .Dim = c(1L, 1L, 1L),
                                          .Dimnames = list(new = "dummy", year.month.day = "y2000.april.20",
                                                           type.species.color = "animal.rabbit.black")))
  expect_identical(addDim(a0, dim = 1), ref4)
  expect_identical(addDim(a0, dim = 1.2), ref4)

  p <- maxample("pop")
  expect_error(addDim(p, dim = 3.2, dimName = "scenario"), "Dimension .* does already exist")
  expect_silent(p <- addDim(p, 3.1))
  expect_silent(p <- addDim(p, 3.2))
  expect_identical(getSets(p, fulldim = FALSE)[3], "new.new1.scenario")
  expect_silent(p <- addDim(p, 3.4))
  expect_identical(getSets(p, fulldim = FALSE)[3], "new.new1.scenario.new2")

  p <- addDim(dimSums(p, 3), item = c("a.b", "c.d"))
  expect_identical(getItems(p, 3), c("a.b", "c.d"))
})

test_that("addDim's expand argument works", {
  p <- maxample("pop")
  p <- addDim(dimSums(p, 3), item = c("a.b", "c.d"))
  p2 <- addDim(p, item = c("e", "f"), expand = FALSE)
  expect_identical(getItems(p2, 3), c("e.a.b", "f.c.d"))
  p3 <- addDim(p, item = c("e", "f"), expand = TRUE)
  expect_identical(getItems(p3, 3), c("e.a.b", "e.c.d", "f.a.b", "f.c.d"))
})

test_that("addDim works for objects with inconsistent set information", {
  a <- maxample("animal")
  expect_silent(b <- addDim(a))
  expect_identical(getItems(b, 3.1), "dummy")
})
