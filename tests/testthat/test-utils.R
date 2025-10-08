test_that("get mode works", {
  x <- c("a", "b", "c", "a")
  expect_equal(get_mode(x), "a")
})

test_that("get mode works", {
  x <- c("a", "b", "c", "a", "b")
  expect_equal(get_mode(x), "a")
})

test_that("get mode works", {
  x <- c(1, 2, 3, 1)
  expect_equal(get_mode(x), 1)
})

test_that("species found", {
  expect_equal(assign.common.name("SB"), "black spruce")
})

test_that("species dont exist", {
  expect_equal(assign.common.name("SBB"), "NA")
})
