source("fars_functions.R")

test_that("fars_read", {
  roots <- ProjectRGGit::fars_read(filename = "accident_2013.csv.bz2")
  expect_that( roots, is_a("character") )
})

test_that("make_filename", {
  roots <- make_filename(year = 2013)
  expect_that( roots, is_a("character") )
})

test_that("fars_read_years", {
  roots <- fars_read_years(years = 2013)
  expect_that( roots, is_a("list") )
})

test_that("fars_summarize_years", {
  roots <- fars_summarize_years(years = 2013)
  expect_that( roots, is_a("data.frame") )
})

test_that("fars_map_state", {
  roots <- fars_map_state(state.num = 19, year = 2013)
  expect_that( roots, is_a("data.frame") )
})



