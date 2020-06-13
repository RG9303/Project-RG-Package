# source("fars_functions.R")

# check csv files are in the package

# test_that("test number of columes are 50", {
#
#   file = paste0(system.file("vignettes", package = "ProjectRGGit"), "/",make_filename(2013))
#
#   expect_equal(ncol(fars_read(filename)), 50)
#
# })


devtools::test("C:/Users/rubis/Desktop/GitHubR/ProjectRGGit/tests")

test_that("test number of columes are 50", {

  filename = paste0(system.file("vignettes", package = "ProjectRGGit"), "/",make_filename(2013))

  expect_equal(ncol(fars_read(filename)), 50)

})

#
# test_that("csv data exist", {
#
#   expect_equal(list.files(system.file("extdata", package = "Project-RG-Package")),
#
#                c("accident_2013.csv.bz2",
#
#                  "accident_2014.csv.bz2",
#
#                  "accident_2015.csv.bz2"))
#
#
#
# })
