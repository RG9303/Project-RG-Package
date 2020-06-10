# source("fars_functions.R")

# check csv files are in the package

test_that("test number of columes are 50", {

  file = paste0(system.file("vignettes", package = "ProjectRGGit"), "/",make_filename(2013))

  expect_equal(ncol(fars_read(file)), 50)

})
