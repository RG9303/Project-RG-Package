library(testthat)
# library(ProjectRGGit)
#
# test_check("ProjectRGGit")

devtools::test("C:/Users/rubis/Desktop/GitHubR/ProjectRGGit/tests/testthat")

test_that("test number of columes are 50", {

  file = paste0(system.file("vignettes", package = "ProjectRGGit"), "/",make_filename(2013))

  expect_equal(ncol(fars_read(file)), 50)

})
