
if (!testthat::is_testing()) {
  library(testthat)
  library(devtools)
  document()
} else {
  context("Create and clone.")
}

temp_dir_name <- tempdir()

expect_true(set_envi_path(temp_dir_name))

el <- envi_list()

expect_true(nrow(el) == 0)

expect_true(envi_create("test-env-1"))

expect_true(envi_current_handle() == "test-env-1")


expect_true(envi_create("test-env-2"))

el <- envi_list()

te2_path <- el$path[el$handle == "test-env-2"]

expect_true(envi_uninstall("test-env-2"))

expect_true(is.null(envi_current_handle()))

expect_true(!file.exists(te2_path))

expect_true(purge_envi(confirm = FALSE))

expect_true(!dir.exists(temp_dir_name))
