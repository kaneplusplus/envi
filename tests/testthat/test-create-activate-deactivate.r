
context("Create, activate, and deactive.")

expect_true(set_envi_path(tempdir()))

el <- envi_list()

expect_true(nrow(el) == 0)

expect_true(
  envi_create("test-env-1", "testing-environment-number-1", bare = FALSE))

expect_true(envi_current_handle() == "test-env-1")

el <- envi_list()

expect_true(el$handle[1] == "test-env-1")

expect_true(basename(el$path[1]) == "testing-environment-number-1")

expect_warning(envi_create("test-env-2", bare = TRUE))

expect_true(envi_current_handle() == "test-env-2")

el <- envi_list()

expect_true(el$handle[2] == "test-env-2")

expect_true(basename(el$path[2]) == "test-env-2")

envi_deactivate(confirm = FALSE)

expect_true(is.null(envi_current_handle()))

if (testthat::is_testing()) {
  context("Source control.")
}

library(git2r)

expect_true(envi_create("test-env-3", bare = TRUE, git_init = FALSE))

el <- envi_list()

expect_true(el$handle[3] == "test-env-3")

expect_true(basename(el$path[3]) == "test-env-3")

envi_deactivate(confirm = FALSE)

envi_uninstall("test-env-1")
envi_uninstall("test-env-2")
envi_uninstall("test-env-3")

#unlink(get_envi_path(), recursive = TRUE, force = TRUE)
