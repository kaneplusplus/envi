
context("Create and clone.")

expect_true(set_envi_path(tempdir()))

el <- envi_list()

expect_true(nrow(el) == 0)

expect_true(suppressWarnings(envi_create("test-env-1")))

expect_true(envi_current_handle() == "test-env-1")

el <- envi_list()

library(git2r)

add(envi_env_path(), "*")

commit(envi_env_path(), message = "Initial commit", all)

expect_true(el$handle[1] == "test-env-1")

expect_error(envi_clone(envi_env_path(el$handle[1])))

expect_warning(
  expect_true(envi_clone(envi_env_path("test-env-1"), "test-env-1-clone")))

expect_true(envi_activate("test-env-1-clone"))

commits(envi_env_path())

expect_warning(envi_uninstall("test-env-1"))

envi_uninstall("test-env-1-clone")
