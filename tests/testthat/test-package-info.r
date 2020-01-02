
context("Environment package information")

library(renv)

expect_true(set_envi_path(tempdir()))

el <- envi_list()

expect_true(nrow(el) == 0)

expect_true(suppressWarnings(envi_init("test-env-1")))

expect_true(envi_current_handle() == "test-env-1")

el <- envi_list(env_info = TRUE)

envi_env_info("test-env-1")

expect_warning(expect_true(envi_uninstall("test-env-1")))

expect_error(envi_env_info("this-is-not-an-environment"))

suppressWarnings(envi_deactivate())
