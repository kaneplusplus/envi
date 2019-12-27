
context("Environment package information")

expect_true(set_envi_path(tempdir()))

el <- envi_list()

expect_true(nrow(el) == 0)

expect_true(envi_create("test-env-1"))

expect_true(envi_current_handle() == "test-env-1")

el <- envi_list(env_info = TRUE)

envi_env_info("test-env-1")

unlink(get_envi_path(), recursive = TRUE, force = TRUE)
