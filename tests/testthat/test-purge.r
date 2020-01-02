
context("Purge an environment")

library(renv)

temp_dir_name <- tempdir()

expect_true(set_envi_path(temp_dir_name))

el <- envi_list()

expect_true(nrow(el) == 0)

expect_true(suppressWarnings(envi_init("test-env-1")))

expect_true(envi_current_handle() == "test-env-1")

expect_true(envi_deactivate(snapshot = FALSE))

expect_true(purge_envi(confirm = FALSE))

envi_globals <- new.env(parent=emptyenv())

# expect_true(purge_envi(confirm = FALSE))

# expect_true(!dir.exists(temp_dir_name))

suppressWarnings(envi_deactivate())
