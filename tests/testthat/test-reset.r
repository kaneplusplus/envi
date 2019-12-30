
context("Reset and environment")

temp_dir_name <- tempdir()

expect_true(set_envi_path(temp_dir_name))

library(git2r)

vapply(envi_list()$handle, function(handle) envi_uninstall(handle), NA)

expect_true(suppressWarnings(envi_create("test-env-1")))

writeLines("test-file-1", file.path(envi_env_path(), 'test-file-1.txt'))
init(envi_env_path())
add(envi_env_path(), "*")
commit(envi_env_path(), message = "Initial commit", all = TRUE)

envi_deactivate()

envi_clone(envi_env_path("test-env-1"), "test-env-1-clone")

envi_activate("test-env-1-clone")

writeLines("test-file-1-clone", file.path(envi_env_path(), 'test-file-1.txt'))
writeLines("test-file-1-clone", 
  file.path(envi_env_path(), 'test-file-1-clone.txt'))

expect_warning(expect_true(envi_hard_reset(clean = FALSE, confirm = FALSE)))

expect_true(
  readLines(file.path(envi_env_path('test-env-1-clone'), 'test-file-1.txt')) == 
    "test-file-1")

expect_true(file.exists(
  file.path(envi_env_path("test-env-1-clone"), "test-file-1-clone.txt")))

envi_activate("test-env-1-clone")

expect_warning(expect_true(envi_hard_reset(clean = TRUE, confirm = FALSE)))

expect_false(file.exists(file.path(envi_env_path("test-env-1-clone"), 
  "test-file-1-clone.txt")))

vapply(envi_list()$handle, function(handle) envi_uninstall(handle), NA)
