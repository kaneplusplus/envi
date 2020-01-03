
context("Create and clone.")

expect_true(is.null(envi_env_path(NA)))

expect_true(set_envi_path(tempdir()))

el <- envi_list()

expect_true(nrow(el) == 0)

expect_true(suppressWarnings(envi_init("test-env-1")))

expect_error(envi_activate("not-a-real-environment"))

expect_true(envi_current_handle() == "test-env-1")

el <- envi_list()

git2r::add(envi_env_path(), "*", force = TRUE)

git2r::config(repository(envi_env_path()), 
       user.name="tester", user.email="tester@testthat.org")

commit(envi_env_path(), message = "Initial commit", all = TRUE)

expect_true(el$handle[1] == "test-env-1")

expect_error(envi_clone(envi_env_path(el$handle[1])))

expect_warning(
  expect_true(envi_clone(envi_env_path("test-env-1"), "test-env-1-clone")))

expect_true(envi_activate("test-env-1-clone"))

commits(envi_env_path())

expect_warning(envi_uninstall("test-env-1"))

envi_uninstall("test-env-1-clone")

td <- file.path(tempdir(), "not-an-environment")
if (!dir.exists(td)) {
  dir.create(td)
}

git2r::init(td)
file.create(file.path(td, "touch.txt"))
writeLines("a file with text", file.path(td, "touch.txt"))
git2r::add(td, file.path(td, "touch.txt"))
git2r::config(
  git2r::repository(td), user.name="tester", user.email="tester@testthat.org")
try(commit(td, message = "Initial commit.", all = TRUE))

expect_warning(expect_false(envi_clone(td, "bunk-clone")))

expect_warning(envi_deactivate("bunk-clone"))
