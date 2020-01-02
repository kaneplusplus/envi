library(testthat)
library(envi)
library(renv)
library(cli)
library(git2r)

options(renv.testing = TRUE)

test_check("envi")
