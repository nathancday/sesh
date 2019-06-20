context("sesh")

start <- save_sesh("start.csv")

test_that("saving works",{
    expect_message(save_sesh("start.csv"), "Saved")
})
test_that("saving fails", {
    expect_error(save_sesh(devtools::sessionInfo()))
})
test_that("reading words", {
    expect_is(read_sesh("end.csv"), "data.frame")
})
test_that("reading fails", {
    expect_error(read_sesh("not a path"))
})

test_that("checking works", {
    expect_message(check_sesh("start.csv"), "match")
})

# build an impossible version number by hand
read_sesh("start.csv") %>%
    dplyr::select(-v) %>%
    dplyr::mutate(version = 1000) %>%
    readr::write_csv("bad.csv")

test_that("checking fails", {
    expect_message(check_sesh("bad.csv"), "mismatched")
})

library(tidyr)
save_sesh("end.csv")
pacman::p_unload("tidyr")

test_that("loading works", {
    dat <- load_sesh("end.csv")
    expect_equal(dat$sesh_v, dat$loaded_v)
})

test_that("loading fails", {
    dat <- load_sesh("bad.csv")
    expect_false(all(dat$sesh_v == dat$loaded_v))
})

# old ggplot2
read_sesh("end.csv") %>%
    dplyr::mutate(sesh_v = ifelse(package == "tidyr", "0.8.0", sesh_v)) %>%
    readr::write_csv("old.csv")

test_that("install works", {
    install_sesh("old.csv")
})

# clean up / remove created files
# file.remove("old.csv")
# file.remove("start.csv")
# file.remove("end.csv")

