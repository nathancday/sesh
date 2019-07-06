context("sesh")

start <- save_sesh("start.csv")

test_that("saving works",{
    expect_message(save_sesh("start.csv"), "Saved")
})
test_that("reading words", {
    expect_is(read_sesh("start.csv"), "data.frame")
})
test_that("reading fails", {
    expect_error(supressWarning(read_sesh("not a path")))
})

test_that("checking works", {
    expect_message(check_sesh("old.csv"), "installed")
    expect_message(check_sesh("old.csv"), "missing")
})

# build an impossible version number by hand
read_sesh("start.csv") %>%
    dplyr::mutate(loadedversion = "1000") %>%
    write.csv("bad.csv", row.names = FALSE)

test_that("checking fails", {
    expect_message(check_sesh("bad.csv"), "missing")
})

# library(tidyr)
# save_sesh("end.csv")
# detach("package:tidyr", force = TRUE)
end <- read_sesh("end.csv")

test_that("loading works", {
    expect_message(load_sesh("end.csv"), "Loading")
    cur <- sesh()
    expect_equal(end$loadedversion[end$package == "tidyr"],
                 cur$loadedversion[cur$package == "tidyr"])
})

test_that("loading fails", {
    expect_message(load_sesh("bad.csv"), "do not match")
})

test_that("install works", {
    skip_on_travis()
    install_sesh("test.csv")
})

# clean up / remove created files
# file.remove("old.csv")
# file.remove("start.csv")
# file.remove("end.csv")

