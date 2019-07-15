context("sesh")

start <- save_sesh("start.csv")
si <- devtools::session_info()
saveRDS(si, "si.RDS")

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
    expect_message(check_sesh("old.csv"), "missing")
})

test_that("converting works", {
    convert_session_info("si.RDS")
    expect_true(file.exists("si.csv"))
    expect_is(read_sesh("si.csv"), "data.frame")
})

# build an impossible version number by hand
read_sesh("start.csv") %>%
    dplyr::mutate(loadedversion = "1000") %>%
    write.csv("bad.csv", row.names = FALSE)

test_that("checking fails", {
    expect_message(check_sesh("bad.csv"), "missing")
})

# library(dplyr)
# save_sesh("end.csv")
# detach("package:dplyr", force = TRUE)
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

rm(si)
file.remove("si.RDS")
file.remove("si.csv")
# clean up / remove created files
# file.remove("old.csv")
# file.remove("start.csv")
# file.remove("end.csv")

