library(testthat)
library(sesh)

test_check("sesh")

library(devtools)
library(tidyverse)
library(assayr)

tst <- session_info()$packages
tst <- tst %>% filter(package %in% c("dplyr", "tidyr")
                      # dummy cran test with ggplot2 and tidyr
                      # tst <- tibble(package = c("ggplot2", "tidyr"),
                      #                      source = "cran",
                      #                      version = c("3.2.1", "0.8.0"))

install_sesh(tst, auth_token = Sys.getenv("GH"), dep = F)
