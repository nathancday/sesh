# shorthand for package development

si <- function(...) {dplyr::filter(devtools::session_info(...)$packages, `*` == "*")}
