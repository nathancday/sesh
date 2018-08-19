#' Call devtools::document()
#'
#' Fixes broken RStudio short cut CMD+SHIFT+S
#' @export
executeDevtoolsDocument <- function() {
    rstudioapi::sendToConsole("devtools::document()")
}
