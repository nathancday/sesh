#' Call devtools::document()
#'
#' Fixes broken RStudio short cut CMD+SHIFT+S
#' @importFrom rstudioapi sendToConsole
#' @export
executeDevtoolsDocument <- function() {
    sendToConsole("devtools::document()")
}
