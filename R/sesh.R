
#' Attached packages and their versions
#'
#' A light-weight `devtools::session_info()`.
#' @return A data frame.
#' @importFrom magrittr "%>%"
#' @export
sesh <- function() {
    devtools::session_info()$packages %>%
        dplyr::filter(`*` == "*", (!grepl("local", source) | package == "base")) %>%
        dplyr::select(package, sesh_v = version, source)
}

#' Save `sesh` output as csv.
#'
#' Works via `devtools::session_info()`.
#'
#' @return Saves a CSV with essential information about loaded packages.
#' @importFrom magrittr "%>%"
#' @examples
#' save_sesh()
#' @export
save_sesh <- function(path = 'sesh_{as.character(Sys.Date())}.csv') {

    file_name <- glue::glue(path)

    sesh() %>%
        readr::write_csv(file_name)

    message(glue::glue('Saved sesh as: {file_name}'))
}


#' Read a sesh.
#'
#' Gives a table from
#'
#' @importFrom magrittr "%>%"
#' @export
read_sesh <- function(path) {
    read <- suppressMessages(readr::read_csv(path))
    if ("version" %in% names(read)) {
        message("Assuming `version` represents `sesh_v`")
        read <- dplyr::rename(read, sesh_v = version)
    }
    return(read)
}

#' Check current conditions against a sesh.
#'
#' The workhorse that checks loaded and installed pacakge versions againstst
#' those specified in sesh.
#'
#' @md
#' @importFrom magrittr "%>%"
#' @param path A character. Valid path to a sesh CSV.
#' @export
check_sesh <- function(path) {

    sesh <- read_sesh(path)

    diff_versions <- devtools::session_info()$packages %>%
        dplyr::filter(`*` == "*", (!grepl("local", source) | package == "base")) %>%
        dplyr::select(package, cur_v = version) %>%
        dplyr::left_join(sesh, ., by = "package") %>%
        dplyr::select(package, dplyr::matches("_v"), source) %>%
        dplyr::filter(cur_v != sesh_v | is.na(cur_v))

    # Look through installed packages (even if they are not loaded)
    already_installed <- diff_versions %>%
        dplyr::filter(is.na(cur_v)) %>%
        dplyr::mutate(cur_v = purrr::map_chr(package, .check_installed))

    diff_versions <- diff_versions %>%
        dplyr::filter(!is.na(cur_v)) %>%
        dplyr::bind_rows(already_installed) %>%
        dplyr::filter(sesh_v != cur_v)

    if (nrow(diff_versions) > 0) {
        message("Current versions mismatched.\nUse `install_sesh()` to install mismatched versions.")
        return(diff_versions)
        }
    else {
        message("Current versions match sesh.")
        return(dplyr::mutate(sesh, cur_v = sesh_v) %>%
                   dplyr::select(package, dplyr::contains("_v")))
        }
}

#' Function to load out a sesh if required versions are installed.
#'
#' @importFrom magrittr "%>%"
#' @export
load_sesh <- function(path) {

    sesh <- suppressMessages(check_sesh(path))

    loadable <- sesh %>%
        filter(sesh_v == cur_v)

    if (nrow(loadable) > 0) {
        pacman::p_load(char = sesh$package)
    }
    dplyr::left_join(sesh, .check_loaded(), by = c("package")) %>%
        dplyr::select(package, sesh_v, loaded_v) %>%
        dplyr::mutate(load_result = ifelse(sesh_v == loaded_v, "Success", "Error"))
}

#' A function to install specific package versions.
#'
#' Works for packages installed from CRAN and GitHub.
#'
#'@md
#'@param path A character. Valid file path to a `save_sesh()` output CSV.
#'@param auth_token A character. Passed to `devtools::install_github()` for accessing
#'private repos.
#'@param ... Arguments passed to `devtools::install()`.
#'@importFrom magrittr "%>%"
#'@export
install_sesh <- function(path,
                         auth_token = devtools::github_pat(quiet),
                         ...) {

    sesh <- suppressWarnings(check_sesh(path))

    # drop already loaded packages if versions match
    needed <- sesh %>%
        # dplyr::mutate(cur_v = purrr::map_chr(package, .check_installed)) %>%
        dplyr::filter(sesh_v != cur_v)

    if (nrow(needed) == 0) {
       message("No installs required.")
    }
    else {

        # GitHub is the easiest
        gh <- needed %>%
            dplyr::filter(grepl("github", source, ignore.case = TRUE))
        if (nrow(gh) > 0) {
            gh <- gh %>%
                dplyr::mutate(repo = gsub(".*\\((.*)\\)", "\\1", source)) %>%
                dplyr::mutate(install_result = purrr::map(repo,
                                purrr::safely(~ devtool::install_github(., auth_token = auth_token, ...))))
        }

        # CRAN is harder
        cran <- needed %>%
            dplyr::filter(grepl("cran|url", source, ignore.case = TRUE))
        if (nrow(cran) > 0) {
            cran <- cran %>%
                dplyr::mutate(install_result = purrr::map2(package, sesh_v,
                                 purrr::safely(function(x, y, ...) devtools::install_version(x, y, ...))))
        }

        needed <- dplyr::bind_rows(gh, cran) %>%
            dplyr::mutate(install_result = .extract_result(install_result))

        # retry (required if updated to current version on CRAN)
        repeats <- needed %>%
            dplyr::filter(install_result == "Error") %>%
            dplyr::mutate(install_result = purrr::map(package,
                                                      purrr::safely(function(x, ...) devtools::install_cran(x, ...))))
        if (nrow(repeats) > 0) {
            repeats %>%
                dplyr::mutate(install_result = .extract_result(install_result)) %>%
                dplyr::bind_rows(dplyr::filter(needed, install_result != "Error")) %>%
                dplyr::select(package, sesh_v, install_result)
        }
        else {
            needed %>%
                dplyr::select(package, sesh_v, install_result)
        }
    }
}

# https://stackoverflow.com/questions/26083625/how-do-you-include-data-frame-output-inside-warnings-and-errors
.print_capture <- function(x) {
    paste0(paste(capture.output(print(x)), collapse = "\n"), "\n")
}

#' a hidden function to parse return from 'safely()'
#' @importFrom magrittr "%>%"
.extract_result <- . %>%
    purrr::map(., purrr::possibly(~ ifelse(is.null(.[["result"]]),
                                           "Error",
                                           "Success"), NULL)) %>%
    unlist()

# returns installed version
.check_installed <- function(package) {
    ip <- installed.packages()

    if (package %in% rownames(ip)) {
        info = ip[package == rownames(ip), ]["Version"]
        # message(glue::glue('{package} v{info} installed :)'))
        return(info)
    }
    else {
        stop(glue::glue('{package} not installed :('))
    }
}

.check_loaded <- function() {
    dplyr::filter(devtools::session_info()$packages, `*` == "*", (!grepl("local", source) | package == "base")) %>%
        dplyr::select(package, loaded_v = version)
}

