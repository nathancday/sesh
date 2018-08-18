
#' Attached packages and their versions
#'
#' A light-weight `devtools::session_info()`.
#' @return A data frame.
#' @importFrom magrittr "%>%"
#' @export
sesh <- function() {
    devtools::session_info()$packages %>%
        dplyr::filter(`*` == "*", (!grepl("local", source) | package == "base")) %>%
        dplyr::select(package, version, source)
}

#' Save `sesh` output as csv.
#'
#' Renames columns from `sesh()` to work `sesh_check()`.
#'
#' @return Saves a CSV with essential information about loaded packages.
#' @importFrom magrittr "%>%"
#' @examples
#' save_sesh()
#' @export
save_sesh <- function(path = 'sesh_{as.character(Sys.Date())}.csv') {

    file_name <- glue::glue(path)

    sesh() %>%
        dplyr::rename(v = version, s = source) %>%
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

    # want ability to read output from 'session_info$packages %>% data.frame %>% write_csv' too

    if ("version" %in% names(read)) {
        message("Assuming `version` represents `v`")
        read <- dplyr::rename(read, v = version)
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

    past <- read_sesh(path)
    cur <- sesh()

    require_action <- dplyr::full_join(past, cur) %>%
        dplyr::filter(v != version | is.na(version))
    # these pacakges fall into two catergories (installed but not loaded and wrong version installed)

    # check that correct versions just aren't loaded
    ready_to_load <- require_action %>%
        dplyr::filter(is.na(version)) %>%
        dplyr::mutate(installed_version = purrr::map_chr(package, ~ .check_installed(.))) %>%
        dplyr::filter(v == installed_version)

    if (nrow(ready_to_load) > 0) {
        message( glue::glue('These sesh-version / installed-version match:
                           {paste(ready_to_load$package,
                           ready_to_load$v, "/", ready_to_load$installed_version)}
                           call load_sesh() to load them') )
    }

    # versions don't match
    require_install <- require_action %>%
        dplyr::filter(v != version)

    if (nrow(require_install) > 0) {
        message( glue::glue('These sesh-version / installed-version do not match:
                           {paste(require_install$package,
                           require_install$v, "/", require_install$version)}
                           call install_sesh() to safely install') )
    }
}

#' Function to load out a sesh if required versions are installed.
#'
#' @importFrom magrittr "%>%"
#' @export
load_sesh <- function(path) {

    past <- read_sesh(path)
    cur <- sesh()

    require_action <- dplyr::full_join(past, cur) %>%
        dplyr::filter(v != version | is.na(version))

    ready_to_load <- require_action %>%
        dplyr::filter(is.na(version)) %>%
        dplyr::mutate(installed_version = purrr::map_chr(package, ~ .check_installed(.))) %>%
        dplyr::filter(v == installed_version)

    if (nrow(ready_to_load) > 0) {
        pacman::p_load(char = ready_to_load$package)
    }

    require_action %<>%
        dplyr::anti_join(ready_to_load)

    if (nrow(require_action) > 0 ) {
        message( glue::glue('These sesh-version / installed-version do not match:
                           {paste(require_install$package,
                            require_install$v, "/", require_install$version)}
                            call install_sesh() to safely install') )
    }
}

#' A function to install specific package versions in a temporary library.
#'
#' Works for packages installed from CRAN and GitHub. Assumes any GitHub PAT is
#' in `envar GITHUB_PAT`, per `devtools`.
#'
#'@md
#'@param path A character. Valid file path to a `save_sesh()` output CSV.
#'private repos.
#'@param ... Arguments passed to `devtools::install()`.
#'@importFrom magrittr "%>%"
#'@export
install_sesh <- function(path, ...) {

    needed <- suppressWarnings(check_sesh(path))

    if (nrow(needed) == 0) {
        message("No installs required.")
    }


    else {

        # make a throwaway install for sesh
        sesh_lib <- glue::glue('~/.Trash/sesh_{as.character(Sys.Date())}/')

        # maybe add overwrite as argument
        if (dir.exists(sesh_lib)) unlink(sesh_lib, recursive = TRUE)
        # will cause loading problems if old folder exists when installs are performed

        dir.create(sesh_lib)
        message(glue::glue('Installing sesh lib in: {sesh_lib}'))

        # Go after the repos
        # GitHub is easier
        gh <- needed %>%
            dplyr::filter(grepl("github", source, ignore.case = TRUE))
        if (nrow(gh) > 0) {
            gh <- gh %>%
                dplyr::mutate(repo = gsub(".*\\((.*)\\)", "\\1", source)) %>%
                dplyr::mutate(
                    install_result = purrr::map(repo,
                                purrr::safely(
                                    ~ withr:with_libpaths(sesh_lib,
                                                          devtools::install_github(., auth_token = auth_token)))))
        }

        # CRAN is harder
        cran <- needed %>%
            dplyr::filter(grepl("cran|url", source, ignore.case = TRUE))
        if (nrow(cran) > 0) {
            cran <- cran %>%
                dplyr::mutate(install_result = purrr::map2(package, sesh_v,
                                 purrr::safely(function(x, y, ...) withr::with_libpaths(sesh_lib, devtools::install_version(x, y)))))
        }

        needed <- dplyr::bind_rows(gh, cran) %>%
            dplyr::mutate(install_result = .extract_result(install_result))

        # retry (required if needed update is to current CRAN)
        repeats <- needed %>%
            dplyr::filter(install_result == "Error") %>%
            dplyr::mutate(install_result = purrr::map(package,
                                                      purrr::safely(function(x, ...) withr::with_libpaths(sesh_lib, devtools::install_cran(x)))),
                          install_result = .extract_result(install_result))

        needed <- needed %>%
            dplyr::filter(install_result != "Error") %>%
            dplyr::bind_rows(repeats) %>%
            dplyr::select(package, install_result, sesh_lib) %>%
            dplyr::distinct() # not sure if this is really needed

        needed$sesh_lib <- sesh_lib

        read_sesh(path) %>%
            dplyr::full_join(needed) %>%
            readr::write_csv(glue::glue('{path}'))

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

# returns installed version for anything in .libPaths()
.check_installed <- function(package) {
    ip <- installed.packages()

    if (package %in% rownames(ip)) {
        version = ip[package == rownames(ip), "Version"] %>%
            unique()
        # message(glue::glue('{package} v{info} installed :)'))
        return(version)
    }
    else {
        # message(glue::glue('{package} not installed :('))
        return(NA_character_)
    }
}

.check_loaded <- function() {
    dplyr::filter(devtools::session_info()$packages, `*` == "*", (!grepl("local", source) | package == "base")) %>%
        dplyr::select(package, loaded_v = version)
}


