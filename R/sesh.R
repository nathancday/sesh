
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

    suppressMessages(
        require_action <- dplyr::full_join(past, cur) %>%
            dplyr::filter( v != version | is.na(version) )
    )

    if ( !"sesh_lib" %in% names(require_action) ) {
        require_action <- require_action %>%
            dplyr::mutate(sesh_lib = NA)
    }
    require_action <- require_action %>%
        dplyr::filter(is.na(sesh_lib))

    # these pacakges fall into two catergories (installed but not loaded and wrong version installed)

    if (nrow(require_action) == 0 ) {
        message("Loaded versions match sesh!")
    }

    # check that correct versions just aren't loaded
    ready_to_load <- require_action %>%
        dplyr::filter(is.na(version)) %>%
        dplyr::mutate(installed_version = purrr::map2_chr(package, v, ~ .check_installed(.x, .y))) %>%
        dplyr::filter(v == installed_version)

    if (nrow(ready_to_load) > 0) {
        message('These sesh-version / installed-version match:')
        .print_capture( paste(ready_to_load$package,
                           ready_to_load$v, "/", ready_to_load$installed_version ) )
        message("call install_sesh() to safely install")
    }

    # versions don't match
    suppressMessages(
        require_install <- require_action %>%
        dplyr::anti_join(ready_to_load)
    )

    if (nrow(require_install) > 0) {
        message('These sesh_version / installed_version do not match:')
        .print_capture(
            require_install %>%
                dplyr::select(package, sesh_version = v, installed_version = version)
        )
        message("call install_sesh() to safely install")
    }
}

#' Function to load out a sesh if required versions are installed.
#'
#' @importFrom magrittr "%>%"
#' @importFrom magrittr "%<>%"
#' @export
load_sesh <- function(path) {

    past <- read_sesh(path)
    cur <- sesh()

    suppressMessages(
        require_action <- dplyr::full_join(past, cur) %>%
        dplyr::filter(v != version | is.na(version))
    )

    if (nrow(require_action) == 0 ) {
        message("Loaded versions match sesh!")
    }

    sesh_to_load <- require_action %>%
        dplyr::filter(!is.na(sesh_lib))

    if (nrow(sesh_to_load) > 0) {
        message(glue::glue('Loading from sesh library: { paste(sesh_to_load$package, collapse = ", ") }'))

        purrr::walk2(sesh_to_load$package, sesh_to_load$sesh_lib,
                     function(x, y) {
                         unloadNamespace(paste0("package:", x)) # detach package if loaded
                         library(x, lib.loc = y, character.only = TRUE)
                     } )
    }

    ready_to_load <- require_action %>%
        dplyr::filter(is.na(sesh_lib)) %>%
        dplyr::mutate(installed_lib = purrr::map2_chr(package, v, ~.check_installed(.x, .y))) %>%
        dplyr::filter( !is.na(installed_lib) )

    if (nrow(ready_to_load) > 0) {
        message(glue::glue('Loading: { paste(ready_to_load$package, collapse = ", ") }'))

        purrr::walk2(ready_to_load$package, ready_to_load$installed_lib,
              function(x, y) {
                  unloadNamespace(paste0("package:", x)) # detach package if loaded
                  library(x, lib.loc = y, character.only = TRUE)
              } )
    }

    suppressMessages(
        require_action %<>%
        dplyr::anti_join(ready_to_load) %>%
        dplyr::anti_join(sesh_to_load)
    )

    if (nrow(require_action) > 0 ) {
        message( glue::glue('These sesh-version / installed-version do not match:
                           {paste(require_install$package,
                            require_install$v, "/", require_install$version)}
                            call install_sesh() to safely install'))
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

    past <- read_sesh(path)
    cur <- sesh()

    suppressMessages(
        require_action <- dplyr::full_join(past, cur) %>%
            dplyr::filter(v != version | is.na(version))
    )

    if (nrow(require_action) == 0 ) {
        message("No installs required")
    }
    else {

        loadable <- require_action %>%
            dplyr::filter(is.na(version)) %>%
            dplyr::mutate(installed_version = purrr::map2_chr(package, v, ~ .check_installed(.x, .y))) %>%
            dplyr::filter(v == installed_version)

        if (nrow(loadable) == nrow(require_action)) {
            message("Sesh versions already installed, use `load_sesh()` to attach.")
            return()
        }

        # make a throwaway install for sesh
        sesh_lib <- glue::glue('~/.Trash/sesh_{as.character(Sys.Date())}/')

        # maybe add overwrite as argument
        if (dir.exists(sesh_lib)) unlink(sesh_lib, recursive = TRUE)
        # will cause loading problems if old folder exists when installs are performed

        dir.create(sesh_lib)
        message( glue::glue('Installing sesh lib in: {sesh_lib}') )

        # Go after the repos
        # GitHub is easier
        gh <- require_action %>%
            dplyr::filter(grepl("github", s, ignore.case = TRUE))

        if (nrow(gh) > 0) {
            gh <- gh %>%
                dplyr::mutate(repo = gsub(".*\\((.*)\\)", "\\1", source)) %>%
                dplyr::mutate(
                    install_result = purrr::map(repo,
                                purrr::safely(
                                    ~ withr:with_libpaths(sesh_lib,
                                                          devtools::install_github(., auth_token = auth_token)))),
                    install_result = .extract_result(install_result) )
        }

        # CRAN is harder
        cran <- require_action %>%
            dplyr::filter(grepl("cran|url", s, ignore.case = TRUE))

        if (nrow(cran) > 0) {
            cran <- cran %>%
                dplyr::mutate(
                    install_result = purrr::map2(package, v,
                                 purrr::safely(function(x, y, ...) withr::with_libpaths(sesh_lib, devtools::install_version(x, y, reload = FALSE)))),
                    install_result = .extract_result(install_result) )
        }

        needed <- dplyr::bind_rows(gh, cran)

        # (required if needed update is to current CRAN)
        cran_retries <- needed %>%
            dplyr::filter(install_result == "Error")

        if (nrow(cran_retries) > 0) {
             repeats <- cran_retries %>%
                dplyr::mutate(install_result = purrr::map(package,
                                                      purrr::safely(function(x, ...) withr::with_libpaths(sesh_lib, devtools::install_cran(x)))),
                          install_result = .extract_result(install_result))

             gotten <- needed %>%
                 dplyr::bind_rows(repeats) %>%
                 dplyr::filter(install_result != "Error")
        }
        else {
            gotten <- needed %>%
                dplyr::filter(install_result != "Error")
        }

        gotten %<>%
            dplyr::select(package, install_result) %>%
            dplyr::distinct() %>% # not sure if this is really needed
            dplyr::mutate(sesh_lib = sesh_lib)

        # gotten %>%
        #     purrr::walk2(.$package, .$sesh_lib,
        #                  function(x, y) {
        #                      unloadNamespace(paste0("package:", x)) # detach package if loaded
        #                      library(x, lib.loc = y, character.only = TRUE)
        #                  } )

        message(glue::glue('{nrow(gotten)} succesful install of {nrow(needed)} needed package.
                           Adding sesh lib and resaving CSV ...'))

        suppressMessages(
            read_sesh(path) %>%
            dplyr::full_join(gotten) %>%
            readr::write_csv(glue::glue('{path}'))
        )

        message("Use `sesh_load()` to attach sesh versions.")
    }
}

#' Unload all sesh packages
#'
#' Useful for going back to gloabal versions when leaving a sesh.
#'
#' @export
unload_sesh <- function(path) {
    sesh <- read_sesh(path) %>%
        dplyr::filter(package != "base")

    purrr::walk(sesh$package, purrr::safely(~ unloadNamespace(.)))
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
.check_installed <- function(package, version) {
    ip <- installed.packages()

    if (package %in% rownames(ip)) {
        matches <- ip[package == rownames(ip), ] %>%
            as.data.frame() %>%
            dplyr::filter(Version == version) %>%
            dplyr::slice(1)
    }

    if (nrow(matches) > 0) return( as.character(matches$LibPath) )

    else return(NA_character_)
}

.check_loaded <- function() {
    dplyr::filter(devtools::session_info()$packages, `*` == "*", (!grepl("local", source) | package == "base")) %>%
        dplyr::select(package, loaded_v = version)
}


