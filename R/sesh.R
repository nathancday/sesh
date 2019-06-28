#' See info about attached R packages during an R session
#'
#' A lighter version `devtools::session_info()` that returns a dataframe
#' and only *attached* libraries
#' @md
#' @return A data frame.
#' @export
sesh <- function() {
  session_info() %>%
    .extract_sesh()
}
#' Save `sesh` output as csv.
#'
#' Renames columns from `sesh()` to work `sesh_check()`.
#'
#' @return Saves a RDS file with essential package information.
#' @importFrom magrittr "%>%"
#' @importFrom dplyr rename_all
#' @importFrom glue glue
#' @examples
#' save_sesh()
#' @export
save_sesh <- function(path = 'sesh_{as.character(Sys.Date())}.RDS') {
    file_name <- glue(path)
    sesh() %>%
        rename_all(~ paste0(., "_sesh")) %>%
        saveRDS(path)
    message(glue('Saved sesh as: {file_name}'))
}
#' Read sesh RDS to see critical package info.
#'
#' @importFrom magrittr "%>%"
#' @export
read_sesh <- function(path) {
    read <- readRDS(path)

    # want ability to read output from 'session_info()`


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
            dplyr::filter( (v != version) | is.na(version) )
    )


    # these pacakges fall into three catergories (installed but not loaded, already sesh installed but not loaded, and wrong version installed)
    if (nrow(require_action) == 0 ) {
        message("Loaded versions match sesh!")
        return(invisible())
    }

    # Make a temporary library to not interfer with global installs
    sesh_name <-  gsub(".*(sesh_.*)\\.csv", "\\1", path)
    sesh_lib <- glue::glue('~/.Trash/{sesh_name}/')

    # check if a library was already built
    # prevents loading problems if install exists are performed
    if ( dir.exists(sesh_lib) ) {
        sesh_installed <- dplyr::filter(require_action, package %in% dir(sesh_lib))
        require_action <- require_action %>%
            dplyr::anti_join(sesh_installed)
    } else sesh_installed <- data.frame()

    if (nrow(sesh_installed) > 0) {
        message(
            'These sesh-version are already installed in a sesh-lib.\n',
            .print_capture( paste0(sesh_installed$package, " ",
                              sesh_installed$v, " installed in ", sesh_lib ,".") ),
            "\nCall load_sesh() to attach them.")
    }


    # check that correct versions just aren't loaded, but installed globally
    ready_to_load <- require_action %>%
        dplyr::filter(is.na(version)) %>%
        dplyr::mutate(installed_version = purrr::map2_chr(package, v, ~ .check_installed(.x, .y))) %>%
        dplyr::filter(v == installed_version)

    if (nrow(ready_to_load) > 0) {
        message('These sesh-version / installed-version already match:\n',
        .print_capture( paste(ready_to_load$package,
                           ready_to_load$v, "/", ready_to_load$installed_version ) ),
        "\nCall load_sesh() to attach them")
    }

    # versions don't match
    suppressMessages(
        require_install <- require_action %>%
        dplyr::anti_join(ready_to_load)
    )

    if (nrow(require_install) > 0) {
        message('These sesh_version / installed_version do not match:\n',
        .print_capture(
            dplyr::select(require_install, package, sesh_version = v, installed_version = version)
        ),
        "\nCall install_sesh() to safely install.")
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

    # Make a temporary library to not interfer with global installs
    sesh_name <- gsub(".*(sesh_.*)\\.csv", "\\1", path)
    sesh_lib <- glue::glue('~/.Trash/{sesh_name}/')

    # check if a library was already built
    # prevents loading problems if install exists are performed
    if ( dir.exists(sesh_lib) ) {
        already_seshed <- dplyr::filter(require_action, package %in% dir(sesh_lib))
        require_action %<>% dplyr::anti_join(already_seshed)
    } else dir.create(sesh_lib)

    if (nrow(already_seshed) > 0) {
        message(glue::glue('Loading from sesh library: { paste(already_seshed$package, collapse = ", ") }'))

        purrr::walk2(already_seshed$package, sesh_lib,
                     function(x, y) {
                         # unloadNamespace(paste0("package:", x)) # detach package if loaded
                         library(x, lib.loc = y, character.only = TRUE)
                     } )
    }

    ready_to_load <- require_action %>%
        dplyr::mutate(installed_lib = purrr::map2_chr(package, v, ~.check_installed(.x, .y))) %>%
        dplyr::filter( !is.na(installed_lib) )

    if (nrow(ready_to_load) > 0) {
        message(glue::glue('Loading: { paste(ready_to_load$package, collapse = ", ") }'))

        purrr::walk2(ready_to_load$package, ready_to_load$installed_lib,
              function(x, y) {
                  # unloadNamespace(paste0("package:", x)) # detach package if loaded
                  library(x, lib.loc = y, character.only = TRUE)
              } )
    }

    suppressMessages(
        require_install <- require_action %>%
        dplyr::anti_join(ready_to_load) %>%
        dplyr::anti_join(already_seshed)
    )

    if (nrow(require_install) > 0 ) {
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

        already_installed <- require_action %>%
            dplyr::filter(is.na(version)) %>%
            dplyr::mutate(installed_version = purrr::map2_chr(package, v, ~ .check_installed(.x, .y))) %>%
            dplyr::filter(v == installed_version)

        if (nrow(already_installed) == nrow(require_action)) {
            message("Installed versions match, use `load_sesh()` to attach.")
            return()
        }

        suppressMessages(
            require_action <- require_action %>%
            dplyr::anti_join(already_installed)
        )

        if (nrow(require_action) == 0) return()

        # Make a temporary library to not interfer with global installs
        sesh_name <- gsub(".*(sesh_.*)\\.csv", "\\1", path)
        sesh_lib <- glue::glue('~/.Trash/{sesh_name}/')

        # check if a library was already built
        # prevents loading problems if install exists are performed
        if ( dir.exists(sesh_lib) ) {
            already_seshed <- dplyr::filter(require_action, package %in% dir(sesh_lib))
            require_action %<>% dplyr::anti_join(already_seshed)
        } else dir.create(sesh_lib)

        message( glue::glue('Installing sesh lib in: {sesh_lib}') )

        # Go after the repos
        # GitHub is easier
        gh <- require_action %>%
            dplyr::filter(grepl("github", s, ignore.case = TRUE))

        if (nrow(gh) > 0) {
            gh <- gh %>%
                dplyr::mutate(repo = gsub(".*\\((.*)\\)", "\\1", s)) %>%
                dplyr::mutate(
                    install_result = purrr::map(repo,
                                purrr::safely(
                                    ~ withr::with_libpaths(sesh_lib,
                                                          devtools::install_github(., reload = F)))),
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
                dplyr::filter(install_result == "Success")
        }

        message(glue::glue('{nrow(gotten)} succesful install of {nrow(needed)} needed package.'))

        # suppressMessages(
        #     read_sesh(path) %>%
        #     dplyr::full_join(gotten) %>%
        #     readr::write_csv(glue::glue('{path}'))
        # )

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
    paste(capture.output(print(x)), collapse = "\n")
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
    ip <- as.data.frame(installed.packages())

    if (package %in% rownames(ip)) {
        matches <- ip %>%
            dplyr::filter(Package == package,
                          Version == version) %>%
            dplyr::slice(1)
    }

    if (nrow(matches) > 0) return( as.character(matches$LibPath) )

    else return(NA_character_)
}

.check_loaded <- function() {
    dplyr::filter(devtools::session_info()$packages,
                  attached == TRUE,
                  (!grepl("local", source) | package == "base")) %>%
        dplyr::select(package, loaded_v = loadedversion)
}

#' Extract a data frame of attached packages
#' @importFrom magrittr "%>%"
#' @importFrom devtools session_info
#' @importFrom dplyr filter select
.extract_sesh <- function(sesh) {
  stopifnot(class(sesh) == "session_info")
  sesh %>%
    .[["packages"]] %>%
    filter(attached == TRUE,
           (!grepl("local", source) | package == "base")) %>%
    select(package, loadedversion, source)
}

