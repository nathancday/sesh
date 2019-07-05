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
      write.csv(path, row.names = FALSE)
    message(glue('Saved sesh as: {file_name}'))
}
#' Read sesh RDS to see critical package info.
#'
#' @importFrom magrittr "%>%"
#' @export
read_sesh <- function(path) {
    read.csv(path, colClasses = "character", stringsAsFactors = FALSE)
    # want ability to read output from 'session_info()``
}

#' Check current conditions against a sesh.
#'
#' The workhorse that checks loaded and installed pacakge versions againstst
#' those specified in sesh.
#'
#' @md
#' @param path A character. Valid path to a sesh CSV.
#' @importFrom dplyr anti_join mutate
#' @importFrom purrr map2_chr
#' @export
check_sesh <- function(path) {

    past <- read_sesh(path)
    cur <- sesh()

    require_action <- anti_join(
      past, cur,
      by = c("package", "loadedversion", "source")
    )
    # these mismatched pacakges fall into 3 catergories:
    # 1. installed but not loaded
    # 2. already sesh installed but not loaded
    # 3. wrong version installed
    if (nrow(require_action) == 0 ) {
        message("Loaded versions match sesh!")
        return(invisible())
    }

    # Make a separate library to not interfer with global installs
    # sesh_name <-  glue('sesh_lib')
    sesh_lib <- '~/.Trash/sesh_lib/'

    # check if a library was already built
    # prevents loading problems if install exists are performed
    if ( dir.exists(sesh_lib) ) {
        sesh_installed <- dplyr::filter(require_action, package %in% dir(sesh_lib))
        require_action <- require_action %>%
            dplyr::anti_join(sesh_installed,
                             by = c("package", "loadedversion", "source"))
    } else sesh_installed <- data.frame()

    if (nrow(sesh_installed) > 0) {
        message(
            'These package versions are installed already in a sesh_lib.\n',
            .print_capture( paste0(sesh_installed$package, " ",
                              sesh_installed$v, " installed in ", sesh_lib ,".") ),
            "\nCall load_sesh() to attach them.")
    }
    # check if versions installed globally, but just aren't loaded
    ready_to_load <- require_action %>%
      # dplyr::filter(is.na(version)) %>%
      mutate(installed_location = map2_chr(package, loadedversion, ~ .check_installed(.x, .y))) %>%
      filter(!is.na(installed_location))
    if (nrow(ready_to_load) > 0) {
      message('These package versions are installed already:\n',
      .print_capture( paste(ready_to_load$package_sesh,
                         ready_to_load$loadedversion_sesh, "/", ready_to_load$source_sesh ) ),
      "\nCall load_sesh() to attach them\n")
        require_install <- anti_join(
          require_action, ready_to_load,
          by = c("package", "loadedversion", "source"))
    } else { require_install <- require_action }

    if (nrow(require_install) > 0) {
        message('These package versions are missing:\n',
        .print_capture(
            require_install
        ),
        "\nCall install_sesh() to safely install.")
    }
}

#' Function to load out a sesh if required versions are installed.
#'
#' @importFrom magrittr "%>%" "%<>%"
#'
#' @export
load_sesh <- function(path, interactive = TRUE) {

    past <- read_sesh(path)
    cur <- sesh()

    require_action <- anti_join(
      past, cur,
      by = c("package", "loadedversion", "source"))

    if (nrow(require_action) == 0 ) {
        message("Loaded versions match sesh!")
    }

    # Make a temporary library to not interfer with global installs
    # sesh_name <- gsub(".*(sesh_.*)\\.csv", "\\1", path)
    sesh_lib <- glue::glue('~/.Trash/sesh_lib/')

    # check if a library was already built
    # prevents loading problems if install exists are performed
    if ( dir.exists(sesh_lib) ) {
        already_seshed <- filter(require_action, package %in% dir(sesh_lib))
        require_action %<>% anti_join(already_seshed,
                                      by = c("package", "loadedversion", "source"))

        if (nrow(already_seshed) > 0) {
          message(glue::glue('Loading: { paste(
                           already_seshed$package,
                           already_seshed$loadedversion,
                           sep = "_", collapse = ", ") }'))

          purrr::walk2(already_seshed$package, sesh_lib,
                       function(x, y) {
                         # unloadNamespace(paste0("package:", x)) # detach package if loaded
                         library(x, lib.loc = y, character.only = TRUE)
                       } )
        }
    } else dir.create(sesh_lib)

    ready_to_load <- require_action %>%
        mutate(installed_lib = map2_chr(package, loadedversion, ~.check_installed(.x, .y))) %>%
        filter( !is.na(installed_lib) )

    if (nrow(ready_to_load) > 0) {
        message(glue::glue('Loading: { paste(
                           ready_to_load$package,
                           ready_to_load$loadedversion,
                           sep = "_", collapse = ", ") }'))

        purrr::walk2(ready_to_load$package, ready_to_load$installed_lib,
              function(x, y) {
                  # unloadNamespace(paste0("package:", x)) # detach package if loaded
                  library(x, lib.loc = y, character.only = TRUE)
              } )
        require_action %<>% anti_join(ready_to_load,
                    by = c("package", "loadedversion", "source"))
    }
    if (nrow(require_action) > 0 ) {
      if (interactive){
        message( glue::glue('These versions do not match :(
                           {paste(require_action$package,
                            require_action$loadedversion, sep = "_", collapse = ", ")}
                            call install_sesh() to safely install'))
      } else return(require_action)
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
#'@importFrom purrr map safely
#'@importFrom dplyr filter mutate bind_rows
#'@importFrom remotes install_github install_cran install_version
#'@importFrom glue glue
#'@importFrom withr with_libpaths
#'@export
install_sesh <- function(path, sesh_lib = '~/.Trash/sesh_lib/', ...) {

    # past <- read_sesh(path)
    # cur <- sesh()

    require_action <- load_sesh(path, interactive = FALSE)

    if (is.null(require_action)) {
        message("No installs required")
      return()
    } else { # install in sesh_lib
        message(glue('Installing sesh lib in: {sesh_lib}'))

        # Go after the repos
        # GitHub is easier
        gh <- require_action %>% filter(grepl("github", source, ignore.case = TRUE))

        if (nrow(gh) > 0) {
            gh <- gh %>%
              mutate(repo = gsub(".*\\((.*)\\)", "\\1", source),
                     install_result = map(repo, safely(
                                    ~ with_libpaths(sesh_lib, install_github(., upgrade = FALSE)))),
                    install_result = .extract_result(install_result) )
        }

        # CRAN is harder
        cran <- require_action %>% filter(grepl("cran|url", source, ignore.case = TRUE))

        if (nrow(cran) > 0) {
            cran %<>% mutate(
                    install_result = map2(package, loadedversion,
                                 safely(function(x, y, ...) with_libpaths(sesh_lib, install_version(x, y, upgrade = FALSE)))),
                    install_result = .extract_result(install_result) )
        }

        needed <- dplyr::bind_rows(gh, cran)

        # (required if needed update is to current CRAN)
        cran_retries <- needed %>% dplyr::filter(install_result == "Error")

        if (nrow(cran_retries) > 0) {
             cran_retries %<>%
                mutate(install_result = map(package,
                                                      safely(function(x, ...) with_libpaths(sesh_lib, install_cran(x, upgrade = FALSE)))),
                          install_result = .extract_result(install_result))

             gotten <- filter(cran_retries, install_result != "Error")
        }
        else {
            gotten <- needed %>% filter(install_result == "Success")
        }
        message(glue::glue('{nrow(gotten)} succesful install of {nrow(needed)} needed package.'))

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
        installed_already <- ip %>%
            dplyr::filter(Package == package,
                          Version == version) %>%
            dplyr::slice(1)
    }

    if (nrow(installed_already) == 1) return( as.character(installed_already$LibPath) )

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

