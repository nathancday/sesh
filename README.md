
<!-- README.md is generated from README.Rmd. Please edit that file -->

# sesh <img src="inst/sesh_hex.png" align="right" />

<!-- badges: start -->

[![Travis build
status](https://travis-ci.org/nathancday/sesh.svg?branch=master)](https://travis-ci.org/nathancday/sesh)
[![Codecov test
coverage](https://codecov.io/gh/nathancday/sesh/branch/master/graph/badge.svg)](https://codecov.io/gh/nathancday/sesh?branch=master)
<!-- badges: end -->

## Overview

A light-weight tool for tracking R packages. It sits somewhere between
`reprex` and `packrat` and revolves around `CSV` files to store key
package information.

The main goal of `sesh`, is to make it simpler to reproduce R code. The
ability to restore specific versions/commits of packages, is security
for rapid development. And `sesh` doesn’t require anything beyond an
Rscript, no Docker, external managers or RStudio projects.

``` r
devtools::install_github("nathancday/sesh")
```

``` r
library(sesh)
```

## Purpose

The concept behind `sesh` is being able to record information about
specific package versions for sharing with others, including your future
self. Using `sesh` as part of your workflow gives you extra
reprodu-security for individual scripts.

It is very similar in aim to `packrat`, but doesn’t tie you into a
RStudio Project. It is also similar to `docker` but doesn’t try to track
everything, just the R packages you are using.

## Demo

Lets pretend we are using two `tidyverse` pacakges, `forcats` and
`tibble`, over the course of a two year project. Today we are using the
current CRAN versions, but in the past we have used various version and
we prefer updating to keep up with the latest features.

``` r
# today
library(forcats) # v0.4.0
library(tidyr) # v0.8.3
```

### Saving critial packages for your script

Recording the package versions we use for analysis is important for
reproducibily. But part of the open-source experience is being able play
around with lots of rapidly changing tools.

Using `devtools::session_info()` or `sessionInfo()` to look at this
information is great and `sesh` makes it easier to manage this data. The
function `sesh()` gives an overview of the attached packages, their
version and their source.

``` r
sesh()
```

This dataframe is the essense of `sesh`, a light, easy to share, record
of your R session’s essential information.

### Saving your `sesh`

To save your current `sesh()`, use `save_sesh()`. It will record the
vital information to re-load all of your attached `pacakge@verion`s and
write it to disk as a `CSV`.

By recording the attached packages and saving it with your analysis, you
can pick up scripts in the future with no worries about breaking package
changes.

``` r
save_sesh()
```

The default `path`, is set up to name the output as
“sesh\_$SYS-DATE.csv”. But it uses the
[`glue`](https://github.com/tidyverse/glue) package to paste together R
variables, so you could include custom gloabls to fit your tastes.

### A simple case

The function `check_sesh()` will compare the currently loaded/installed
package versions against a `sesh` record. It will report which packages
are already loaded, installed (but not loaded) or require
installation.

``` r
check_sesh("sesh_2019-06-20.csv") # check against currently installed versions
```

This just confirms our current session info matches the session info we
saved two seconds ago, duh. Not a very cool use case

### A messier case

We need to reproduce a script we wrote back on October 1st 2017.

To re-create this scenario, we will re-install two common cases:

  - a prior CRAN release version
  - a package from GitHub at prior commit

<!-- end list -->

``` r
devtools::install_version("forcats", "0.1.0", reload = F)
devtools::install_github("tidyverse/tidyr@bd0c6b09052e91a4d283b2be6c8d3c5a6769b910", reload = F)
```

It is a good idea to restart your R session whenever you install an
already attached package, so like now.

``` r
library(forcats)
library(tidyr)

save_sesh("sesh_2017-10-01.csv")
```

### Ending the time warp

And bringing us back to back to the present

``` r
install.packages("forcats")
install.packages("tidyr")

library(forcats)
library(tidyr)
```

### Picking up old code

First thing we do if we are picking up that year-old, dusty script, is
look at the packages we used.

``` r
read_sesh("sesh_2017-10-01.csv")
```

Sure, we are a little nervous because perhaps by upgrading our package
versions we have inadvertantly broken something. But our new found
`sesh`abilities make it easy to replicate specific versions.

### Checking diffs

In order to see what pacakges are differnt between the “old script” and
our library today, we would use `check_sesh` to learn the next steps.

``` r
check_sesh("sesh_2017-10-01.csv")
```

We see we will need to install the two older copies fo the libraries, so
`sesh` makes a temporary library in `~/.Trash/`.

The function `install_sesh()` will handle that and re-install matching
versions. By looking at `source` and `version`, it will try to track the
right version from CRAN or GitHub and tell you if it was succesful. By
keeping a temporary library in `~/.Trash/`, `sesh` contains conflicts
and doesn’t take up disk space long term, because `macOS` deletes any
files in there after 30 days.

`sesh` does not touch `.libPaths()`, so it will not interfer with your
globally installed package versions.

``` r
install_sesh("sesh_2017-10-01.csv")
```

Great, now we have the the right package versions installed to rerun our
“old” script, but as it sits right now the matching versions are not
loaded.

We need to call `sesh_load()` to attach them. We should also restart the
R session again because we re-installed loaded packages.

``` r
load_sesh("sesh_2017-10-01.csv")
```

And re-checking we see…

``` r
check_sesh("sesh_2017-10-01.csv")
```

All that is left to do now is source that old script\!

##### Un-seshing

When you are done working with the past `sesh` versions, you can go back
to your current global versions immediately. Just restart your R session
and attach your libraries like normal.

There is also the helper function `unload_sesh()` to do this without
restarting, but it’s vunerable to `dependencies`,

``` r
unload_sesh("sesh_2017-10-01.csv")

library(forcats)
library(tidyr)

sesh()
```
