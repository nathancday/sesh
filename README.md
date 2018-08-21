
<!-- README.md is generated from README.Rmd. Please edit that file -->
sesh <img src="inst/sesh_hex.png" align="right" />
==================================================

Is a light-weight tool for tracking R packages. It sits somewhere between `reprex` and `packrat` and revolves around `CSV` files to store key package information.

The main goal of `sesh`, is to make it simpler to reproduce R code. The ability to restore specific versions/commits of packages, is security for rapid development. And `sesh` doesn't require anything beyond an Rscript, no Docker, external managers or RStudio projects.

``` r
devtools::install_github("nathancday/sesh")
```

``` r
library(sesh)
```

### Purpose

The concept behind `sesh` is being able to record information about specific package versions for sharing with others, including your future self. Using `sesh` as part of your workflow gives you extra reprodu-security for individual scripts.

It is very similar in aim to `packrat`, but doesn't tie you into a RStudio Project. It is also similar to `docker` but doesn't try to track everything, just the R packages you are using.

### Demo

Lets pretend we are using two `tidyverse` pacakges, `forcats` and `tibble`, over the course of a two year project. Today we are using the current CRAN versions, but in the past we have used various version and we prefer updating to keep up with the latest features.

``` r
# today
library(forcats) # v0.3.0
library(tidyr) # v0.8.1
```

##### Saving critial packages for your script

Recording the package versions we use for analysis is important for reproducibily. But part of the open-source experience is being able play around with lots of rapidly changing tools.

Using `devtools::session_info()` or `sessionInfo()` to look at this information is great and `sesh` makes it easier to manage this data. The function `sesh()` gives an overview of the attached packages, their version and their source.

``` r
sesh()
##   package version                           source
## 1    base   3.5.0                            local
## 2 forcats   0.3.0                   CRAN (R 3.5.0)
## 3    sesh   0.0.1 Github (nathancday/sesh@096309f)
## 4   tidyr   0.8.1                   CRAN (R 3.5.0)
```

This dataframe is the essense of `sesh`, a light, easy to share, record of your R session's essential information.

##### Saving your `sesh`

To save your current `sesh()`, use `save_sesh()`. It will record the vital information to re-load all of your attached `pacakge@verion`s and write it to disk as a `CSV`.

By recording the attached packages and saving it with your analysis, you can pick up scripts in the future with no worries about breaking package changes.

``` r
save_sesh()
## Saved sesh as: sesh_2018-08-20.csv
```

The default `path`, is set up to name the output as "sesh\_$SYS-DATE.csv". But it uses the [`glue`](https://github.com/tidyverse/glue) package to paste together R variables, so you could include custom gloabls to fit your tastes.

##### A simple case

The function `check_sesh()` will compare the currently loaded/installed package versions against a `sesh` record. It will report which packages are already loaded, installed (but not loaded) or require installation.

``` r
check_sesh("sesh_2018-08-20.csv") # check against currently installed versions
## Loaded versions match sesh!
## NULL
```

This just confirms our current session info matches the session info we saved two seconds ago, duh. Not a very cool use case

##### Simulated time travel

Close your eyes and pretend....

We need to reproduce a script we wrote back on October 1st 2016.

To re-create this scenario, we will re-install two common cases:

-   a prior CRAN release version
-   a package from GitHub at prior commit

``` r
devtools::install_version("forcats", "0.1.0", reload = F)
devtools::install_github("tidyverse/tidyr@bd0c6b09052e91a4d283b2be6c8d3c5a6769b910", reload = F)
```

It is a good idea to restart your R session whenever you install an already attached package, like now.

``` r
library(forcats)
library(tidyr)

save_sesh("sesh_2017-10-01.csv")
```

##### Ending time warp

Bringing us back to back to now.

``` r
install.packages("forcats")
install.packages("tidyr")

library(forcats)
library(tidyr)
```

##### Pretend pick up

So now let's pretend we are picking up that year-old, dusty script.

``` r
read_sesh("sesh_2017-10-01.csv")
## # A tibble: 3 x 3
##   package v     s                               
##   <chr>   <chr> <chr>                           
## 1 base    3.5.0 local                           
## 2 forcats 0.1.0 url                             
## 3 tidyr   0.7.1 Github (tidyverse/tidyr@bd0c6b0)
```

Sure, we are a little nervous because perhaps by upgrading our package versions we have inadvertantly broken something.

But our new found `sesh`abilities make dealing with diffs simple and straight forward.

##### Check list

In order to see what pacakges are differnt between the "old script" and our library today, use `check_sesh`.

``` r
check_sesh("sesh_2017-10-01.csv")
## These sesh_version / installed_version do not match:
## Call install_sesh() to safely install.
```

That's shows us the difference between our currently installed versions and the "past" versions.

The function `install_sesh()` will re-install matching versions. By looking at `source` and `version`, it will attempts to install the sesh version in `~/.Trash/` and tell you if it was succesful. By keeping a temporary library in `~/.Trash/`, `sesh` contains conflicts and doesn't take up disk space long term, because `macOS` deletes any files in there after 30 days.

`sesh` does not touch `.libPaths()`, so it will not interfer with your globally installed package versions.

``` r
install_sesh("sesh_2017-10-01.csv")
## Installing sesh lib in: ~/.Trash/sesh_2017-10-01/
## Using GitHub PAT from envvar GITHUB_PAT
## Downloading GitHub repo tidyverse/tidyr@bd0c6b0
## from URL https://api.github.com/repos/tidyverse/tidyr/zipball/bd0c6b0
## Installing tidyr
## Installing rlang
## '/Library/Frameworks/R.framework/Resources/bin/R' --no-site-file  \
##   --no-environ --no-save --no-restore --quiet CMD INSTALL  \
##   '/private/var/folders/09/fpsmqczx5sb6t4v1b66tv7tm0000gp/T/RtmpNJfZS2/devtools149589d8f9e/rlang'  \
##   --library='/Users/nathanday/.Trash/sesh_2017-10-01' --install-tests
## 
## '/Library/Frameworks/R.framework/Resources/bin/R' --no-site-file  \
##   --no-environ --no-save --no-restore --quiet CMD INSTALL  \
##   '/private/var/folders/09/fpsmqczx5sb6t4v1b66tv7tm0000gp/T/RtmpNJfZS2/devtools149515912a7d/tidyverse-tidyr-bd0c6b0'  \
##   --library='/Users/nathanday/.Trash/sesh_2017-10-01' --install-tests
## 
## Installation failed: trying to use CRAN without setting a mirror
## 2 succesful install of 2 needed package.
## Use `sesh_load()` to attach sesh versions.
```

Great, now we have the the right package versions installed to rerun our "old" script, but as it sits right now the matching versions are not loaded.

We need to call `sesh_load()` to attach them. We should also restart the R session again because we re-installed loaded packages.

``` r
load_sesh("sesh_2017-10-01.csv")
## Joining, by = c("package", "v", "s", "version", "source")
## Loading from sesh library: tidyr
## These sesh-version / installed-version do not match:
## forcats 0.1.0 / NA
##  call install_sesh() to safely install
```

And re-checking we see...

``` r
check_sesh("sesh_2017-10-01.csv")
## Joining, by = c("package", "v", "s", "version", "source")
## These sesh_version / installed_version do not match:
## Call install_sesh() to safely install.
```

All that is left to do now is source that old script!

##### Un-seshing

When you are done working with the past `sesh` versions, you can go back to your current global versions immediately. Just restart your R session and attach your libraries like normal.

There is also the helper function `unload_sesh()` to do this without restarting, but it's vunerable to `dependencies`,

``` r
unload_sesh("sesh_2017-10-01.csv")

library(forcats)
library(tidyr)

sesh()
##    package version                           source
## 1     base   3.5.0                            local
## 2 bindrcpp   0.2.2                   CRAN (R 3.5.0)
## 3  forcats   0.3.0                   CRAN (R 3.5.0)
## 4     sesh   0.0.1 Github (nathancday/sesh@096309f)
## 5    tidyr   0.8.1                   CRAN (R 3.5.0)
```
