## sesh

Is a light-weigth package manager for R. It sits somewhere between `reprex` and `packrat` and revolves around `CSV` files to save key package information.

```{r load_sesh}
devtools::load_all("~/future/sesh/")
# devtools::install_github("nathancday/sesh")
```

#### Purpose

The idea behind `sesh` is being able to share specific package version info with others, including your future self.

Installing and loading `forcats` and `tibble` from their current CRAN versions. To attach some packages beyond R-core.

```
install.packages("forcats", dep = T)
install.packages("tibble")

library(forcats) # example
library(tibble) # example
```

`session_info()` is an improvement on `sessionInfo()` for sure, but is still fairly verbose.

`sesh` aims to filter this output to only essential elements, the attached packages (with their versions) and the `R-core` version.

```{r sesh}
sesh()
```

This is the essense of `sesh`.

And while this strategy doesn't cover every NAMESPACE'd load in, it does cover the major pacakge changes that are the typical causes of script breaking. It makes sharing your current pacakge load out easy, which is useful for asking version specific code problems, on a forum or mailing list.

#### Use case

Let's save the current `sesh()` as a CSV. The argument `path` used the `glue` package to paste together R variables, so gloabl variables can be used in the file name.

```{r save_easy, message = T}
save_sesh()
```

Now, let's re-check the `sesh` we just saved.

```
check_sesh("sesh_2018-08-12.csv") # check against currently installed versions
```

#### Simulated time-travel

Let's mess it up a little bit to show the point.

Pretend you are picking up a script that you wrote last year. Scary for sure, but a little less scary, when you let `sesh` take care of the package dependencies.

Here are two common cases: a prior CRAN release version and a package from GitHub at a specific commit.

```
devtools::install_version("forcats", "0.2.0")
devtools::install_github("tidyverse/tibble@74b66ffcfca4459db52b6e9991d51aba93759dd6")
```

If you run into problems you might need to restart your R session here.

Now re-check our previous session

```
check_sesh("sesh_2018-08-12.csv")
```

That's cool, `sesh::check_sesh()` shows us the difference between our currently installed packages, `cur_v` and the "past" session version we are trying to get back to, `sesh_v`.

The function `install_sesh()` will restore matching versions. By looking at `source` and `sesh_v`, it will try to re-install the appropriate version from either CRAN archives or GitHub and let you know if it was succesful.

```
install_sesh("sesh_2018-08-12.csv")
```
