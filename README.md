![Current CRAN release](https://www.r-pkg.org/badges/version/lsasim) [![R-CMD-check](https://github.com/tmatta/lsasim/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/tmatta/lsasim/actions/workflows/R-CMD-check.yaml)

# LSASIM

Overall aim: to simulate data that mimics large-scale assessments (LSAs), including background questionnaire data and cognitive item responses that adhere to a multiple-matrix sampled design.  With specified correlations between background data and latent cognitive traits, the package also has functionality for theoretical and Monte Carlo regression coefficients for the effect of background data on the latent trait. This package was created as part of the Embracing Heterogeneity Project.

# Development team
  * Tyler Matta
  * Yuan-Ling Linda Liaw
  * Leslie Rutkowski
  * David Rutkowski
  * Kondwani Kajera Mughogho
  * Waldir Leoncio

# License

In the spirit of the open science movement, `lsasim` (pronounced "L.S.A.-sim") is a free and open source software, licensed under the [GNU General Public License v3.0](https://www.gnu.org/licenses/gpl-3.0.en.html). lsasim is not being actively developed, but PRs introducing new features will be considered, and bug reports will be addressed by the maintainer. Please use the [GitHub issues page](https://github.com/tmatta/lsasim/issues) to report bugs or request new features.

# Installation

The latest official, stable version of lsasim is available on [CRAN](https://cran.r-project.org) and can be installed like any other R package, i.e. by issuing the following command in your R prompt:

```R
install.packages("lsasim")
```

[lsasim's GitHub repository](https://github.com/tmatta/lsasim/) is the central location for development of the package. Stable and test versions of lsasim can be built from this repository, but they are to be considered unstable and are not guaranteed to work properly. The development team has several coding standards and testing procedures in place to avoid issues at any branch, but unless you know what you are doing, we recommend you stick with the CRAN version of our package.

To install lsasim from GitHub, first make sure you have an up-to-date version of the `remotes` package installed in your machine. Then, run the following command in you R terminal:

```R
remotes::install_github("tmatta/lsasim", ref = "master")  # "ref" can be any branch, tag or commit
```

Package releases can also be downloaded as compressed files from https://github.com/tmatta/lsasim/releases. These files can be installed in R using the `install.packages()` command with the downloaded file path as argument.

# Usage

There are several sources of information that help a user get started with `lsasim` and discover its most important features. The package's official repository hosts a [Wiki](https://github.com/tmatta/lsasim/wiki); Once `lsasim` is installed in your machine, you can also issue the command `help("lsasim")`&mdash;or `?lsasim` for short&mdash;to learn more about the package in general. You can also use the `help()` or `?` functions to access the documentation for a specific function, for example `?questionnaire_gen`. Some functions such as `beta_gen` contain equations, which will be displayed when calling `help("beta_gen")`; however, proper formatting of the equations can be seen by running `help("beta_gen", help_type = "PDF")` and opening the beta_gen.pdf file that is created in your working directory.

This package contains vignettes. If you are installing `lsasim` from GitHub, remember to include `build_vignettes=TRUE` in your `remotes::install_github()` call. Afterwards, you can browse the vignettes by issuing `browseVignettes("lsasim")` in your R terminal.
