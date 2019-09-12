[![Build Status](https://travis-ci.org/tmatta/lsasim.svg?branch=master)](https://travis-ci.org/tmatta/lsasim)

# README

Overall aim: to simulate data that mimics large-scale assessments (LSAs), including background questionnaire data and cognitive item responses that adhere to a multiple-matrix sampled design.  With specified correlations between background data and latent cognitive traits, the package also has functionality for theoretical and Monte Carlo regression coefficients for the effect of background data on the latent trait. This package was created as part of the [Embracing Heterogeneity Project](https://embracingheterogeneity.com/).

## Development Team
  * Tyler Matta
  * Yuan-Ling Linda Liaw
  * Leslie Rutkowski
  * David Rutkowski
  * Kondwani Kajera Mughogho
  * Waldir Leoncio

## License

In the spirit of the [open science](https://openscience.com) movement, `lsasim` (pronounced "LSA-sim") is a free and open source software, licensed under the [GNU General Public License v3.0](https://www.gnu.org/licenses/gpl-3.0.en.html). Contributions to the package are welcome, and we recommend you [contact us](https://embracingheterogeneity.com/contact/) if you're interested in doing so.

## Installation

The latest official, stable version of lsasim is available on [CRAN](https://cran.r-project.org) and can be installed like any other R package, i.e. by issuing the following command in your R prompt:

```
install.packages("lsasim")
```

[lsasim's GitHub repository](https://github.com/tmatta/lsasim/) is the central location for development of the package. Previous and test versions of lsasim can be built from this repository, but they are to be considered unstable and are not guaranteed to work properly. The development team has several coding standards and testing procedures in place to avoid issues at any branch, especially the master one, but unless you know what you are doing, we recommend you stick with the CRAN version of our package.

To install lsasim from GitHub, first make sure you have an up-to-date version of the `devtools` package installed in your machine. Then, run the following command in you R terminal:

```
devtools::install_github("tmatta/lsasim", ref = "master")  # "ref" can be any branch, tag or commit
```
Package releases can also be downloaded as compressed files from https://github.com/tmatta/lsasim/releases. These files can be installed in R using the `install.packages()` command with the downloaded file path as argument.

## Planned features

### `questionnaire_gen`:

1. Improve handling of `NULL` parameters
2. Support `family = "binomial"` and `"poisson"`
3. Include indicator grouping by factor
4. Include factor loadings

### `beta_gen`:

1. Support more complex functions, e.g. including interactions between the covariates.

## Reporting bugs and requesting features

Bugs should be reported by creating a new issue on https://github.com/tmatta/lsasim/issues. For feature requests, please use [this form](https://embracingheterogeneity.com/contact/). If you would rather contribute with source code yourself, please fork the project and issue a [pull request](https://github.com/tmatta/lsasim/pulls) when you are ready to have us review your contribution.
