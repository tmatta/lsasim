# lsasim 2.1.5

## Bug fixes

* Fixed `block_design()` for non-sequential items (issue #50)
* Fixed typos in documentation
* Reimplemented continuous integration routine (issue #47)

# lsasim 2.1.4

## Bug fixes

* Fixed top-level package documentation (`?lsasim`)

# lsasim 2.1.3

## Bug fixes

* Fixes generation of thresholds on `item_gen()` (issue #48)
* Refactored usage of `class()` on if-statements

# lsasim 2.1.2

## Bug fixes

* Refactoring to fix building on Fedora/Clang and M1-powered Macs

# lsasim 2.1.1

## Bug fixes

* Fixes test unit dependency on GNU libiconv

# lsasim 2.1.0

## API changes

* Adds `cluster_gen`, a function to generate background questionnaires (with `questionnaire_gen`) in a cluster sampling structure.
* Package description now includes a literary reference for the theoretical framework behind the package (issue #38, pull request #42)

## Bug fixes

* Fixes item parameter generation by `irt_gen` when the guessing parameter is larger than zero (issue #40)

# lsasim 2.0.2

## Bug fixes

* Solves [Issue #11](https://github.com/tmatta/lsasim/issues/11), which was causing `item_gen()` to produce out-of-bounds item difficulties in some situations.

# lsasim 2.0.1

## Bug fixes

* Makes lsasim compliant with changes to be introduced in the next major release of R. lsasim 2.0.1 is functionally identical to 2.0.0, as all changes relate to how `if (class(x) == "matrix"` statements are written. Specifically, such statements were changed to `if(class(x))[1] == "matrix"` to accommodate for cases where `x` may be a matrix with complex classes such as `"matrix" "array"`.

# lsasim 2.0.0

## API changes

This release adds several features to the `questionnaire_gen` function, such as:

* Generation of questionnaires given a much wider range of arguments.
* Reduction of the number of mandatory arguments to 1 (number of observations).
* Generation of answers which are jointly-distributed as a multivariate normal distribution.
* Generation of theoretical and empirical (Monte Carlo) regression coefficients of `theta` as a linear function of the background questionnaire items.

Please read the function's help file for more details on how to access these new features.

*Note*: any R code written for the `questionnaire_gen` function from previous versions of lsasim may not work properly on this version due to the rearrangement of the function's arguments and slight changes in its default behavior. Please review your code before executing old scripts.

# lsasim 1.0.1

## API changes
* Added `cov_bounds` to `cor_gen`.
* Removed `d1` from the returned data frame of `item_gen` when `thresholds = 1`.


# lsasim 1.0.1.9000

## API changes
* Added `item_no` to `response_gen`.

## Bug fixes
* `response_gen` can now handle item subsets.


# lsasim 1.0.0

* Launched
