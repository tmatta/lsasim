# lsasim (development version)

lsasim 2.0.2
------------

### Bug fixes

* Solves [Issue #11](https://github.com/tmatta/lsasim/issues/11), which was causing `item_gen()` to produce out-of-bounds item difficulties in some situations.

lsasim 2.0.1
------------

### Bug fixes

* Makes lsasim compliant with changes to be introduced in the next major release of R. lsasim 2.0.1 is functionally identical to 2.0.0, as all changes relate to how `if(class(x)) == "matrix"` statements are written. Specifically, such statements were changed to `if(class(x))[1] == "matrix"` to accomodate for cases where `x` may be a matrix with complex classes such as `"matrix" "array"`.

lsasim 2.0.0
------------

### API changes

This release adds several features to the `questionnaire_gen` function, such as:

* Generation of questionnaires given a much wider range of arguments.
* Reduction of the number of mandatory arguments to 1 (number of observations).
* Generation of answers which are jointly-distributed as a multivariate normal distribution.
* Generation of theoretical and empirical (Monte Carlo) regression coefficients of `theta` as a linear function of the background questionnaire items.

Please read the function's help file for more details on how to access these new features.

*Note*: any R code written for the `questionnaire_gen` function from previous versions of lsasim may not work properly on this version due to the rearrangement of the function's arguments and slight changes in its default behavior. Please review your code before executing old scripts.

lsasim 1.0.1
-------------

### API changes
* Added `cov_bounds` to `cor_gen`.
* Removed `d1` from the returned data frame of `item_gen` when `thresholds = 1`.


lsasim 1.0.1.9000
-------------

### API changes
* Added `item_no` to `response_gen`.

### Bug fixes
* `response_gen` can now handle item subsets.


lsasim 1.0.0
-------------

* Launched


