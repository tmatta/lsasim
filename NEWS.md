%lsasim 2.0.1
%-------------
%
%### Compatibility-breaking changes
%* `questionnaire_gen()` now contains multiple parameters starting with `n`.
%  Therefore, calls to the function of the type `questionnaire_gen(n = 10)`
%  will no longer work because `n` no longer obviously refers to the parameter
%  `n_obs`.

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


