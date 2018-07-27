[![Build Status](https://travis-ci.org/tmatta/lsasim.svg?branch=master)](https://travis-ci.org/tmatta/lsasim)

# README

Overall aim: to simulate data that mimics large-scale assessments, including background questionnaire data and cognitive item responses that adhere to a multiple-matrix sampled design. 


## Development Team
  * Tyler Matta
  * Yuan-Ling Linda Liaw
  * Leslie Rutkowski
  * David Rutkowski
  * Kondwani Kajera Mughogho
  * Waldir Leoncio

## License
  GPL-3

## To-do

### `questionnaire_gen()`:

* Currently assuming all r.v. (_Y_ and _W_) have `sd = 1`, so correlations and
covariances are interchangeable. Lift that assumption by letting user provide
a covariance matrix.

* Allow `family = "binomial"` or `"poisson"`



