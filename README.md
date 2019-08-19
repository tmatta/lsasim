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

Consider the implementation of the features below:

### `questionnaire_gen`:

1. Improve handling of `NULL` parameters
2. Support `family = "binomial"` and `"poisson"`
3. Include indicator grouping by factor
4. Include factor loadings

### `beta_gen`:

1. Support more complex functions, e.g. including interactions between the covariates.
