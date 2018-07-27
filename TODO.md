# To-do list

## questionnaire_gen():

* Currently assuming all r.v. (Y and W) have `sd = 1`, so correlations and
covariances are interchangeable. Lift that assumption by letting `cov_yw` have
values above 1 or below -1.

* Eliminate parameter redundancy between `cov_yw` and `cor_matrix`.

* Allow `family = "binomial"` or `"poisson"`
