#' @title Generate an ANOVA table for LSASIM clusters
#' @description Prints Analysis of Variance table for `cluster_gen` output.
#' @param object list output of `cluster_gen`
#' @param print if `TRUE`, output will be a list containing estimators; if `FALSE` (default), output are formatted tables of this information
#' @param calc.se if `TRUE`, will try to calculate the standard error of the intreaclass correlation
#' @param ... additional objects of the same type (see `help("anova")` for details)
#' @return Printed ANOVA table or list of parameters
#' @note  If the rhos for different levels are varied in scale, the generated rho will be less accurate.
#' @references Snijders, T. A. B., & Bosker, R. J. (1999). Multilevel Analysis. Sage Publications.
#' @importFrom stats anova
#' @method anova lsasimcluster
#' @export
anova.lsasimcluster <- function(object, print = TRUE, calc.se = TRUE, ...) {
    # Wrap data in a list (for !separate_questionnaires) =======================
    if (all(sapply(object, class) != "list")) {
        data <- list(object)
        names(data) <- gsub("[0-9]", "", names(data[[1]])[1])
    } else {
        data <- object
    }

    # Create summary statistics ================================================
    data_summary <- summary.lsasimcluster(data, print = "none")

    # Create other relevant input ==============================================
    out_complete <- list()
    for (n in names(data)) {
        ds <- data_summary[[n]]
        if (ds$N == 1) {
            message("There is only one group of ", pluralize(n),
                    ". Skipping ANOVA for that level.")
        } else {
            ## Sample statistics -----------------------------------------------
            s2_within <- calc_var_within(ds$n_j, ds$s2_j, ds$M, ds$N)
            s2_between <- calc_var_between(ds$n_j, ds$y_bar_j, ds$y_bar, ds$n_tilde, ds$N)
            s2_total <- calc_var_tot(ds$M, ds$N, ds$n_tilde, s2_within, s2_between)
            out <- list(sample_statistics = c(s2_within = s2_within,
                                            s2_between = s2_between,
                                            s2_total = s2_total))

            ## Population parameters and ANOVA table ---------------------------
            sigma2_hat <- s2_within
            X <- names(sigma2_hat)
            for (x in X) {
                tau2_hat <- max(s2_between[x] - s2_within[x] / ds$n_tilde, 0)
                rho_hat <-  intraclass_cor(tau2_hat, sigma2_hat[x])
                if (stats::var(ds$n_j) == 0) {
                    if (calc.se) {
                        se_rho <- calc_se_rho(rho_hat, ds$n_j, ds$N)
                    } else {
                        se_rho <- NA
                    }
                } else {
                    se_rho <- NA
                    if (calc.se) {
                        warning(
                            "SE not yet implemented for different ",
                            "sample sizes. You can set calc.se=FALSE ",
                            "to get rid of this message."
                        )
                    }
                }
                out$population_estimates[[x]] <- c(sigma2_hat = sigma2_hat[x],
                                                tau2_hat = tau2_hat,
                                                rho_hat = rho_hat,
                                                se_rho = se_rho)

                ### ANOVA table ................................................
                if (print) {
                    message("\nANOVA table for ", pluralize(n), ", ", x)
                    print_anova(s2_within[x], s2_between[x], s2_total[x],
                                    sigma2_hat[x], tau2_hat, rho_hat, se_rho,
                                    ds$n_tilde, ds$M, ds$N)
                }
            }
            if (print & sapply(data, class)[1] == "list" &
                (n != names(data)[length(names(data))])) cli::cat_rule()
            out_complete[[n]] <- out
        }
    }
    if (!print) {
        if (length(out_complete) > 1) {
            return(out_complete)
        } else {
            return(out)
        }
    }
}
