#' @title Print ANOVA table
#' @description Prints Analysis of Variance table for `cluster_gen` output.
#' @param data list output of `cluster_gen`
#' @return Printed ANOVA table
#' @export
anova_table <- function(data) {
    # Create summary statistics ================================================
    data_summary <- summarize_clusters(data, print = FALSE)

    # Create other relevant input ==============================================
    for (n in names(data)) {
        ds <- data_summary[[n]]

        ## Sample statistics ---------------------------------------------------
        s2_within <- calc_var_within(ds$n_j, ds$s2_j, ds$M, ds$N)
        s2_between <- calc_var_between(ds$n_j, ds$y_bar_j, ds$y_bar, ds$n_tilde, ds$N)
        s2_total <- calc_var_tot(ds$M, ds$N, ds$n_tilde, s2_within, s2_between)

        ## Population parameters and ANOVA table -------------------------------
        sigma2_hat <- s2_within
        X <- names(sigma2_hat)
        for (x in X) {
            tau2_hat <- max(s2_between[x] - s2_within[x] / ds$n_tilde, 0)
            rho_hat <-  intraclass_cor(tau2_hat, sigma2_hat[x])
            if (stats::var(ds$n_j) == 0) {
                se_rho <- calc_se_rho(rho_hat, ds$n_j, ds$N)
            } else {
                se_rho <- NA
                warning("SE not yet implemented for different sample sizes")
            }

            ### ANOVA table ...................................................,
            message("\nANOVA table for ", pluralize(n), ", ", x)
            print_anova_table(s2_within[x], s2_between[x], s2_total[x], sigma2_hat[x], tau2_hat, rho_hat, se_rho, ds$n_tilde, ds$M, ds$N)
        }
        if (n != names(data)[length(names(data))]) cli::cat_rule()
    }
}