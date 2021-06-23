#' @title Print the ANOVA table
#' @param s2_within Within-class variance
#' @param s2_between Between-class variance
#' @param s2_total Total variance
#' @param tau2_hat estimate of the true between-class correlation
#' @param sigma2_hat estimate of the true within-class correlation
#' @param rho_hat estimated intraclass correlation
#' @param se_rho standard errors of `rho_hat`
#' @param n_tilde function of the variance of n_N, M and N. See documentation and code of \code{lsasim:::summary.lsasimcluster} for details
#' @param M total sample size
#' @param N number of classes j
#' @references Snijders, T. A. B., & Bosker, R. J. (1999). Multilevel Analysis. Sage Publications.
#' @seealso anova
print_anova <- function(s2_within, s2_between, s2_total,
    sigma2_hat, tau2_hat, rho_hat, se_rho, n_tilde, M, N) {
    sources <- c(
        "Within-group variance",
        "Between-group variance",
        "Total variance"
    )
    table <- data.frame(
        "Source" = sources,
        "Sample statistic" = c(s2_within, s2_between, s2_total),
        "Population estimate" = c(sigma2_hat, tau2_hat, NA)
    )
    rho <- data.frame(
        "Estimated" = rho_hat,
        "Standard error" = se_rho
    )
    cat("ANOVA estimators\n")
    print(table)
    cat("\nIntraclass correlation\n")
    print(rho)

    # Testing for group differences ============================================
    F_stat <- n_tilde * s2_between / s2_within
    F_stat_p_value <- stats::pf(F_stat, N - 1, M - N, lower.tail = FALSE)
    cat("\nTesting for group differences\n")
    cat("F-statistic:", F_stat, "on", N - 1, "and", M - N, "DF. p-value: ",
        F_stat_p_value, "\n")
}