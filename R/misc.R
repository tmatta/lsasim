# This file contains miscellaneous functions that are not to be used by a package user.

#' @title Prints welcome message on package load
#' @param libname no idea, but will break devtools::document() if removed
#' @param pkgname name of the package
#' @description Prints "This is lsasim <version number>" on package load
#' @note This function was adapted from the lavaan package, so credit for it goes to lavaan's creator, Yves Rosseel
#' @references Yves Rosseel (2012). lavaan: An R Package for Structural Equation
#'  Modeling. Journal of Statistical Software, 48(2), 1-36. URL
#'  http://www.jstatsoft.org/v48/i02/.
.onAttach <- function(libname, pkgname) {
    version <- read.dcf(file   = system.file("DESCRIPTION", package = pkgname),
                        fields = "Version")
    packageStartupMessage("This is ", paste(pkgname, version))
}