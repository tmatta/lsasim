# Contains miscellaneous functions that are not to be used by a package user.

.onAttach <- function(libname, pkgname) {
    # Prints "This is lsasim <version number>" on package load
    version <- read.dcf(file   = system.file("DESCRIPTION", package = pkgname),
                        fields = "Version")
    packageStartupMessage("This is ", paste(pkgname, version))
}