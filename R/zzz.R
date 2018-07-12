.onAttach <- function(libname, pkgname) {
  ## This function should not be called by the user.
  ## It displays a message when the package is being loaded.
  packageStartupMessage(## display message
    "\n Welcome to adRes,",
    "\n ",
    "\n This package has been created to document the analysis of the paper:",
    "\n Mate choice, reproductive success and inbreeding in white rhinos",
    "\n by Kretzschmar et al. (in prep)",
    "\n ",
    "\n Type ?matingRhinos for information about how to use this package!",
    "\n ",
    "\n WARNING: this package has not been conceived for general use.",
    "\n"
  )
}
