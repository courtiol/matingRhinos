.onAttach <- function(libname, pkgname) {
  ## This function should not be called by the user.
  ## It displays a message when the package is being loaded.
  packageStartupMessage(## display message
    '\n Welcome to matingRhinos,',
    '\n ',
    '\n This package has been created to document the analysis of the paper:',
    '\n Mate choice, reproductive success and inbreeding in white rhinoceros...',
    '\n by Kretzschmar et al. (in prep)',
    '\n ',
    '\n Type ?matingRhinos for information about how to use this package!',
    '\n ',
    '\n WARNING: this package has not been conceived for general use.',
    '\n'
  )
}

.info_package <- function() {
  print(paste("number of functions =", length(ls("package:matingRhinos"))))
  if (requireNamespace("R.utils", quietly = TRUE)) {
    files <- dir(paste0(system.file(package = "matingRhinos"), "/R/"))
    filenames_R <- paste0(system.file(package = "matingRhinos"), "/R/", files)
    lines_code <- sum(sapply(filenames_R, function(file) R.utils::countLines(file)))
    print(paste("number of lines of code =", lines_code))
  } else {message("Install the package R.utils for more info.")}
  return(invisible(NULL))
}
