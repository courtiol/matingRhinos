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

.pretty_p <- function(p, prefix = TRUE, raw = FALSE, digits = 2L) {
  stopifnot(length(p) == 1)
  if (is.na(p)) return(NA)
  out_txt <- format(signif(p, digits = digits), nsmall = 2L)
  if (raw) return(out_txt)
  if (p < 0.001) {
    out_txt <- ifelse(prefix, 'p < 0.001', '< 0.001')
  } else if (out_txt != "1.00") {
    out_txt <- ifelse(prefix, paste('p =', out_txt), out_txt)
  } else out_txt <- ifelse(prefix, paste('p ~', out_txt), paste('~', out_txt))
  return(out_txt)
}

.pretty_ps <- Vectorize(.pretty_p)

.pretty_star <- function(p) {
  stars <- rep('ns', length(p))
  stars[p < 0.1] <- '.'
  stars[p < 0.05] <- '*'
  stars[p < 0.01] <- '**'
  stars[p < 0.001] <- '***'
  stars
}
