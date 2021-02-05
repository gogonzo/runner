lintr_single <- function(file, ...) {
  if (!require(lintr)) {
    install.packages("lintr", repos = "http://cran.rstudio.com")
  }
  file <- normalizePath(file)
  lints <- lint(
    file,
    linters = with_defaults(
      line_length_linter = line_length_linter(120),
      cyclocomp_linter = NULL
    )
  )
  has_lints <- length(lints) > 0
  lint_output <- NULL
  if (has_lints) {
    lint_output <- lapply(lints, function(x) {
      paste(capture.output(print(x)), collapse = "\n")
    })
  }
  list(basename(file), has_lints, paste(lint_output, sep = "\n"))
}

files <- commandArgs(trailingOnly = TRUE)
files <- grep("\\.R", x = files, value = TRUE)
outputs <- lapply(files, lintr_single)
is_error <- vapply(outputs, function(x) x[[2]], FUN.VALUE = logical(1))
if (any(isTRUE(is_error))) {
  errors <- c("Some lint errors:", unlist(lapply(outputs, function(x) x[[3]])))
  stop(paste(errors, collapse = "\n\n"))
}
