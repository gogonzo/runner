if (!require("spelling")) {
  install.packages("spelling", repos = "http://cran.rstudio.com")
}
out <- capture.output(
  spelling::spell_check_package()
)

if (!identical(out, "No spelling errors found.")) {
  stop(paste(out, collapse = "\n"))
}
