if (!require("covr")) {
  install.packages("covr", repos = "http://cran.rstudio.com")
}
covr::codecov()
