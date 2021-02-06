if (!require("pkgdown")) {
  install.packages("pkgdown", repos = "http://cran.rstudio.com")
}
pkgdown::build_site()
