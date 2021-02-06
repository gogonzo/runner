if (!require("devtools")) {
  install.packages("devtools", repos = "http://cran.rstudio.com")
}

devtools::check_win_devel()
devtools::check_win_oldrelease()
devtools::check_win_release()
