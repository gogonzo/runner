rmdsources := $(wildcard *.Rmd)
rmdvignettes := 	$(rmdsources:.Rmd=rmd)

all: deps ${rmdvignettes}

%rmd: %.Rmd
	$(R_HOME)/bin/Rscript -e 'rmarkdown::render("$<")'

deps:
	$(R_HOME)/bin/Rscript -e 'if (!require("knitr")) install.packages("knitr", repos = "http://cran.rstudio.com")'
	$(R_HOME)/bin/Rscript -e 'if (!require("rmarkdown")) install.packages("rmarkdown", repos = "http://cran.rstudio.com")'

clean:
	@rm -rf auto
