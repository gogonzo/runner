rmdsources := $(wildcard *.Rmd)
rmdvignettes := 	$(rmdsources:.Rmd=rmd)

all: deps ${rmdvignettes}

%rmd: %.Rmd
	Rscript -e 'rmarkdown::render("$<")'

deps:
	Rscript -e 'if (!require("knitr")) install.packages("knitr", repos = "http://cran.rstudio.com")'
	Rscript -e 'if (!require("rmarkdown")) install.packages("rmarkdown", repos = "http://cran.rstudio.com")'

clean:
	@rm -rf auto
