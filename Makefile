# prepare the package for release
PKGNAME := $(shell sed -n "s/Package: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGVERS := $(shell sed -n "s/Version: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGSRC  := $(shell basename `pwd`)

all: check clean

deps:
	Rscript -e 'if (!require("Rd2roxygen")) install.packages("Rd2roxygen", repos="http://cran.rstudio.com")'

build: docs
	cd ..;\
	R CMD build $(PKGSRC)

check: build
	cd ..;\
	R CMD check $(PKGNAME)_$(PKGVERS).tar.gz --as-cran

pkgdown:
	$(R_HOME)/Rscript -e 'if (!require("pkgdown")) install.packages("pkgdown", repos = "http://cran.rstudio.com")'
	$(R_HOME)/Rscript -e 'pkgdown::build_site()'

docs: render-readme

render-readme:
	$(R_HOME)/Rscript -e 'if (!require("rmarkdown")) install.packages("rmarkdown", repos = "http://cran.rstudio.com")'
	$(R_HOME)/Rscript -e 'rmarkdown::render("README.Rmd")'

render-vignettes:
	$(MAKE) -C vignettes/

clean:
	cd ..;\
	$(RM) -r $(PKGNAME).Rcheck/
	rm vignettes/*.html
	rm *.o *.so
