# prepare the package for release
PKGNAME := $(shell sed -n "s/Package: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGVERS := $(shell sed -n "s/Version: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGSRC  := $(shell basename `pwd`)

all: check clean

deps:
	Rscript -e 'if (!require("Rd2roxygen")) install.packages("Rd2roxygen", repos="http://cran.rstudio.com")'

build:
	cd ..;\
	R CMD build --no-manual $(PKGSRC)

build-cran: render-vignettes
	cd ..;\
	R CMD build $(PKGSRC)

install: build
	cd ..;\
	R CMD INSTALL $(PKGNAME)_$(PKGVERS).tar.gz

check: build
	cd ..;\
	R CMD check $(PKGNAME)_$(PKGVERS).tar.gz --as-cran

travis: build
	cd ..;\
	R CMD check $(PKGNAME)_$(PKGVERS).tar.gz --no-manual

pkgdown:
	Rscript -e 'if (!require("pkgdown")) install.packages("knitr", repos = "http://cran.rstudio.com")'
	Rscript -e 'pkgdown::build_site()'

readme:
	Rscript -e 'rmarkdown("README.Rmd")'

render-vignettes:
	$(MAKE) -C vignettes/rmd

clean:
	cd ..;\
	$(RM) -r $(PKGNAME).Rcheck/
	rm vignettes/*.html
	rm *.o *.so
