# prepare the package for release
PKGNAME := $(shell sed -n "s/Package: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGVERS := $(shell sed -n "s/Version: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGSRC  := $(shell basename `pwd`)
GITFIL := $(shell git diff --cached --name-only --diff-filter=ACM)

all: generate-codemeta render-readme check clean

deps:
	Rscript -e "devtools::install_deps(dependencies = TRUE)"

build: docs
	cd ..;\
	R CMD build $(PKGSRC) -no-build-vignettes

check: build
	cd ..;\
	R CMD check $(PKGNAME)_$(PKGVERS).tar.gz --as-cran -no-build-vignettes

install:
	R CMD INSTALL . --no-multiarch --with-keep.source

render-pkgdown:
		Rscript "inst/checks/pkgdown.R"

code-coverage:
	Rscript "inst/checks/code-coverage.R"

check-spelling:
	Rscript "inst/checks/spelling.R"

docs: render-readme

render-readme:
	Rscript -e "\
	if(!require("rmarkdown"))\n\
	install.packages('rmarkdown', repos = 'http://cran.rstudio.com')\n\
	rmarkdown::render('README.Rmd', output_format = rmarkdown::md_document('gfm'))"

#render-vignettes:
#	$(MAKE) -C vignettes/

check-win:
		Rscript "inst/checks/check-win.R"

cran-release:
	Rscript -e "\
	if (!require('devtools')) install.packages('devtools', repos = 'http://cran.rstudio.com')\n\
	built_path <- devtools:::build_cran('.', args = args)\n\
	devtools:::upload_cran('../runner',  built_path)"

generate-codemeta:
	Rscript -e "\
	if (!require('codemetar')) install.packages('codemetar', repos = 'http://cran.rstudio.com')\n\
	codemetar::write_codemeta()"

check-commit-linters:
	Rscript "inst/checks/lintr.R" $(shell git diff --cached --name-only --diff-filter=ACM)


clean:
	cd ..;\
	$(RM) -r $(PKGNAME).Rcheck/
	rm vignettes/*.html
	rm *.o *.so
