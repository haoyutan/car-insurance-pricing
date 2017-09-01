TARGET = ./target
DATA = ht-d1-v0.3.csv.gz
DOC_1 = D1-Data-Analysis-Report

all: build

clean:
	rm -rf $(TARGET)

init:
	mkdir -p $(TARGET)

build: init build_doc

build_doc: build_doc1

build_doc1:
	mkdir -p $(TARGET)/doc/$(DOC_1)
	-ln -s ../../../src/$(DOC_1).Rmd $(TARGET)/doc/$(DOC_1)/$(DOC_1).Rmd
	-ln -s ../../../data/$(DATA) $(TARGET)/doc/$(DOC_1)/$(DATA)
	echo "library(knitr); knit('./$(DOC_1).Rmd');" > \
		$(TARGET)/doc/$(DOC_1)/generate_md.R
	cd $(TARGET)/doc/$(DOC_1); R CMD BATCH generate_md.R; \
		pandoc -f markdown -t html -s -c pandoc.css --mathjax \
			--toc -o $(DOC_1).html $(DOC_1).md
