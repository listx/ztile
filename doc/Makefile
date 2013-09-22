TEXS = $(shell find -type f -regex ".*\.tex")
SRCS = $(shell find ../src -type f -regex ".*\.lhs")

all: $(TEXS) $(SRCS)
	xelatex -halt-on-error ztile.tex
watch: $(TEXS) $(SRCS)
	rm -f watch
	for i in $(TEXS) $(SRCS); do \
		echo $$i >> watch; \
	done
