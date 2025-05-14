# Frontend to dune.

.PHONY: default build install uninstall test clean

default: build

build:
	dune build

test:
	@dune exec ./src/concrete/tests_concrete.exe
#	uncomment when working on step 2
#	@dune exec ./src/abstract/tests_abstract.exe -- --const
#	uncomment when working on step 4
#	@dune exec ./src/abstract/tests_abstract.exe -- --interval

interpretertest:
	@dune exec ./src/concrete/tests_concrete.exe -- --verbose

consttest:
	@dune exec ./src/abstract/tests_abstract.exe -- --verbose --const

intervaltest:
	@dune exec ./src/abstract/tests_abstract.exe -- --verbose --interval

pdf:
	@find . -type f -name "*.tig" | sort | while read -r file; do \
		echo "Processing file: $$file"; \
		./src/abstract/tiger_abstract.exe --domain interval --pdf "$$file"; \
	done

install:
	dune install

uninstall:
	dune uninstall

clean:
	dune clean
	rm -f *.pdf
	find . -name '*.tex' -exec rm -f {} +
