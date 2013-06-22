CARTON ?= carton
export EMACS ?= emacs

.PHONY: test deps clean purge

test: deps
	${CARTON} exec ${EMACS} -Q --batch \
		--directory . --load test-python-environment.el \
		-f ert-run-tests-batch-and-exit

deps: elpa
elpa: Carton
	${CARTON} install
	touch $@

clean:
	rm -f *.elc

purge: clean
	rm -rf elpa
