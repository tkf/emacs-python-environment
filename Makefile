CARTON ?= carton
export EMACS ?= emacs
EMACS_TEST = ${CARTON} exec ${EMACS} -Q \
--directory . --load test-python-environment.el

.PHONY: test deps clean purge

test: deps
	${EMACS_TEST} --batch -f ert-run-tests-batch-and-exit

itest: deps
	${EMACS_TEST} --eval "(ert t)"

deps: elpa
elpa: Carton
	${CARTON} install
	touch $@

clean:
	rm -f *.elc

purge: clean
	rm -rf elpa
