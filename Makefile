CASK ?= cask
export EMACS ?= emacs
EMACS_TEST = ${CASK} exec ${EMACS} -Q \
--directory . --load test-python-environment.el

ELPA_DIR = \
	.cask/$(shell ${EMACS} -Q --batch --eval '(princ emacs-version)')/elpa
# See: cask-elpa-dir

.PHONY: test deps clean purge

test: deps
	${EMACS_TEST} --batch -f ert-run-tests-batch-and-exit

itest: deps
	${EMACS_TEST} --eval "(ert t)"

deps: ${ELPA_DIR}
${ELPA_DIR}: Cask
	${CASK} install
	touch $@

clean:
	rm -f *.elc

purge: clean
	rm -rf ${ELPA_DIR}
