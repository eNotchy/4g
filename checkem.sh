#!/bin/bash

readonly VERBOSE=$([ "$1" = -v ] && echo true || echo false)

$VERBOSE && echo Running byte-compile...
emacs -Q --batch -f batch-byte-compile 4g.el

if emacs -Q --batch --eval '(unless (native-comp-available-p) (kill-emacs 1))'; then
	$VERBOSE && echo Running native-compile...
	emacs -Q --batch -f batch-native-compile 4g.el
else
	echo native-comp unavailable, skipping...
fi

$VERBOSE && echo Running ert tests...
emacs -Q --batch -L . -l 4g.el -l test/4g-tests.el -f ert-run-tests-batch-and-exit

$VERBOSE && echo Running checkdoc...
emacs -Q --batch -l checkdoc --eval '(let ((checkdoc-force-docstrings-flag nil)) (checkdoc-file "4g.el"))'

$VERBOSE && echo Done.
