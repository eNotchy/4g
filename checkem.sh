#!/bin/bash
emacs -Q --batch -f batch-byte-compile 4g.el
# REVIEW: Don't attempt native-compile on platforms where it doesn't make sense. Which are...?
emacs -Q --batch -f batch-native-compile 4g.el
emacs -Q --batch -l checkdoc --eval '(let ((checkdoc-force-docstrings-flag nil)) (checkdoc-file "4g.el"))'
emacs -Q --batch -L . -l 4g.el -l test/4g-tests.el -f ert-run-tests-batch-and-exit
