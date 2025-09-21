test-ploc:
	guile -L . -s tests/ploc-test-suite.scm

test-tree:
	guile -L . -s tests/tree-test-suite.scm

bench:
	guile -L . -l tests/benches.scm -c "(import (tests benches)) (run-all-benches 128)"
