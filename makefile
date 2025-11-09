test-ploc:
	guile -L . -s tests/ploc-test-suite.scm

test-tree:
	guile -L . -s tests/tree-test-suite.scm

bench:
ifdef D
	guile -L . -l tests/benches.scm -c "(import (tests benches)) (run-benches 128 $D)"
else
	guile -L . -l tests/benches.scm -c "(import (tests benches)) (run-all-benches 128)"
endif
