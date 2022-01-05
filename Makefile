EMACS ?= emacs

.PHONY: all
all: clean compile test

.PHONY: clean
clean: 
	@echo "Removed: " *.elc
	@rm -f *.elc

.PHONY: compile
compile: *.el
	$(EMACS) -Q --batch -L . -f batch-byte-compile *.el

.PHONY: test
test: parcel.elc
	$(EMACS) -Q --batch -L . -l ert -l ./test/parcel-test.el \
  --eval "(let ((ert-quiet t)) (ert-run-tests-batch-and-exit))"

