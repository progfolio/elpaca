EMACS ?= emacs

.PHONY: all
all: clean autoloads compile test

.PHONY: clean
clean: 
	@echo "Removed: " *.elc parcel-autoloads.el
	@rm -f *.elc parcel-autoloads.el

.PHONY: compile
compile: *.el
	$(EMACS) -Q --batch -L . -f batch-byte-compile *.el

.PHONY: test
test: parcel.elc
	$(EMACS) -Q --batch -L . -l ert -l ./test/parcel-test.el \
  --eval "(let ((ert-quiet t)) (ert-run-tests-batch-and-exit))"

.PHONY: autoloads
autoloads:
	$(EMACS) -Q --batch --eval '(make-directory-autoloads "./" "parcel-autoloads.el")'
