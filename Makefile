EMACS ?= emacs

.PHONY: all
all: clean autoloads compile test

.PHONY: clean
clean: 
	@echo "Removed: " *.elc elpaca-autoloads.el
	@rm -f *.elc elpaca-autoloads.el

.PHONY: compile
compile: *.el
	$(EMACS) -Q --batch -L . -f batch-byte-compile *.el

.PHONY: test
test: elpaca.elc
	$(EMACS) -Q --batch -L . -l ert -l ./test/elpaca-tests.el \
  --eval "(let ((ert-quiet t)) (ert-run-tests-batch-and-exit))"

.PHONY: autoloads
autoloads:
	$(EMACS) -Q -L . -l elpaca --batch --eval '(elpaca-generate-autoloads "elpaca" default-directory)'
