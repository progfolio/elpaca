---
name: elpaca-reproduce
description: Build a minimal, shareable reproduction of an Elpaca problem with the elpaca-test macro, which installs Elpaca in a throwaway Emacs environment. Use to decide whether a problem comes from Elpaca or from the user's configuration, to bisect an init file, to test a branch or commit, or to produce test output for a bug report.
---

# Reproducing Elpaca problems with `elpaca-test`

`elpaca-test` starts a child Emacs in a temporary directory, installs Elpaca
there, and runs the forms you give it. The user's real config and
`elpaca-directory` are untouched, and `:dir` refuses to be the real
`user-emacs-directory`.

You usually cannot run this yourself. Write the form, ask the user to evaluate
it, and ask them to paste the full results buffer back.

## Running a test

- Evaluate the form in a buffer with `lexical-binding` enabled (`*scratch*`
  works); the macro errors otherwise. Use `C-x C-e` or `C-j`.
- `elpaca-test` ships in `elpaca-test.el`, which the standard installer links
  via `:files`. If `elpaca-test` is void, their installer block is probably out
  of date.
- It needs network access: unless an init file is supplied or `:ref local` is
  used, it downloads the upstream `doc/installer.el`.
- A non-interactive test finishes with a results buffer holding **Test Case**,
  **Host Env** (Elpaca commit, installer version, Emacs, git) and **Output**,
  already formatted for pasting into an issue. It must be shared complete and
  unaltered.

## Keywords

`:name` label, `:init` (forms, `(:file "path")`, or `user` for their own
`init.el`), `:early-init` (same forms), `:before` (forms run in the *current*
Emacs to stage files), `:ref` (branch, tag, commit, or `local`), `:depth`,
`:repo`, `:installer`, `:interactive` (start an interactive Emacs instead of
batch), `:keep` (do not delete the test directory), `:timeout` (seconds before
pending orders are failed), `:args` (extra Emacs arguments), `:buffer`, `:dir`.

With no `:init` the test uses the upstream `doc/init.el`. Forms given to `:init`
replace everything after the installer in it.

## Workflow

1. **Baseline.** `(elpaca-test)`. If this fails, suspect the environment
   (network, git, Emacs build) or Elpaca itself.
2. **Minimal case.** Replace the init with only what is needed:
   ```emacs-lisp
   (elpaca-test
     :name "short description of the symptom"
     :timeout 120
     :init
     (elpaca some-package))
   ```
   `:timeout` makes a hung order fail instead of hanging the test.
3. **Their configuration.** If the minimal case passes but their setup fails,
   run `(elpaca-test :init (:file "~/.emacs.d/init.el"))` (or `:init user`,
   with `:early-init user` if they have an early-init file). Then bisect by
   keeping half of the init at a time until the smallest failing set is left.
   Note this runs their config in a child Emacs, so it may reach the network or
   run side effects.
4. **Other versions.** `:ref "branch-or-commit"` tests another revision of
   Elpaca; `:ref local` tests the local clone and cannot be combined with
   `:init (:file ...)`. Confirm a bug against the latest Elpaca first.
5. **Interactive bugs.** Add `:interactive t` (UI, prompts, key bindings) and
   `:keep t` to inspect the directory afterwards.
6. **Synthetic packages.** `elpaca-test-package-stub` can generate a tiny stub
   package file (options `:requires`, `:version`, `:body`, `:path`), meant to be
   called from `:before`. Read its source in `elpaca-test.el` before using it.

## Interpreting results

- Test passes with the minimal init but fails with theirs: the cause is in
  their configuration; hand back to **elpaca-debug** with the bisected snippet.
- Test fails even with the minimal init on the latest Elpaca and a stable
  Emacs: likely an Elpaca bug. Use the issue checklist in **elpaca-support**
  and include the whole results buffer.

Before the user shares output, have them skim it for private data such as home
directory paths or tokens that came from their init.
