---
name: elpaca-debug
description: Diagnose problems with Elpaca, the Emacs package manager. Use when the user pastes an Elpaca error, warning, log or backtrace, or reports an order that failed, is blocked or never finishes, a package that will not install, build or update, startup warnings (installer version mismatch, "Package.el loaded before Elpaca", "Duplicate item ID queued", "loaded before Elpaca activation", "Unable to determine recipe URL", "Outdated dependency"), or a package that installs but is not configured or loaded correctly.
---

# Debugging Elpaca

Work from evidence in the user's own logs, change one thing at a time, and
start with the least destructive fix. You usually cannot run their Emacs: ask
for commands to be run and for the output to be pasted as text.

## 1. Gather

- `M-x elpaca-version` output, operating system, and how Emacs is started
  (GUI, terminal, daemon, wrapped by Nix or a launcher script).
- The exact message and where it appeared: `*Warnings*`, `*elpaca-log*`, the
  echo area, or `*Backtrace*`.
- For a specific package: `M-x elpaca-info` for it. This is usually the single
  most useful artifact: menu recipe, computed recipe, installed version,
  dependencies and dependents, statuses, and the package's full log.
- The installer block from their init and the failing declaration, with any
  secrets removed.

## 2. Where Elpaca reports things

- `*elpaca-bootstrap*`: git and compile output from the installer's first run.
- `*elpaca-log*` (`M-x elpaca-log`): opens by itself at startup when an order
  needs building or has failed. Useful search queries: `#unique`, `#latest`
  (since the last queue processing), `#linked-errors` (byte-compile errors with
  links to the source), `#update-log`. `!` inverts a tag and `|` splits a query
  by column, e.g. `#unique | failed`.
- `*Warnings*`: Elpaca emits several of the warnings below here.
- Status colours: finished, failed, blocked, and busy. Busy means a subprocess
  has produced no output for `elpaca-busy-interval` (default 60 s): suspect the
  network, a stalled git, or a prompt waiting for input (`!` in the log buffer
  sends input to the process).
- A blocked order prevents `elpaca-after-init-hook` from running, and blocked
  or failed orders prevent `elpaca-post-queue-hook`. "My post-install config
  never ran" often means an order is stuck.

## 3. Known messages (wording taken from the current source)

| Message | Meaning and fix |
|---|---|
| `Package.el loaded before Elpaca` | package.el was activated first. Set `(setq package-enable-at-startup nil)` in `early-init.el`; remove `(package-initialize)` / `(package-activate-all)` from init. |
| `FILE installer version does not match .../doc/installer.el.` | `FILE` is the init file (or "Init"). Its installer block is not version 0.12. Replace it with the current one from the README or `doc/installer.el`. |
| `Unable to determine recipe URL` | (a) `use-package` with `:ensure t` (or `use-package-always-ensure`) on a built-in, the `emacs` pseudo-feature, or a sub-feature of another package (e.g. `latex` from auctex): add `:ensure nil`. (b) Stale or corrupt menu cache, or a very new package: `M-x elpaca-update-menus`. (c) Incomplete recipe for your own or a local package: add `:repo` (plus `:host` if hosted). |
| `Unable to find main elisp file for PKG` | Unusual main-file name or location: add `:main "path/to/file.el"`. |
| `Outdated dependency. DEP version X installed, Y required` | Update DEP, then check what version Elpaca detects with `M-x elpaca-info` on DEP. If only a commit hash shows: shallow clones hide tags, so try `:depth nil`; for unusual tag names use `:version-regexp`; for odd file layout use `:main`; if the package carries no version metadata, supply a `:version` function. If DEP is a built-in that is too old, declare it explicitly (e.g. `(elpaca compat)`); Elpaca does not update built-ins automatically. |
| `Missing dependency DEP` | The dependency was never queued, usually because no menu has a recipe for it. Try `elpaca-update-menus`, or declare it with a full recipe. |
| `Duplicate item ID queued: ID` | Raised while the init file loads. The same package is declared twice (e.g. two `use-package` forms). Merge them. |
| `ID previously queued as dependency of: (...)` | Raised while the init file loads. The package was declared after an `elpaca-wait` had already installed it as a dependency. Declare it before its dependents, or move every declaration that needs a wait to the top. `M-x elpaca-dependents` lists dependents. |
| `PKG loaded before Elpaca activation` | The feature was already loaded when Elpaca activated its copy. Typical cause: init loaded through `org-babel-load-file`, which loads built-in Org first. Load the feature only after activation, or do not queue it; if it is queued only as a dependency, add it to `elpaca-ignored-dependencies`. |
| `Unable to determine elpaca-core-date` / `Emacs X assigned Y elpaca-core-date` | Development Emacs build. Set `(setq elpaca-core-date '(YYYYMMDD))` (the build date) *before* the installer. On a stable release with current Elpaca this is likely a bug worth reporting. |
| `Too many open files` | OS file-handle limit. Lower `elpaca-queue-limit`, e.g. `(setq elpaca-queue-limit 30)`. |
| Symlink errors (Windows) | Enable `(elpaca-no-symlink-mode)`. If git arguments are mangled, the wiki reports the official Git for Windows client works where an MSYS2 git may not. |

Also rule out basics: Elpaca needs network access to the forges and ELPAs
during install and most operations.

## 4. Installed, but behaves wrongly

- **Config did not apply or hooks did not fire.** Elpaca installs and activates
  after the init file is read, so `after-init-hook` and `emacs-startup-hook`
  run *before* packages are activated. Move that code into the package's BODY
  or `use-package` config, or use `elpaca-after-init-hook`. The same applies to
  loading `custom-file` and to `desktop-save-mode` (which reads the desktop in
  `after-init-hook`; defer `desktop-read` to `elpaca-after-init-hook`).
- **A `use-package` keyword from another package is "unknown".** The provider
  must be installed before that form is expanded: `:ensure (:wait t)` on it.
- **Rebuilt or changed the recipe but old behaviour persists.** Rebuilding does
  not reload code. Restart Emacs.
- **Package will not update.** Check `elpaca-info` for a "pinned" line (`:pin`,
  `:tag` and `:ref` all pin). `elpaca-update` is an alias of `elpaca-pull`;
  fetching, reviewing, then merging is the recommended path.

## 5. Backtraces

If the error happens while the init file is read, start Emacs with
`--debug-init`; once init finishes Elpaca turns `debug-on-error` on so later
asynchronous errors also produce backtraces. Otherwise run
`M-x toggle-debug-on-error` (or `(setq debug-on-error t)`) before triggering
the error. Set `(setq backtrace-line-length nil)` first so the user can copy
the whole, untruncated `*Backtrace*`.

## 6. Escalation ladder

Move down only when the step above did not help, and get the user's consent
before any destructive step.

1. Read `elpaca-info` and the log; `M-x elpaca-update-menus`.
2. `M-x elpaca-rebuild` for the package, then restart Emacs.
3. `M-x elpaca-delete` for the package. It asks for confirmation (`C-u` also
   deletes dependencies). If it is declared in init, it reinstalls on the next
   start. Deleting removes the source clone, so check for uncommitted local
   work first (`elpaca-visit`). It refuses while packages that depend on it are
   still on disk, and for a recipe whose `:repo` is a local path it removes
   only the build, not the user's own repository.
4. Remove that one package's folders under `builds/` and `sources/` by hand.
5. Last resort only: remove the whole `elpaca-directory`. It destroys the
   evidence and every package and cache, and is rarely necessary. Never lead
   with this.

## 7. Hand-off

If the problem survives and may be Elpaca's fault, use **elpaca-reproduce** to
separate it from the user's configuration, then the issue checklist in
**elpaca-support**. A bug in a package that installed correctly belongs in that
package's tracker.
