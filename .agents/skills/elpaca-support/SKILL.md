---
name: elpaca-support
description: Help with using Elpaca, the Emacs package manager. Use when the user asks how to install, configure, update or remove packages with Elpaca, asks about recipes, orders, queues, menus, use-package integration, pinning or lock files, is migrating from straight.el or package.el, or wants to file an Elpaca bug or support issue. For errors, warnings and failed installs use elpaca-debug; for a clean reproduction use elpaca-reproduce.
---

# Elpaca support

Answer usage questions accurately, route real failures to the right skill, and
help the user write an issue the maintainer can act on.

## Working rules

- You usually cannot see the user's Emacs. Ask them to run a command and paste
  the **text** result (not a screenshot).
- Sources of truth, best first: the user's installed Elpaca source
  (`elpaca-sources-directory`, default `~/.emacs.d/elpaca/sources/elpaca/`),
  the manual (`doc/manual.md`), command docstrings (`C-h f`, `C-h v`), then the
  wiki (<https://github.com/progfolio/elpaca/wiki>), which can lag the code.
  If you answer from memory, say so and point at the docstring.
- Ask for `M-x elpaca-version` early. It reports the Elpaca commit, installer
  version, Emacs version and git version. The current installer version is
  `0.12`; anything else means the installer block in their init is stale.
- Do not edit the user's init file, delete directories, or run package
  operations for them unless asked. Offer snippets instead.
- Requirements: Emacs >= 27.1 and git.

## Mental model

- **Recipe** `(ID . PROPS)` tells Elpaca how to fetch and build a package.
  **Order**: a symbol or partial recipe passed to `elpaca`.
  **Menu**: a source of recipes (MELPA, NonGNU/GNU ELPA, Org, ...).
  **Queue**: a group of orders processed together.
- Recipe precedence, highest first: `elpaca-recipe-functions`, the declared
  recipe, `elpaca-order-functions`, `elpaca-menu-functions`.
  `M-x elpaca-info` shows the menu recipe and the computed recipe.
- **Elpaca is asynchronous.** It processes queues *after* Emacs has read the
  init file, so top-level forms that are not inside an `elpaca` BODY run first.
  Put code that needs a package in that declaration's BODY (or its
  `use-package` config), use `elpaca-after-init-hook` instead of
  `after-init-hook` / `emacs-startup-hook`, and reserve `:wait t` /
  `(elpaca-wait)` for packages the init file itself needs at top level
  (for example a package that provides a `use-package` keyword). Declare
  wait-requiring packages first.
- Files live under `elpaca-directory` (default `<user-emacs-directory>/elpaca/`)
  in `sources/` (clones), `builds/` (linked build output) and `cache/`.
  Builds are symlinks into sources; `elpaca-no-symlink-mode` copies instead.

## Everyday operations

| Task | Command | Key in `elpaca-manager` |
|---|---|---|
| Find packages | `elpaca-manager`, `elpaca-info` | `g m` |
| Try for this session only | `elpaca-try` | `i` then `x` |
| Fetch updates | `elpaca-fetch`, `elpaca-fetch-all` | `f` then `x` |
| Merge fetched updates | `elpaca-merge`, `elpaca-merge-all` | `m` then `x` |
| Update (alias of pull) | `elpaca-update`, `elpaca-update-all` | `p` then `x` |
| Rebuild | `elpaca-rebuild` | `r` then `x` |
| Delete | `elpaca-delete` | `d` then `x` |
| View logs | `elpaca-log` | `g l` |
| Visit source / build dir | `elpaca-visit` (`C-u` for build dir) | `v` / `C-u v` |
| Open package website | `elpaca-browse` | `b` |

- The manual recommends fetch, review, then merge rather than pulling directly.
- Packages installed with `elpaca-try` and friends are not loaded after a
  restart. To persist, declare them with `(elpaca ...)` in the init file after
  the installer.
- Manager search: `s` prompts for a regexp; `|` limits a term to a column;
  `#tag` applies a tag (`#installed`, `#declared`, `#marked`, `#orphan`,
  `#unique`, `#random`); `!#tag` inverts it.

## Frequent questions

- **Installing / bootstrapping.** Use the installer block from the README (also
  `doc/installer.el`). It must come before any other Elpaca call. Put
  `(setq package-enable-at-startup nil)` in `early-init.el` and remove
  package.el calls such as `(package-activate-all)`. On Windows, symlinks must
  work or `(elpaca-no-symlink-mode)` must be enabled; the wiki also reports
  that the official Git for Windows client works where an MSYS2 git can mangle
  arguments.
- **use-package.** `(elpaca elpaca-use-package (elpaca-use-package-mode))`
  enables `:ensure` with `t`, `nil`, an ID symbol, or an order/recipe. Use
  `:ensure nil` for built-ins, the `emacs` pseudo-feature, and features that
  belong to another package. Wait for a keyword-providing package with
  `:ensure (:wait t)`.
- **A package is not in the menu / recipe missing.** `M-x elpaca-update-menus`
  (`C-u` updates all menus). Check the recipe with `M-x elpaca-info`.
- **Changed a recipe.** Re-evaluate the `elpaca` form interactively to
  reprocess it. Rebuilding does not reload code; restart Emacs afterwards.
- **Pinning.** `:pin t` excludes a package from update commands. In the source,
  `:tag` and `:ref` also count as pins, and `elpaca-info` shows a "pinned"
  line.
- **Lock files.** `elpaca-write-lock-file` writes one; set `elpaca-lock-file`
  to use it (`elpaca-menu-lock-file` must be first in `elpaca-menu-functions`,
  which is the default).
- **Newer built-in package needed.** Elpaca does not update built-ins on its
  own; declare it explicitly, e.g. `(elpaca compat)`. To stop a built-in being
  installed as someone's dependency, add it to `elpaca-ignored-dependencies`.
- **Coming from straight.el.** `:flavor`, `:includes` and `:upstream` have no
  Elpaca equivalent; `:fork` becomes `:remotes`; straight's `:source` becomes
  `:inherit`. Elpaca does have a `:type` keyword, but it selects the package
  type (`git`, `tar`, `file`); there is no `built-in` type, so use
  `elpaca-ignored-dependencies` instead. `:depth` takes an integer, `nil`
  (full clone), `blobless` or `treeless`. Elpaca enforces minimum dependency
  versions, which straight does not, so new "Outdated dependency" failures are
  expected.

## Routing

- Error, warning, failed or stuck order, "package installed but config does not
  work": use **elpaca-debug**.
- Suspected Elpaca bug, or unsure whether it is the user's config: use
  **elpaca-reproduce**.
- A bug inside a package that installed fine: that package's own tracker.

## Filing an issue

Blank issues are disabled; the tracker uses a "Bug and/or Support Request"
template at <https://github.com/progfolio/elpaca/issues>. Before suggesting the
user file one, confirm they have:

1. Read the manual, wiki and relevant docstrings, and searched open and closed
   issues for duplicates.
2. Reproduced it on the latest Elpaca and a stable Emacs release (development
   Emacs builds are not officially supported).
3. Collected: `M-x elpaca-version` output, operating system, and a specific
   description (expected vs. actual, exact error text, backtrace or log).
   For a single package include its `M-x elpaca-info` output.
4. Kept the report to one issue, with text rather than screenshots.

Draft the issue text for the user to review and submit; do not open it for
them. A results buffer from `elpaca-test` is already formatted for pasting.
