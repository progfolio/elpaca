- [Installation](#installation)
  - [Requirements](#installation-requirements)
  - [Installer](#installer)
- [Usage](#usage)
  - [Quick Start](#quick-start)
  - [Basic concepts](#basic-concepts)
    - [Recipes](#recipes)
      - [:host | :fetcher](#recipe-keyword-host)
      - [:repo](#recipe-keyword-repo)
      - [:branch](#recipe-keyword-branch)
      - [:tag](#recipe-keyword-tag)
      - [:ref](#recipe-keyword-ref)
      - [:pin](#recipe-keyword-pin)
      - [:depth](#recipe-keyword-depth)
      - [:files](#recipe-keyword-files)
      - [:protocol](#recipe-keyword-protocol)
      - [:remotes](#recipe-keyword-remotes)
      - [:main](#recipe-keyword-main)
      - [:build](#recipe-keyword-build)
      - [:inherit](#recipe-keyword-inherit)
      - [:pre-build](#recipe-keyword-pre-build)
      - [:post-build](#recipe-keyword-post-build)
      - [:autoloads](#recipe-keyword-autoloads)
      - [:version](#recipe-keyword-version)
      - [:vars](#recipe-keyword-vars)
      - [:wait](#recipe-keyword-wait)
      - [Inheritance precedence](#inheritance-precedence)
      - [elpaca-recipe-functions](#elpaca-recipe-functions)
    - [Menus](#menus)
      - [elpaca-menu-functions](#elpaca-menu-functions)
      - [Updating menus](#orga583e10)
    - [Orders](#orders)
      - [elpaca-order-functions](#elpaca-order-functions)
    - [Queues](#queues)
    - [Installing Packages](#installing-packages)
    - [Lock Files](#lock-files)
  - [use-package Integration](#use-package-integration)
- [UI](#ui)
  - [Searching](#searching)
  - [Search tags](#search-tags)

Elpaca is an elisp package manager. It allows users to find, install, update, and remove third-party packages for Emacs. It is a replacement for the built-in Emacs package manager, package.el.

> Copyright (C) 2022-2025 Nicholas Vollmer
> 
> You can redistribute this document and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
> 
> This document is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.


<a id="installation"></a>

# Installation


<a id="installation-requirements"></a>

## Requirements

Elpaca requires:

-   Emacs >= 27.1
-   git (minimum version TBD)


<a id="installer"></a>

## Installer

To install Elpaca, add the following elisp to your init.el. It must come before any calls to other Elpaca functions/macros. This will clone Elpaca into your `user-emacs-directory` under the `elpaca` subdirectory. It then builds and activates Elpaca.

```emacs-lisp
(defvar elpaca-installer-version 0.11)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (let ((load-source-file-function nil)) (load "./elpaca-autoloads"))))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))
```

-   Windows users must be able to create symlinks<sup><a id="fnr.-0-1" class="footref" href="#fn.-0-1" role="doc-backlink">1</a></sup>, or enable `elpaca-no-symlink-mode`

```emacs-lisp
;; Uncomment for systems which cannot create symlinks:
;; (elpaca-no-symlink-mode)
```

You&rsquo;ll also want to disable package.el in your early-init file<sup><a id="fnr.-0-2" class="footref" href="#fn.-0-2" role="doc-backlink">2</a></sup>:

```emacs-lisp
(setq package-enable-at-startup nil)
```

And remove anything related to package.el in your init file. e.g. calls to `(package-activate-all)`.


<a id="usage"></a>

# Usage


<a id="quick-start"></a>

## Quick Start

| Operation                             | UI (keys apply in elpaca-ui-mode)   | completing-read interface commands     |
|------------------------------------- |----------------------------------- |-------------------------------------- |
| Finding Packages                      | `g` `m` (or `M-x` `elpaca-manager`) | `elpaca-info`                          |
| Trying Packages (for current session) | `i` `x`                             | `elpaca-try`                           |
| Fetching Package Updates              | `f` `x`                             | `elpaca-fetch` or `elpaca-fetch-all`   |
| Merging Updates                       | `m` `x`                             | `elpaca-merge` or `elpaca-merge-all`   |
| Updating Packages<sup>\*</sup>        | `p` `x`                             | `elpaca-update` or `elpaca-update-all` |
| Rebuilding Packages                   | `r` `x`                             | `elpaca-rebuild`                       |
| Deleting Packages                     | `d` `x`                             | `elpaca-delete`                        |
| View Package Logs                     | `g` `l`                             | `elpaca-log`                           |
| Visit Package Repository Directory    | `v`                                 | `elpaca-visit`                         |
| Visit Package Build Directory         | `C-u` `v`                           | `C-u M-x` `elpaca-visit`               |
| Browse Package Website                | `b`                                 | `elpaca-browse`                        |

​\* Update is an alias for &ldquo;pull&rdquo;. It&rsquo;s encouraged to fetch, review, and **then** merge package updates rather than pulling.

Packages installed via the above commands are not loaded on subsequent Emacs sessions (after restarting). To install and load packages persistently (across Emacs restarts), use the `elpaca` macro in your init file after the installer. ([installer](#installer))

For example:

```emacs-lisp
;; Install a package via the elpaca macro
;; See the "recipes" section of the manual for more details.

;; (elpaca example-package)

;; Install use-package support
(elpaca elpaca-use-package
  ;; Enable use-package :ensure support for Elpaca.
  (elpaca-use-package-mode))

;;When installing a package used in the init file itself,
;;e.g. a package which adds a use-package key word,
;;use the :wait recipe keyword to block until that package is installed/configured.
;;For example:
;;(use-package general :ensure (:wait t) :demand t)

;; Expands to: (elpaca evil (use-package evil :demand t))
(use-package evil :ensure t :demand t)

;;Turns off elpaca-use-package-mode current declaration
;;Note this will cause evaluate the declaration immediately. It is not deferred.
;;Useful for configuring built-in emacs features.
(use-package emacs :ensure nil :config (setq ring-bell-function #'ignore))
```

**IMPORTANT**:

Elpaca installs and activates packages asynchronously. Elpaca processes its package queues *after* Emacs reads the init file.<sup><a id="fnr.-0-3" class="footref" href="#fn.-0-3" role="doc-backlink">3</a></sup> Consider the following example:

```emacs-lisp
(elpaca package-a (message "First")) ; Queue First
(message "Second") ; Second messaged
(elpaca package-b (message "Third")) ; Queue Third
(elpaca-process-queues) ; Process queue: First messaged, Third messaged.
```

&ldquo;Second&rdquo; will be message *before* &ldquo;First&rdquo; and &ldquo;Third&rdquo;. If a form should be evaluated after a package is installed/activated, put it in that package declaration&rsquo;s *BODY*. Declaration *BODY* forms are evaluated synchronously in declared order. e.g.

```emacs-lisp
(elpaca package-a (message "First") (message "Second"))  ; Queue First, Second
(elpaca package-b (message "Third"))  ; Queue Third
(elpaca-process-queues) ; Process queue: First, Second, then Third messaged.
```

Add configuration which relies on `after-init-hook`, `emacs-startup-hook`, etc to `elpaca-after-init-hook` so it runs after Elpaca has activated all queued packages. This includes loading of saved customizations. e.g.

```emacs-lisp
(setq custom-file (expand-file-name "customs.el" user-emacs-directory))
(add-hook 'elpaca-after-init-hook (lambda () (load custom-file 'noerror)))
```


<a id="basic-concepts"></a>

## Basic concepts


<a id="recipes"></a>

### Recipes

A recipe provides Elpaca with the metadata necessary to build and install a package. It is a list of the form:

```emacs-lisp
(ID . PROPS)
```

*ID* is a symbol uniquely identifying the package. *PROPS* is a plist with any of the following recipe keywords:


<a id="recipe-keyword-host"></a>

#### :host | :fetcher

A symbol or string representing the hosting service of the repository. Strings are inserted in the URI verbatim.

```emacs-lisp
(example :host github)
(example :fetcher gitlab)
(example :host "www.example.com")
```


<a id="recipe-keyword-repo"></a>

#### :repo

A string of the form `USER/REPO` when used with the `:host` keyword; a local file path or remote URL when `:host` is not used.

```emacs-lisp
(example :host github :repo "user/example") ;;downloaded from github
```

```emacs-lisp
(local :repo "~/repos/local/") ;;built from local filesystem
```

```emacs-lisp
(remote :repo "https://foo.example/example.git") ;;remote clone
```

A cons cell of the form `(REMOTE . LOCAL)` will rename the local repository:

```emacs-lisp
(remote :repo ("https://foo.example/example.git" . "local-name"))
;;This will still clone the repository under `elpaca-repos-directory'
(remote :repo ("~/repos/local" . "local-name"))
```


<a id="recipe-keyword-branch"></a>

#### :branch

The repository branch to check out when installing the package.

```emacs-lisp
(example :host github :repo "user/example" :branch "main")
```


<a id="recipe-keyword-tag"></a>

#### :tag

The tag to check out when installing the package.

```emacs-lisp
(example :host github :repo "user/example" :tag "v1.0")
```


<a id="recipe-keyword-ref"></a>

#### :ref

The git ref<sup><a id="fnr.4" class="footref" href="#fn.4" role="doc-backlink">4</a></sup> to check out when installing the package.

```emacs-lisp
(example :host github :repo "user/example" :ref "a76ca0a") ;; Check out a specific commit.
```


<a id="recipe-keyword-pin"></a>

#### :pin

When non-nil, ignore the package during update commands.

```emacs-lisp
(example :pin t)
```


<a id="recipe-keyword-depth"></a>

#### :depth

The package repository&rsquo;s history depth.

```emacs-lisp
(example :depth treeless) ;; https://git-scm.com/docs/partial-clone
(example :depth blobless) ;; https://git-scm.com/docs/partial-clone
(example :depth 1) ;; Shallow clone with history truncated to 1 commit.
(example :depth nil) ;; Full repository clone.
```


<a id="recipe-keyword-files"></a>

#### :files

The files linked from the package&rsquo;s repository to its build directory.

Each element of the list is either:

-   The symbol `:defaults`, which expands to `elpaca-default-files-directive`.
-   A string naming files or folders. Shell glob patterns match multiple files.
-   A list starting with the `:exclude` keyword. The remaining elements are not linked.

```emacs-lisp
(example :files (:defaults "extensions/*")) ;; Link everything in the extensions folder.
(example :files (:defaults (:exclude "*.c"))) ;; Exclude all files with the "c" file extension.
```


<a id="recipe-keyword-protocol"></a>

#### :protocol

The protocol to use when cloning repositories.

The value must be a symbol, either `https` or `ssh`.

```emacs-lisp
(example :protocol https) ; Use the https protocol.
(example :protocol ssh) ; Use the ssh protocol.
```


<a id="recipe-keyword-remotes"></a>

#### :remotes

Configures the repository remotes<sup><a id="fnr.5" class="footref" href="#fn.5" role="doc-backlink">5</a></sup>.

The value must be a single remote spec or a list of remote specs. The first remote given will have its ref checked out when cloning the repository. A spec may be a string to rename the default remote. The following will rename the cloned remote (usually &ldquo;origin&rdquo; by git convention) to &ldquo;upstream&rdquo;:

```emacs-lisp
(example :remotes "upstream")
```

In order to add a another remote, a spec may be a list of the form:

```emacs-lisp
("NAME" [PROPS])
```

*NAME* is a string indicating the name of the remote. *PROPS* is an optional plist used to override inherited recipe keywords.

For example:

```emacs-lisp
(example :host github :repo "upstream/example"
         :remotes ("fork" :repo "fork/zenburn-emacs"))
```

Will add a remote named fork which points to a repository hosted on the same forge as the upstream remote. The following does the same above, additionally adding a third remote at a different forge.

```emacs-lisp
(example :host github :repo "upstream/example"
         :remotes (("fork" :repo "fork/zenburn-emacs") ; :host github inherited from above
                   ("other" :host gitlab :repo "other/zenburn-emacs")))
```


<a id="recipe-keyword-main"></a>

#### :main

The name of the main elisp file. When provided this can speed up the process of cloning and loading a package&rsquo;s dependencies. When declared `nil`, skip dependency check.

```emacs-lisp
(example :main "example.el")
```

```emacs-lisp
(example :main nil)
```


<a id="recipe-keyword-build"></a>

#### :build

A list of build steps, nil or t. To remove steps from `elpaca-default-build-steps` by starting the list with the `:not` keyword.

```emacs-lisp
(example :build (:not elpaca--byte-compile))
```


<a id="recipe-keyword-inherit"></a>

#### :inherit

When non-nil, inherit *PROPS* from `elpaca-order-functions` and possibly `elpaca-menu-functions`. For example, without inheritance:

```emacs-lisp
(elpaca-recipe '(doct :inherit nil))
```

returns the recipe as declared:

```emacs-lisp
(:source nil :inherit nil :package "doct")
```

With inheritance enabled:

```emacs-lisp
(elpaca-recipe '(dracula-theme :inherit t)))
```

```emacs-lisp
(:package "dracula-theme" :fetcher github :repo "dracula/emacs" :files
          ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
           "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
           "docs/*.info" "docs/*.texi" "docs/*.texinfo"
           (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                     "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
          :source "MELPA" :protocol https :inherit t :depth treeless)
```

the Elpaca&rsquo;s MELPA menu provides the rest of the recipe.

The value may also be a menu symbol or list of menu symbols. This is a per-recipe way of setting `elpaca-menu-functions`.

```emacs-lisp
(elpaca-recipe '(dracula-theme :inherit elpaca-menu-non-gnu-elpa))
```

```emacs-lisp
(:package "dracula-theme" :repo
          ("https://github.com/dracula/emacs" . "dracula-theme") :files
          ("*"
           (:exclude ".git" "INSTALL.md" "screenshot.png" "start_emacs_test.sh"
                     "test-profile.el"))
          :source "NonGNU ELPA" :protocol https :inherit
          elpaca-menu-non-gnu-elpa :depth treeless)
```


<a id="recipe-keyword-pre-build"></a>

#### :pre-build

Commands and/or elisp evaluated prior to `:build` steps with the package repository as `default-directory`. Each command is either an elisp form or a list of strings executed in a shell context of the form:

```emacs-lisp
("executable" "argument"...)
```

For example:

```emacs-lisp
(elpaca (example :pre-build (("configure") ("make" "install"))))
```


<a id="recipe-keyword-post-build"></a>

#### :post-build

The same as `:pre-build`, but run just before activating a package.

```emacs-lisp
(elpaca (example :post-build (message "activate next")))
```


<a id="recipe-keyword-autoloads"></a>

#### :autoloads

The name of the file the package&rsquo;s autoload file. When `nil`, autoload loading and generation are disabled for the package. When `t`, the default autoload file is generated/loaded (`PACKAGE-NAME-autoloads.el`). The value may also be a string which is expanded relative to the package&rsquo;s build directory. e.g. `"org-loaddefs.el"`.


<a id="recipe-keyword-version"></a>

#### :version

A function which must accept an Elpaca struct as its sole argument. It must return a version string understood by `version-to-list`. e.g.

```emacs-lisp
(elpaca (example :version (lambda (_) "1.0.0")))
```


<a id="recipe-keyword-vars"></a>

#### :vars

A list of values to bind via `let*` when executing a package&rsquo;s build steps. e.g.

```emacs-lisp
(elpaca (example :vars ((some-dynamic-var t))))
```

The current elpaca data structure and current build step are bound to the `elpaca` and `elpaca-build-step` variables within the form.

Wrapping a declaration in a `let*` form will not suffice because the steps are run asynchronously. The bindings will not be in scope by the time each build step is run.


<a id="recipe-keyword-wait"></a>

#### :wait

When non-nil, process all queued orders immediately before continuing. e.g.

```emacs-lisp
(elpaca (general :wait t))
```


<a id="inheritance-precedence"></a>

#### Inheritance precedence

The following list shows the precedence of inheritance from highest to lowest:

-   elpaca-recipe-functions
-   declared recipe
-   elpaca-order-functions
-   elpaca-menu-functions

The `elpaca-info` command shows inherited recipe properties:

```emacs-lisp
( :package "evil"
  ;; Inherited from elpaca-order-functions.
  :depth 1
  :inherit t
  :protocol https
  ;; Inherited from elpaca-menu-item.
  :files ( :defaults "doc/build/texinfo/evil.texi"
           (:exclude "evil-test-helpers.el"))
  :fetcher github
  :repo "emacs-evil/evil")
```


<a id="elpaca-recipe-functions"></a>

#### elpaca-recipe-functions

The abnormal hook `elpaca-recipe-functions` runs via `run-hook-with-args-until-success` just before installing the package. Each function in the list should accept the current recipe as its sole argument and return either nil or a plist. The first function to return a plist has its return value merged with the current recipe.

This is useful if you want to guarantee the values of certain keywords despite allowing recipe inheritance.

```emacs-lisp
(let ((elpaca-recipe-functions '((lambda (_) "Add extra cheese to everything."
                                   (list :cheese 'extra)))))
  (elpaca-recipe 'burger))
```

```emacs-lisp
(:source nil :protocol https :inherit t :depth treeless :package "burger"
         :cheese extra)
```


<a id="menus"></a>

### Menus

A menu is a function which returns an alist of the form:

```emacs-lisp
((ID . DATA)...)
```

*ID* is a symbol uniquely identifying a package. *DATA* is a plist of package metadata. *DATA* must contain the following keywords:

-   **:recipe:** A package recipe. ([recipe](#recipes))
-   **:source:** A string naming the menu.

It may also provide additional information about a package. For example, the Elpaca UI utilizes the following keywords when present:

-   **:url:** The package&rsquo;s website URL.
-   **:description:** A description of the package.
-   **:date :** The time of package&rsquo;s last update.

The function must accept one of the following *REQUEST* symbols as an argument:

-   **index:** Return the alist described above
-   **update:** update the menu&rsquo;s alist.

```emacs-lisp
(defun elpaca-menu-minimal (request_)
  "A minimal menu example.
Ignore REQUEST, as this is a static, curated list of packages."
  '((example :source "EXAMPLE" :recipe (example :host github :repo "user/example"))
    (two :source "EXAMPLE" :recipe (two :host gitlab :repo "user/two"))))
```

Menus allow one to offer Elpaca users curated lists of package recipes. For example, [melpulls](https://www.github.com/progfolio/melpulls) implements an Elpaca menu for pending MELPA packages.


<a id="elpaca-menu-functions"></a>

#### elpaca-menu-functions

The `elpaca-menu-functions` variable contains menu functions for the following package sources by default:

-   [MELPA](https://www.github.com/melpa/melpa)
-   [Org](https://git.savannah.gnu.org/cgit/emacs/org-mode.git/)
-   [Org-contrib](https://git.sr.ht/~bzg/org-contrib)
-   [GNU ELPA Mirror](https://www.github.com/emacs-straight/gnu-elpa-mirror)
-   [NonGNU ELPA](https://elpa.nongnu.org)

Menus are checked in order until one returns the requested menu item or the menu list is exhausted.


<a id="orga583e10"></a>

#### Updating menus

Menus can be updated via the `elpaca-update-menus` command. Doing so will fetch the latest recipes from the menu source and overwrite the menu item cache for the updated menus.


<a id="orders"></a>

### Orders

At a minimum, an order is a symbol which represents the name of a menu item ([menu](#menus)):

```emacs-lisp
(elpaca example)
```

An order may also be a partial or full recipe:

```emacs-lisp
(elpaca (example :host gitlab))
(elpaca (example :host gitlab :repo "user/example" :inherit nil))
```


<a id="elpaca-order-functions"></a>

#### elpaca-order-functions

The abnormal hook `elpaca-order-functions` runs via `run-hook-with-args-until-success` before `elpaca-menu-functions`. Each function in the list should accept the current order as its sole argument and return either nil or a plist. The first function to return a plist has its return value merged with the current order.

This is useful for declaring default order properties. For example, the following function disables recipe inheritance by default:

```emacs-lisp
(let ((elpaca-order-functions '((lambda (_) "Disable inheritance." '(:inherit nil)))))
  (elpaca-recipe 'burger))
```

```emacs-lisp
(:source nil :inherit nil :package "burger")
```


<a id="queues"></a>

### Queues

Elpaca installs packages asynchronously. Orders ([orders](#orders)) are automatically queued in a list. When all of a queues orders have either finished or failed Elpaca considers it &ldquo;processed&rdquo;.

Queues ensure packages installation, activation, and configuration take place prior to packages in other queues. The `:wait` recipe keyword splits the current queue and immediately begins processing prior queues. This is useful when one wants to use a package from a previous queue in their init file. For example, a package which implements an Elpaca menu ([menu](#menus)):

```emacs-lisp
(elpaca (melpulls :host github :repo "progfolio/melpulls" :wait t)
  (add-to-list 'elpaca-menu-functions #'melpulls)
  (elpaca-update-menus #'melpulls)))

;; Implicitly queued into a new queue.
(elpaca menu-item-available-in-melpulls)
```


<a id="installing-packages"></a>

### Installing Packages

-   **elpaca:** `(order &rest body)`

Installs *ORDER* ([orders](#orders)) and evaluate *BODY* after processing ORDER&rsquo;s queue ([queue](#queues)).

This macro is for programmatic use in one&rsquo;s init file. Any of the following will install the &ldquo;example&rdquo; package:

```emacs-lisp
(elpaca example) ;; recipe looked up in `elpaca-menu-functions'.
```

```emacs-lisp
(elpaca example (message "Messaged after the order's queue has processed."))
```

```emacs-lisp
(elpaca (example :host github :repo "user/example"))
```

```emacs-lisp
(elpaca `(example :host github :repo "user/example"
                  ,@(when (eq system-type 'darwin) ;; backqouting supported
                      (list :pre-build ((message "Mac specific pre-build"))))))
```

Interactively evaluating an `elpaca` declaration will re-process the order. This can be used to change a package&rsquo;s recipe prior to rebuilding it. Note that rebuilding a package does not **reload** a package. It&rsquo;s best to restart Emacs after a successful rebuild if you wish to have the changes loaded.


<a id="lock-files"></a>

### Lock Files

A lock file is a collection of recipes for the exact versions of installed packages. They can be used to build different versions of an Emacs configuration when combined with init file package declarations.

The `elpaca-write-lock-file` command is used to write a lock file to disk. Setting the `elpaca-lock-file` variable to that file&rsquo;s path will cause Elpaca to use those versions of the recipes when installing packages assuming the `elpaca-menu-lock-file` is the first menu in `elpaca-menu-functions`.


<a id="use-package-integration"></a>

## use-package Integration

Adding the following elisp to your init file will enable Elpaca&rsquo;s optional integration with the use-package configuration macro:

```emacs-lisp
(elpaca elpaca-use-package
  ;; Enable Elpaca support for use-package's :ensure keyword.
  (elpaca-use-package-mode))
```

```emacs-lisp
(use-package example :ensure t)
```

Expands to:

```emacs-lisp
(elpaca example (use-package example))
```

With `elpaca-use-package-mode` enabled the `:ensure` use-package keyword can also accept a recipe.

```emacs-lisp
(use-package example :ensure (:host host :repo "user/repo"))
```

Expands to:

```emacs-lisp
(elpaca (example :host host :repo "user/repo")
  (use-package example))
```

Use the `:wait` recipe keyword to block until a package has been installed and configured. For example:

```emacs-lisp
(use-package general :ensure (:wait t) :demand t :ensure t)
;; use-package declarations beyond this point may use the `:general' use-package keyword.
```

In order to turn off `elpaca-use-package-mode` for a given declaration, specify `:ensure nil`:

```emacs-lisp
;; `emacs' is a pseudo-feature which can be used to configure built-in functionality.
(use-package emacs :ensure nil :config (setq ring-bell-function #'ignore))
```

Note forms like this are not deferred by Elpaca&rsquo;s queue system.


<a id="ui"></a>

# UI

Elpaca has a UI mode available for managing packages. The main entry points to the UI are the `elpaca-manager` and `elpaca-log` commands. Each of these commands utilize modes derived from `elpaca-ui-mode`.

The following commands are available in the `elpaca-ui-mode`:

| Command                    | Binding | Description                                                                 |
|-------------------------- |------- |--------------------------------------------------------------------------- |
| elpaca-ui-send-input       | !       | Send input string to current process.                                       |
| elpaca-ui-show-hidden-rows | +       | Append rows up to N times ‘elpaca-ui-row-limit’.                            |
| elpaca-ui-info             | RET     | Show info for current package.                                              |
| elpaca-ui-browse-package   | b       | Browse current package’s URL via ‘browse-url’.                              |
| elpaca-ui-mark-delete      | d       | Mark package at point for ‘elpaca-delete’.                                  |
| elpaca-ui-mark-fetch       | f       | Mark package at point for ‘elpaca-fetch’.                                   |
| elpaca-ui-search-marked    | g a     | Search for &ldquo;#unique #marked&rdquo;                                    |
| elpaca-ui-search-installed | g i     | Search for &ldquo;#unique #installed&rdquo;                                 |
| elpaca-log                 | g l     | When INTERACTIVE is non-nil, Display ‘elpaca-log-buffer’ filtered by QUERY. |
| elpaca-manager             | g m     | Display Elpaca’s package management UI.                                     |
| elpaca-ui-search-orphaned  | g o     | Search for &ldquo;#unique #orphan&rdquo;                                    |
| elpaca-ui-search-refresh   | g r     | Rerun the current search for BUFFER.                                        |
| elpaca-ui-search-tried     | g t     | Search for &ldquo;#unique #installed !#declared&rdquo;                      |
| elpaca-ui-mark-try         | i       | Mark package at point for ‘elpaca-try’.                                     |
| elpaca-ui-mark-merge       | m       | Mark package at point for ‘elpaca-merge’.                                   |
| elpaca-ui-mark-pull        | p       | Mark package at point for ‘elpaca-pull’.                                    |
| elpaca-ui-mark-rebuild     | r       | Mark package at point for ‘elpaca-rebuild’.                                 |
| elpaca-ui-search           | s       | Filter current buffer by QUERY. If QUERY is nil, prompt for it.             |
| elpaca-ui-unmark           | u       | Unmark current package or packages in active region.                        |
| elpaca-ui-visit            | v       | Visit current package’s repo or BUILD directory.                            |
| elpaca-ui-execute-marks    | x       | Execute each mark in ‘elpaca-ui-marked-packages’.                           |

-   **Function: elpaca-manager `&optional recache`:** Display packages registered with Elpaca. Packages can searched for, installed, updated, rebuilt, and deleted from this interface. When `RECACHE` is non-nil, via lisp or interactively via the `universal-argument`, recompute Elpaca&rsquo;s menu item cache before display.

-   **Function: elpaca-log `&optional query`:** Display the log for queued packages filtered by `QUERY`. For acceptable values for `QUERY` see [searching](#searching).


<a id="searching"></a>

## Searching

The `elpaca-ui-search` command (`s`) prompts the user for a search query in the minibuffer. Altering the query updates the UI table. Calling with a `universal-argument` (`C-u`) populates the minibuffer with the current search query for editing. Setting the query to an empty string resets the query to `elpaca-ui-default-query`. The buffer&rsquo;s header line displays the current query.

Queries are regular expressions checked against each row of the UI table. For example, `test` will match any row which contains the string &ldquo;test&rdquo;. Some characters change the matching behavior in queries.

The pipe character, `|`, will delimit text searches to specific columns of the table. Considering the following table:

| number | A     | B     | C |
|------ |----- |----- |--- |
| 1      | one   | two   | 3 |
| 2      | four  | five  | 6 |
| 3      | seven | eight | 9 |

The query `o` will match rows 1 (on `one`) and 2 (on `four`). The query `3 |` will only search for `3` in the first column and match row three. While `||| 3` Will search for `3` in the fourth column of the table and match row 1.

The pound (a.k.a. hash) character, `#`, followed by the name of a search tag filters table entries. For example `#random` will display 10 random entries. If the search tag accepts arguments they may passed by wrapping the tag name in parenthesis. e.g. `#(random 20)` will display 20 random entries.


<a id="search-tags"></a>

## Search tags

-   **User Option: elpaca-ui-search-tags:** An alist of with elements of the form (NAME . FILTER). `NAME` is a unique symbol describing the filter function. The user types name after `#` in the minibuffer to apply the filter. `FILTER` is a function which must accept a list of `tabulated-list-entries` as its first argument. It may accept additional, optional arguments. The function must return a list of `tabulated-list-entries`.
    
    For example, the following search tag will embolden the first column of the `elpaca-manager` table when the search query contains `#bold-names`:

```emacs-lisp
(defun +elpaca-bold-names (entries)
  (cl-loop for entry in entries
           for copy = (copy-tree entry)
           for cols = (cadr copy)
           for name = (aref cols 0)
           do (setf (aref cols 0) (propertize name 'face '(:weight bold)))
           collect copy))

(cl-pushnew (cons 'bold-names #'+elpaca-bold-names) elpaca-ui-search-tags)
```

## Footnotes

<sup><a id="fn.1" class="footnum" href="#fnr.1">1</a></sup> [windows symlink guide](https://www.howtogeek.com/16226/complete-guide-to-symbolic-links-symlinks-on-windows-or-linux/)

<sup><a id="fn.2" class="footnum" href="#fnr.2">2</a></sup> [early-init file](https://www.gnu.org/software/emacs/manual/html_node/emacs/Early-Init-File.html)

<sup><a id="fn.3" class="footnum" href="#fnr.3">3</a></sup> This is so Elpaca can build a proper dependency tree. It ensures packages the user explicitly requests are not preempted by dependencies of other packages.

<sup><a id="fn.4" class="footnum" href="#fnr.4">4</a></sup> [git ref](https://git-scm.com/book/en/v2/Git-Internals-Git-References)

<sup><a id="fn.5" class="footnum" href="#fnr.5">5</a></sup> [remotes](https://git-scm.com/book/en/v2/Git-Basics-Working-with-Remotes)
