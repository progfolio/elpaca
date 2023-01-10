- [Installation](#installation)
  - [Requirements](#installation-requirements)
  - [Bootstrap Snippet](#bootstrap-snippet)
- [Usage](#usage)
  - [Quick Start](#quick-start)
  - [Basic concepts](#basic-concepts)
    - [Recipes](#recipes)
    - [Menus](#menus)
    - [Orders](#orders)
    - [Queues](#queues)
    - [Installing Packages](#installing-packages)
- [UI](#ui)
  - [Searching](#searching)
  - [Search tags](#search-tags)

Elpaca is an elisp package manager. It allows users to find, install, update, and remove third-party packages for Emacs. It is a replacement for the built-in Emacs package manager, package.el.

> Copyright (C) 2022-2023 Nicholas Vollmer
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
-   Windows users must be able to create symlinks.


<a id="bootstrap-snippet"></a>

## Bootstrap Snippet

To install Elpaca, add the following bootstrapping snippet to your init.el. It must come before any calls to other Elpaca functions/macros. This will clone Elpaca into your `user-emacs-directory` under the `elpaca` subdirectory. It then builds and activates Elpaca.

```emacs-lisp
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil
                              :build (:not elpaca--activate-package)))
(when-let ((repo  (expand-file-name "repos/elpaca/" elpaca-directory))
           (build (expand-file-name "elpaca/" elpaca-builds-directory))
           (order (cdr elpaca-order))
           ((add-to-list 'load-path (if (file-exists-p build) build repo)))
           ((not (file-exists-p repo))))
  (condition-case-unless-debug err
      (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
               ((zerop (call-process "git" nil buffer t "clone"
                                     (plist-get order :repo) repo)))
               (default-directory repo)
               ((zerop (call-process "git" nil buffer t "checkout"
                                     (or (plist-get order :ref) "--")))))
          (progn
            (byte-recompile-directory repo 0 'force)
            (require 'elpaca)
            (and (fboundp 'elpaca-generate-autoloads)
                 (elpaca-generate-autoloads "elpaca" repo))
            (kill-buffer buffer))
        (error "%s" (with-current-buffer buffer (buffer-string))))
    ((error)
     (warn "%s" err)
     (delete-directory repo 'recursive))))
(require 'elpaca-autoloads)
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))
```

You&rsquo;ll also want to disable package.el in your early-init file<sup><a id="fnr.1" class="footref" href="#fn.1" role="doc-backlink">1</a></sup>:

```emacs-lisp
(setq package-enable-at-startup nil)
```

And remove anything related to package.el in your init file. e.g. calls to `(package-activate-all)`.


<a id="usage"></a>

# Usage


<a id="quick-start"></a>

## Quick Start

| Operation                             | UI (keys apply in elpaca-ui-mode)  | completing-read interface commands           |
|------------------------------------- |---------------------------------- |-------------------------------------------- |
| Finding Packages                      | `M-x elpaca-manager`               | `elpaca-try`                                 |
| Trying Packages (for current session) | `i` `x`                            | `elpaca-try`                                 |
| Updating Packages                     | `u` `x`                            | `elpaca-update` or `M-x` `elpaca-update-all` |
| Rebuilding Packages                   | `r` `x`                            | `elpaca-rebuild`                             |
| Deleting Packages                     | `d` `x`                            | `elpaca-delete`                              |
| View Package Logs                     | `l` filters log to current package | `elpaca-log`                                 |
| View Package Statuses                 | `t` show most recent log entries   | `elpaca-status`                              |
| Visit Package Repository Directory    | `v`                                | `elpaca-visit`                               |
| Visit Package Build Directory         | `C-u` `v`                          | `C-u` `elpaca-visit`                         |
| Browse Package Website                | `b`                                | `elpaca-browse`                              |

Packages installed via the above commands are not loaded on subsequent Emacs sessions (after restarting). To install and load packages persistently (across Emacs restarts), use the `elpaca` or `elpaca-use-package` macros in your init file after bootstrapping. ([bootstrap snippet](#bootstrap-snippet))

For example:

```emacs-lisp
;; Install use-package
(elpaca use-package
  ;; Customize/Configure the package in the BODY of the macro.
  (setq use-package-always-defer t))

;; Expands to: (elpaca evil (use-package evil :demand t))
(elpaca-use-package evil :demand t)

;; Don't install anything. Defer execution of BODY
(elpaca nil (message "deferred"))
```

**IMPORTANT**:

Elpaca installs and activates packages asynchronously. Elpaca processes its package queues *after* Emacs reads the init file.<sup><a id="fnr.2" class="footref" href="#fn.2" role="doc-backlink">2</a></sup> Consider the following example:

```emacs-lisp
(elpaca nil (message "First")) ; Queue First
(message "Second") ; Second messaged
(elpaca nil (message "Third")) ; Queue Third
(elpaca-process-queues) ; Process queue: First messaged, Third messaged.
```

&ldquo;Second&rdquo; will be message *before* &ldquo;First&rdquo; and &ldquo;Third&rdquo;. Defer forms which are dependent on deferred forms. Wrapping the &ldquo;Second&rdquo; message in an `elpaca` declaration will fix the above example:

```emacs-lisp
(elpaca nil (message "First"))  ; Queue First
(elpaca nil (message "Second")) ; Queue Second
(elpaca nil (message "Third"))  ; Queue Third
(elpaca-process-queues) ; Process queue: First, Second, Third messaged.
```

Add any configuration which relies on `after-init-hook`, `emacs-startup-hook`, etc to `elpaca-after-init-hook` so it runs after Elpaca has activated all queued packages.


<a id="basic-concepts"></a>

## Basic concepts

The `elpaca-example` macro in the following examples reduces verbosity. It is not part of Elpaca.

```emacs-lisp
(defun elpaca-example-sort-plist (plist)
  "Return copy of PLIST with :package followed by lexically sorted key/val pairs."
  `(:package ,(plist-get plist :package)
             ,@(cl-loop for k in (cl-sort (cl-loop for key in plist by #'cddr
                                                   unless (eq key :package) collect key)
                                          #'string< :key #'symbol-name)
                        append (list k (plist-get plist k)))))

(defmacro elpaca-example (&rest body)
  "Execute BODY with a clean elpaca environment."
  `(let (elpaca-cache-menu-items
         elpaca-order-functions
         elpaca-recipe-functions
         (elpaca-menu-functions '(elpaca-example-menu)))
     (elpaca-example-sort-plist ,@body)))
```

Examples will use the following recipe menu. ([recipe menu](#menus)) It offers a &ldquo;burger&rdquo; package recipe:

```emacs-lisp
(defun elpaca-example-menu (_)
  '((burger . (:recipe ( :buns 2
                         :lettuce t
                         :tomato t
                         :beef t
                         :cheese t
                         :cook well-done
                         :from elpaca-example-menu)))))
```


<a id="recipes"></a>

### Recipes

A recipe provides Elpaca with the metadata necessary to build and install a package. It is a list of the form:

```emacs-lisp
(ITEM . PROPS)
```

*ITEM* is a symbol uniquely identifying the package. *PROPS* is a plist with any of the following recipe keywords:

-   **:host | :fetcher:** A symbol or string representing the hosting service of the repository.

```emacs-lisp
(example :host github)
(example :fetcher gitlab)
(example :host "www.example.com")
```

-   **:repo:** A string of the form `USER/REPO` when used with the `:host` keyword; a local file path or remote URL when `:host` is not used.

```emacs-lisp
(example :host github :repo "user/example") ;;downloaded from github
```

```emacs-lisp
(local :repo "~/repos/local/") ;;cloned from local filesystem
```

```emacs-lisp
(remote :repo "https://foo.example/example.git") ;;remote clone
```

-   **:branch:** The repository branch to check out when installing the package.

```emacs-lisp
(example :host github :repo "user/example" :branch "main")
```

-   **:tag:** The tag to check out when installing the package.

```emacs-lisp
(example :host github :repo "user/example" :tag "v1.0")
```

-   **:ref:** The git ref<sup><a id="fnr.3" class="footref" href="#fn.3" role="doc-backlink">3</a></sup> to check out when installing the package.

```emacs-lisp
(example :host github :repo "user/example" :ref "a76ca0a") ;; Check out a specific commit.
```

-   **:pin:** When non-nil, ignore the package during update commands.

```emacs-lisp
(example :pin t)
```

-   **:depth:** The package repository&rsquo;s history depth.

```emacs-lisp
(example :depth 1) ;; Shallow clone with history truncated to 1 commit.
(example :depth nil) ;; Full repository clone.
```

-   **:remotes:** Configures the repository remotes<sup><a id="fnr.4" class="footref" href="#fn.4" role="doc-backlink">4</a></sup>.

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

-   **:main:** The name of the main elisp file. When provided this can speed up the process of cloning and loading a package&rsquo;s dependencies.

```emacs-lisp
(example :main "example.el")
```

-   **:build:** A list of build steps, nil or t. To remove steps from `elpaca-default-build-steps` by starting the list with the `:not` keyword.

```emacs-lisp
(example :build (:not elpaca--byte-compile))
```

-   **:inherit:** When non-nil, inherit *PROPS* from `elpaca-order-functions` and possibly `elpaca-menu-functions`. For example, without inheritance:

```emacs-lisp
(elpaca-example (elpaca-recipe '(burger :inherit nil)))
```

returns the recipe as declared:

```emacs-lisp
(:package "burger" :inherit nil)
```

With inheritance enabled:

```emacs-lisp
(elpaca-example (elpaca-recipe '(burger :inherit t)))
```

the elpaca-example-menu provides the rest of the &ldquo;burger&rdquo; recipe.

```emacs-lisp
(:package "burger" :inherit t)
```

1.  Inheritance precedence

    The following list shows the order of precedence for inheritance. Each item takes precedence over the items which follow it.
    
    -   elpaca-recipe-functions
    -   declared recipe
    -   elpaca-order-functions
    -   elpaca-menu-functions
    
    ```emacs-lisp
    (elpaca-example
     (let ((elpaca-recipe-functions (lambda (recipe) '(:from recipe-functions :cheese extra)))
           (elpaca-order-functions (lambda (order) '(:from order-functions :tomato nil))))
       (elpaca-recipe '(burger))))
    ```
    
    ```emacs-lisp
    (:package "burger" :cheese extra :from recipe-functions :tomato nil)
    ```

2.  elpaca-recipe-functions

    The abnormal hook `elpaca-recipe-functions` runs via `run-hook-with-args-until-success` just before installing the package. Each function in the list should accept the current recipe as its sole argument and return either nil or a plist. The first function to return a plist has its return value merged with the current recipe.
    
    This is useful if you want to guarantee the values of certain keywords despite allowing recipe inheritance.
    
    ```emacs-lisp
    (elpaca-example
     (let ((elpaca-recipe-functions
            '((lambda (recipe)
                "If a recipe calls for cheese, I always want extra."
                (when (plist-get recipe :cheese) (list :cheese 'extra))))))
       (elpaca-recipe '(burger))))
    ```
    
    ```emacs-lisp
    (:package "burger")
    ```


<a id="menus"></a>

### Menus

A menu is a function which returns an alist of the form:

```emacs-lisp
((ITEM . DATA)...)
```

*ITEM* is a symbol uniquely identifying a package. *DATA* is a plist of package metadata. *DATA* must contain the following keywords:

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

1.  elpaca-menu-functions

    The `elpaca-menu-functions` variable contains menu functions for the following package sources by default:
    
    -   [MELPA](https://www.github.com/melpa/melpa)
    -   [Org](https://git.savannah.gnu.org/cgit/emacs/org-mode.git/)
    -   [Org-contrib](https://git.sr.ht/~bzg/org-contrib)
    -   [GNU ELPA Mirror](https://www.github.com/emacs-straight/gnu-elpa-mirror)
    -   [NonGNU ELPA](https://elpa.nongnu.org)
    
    Menus are checked in order until one returns the requested menu item or the menu list is exhausted.


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

1.  elpaca-order-functions

    The abnormal hook `elpaca-order-functions` runs via `run-hook-with-args-until-success` before `elpaca-menu-functions`. Each function in the list should accept the current order as its sole argument and return either nil or a plist. The first function to return a plist has its return value merged with the current order.
    
    This is useful for declaring default order properties. For example, the following function disables recipe inheritance by default:
    
    ```emacs-lisp
    (elpaca-example
     (let ((elpaca-order-functions '((lambda (_) '(:inherit nil)))))
       (elpaca-recipe 'burger)))
    ```
    
    ```emacs-lisp
    (:package "burger" :inherit nil)
    ```


<a id="queues"></a>

### Queues

Elpaca installs packages asynchronously. Orders ([orders](#orders)) are automatically queued in a list. When all of a queues orders have either finished or failed Elpaca considers it &ldquo;processed&rdquo;.

Queues ensure packages installation, activation, and configuration take place prior to packages in other queues. The `elpaca-queue` macro wraps calls to `elpaca`. It places orders in its *BODY* in their own queue. This is especially useful when one wants to install a package to use later on in their init file. For example, a package which implements an Elpaca menu ([menu](#menus)):

```emacs-lisp
(elpaca-queue
 (elpaca (melpulls :host github :repo "progfolio/melpulls")
   (add-to-list 'elpaca-menu-functions #'melpulls)
   (elpaca-update-menus #'melpulls)))
;; Implicitly queued into a new queue.
(elpaca menu-item-available-in-melpulls)
```


<a id="installing-packages"></a>

### Installing Packages

-   **elpaca:** `(order &rest body)`

Installs *ORDER* ([orders](#orders)) and executes *BODY* after processing ORDER&rsquo;s queue ([queue](#queues)).

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

If *ORDER* is nil, *BODY* is still executed after processing the current queue.

```emacs-lisp
(elpaca first (message "First configured"))
;; If this weren't wrapped in an `elpaca' call, it would execute FIRST
;; Due to the "first" and "third" package installing asynchronously.
(elpaca nil (message "Second"))
(elpaca third (message "Third configured"))
```

-   **elpaca-use-package:** `(order &rest body)`
    
    A wrapper for the use-package<sup><a id="fnr.5" class="footref" href="#fn.5" role="doc-backlink">5</a></sup> macro. *ORDER* is the same as above. *BODY* must conform to use-package&rsquo;s *ARGS*.
    
    ```emacs-lisp
    (elpaca use-package (require 'use-package)) ; install use-package
    (elpaca-use-package (example :host github :repo "user/example")
      :config (message "Example configured"))
    ```


<a id="ui"></a>

# UI

Elpaca has a UI mode available for managing packages. The main entry points to the UI are the `elpaca-manager`, `elpaca-log`, and `elpaca-status` commands. Each of these commands utilize `elpaca-ui-mode`.

The following commands are available in the `elpaca-ui-mode`:

| Command                    | Binding | Description                                                     |
|-------------------------- |------- |--------------------------------------------------------------- |
| elpaca-ui-send-input       | !       | Send input string to current process.                           |
| elpaca-ui-search-installed | I       | Search for &ldquo;#unique #installed&rdquo;                     |
| elpaca-ui-search-marked    | M       | Search for &ldquo;#unique #marked&rdquo;                        |
| elpaca-ui-search-orphaned  | O       | Search for &ldquo;#unique #orphan&rdquo;                        |
| elpaca-ui-search-refresh   | R       | Rerun the current search for BUFFER.                            |
| elpaca-ui-search-tried     | T       | Search for &ldquo;#unique #installed !#declared&rdquo;          |
| elpaca-ui-unmark           | U       | Unmark current package.                                         |
| elpaca-ui-browse-package   | b       | Browse current package’s URL via ‘browse-url’.                  |
| elpaca-ui-mark-delete      | d       | Mark package for delete action.                                 |
| elpaca-ui-mark-install     | i       | Mark package for install action.                                |
| elpaca-log                 | l       | Display ‘elpaca-log-buffer’.                                    |
| elpaca-manager             | m       | Display elpaca’s package management UI.                         |
| elpaca-ui-mark-rebuild     | r       | Mark package for rebuild action.                                |
| elpaca-ui-search           | s       | Filter current buffer by QUERY. If QUERY is nil, prompt for it. |
| elpaca-status              | t       | Log most recent events for packages.                            |
| elpaca-ui-mark-update      | u       | Mark package for update action.                                 |
| elpaca-visit               | v       | Open ITEM’s local repository directory.                         |
| elpaca-ui-execute-marks    | x       | Execute each action in ‘elpaca-ui-marked-packages’.             |

-   **Function: elpaca-manager `&optional recache`:** Display packages registered with Elpaca. Packages can searched for, installed, updated, rebuilt, and deleted from this interface. When `RECACHE` is non-nil, via lisp or interactively via the `universal-argument`, recompute Elpaca&rsquo;s menu item cache before display.

-   **Function: elpaca-log `&optional filter`:** Display the log for queued packages. When `FILTER` is non-nil, filter entries by the given query. For acceptable values for `FILTER` see [searching](#searching).

-   **Function: elpaca-status:** Display the log for the most recent events for queued packages. This allows one to quickly determine the status and reason for the status of each queued package.


<a id="searching"></a>

## Searching

The `elpaca-ui-search` command (`s`) prompts the user for a search query in the minibuffer. Altering the query updates the UI table. Calling with a `universal-argument` (`C-u`) populates the minibuffer with the current search query for editing. Setting the query to an empty string sets the filter to `elpaca-ui-default-query`. The buffer&rsquo;s header line displays the current query.

Queries are regular expressions checked against each row of the UI table. For example, `test` will match any row which contains the string &ldquo;test&rdquo;. Some characters change the matching behavior in queries.

The pipe character, `|`, will delimit text searches to specific columns of the table. Considering the following table:

| number | A     | B     | C |
|------ |----- |----- |--- |
| 1      | one   | two   | 3 |
| 2      | four  | five  | 6 |
| 3      | seven | eight | 9 |

The query `o` will match rows 1 (on `one`) and 2 (on `four`). The query `3|` will only search for `3` in the first column and match row three. While `|||3` Will search for `3` in the 4th column of the table and match row 1.

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

<sup><a id="fn.1" class="footnum" href="#fnr.1">1</a></sup> <https://www.gnu.org/software/emacs/manual/html_node/emacs/Early-Init-File.html>

<sup><a id="fn.2" class="footnum" href="#fnr.2">2</a></sup> This is so Elpaca can build a proper dependency tree. It ensures packages the user explicitly requests are not preempted by dependencies of other packages.

<sup><a id="fn.3" class="footnum" href="#fnr.3">3</a></sup> [git ref](https://git-scm.com/book/en/v2/Git-Internals-Git-References)

<sup><a id="fn.4" class="footnum" href="#fnr.4">4</a></sup> [remotes](https://git-scm.com/book/en/v2/Git-Basics-Working-with-Remotes)

<sup><a id="fn.5" class="footnum" href="#fnr.5">5</a></sup> <https://github.com/jwiegley/use-package>