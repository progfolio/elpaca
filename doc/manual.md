
Table of Contents
=================

1.  [Installation](#org2a28953)
    1.  [Requirements](#orgc957e3a)
    2.  [Bootstrap Snippet](#bootstrap-snippet)
2.  [Usage](#org2ba5c3c)
    1.  [Quick Start](#quick-start)
    2.  [Basic concepts](#orga1a2806)
        1.  [Recipes](#recipes)
        2.  [Menus](#menus)
        3.  [Orders](#orders)
        4.  [Queues](#queues)
        5.  [Installing Packages](#org70afbaf)
3.  [UI](#org1ca9ce4)
    1.  [Searching](#searching)
    2.  [Search tags](#search-tags)

Elpaca is an elisp package manager.
It allows users to find, install, update, and remove third-party packages for Emacs.
It is a replacement for the built-in Emacs package manager, package.el.

> Copyright (C) 2022-2023 Nicholas Vollmer
> 
> You can redistribute this document and/or modify it under the terms of the GNU
> General Public License as published by the Free Software Foundation, either
> version 3 of the License, or (at your option) any later version.
> 
> This document is distributed in the hope that it will be useful,
> but WITHOUT ANY WARRANTY; without even the implied warranty of
> MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
> General Public License for more details.


<a id="org2a28953"></a>

Installation
============


<a id="orgc957e3a"></a>

Requirements
------------

Elpaca requires:

-   Emacs >= 27.1
-   git (minimum version TBD)
-   Windows users must be able to create symlinks.


<a id="bootstrap-snippet"></a>

Bootstrap Snippet
-----------------

To install Elpaca, add the following bootstrapping snippet to your init.el.
It must come before any calls to other Elpaca functions/macros.
This will clone Elpaca into your `user-emacs-directory` under the `elpaca` subdirectory.
It then builds and activates Elpaca.

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

You&rsquo;ll also want to disable package.el in your early-init file<sup><a id="fnr.1" class="footref" href="#fn.1" role="doc-backlink">1</a></sup>:

    (setq package-enable-at-startup nil)

And remove anything related to package.el in your init file. e.g. calls to `(package-activate-all)`.


<a id="org2ba5c3c"></a>

Usage
=====


<a id="quick-start"></a>

Quick Start
-----------

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">Operation</th>
<th scope="col" class="org-left">UI (keys apply in elpaca-ui-mode)</th>
<th scope="col" class="org-left">completing-read interface commands</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left">Finding Packages</td>
<td class="org-left"><code>M-x elpaca-manager</code></td>
<td class="org-left"><code>elpaca-try</code></td>
</tr>


<tr>
<td class="org-left">Trying Packages (for current session)</td>
<td class="org-left"><code>i</code> <code>x</code></td>
<td class="org-left"><code>elpaca-try</code></td>
</tr>


<tr>
<td class="org-left">Updating Packages</td>
<td class="org-left"><code>u</code> <code>x</code></td>
<td class="org-left"><code>elpaca-update</code> or <code>M-x</code> <code>elpaca-update-all</code></td>
</tr>


<tr>
<td class="org-left">Rebuilding Packages</td>
<td class="org-left"><code>r</code> <code>x</code></td>
<td class="org-left"><code>elpaca-rebuild</code></td>
</tr>


<tr>
<td class="org-left">Deleting Packages</td>
<td class="org-left"><code>d</code> <code>x</code></td>
<td class="org-left"><code>elpaca-delete</code></td>
</tr>


<tr>
<td class="org-left">View Package Logs</td>
<td class="org-left"><code>l</code> filters log to current package</td>
<td class="org-left"><code>elpaca-log</code></td>
</tr>


<tr>
<td class="org-left">View Package Statuses</td>
<td class="org-left"><code>t</code> show most recent log entries</td>
<td class="org-left"><code>elpaca-status</code></td>
</tr>


<tr>
<td class="org-left">Visit Package Repository Directory</td>
<td class="org-left"><code>v</code></td>
<td class="org-left"><code>elpaca-visit</code></td>
</tr>


<tr>
<td class="org-left">Visit Package Build Directory</td>
<td class="org-left"><code>C-u</code> <code>v</code></td>
<td class="org-left"><code>C-u</code> <code>elpaca-visit</code></td>
</tr>


<tr>
<td class="org-left">Browse Package Website</td>
<td class="org-left"><code>b</code></td>
<td class="org-left"><code>elpaca-browse</code></td>
</tr>
</tbody>
</table>

Packages installed via the above commands are not loaded on subsequent Emacs sessions (after restarting).
To install and load packages persistently (across Emacs restarts), use the `elpaca` or `elpaca-use-package` macros in your init file after bootstrapping. ([bootstrap snippet](#bootstrap-snippet))

For example:

    ;; Install use-package
    (elpaca use-package
      ;; Customize/Configure the package in the BODY of the macro.
      (setq use-package-always-defer t))
    
    ;; Expands to: (elpaca evil (use-package evil :demand t))
    (elpaca-use-package evil :demand t)
    
    ;; Don't install anything. Defer execution of BODY
    (elpaca nil (message "deferred"))

**IMPORTANT**:

Elpaca installs and activates packages asynchronously.
Elpaca processes its package queues *after* Emacs reads the init file.<sup><a id="fnr.2" class="footref" href="#fn.2" role="doc-backlink">2</a></sup> 
Consider the following example:

    (elpaca nil (message "First")) ; Queue First
    (message "Second") ; Second messaged
    (elpaca nil (message "Third")) ; Queue Third
    (elpaca-process-queues) ; Process queue: First messaged, Third messaged.

&ldquo;Second&rdquo; will be message *before* &ldquo;First&rdquo; and &ldquo;Third&rdquo;.
Defer forms which are dependent on deferred forms.
Wrapping the &ldquo;Second&rdquo; message in an `elpaca` declaration will fix the above example:

    (elpaca nil (message "First"))  ; Queue First
    (elpaca nil (message "Second")) ; Queue Second
    (elpaca nil (message "Third"))  ; Queue Third
    (elpaca-process-queues) ; Process queue: First, Second, Third messaged.

Add any configuration which relies on `after-init-hook`, `emacs-startup-hook`, etc to `elpaca-after-init-hook` so it runs after Elpaca has activated all queued packages.


<a id="orga1a2806"></a>

Basic concepts
--------------

The `elpaca-example` macro in the following examples reduces verbosity.
It is not part of Elpaca.

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

Examples will use the following recipe menu.
([recipe menu](#menus))
It offers a &ldquo;burger&rdquo; package recipe:

    (defun elpaca-example-menu (_)
      '((burger . (:recipe ( :buns 2
                             :lettuce t
                             :tomato t
                             :beef t
                             :cheese t
                             :cook well-done
                             :from elpaca-example-menu)))))

1.  Recipes

    A recipe provides Elpaca with the metadata necessary to build and install a package.
    It is a list of the form:
    
        (ITEM . PROPS)
    
    *ITEM* is a symbol uniquely identifying the package.
    *PROPS* is a plist with any of the following recipe keywords:
    
    -   **:host | :fetcher:** A symbol or string representing the hosting service of the repository.
    
        (example :host github)
        (example :fetcher gitlab)
        (example :host "www.example.com")
    
    -   **:repo:** A string of the form `USER/REPO` when used with the `:host` keyword; a local file path or remote URL when `:host` is not used.
    
        (example :host github :repo "user/example") ;;downloaded from github
    
        (local :repo "~/repos/local/") ;;cloned from local filesystem
    
        (remote :repo "https://foo.example/example.git") ;;remote clone
    
    -   **:branch:** The repository branch to check out when installing the package.
    
        (example :host github :repo "user/example" :branch "main")
    
    -   **:tag:** The tag to check out when installing the package.
    
        (example :host github :repo "user/example" :tag "v1.0")
    
    -   **:ref:** The git ref<sup><a id="fnr.3" class="footref" href="#fn.3" role="doc-backlink">3</a></sup> to check out when installing the package.
    
        (example :host github :repo "user/example" :ref "a76ca0a") ;; Check out a specific commit.
    
    -   **:pin:** When non-nil, ignore the package during update commands.
    
        (example :pin t)
    
    -   **:depth:** The package repository&rsquo;s history depth.
    
        (example :depth 1) ;; Shallow clone with history truncated to 1 commit.
        (example :depth nil) ;; Full repository clone.
    
    -   **:remotes:** A list of repository remotes<sup><a id="fnr.4" class="footref" href="#fn.4" role="doc-backlink">4</a></sup>.
    
    The first element is the default remote when installing the package.
    If it is a string, it names the default remote.
    The remaining elements are lists of the form:
    
        (NAME . PROPS)
    
    *NAME* is a string used to name the remote.
    *PROPS* are recipe keyword/value pairs used to override values previously declared in the recipe.
    
        (example :remotes ("origin"))
    
        (example :host github :repo "original/example"
                 :remotes ("origin"
                           ("fork" :host gitlab :repo "user/example-fork.el")))
    
    -   **:main:** The name of the main elisp file.
        When provided this can speed up the process of cloning and loading a package&rsquo;s dependencies.
    
        (example :main "example.el")
    
    -   **:build:** A list of build steps, nil or t.
        To remove steps from `elpaca-default-build-steps` by starting the list with the `:not` keyword.
    
        (example :build (:not elpaca--byte-compile))
    
    -   **:inherit:** When non-nil, inherit *PROPS* from `elpaca-order-functions` and possibly `elpaca-menu-functions`.
        For example, without inheritance:
    
        (elpaca-example (elpaca-recipe '(burger :inherit nil)))
    
    returns the recipe as declared:
    
        (:package "burger" :inherit nil)
    
    With inheritance enabled:
    
        (elpaca-example (elpaca-recipe '(burger :inherit t)))
    
    the elpaca-example-menu provides the rest of the &ldquo;burger&rdquo; recipe.
    
        (:package "burger" :inherit t)
    
    1.  Inheritance precedence
    
        The following list shows the order of precedence for inheritance.
        Each item takes precedence over the items which follow it.
        
        -   elpaca-recipe-functions
        -   declared recipe
        -   elpaca-order-functions
        -   elpaca-menu-functions
        
            (elpaca-example
             (let ((elpaca-recipe-functions (lambda (recipe) '(:from recipe-functions :cheese extra)))
                   (elpaca-order-functions (lambda (order) '(:from order-functions :tomato nil))))
               (elpaca-recipe '(burger))))
        
            (:package "burger" :cheese extra :from recipe-functions :tomato nil)
    
    2.  elpaca-recipe-functions
    
        The abnormal hook `elpaca-recipe-functions` runs via `run-hook-with-args-until-success` just before installing the package.
        Each function in the list should accept the current recipe as its sole argument and return either nil or a plist.
        The first function to return a plist has its return value merged with the current recipe.
        
        This is useful if you want to guarantee the values of certain keywords despite allowing recipe inheritance.
        
            (elpaca-example
             (let ((elpaca-recipe-functions
                    '((lambda (recipe)
                        "If a recipe calls for cheese, I always want extra."
                        (when (plist-get recipe :cheese) (list :cheese 'extra))))))
               (elpaca-recipe '(burger))))
        
            (:package "burger")

2.  Menus

    A menu is a function which returns an alist of the form:
    
        ((ITEM . DATA)...)
    
    *ITEM* is a symbol uniquely identifying a package.
    *DATA* is a plist of package metadata.
    *DATA* must contain the following keywords:
    
    -   **:recipe:** A package recipe. ([recipe](#recipes))
    -   **:source:** A string naming the menu.
    
    It may also provide additional information about a package.
    For example, the Elpaca UI utilizes the following keywords when present:
    
    -   **:url:** The package&rsquo;s website URL.
    -   **:description:** A description of the package.
    -   **:date:** The time of package&rsquo;s last update.
    
    The function must accept one of the following *REQUEST* symbols as an argument:
    
    -   **index:** Return the alist described above
    -   **update:** update the menu&rsquo;s alist.
    
        (defun elpaca-menu-minimal (request_)
          "A minimal menu example.
        Ignore REQUEST, as this is a static, curated list of packages."
          '((example :source "EXAMPLE" :recipe (example :host github :repo "user/example"))
            (two :source "EXAMPLE" :recipe (two :host gitlab :repo "user/two"))))
    
    Menus allow one to offer Elpaca users curated lists of package recipes.
    For example, [melpulls](https://www.github.com/progfolio/melpulls) implements an Elpaca menu for pending MELPA packages.
    
    1.  elpaca-menu-functions
    
        The `elpaca-menu-functions` variable contains menu functions for the following package sources by default:
        
        -   [MELPA](https://www.github.com/melpa/melpa)
        -   [Org](https://git.savannah.gnu.org/cgit/emacs/org-mode.git/)
        -   [Org-contrib](https://git.sr.ht/~bzg/org-contrib)
        -   [GNU ELPA Mirror](https://www.github.com/emacs-straight/gnu-elpa-mirror)
        -   [NonGNU ELPA](https://elpa.nongnu.org)
        
        Menus are checked in order until one returns the requested menu item or the menu list is exhausted.

3.  Orders

    At a minimum, an order is a symbol which represents the name of a menu item ([menu](#menus)):
    
        (elpaca example)
    
    An order may also be a partial or full recipe:
    
        (elpaca (example :host gitlab))
        (elpaca (example :host gitlab :repo "user/example" :inherit nil))
    
    1.  elpaca-order-functions
    
        The abnormal hook `elpaca-order-functions` runs via `run-hook-with-args-until-success` before `elpaca-menu-functions`.
        Each function in the list should accept the current order as its sole argument and return either nil or a plist.
        The first function to return a plist has its return value merged with the current order.
        
        This is useful for declaring default order properties.
        For example, the following function disables recipe inheritance by default:
        
            (elpaca-example
             (let ((elpaca-order-functions '((lambda (_) '(:inherit nil)))))
               (elpaca-recipe 'burger)))
        
            (:package "burger" :inherit nil)

4.  Queues

    Elpaca installs packages asynchronously.
    Orders ([orders](#orders)) are automatically queued in a list.
    When all of a queues orders have either finished or failed Elpaca considers it &ldquo;processed&rdquo;.
    
    Queues ensure packages installation, activation, and configuration take place prior to packages in other queues.
    The `elpaca-queue` macro wraps calls to `elpaca`. It places orders in its *BODY* in their own queue.
    This is especially useful when one wants to install a package to use later on in their init file.
    For example, a package which implements an Elpaca menu ([menu](#menus)):
    
        (elpaca-queue
         (elpaca (melpulls :host github :repo "progfolio/melpulls")
           (add-to-list 'elpaca-menu-functions #'melpulls)
           (elpaca-update-menus #'melpulls)))
        ;; Implicitly queued into a new queue.
        (elpaca menu-item-available-in-melpulls)

5.  Installing Packages

    -   **elpaca:** `(order &rest body)`
    
    Installs *ORDER* ([orders](#orders)) and executes *BODY* after processing ORDER&rsquo;s queue ([queue](#queues)).
    
    This macro is for programmatic use in one&rsquo;s init file.
    Any of the following will install the &ldquo;example&rdquo; package:
    
        (elpaca example) ;; recipe looked up in `elpaca-menu-functions'.
    
        (elpaca example (message "Messaged after the order's queue has processed."))
    
        (elpaca (example :host github :repo "user/example"))
    
        (elpaca `(example :host github :repo "user/example"
                          ,@(when (eq system-type 'darwin) ;; backqouting supported
                              (list :pre-build ((message "Mac specific pre-build"))))))
    
    If *ORDER* is nil, *BODY* is still executed after processing the current queue.
    
        (elpaca first (message "First configured"))
        ;; If this weren't wrapped in an `elpaca' call, it would execute FIRST
        ;; Due to the "first" and "third" package installing asynchronously.
        (elpaca nil (message "Second"))
        (elpaca third (message "Third configured"))
    
    -   **elpaca-use-package:** `(order &rest body)`
        
        A wrapper for the use-package<sup><a id="fnr.5" class="footref" href="#fn.5" role="doc-backlink">5</a></sup> macro.
        *ORDER* is the same as above.
        *BODY* must conform to use-package&rsquo;s *ARGS*.
        
            (elpaca use-package (require 'use-package)) ; install use-package
            (elpaca-use-package (example :host github :repo "user/example")
              :config (message "Example configured"))


<a id="org1ca9ce4"></a>

UI
==

Elpaca has a UI mode available for managing packages.
The main entry points to the UI are the `elpaca-manager`, `elpaca-log`, and `elpaca-status` commands.
Each of these commands utilize `elpaca-ui-mode`.

The following commands are available in the `elpaca-ui-mode`:

    (with-temp-buffer
      (describe-map-tree elpaca-ui-mode-map)
      (goto-char (point-min))
      (let (rows)
        (while (re-search-forward "elpaca" nil 'noerror)
          (push (split-string (buffer-substring-no-properties
                               (line-beginning-position) (line-end-position))
                              "\t+")
                rows))
        (setq rows (mapcar (lambda (it)
                             (append
                              (list (car (split-string
                                          (documentation (intern (cadr it)))
                                          "\n")))
                              it))
                           rows))
        (setq rows (cl-sort (mapcar #'nreverse rows) #'string< :key #'cadr))
        (push 'hline rows)
        (push (list "Command" "Binding" "Description") rows)
        rows))

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">Command</th>
<th scope="col" class="org-left">Binding</th>
<th scope="col" class="org-left">Description</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left">elpaca-ui-send-input</td>
<td class="org-left">!</td>
<td class="org-left">Send input string to current process.</td>
</tr>


<tr>
<td class="org-left">elpaca-ui-search-installed</td>
<td class="org-left">I</td>
<td class="org-left">Search for &ldquo;#unique #installed&rdquo;</td>
</tr>


<tr>
<td class="org-left">elpaca-ui-search-marked</td>
<td class="org-left">M</td>
<td class="org-left">Search for &ldquo;#unique #marked&rdquo;</td>
</tr>


<tr>
<td class="org-left">elpaca-ui-search-orphaned</td>
<td class="org-left">O</td>
<td class="org-left">Search for &ldquo;#unique #orphan&rdquo;</td>
</tr>


<tr>
<td class="org-left">elpaca-ui-search-refresh</td>
<td class="org-left">R</td>
<td class="org-left">Rerun the current search for BUFFER.</td>
</tr>


<tr>
<td class="org-left">elpaca-ui-search-tried</td>
<td class="org-left">T</td>
<td class="org-left">Search for &ldquo;#unique #installed !#declared&rdquo;</td>
</tr>


<tr>
<td class="org-left">elpaca-ui-unmark</td>
<td class="org-left">U</td>
<td class="org-left">Unmark current package.</td>
</tr>


<tr>
<td class="org-left">elpaca-ui-browse-package</td>
<td class="org-left">b</td>
<td class="org-left">Browse current package’s URL via ‘browse-url’.</td>
</tr>


<tr>
<td class="org-left">elpaca-ui-mark-delete</td>
<td class="org-left">d</td>
<td class="org-left">Mark package for delete action.</td>
</tr>


<tr>
<td class="org-left">elpaca-ui-mark-install</td>
<td class="org-left">i</td>
<td class="org-left">Mark package for install action.</td>
</tr>


<tr>
<td class="org-left">elpaca-log</td>
<td class="org-left">l</td>
<td class="org-left">Display ‘elpaca-log-buffer’.</td>
</tr>


<tr>
<td class="org-left">elpaca-manager</td>
<td class="org-left">m</td>
<td class="org-left">Display elpaca’s package management UI.</td>
</tr>


<tr>
<td class="org-left">elpaca-ui-mark-rebuild</td>
<td class="org-left">r</td>
<td class="org-left">Mark package for rebuild action.</td>
</tr>


<tr>
<td class="org-left">elpaca-ui-search</td>
<td class="org-left">s</td>
<td class="org-left">Filter current buffer by QUERY. If QUERY is nil, prompt for it.</td>
</tr>


<tr>
<td class="org-left">elpaca-status</td>
<td class="org-left">t</td>
<td class="org-left">Log most recent events for packages.</td>
</tr>


<tr>
<td class="org-left">elpaca-ui-mark-update</td>
<td class="org-left">u</td>
<td class="org-left">Mark package for update action.</td>
</tr>


<tr>
<td class="org-left">elpaca-visit</td>
<td class="org-left">v</td>
<td class="org-left">Open ITEM’s local repository directory.</td>
</tr>


<tr>
<td class="org-left">elpaca-ui-execute-marks</td>
<td class="org-left">x</td>
<td class="org-left">Execute each action in ‘elpaca-ui-marked-packages’.</td>
</tr>
</tbody>
</table>

-   **Function: elpaca-manager `&optional recache`:** Display packages registered with Elpaca.
    Packages can searched for, installed, updated, rebuilt, and deleted from this interface.
    When `RECACHE` is non-nil, via lisp or interactively via the `universal-argument`, recompute Elpaca&rsquo;s menu item cache before display.

-   **Function: elpaca-log `&optional filter`:** Display the log for queued packages.
    When `FILTER` is non-nil, filter entries by the given query.
    For acceptable values for `FILTER` see [searching](#searching).

-   **Function: elpaca-status:** Display the log for the most recent events for queued packages.
    This allows one to quickly determine the status and reason for the status of each queued package.


<a id="searching"></a>

Searching
---------

The `elpaca-ui-search` command (`s`) prompts the user for a search query in the minibuffer.
Altering the query updates the UI table.
Calling with a `universal-argument` (`C-u`) populates the minibuffer with the current search query for editing.
Setting the query to an empty string sets the filter to `elpaca-ui-default-query`.
The buffer&rsquo;s header line displays the current query.

Queries are regular expressions checked against each row of the UI table.
For example, `test` will match any row which contains the string &ldquo;test&rdquo;.
Some characters change the matching behavior in queries.

The pipe character, `|`, will delimit text searches to specific columns of the table.
Considering the following table:

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-right" />

<col  class="org-left" />

<col  class="org-left" />

<col  class="org-right" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-right">number</th>
<th scope="col" class="org-left">A</th>
<th scope="col" class="org-left">B</th>
<th scope="col" class="org-right">C</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-right">1</td>
<td class="org-left">one</td>
<td class="org-left">two</td>
<td class="org-right">3</td>
</tr>


<tr>
<td class="org-right">2</td>
<td class="org-left">four</td>
<td class="org-left">five</td>
<td class="org-right">6</td>
</tr>


<tr>
<td class="org-right">3</td>
<td class="org-left">seven</td>
<td class="org-left">eight</td>
<td class="org-right">9</td>
</tr>
</tbody>
</table>

The query `o` will match rows 1 (on `one`) and 2 (on `four`).
The query `3|` will only search for `3` in the first column and match row three.
While `|||3` Will search for `3` in the 4th column of the table and match row 1.

The pound (a.k.a. hash) character, `#`, followed by the name of a search tag filters table entries.
For example `#random` will display 10 random entries.
If the search tag accepts arguments they may passed by wrapping the tag name in parenthesis.
e.g. `#(random 20)` will display 20 random entries.


<a id="search-tags"></a>

Search tags
-----------

-   **User Option: elpaca-ui-search-tags:** An alist of with elements of the form (NAME . FILTER).
    `NAME` is a unique symbol describing the filter function.
    The user types name after `#` in the minibuffer to apply the filter.
    `FILTER` is a function which must accept a list of `tabulated-list-entries` as its first argument.
    It may accept additional, optional arguments.
    The function must return a list of `tabulated-list-entries`.
    
    For example, the following search tag will embolden the first column of the `elpaca-manager` table when the search query contains `#bold-names`:

    (defun +elpaca-bold-names (entries)
      (cl-loop for entry in entries
               for copy = (copy-tree entry)
               for cols = (cadr copy)
               for name = (aref cols 0)
               do (setf (aref cols 0) (propertize name 'face '(:weight bold)))
               collect copy))
    
    (cl-pushnew (cons 'bold-names #'+elpaca-bold-names) elpaca-ui-search-tags)


Footnotes
=========

<sup><a id="fn.1" href="#fnr.1">1</a></sup> <https://www.gnu.org/software/emacs/manual/html_node/emacs/Early-Init-File.html>

<sup><a id="fn.2" href="#fnr.2">2</a></sup> This is so Elpaca can build a proper dependency tree. It ensures packages the user explicitly requests are not preempted by dependencies of other packages.

<sup><a id="fn.3" href="#fnr.3">3</a></sup> [git ref](https://git-scm.com/book/en/v2/Git-Internals-Git-References)

<sup><a id="fn.4" href="#fnr.4">4</a></sup> [remotes](https://git-scm.com/book/en/v2/Git-Basics-Working-with-Remotes)

<sup><a id="fn.5" href="#fnr.5">5</a></sup> <https://github.com/jwiegley/use-package>
