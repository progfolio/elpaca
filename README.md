# Elpaca: An Elisp Package Manager

<p align="center"><img src="./images/elpaca.svg"/></p>

<p align="center">"Chews data, spits packages."</p>

Elpaca is an elisp package manager. It allows users to find, install, update, and remove third-party packages for Emacs. It is a replacement for the built-in Emacs package manager, package.el.

Elpaca:

-   Installs packages asynchronously, in parallel for fast, non-blocking installations.
-   Includes a flexible UI for finding and operating on packages.
-   Downloads packages from their sources for convenient elisp development.
-   Supports thousands of elisp packages out of the box (MELPA, NonGNU/GNU ELPA, Org/org-contrib).
-   Makes it easy for users to create their own ELPAs.


# Video Tour

<p align="center"><a href="https://www.youtube.com/watch?v=5Ud-TE3iIQY"><img src="./images/elpaca-manager-install.gif"/></a></p>


## Installation


### Requirements

Elpaca requires:

-   Emacs >= 27.1
-   git (minimum version TBD)


<a id="installer"></a>

### Installer

To install Elpaca, add the following elisp to your init.el. It must come before any calls to other Elpaca functions/macros. This will clone Elpaca into your `user-emacs-directory` under the `elpaca` subdirectory. It then builds and activates Elpaca.

```emacs-lisp
(defvar elpaca-installer-version 0.5)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil
                              :files (:defaults (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (call-process "git" nil buffer t "clone"
                                       (plist-get order :repo) repo)))
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
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))
```

-   Windows users must be able to create symlinks<sup><a id="fnr.1" class="footref" href="#fn.1" role="doc-backlink">1</a></sup>, or enable `elpaca-no-symlink-mode`

```emacs-lisp
;; Uncomment for systems which cannot create symlinks:
;; (elpaca-no-symlink-mode)
```

You'll also want to disable package.el in your early-init file<sup><a id="fnr.2" class="footref" href="#fn.2" role="doc-backlink">2</a></sup>:

```emacs-lisp
(setq package-enable-at-startup nil)
```

And remove anything related to package.el in your init file. e.g. calls to `(package-activate-all)`.


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

Packages installed via the above commands are not loaded on subsequent Emacs sessions (after restarting). To install and load packages persistently (across Emacs restarts), use the `elpaca` macro in your init file after the installer. ([installer](#installer))

For example:

```emacs-lisp
;; Install use-package support
(elpaca elpaca-use-package
  ;; Enable :elpaca use-package keyword.
  (elpaca-use-package-mode)
  ;; Assume :elpaca t unless otherwise specified.
  (setq elpaca-use-package-by-default t))

;; Block until current queue processed.
(elpaca-wait)

;;When installing a package which modifies a form used at the top-level
;;(e.g. a package which adds a use-package key word),
;;use `elpaca-wait' to block until that package has been installed/configured.
;;For example:
;;(use-package general :demand t)
;;(elpaca-wait)

;; Expands to: (elpaca evil (use-package evil :demand t))
(use-package evil :demand t)

;;Turns off elpaca-use-package-mode current declartion
;;Note this will cause the declaration to be interpreted immediately (not deferred).
;;Useful for configuring built-in emacs features.
(use-package emacs :elpaca nil :config (setq ring-bell-function #'ignore))

;; Don't install anything. Defer execution of BODY
(elpaca nil (message "deferred"))
```

**IMPORTANT**:

Elpaca installs and activates packages asynchronously. Elpaca processes its package queues *after* Emacs reads the init file.<sup><a id="fnr.3" class="footref" href="#fn.3" role="doc-backlink">3</a></sup> Consider the following example:

```emacs-lisp
(elpaca nil (message "First")) ; Queue First
(message "Second") ; Second messaged
(elpaca nil (message "Third")) ; Queue Third
(elpaca-process-queues) ; Process queue: First messaged, Third messaged.
```

"Second" will be message *before* "First" and "Third". Defer forms which are dependent on deferred forms. Wrapping the "Second" message in an `elpaca` declaration will fix the above example:

```emacs-lisp
(elpaca nil (message "First"))  ; Queue First
(elpaca nil (message "Second")) ; Queue Second
(elpaca nil (message "Third"))  ; Queue Third
(elpaca-process-queues) ; Process queue: First, Second, Third messaged.
```

Add any configuration which relies on `after-init-hook`, `emacs-startup-hook`, etc to `elpaca-after-init-hook` so it runs after Elpaca has activated all queued packages. This includes loading of saved customizations. e.g.

```emacs-lisp
(setq custom-file (expand-file-name "customs.el" user-emacs-directory))
(add-hook 'elpaca-after-init-hook (lambda () (load custom-file 'noerror)))
```

See the [manual](./doc/manual.md) for in-depth information on Elpaca usage, customization, and development. Users who wish to experiment with Elpaca may find the example [init.el](./doc/init.el) and [early-init.el](./doc/early-init.el) files useful.

## Footnotes

<sup><a id="fn.1" class="footnum" href="#fnr.1">1</a></sup> [windows symlink guide](https://www.howtogeek.com/16226/complete-guide-to-symbolic-links-symlinks-on-windows-or-linux/)

<sup><a id="fn.2" class="footnum" href="#fnr.2">2</a></sup> [early-init file](https://www.gnu.org/software/emacs/manual/html_node/emacs/Early-Init-File.html)

<sup><a id="fn.3" class="footnum" href="#fnr.3">3</a></sup> This is so Elpaca can build a proper dependency tree. It ensures packages the user explicitly requests are not preempted by dependencies of other packages.