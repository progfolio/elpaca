;;; elpaca-git.el --- Git Elpaca Support             -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025 Nicholas Vollmer

;; Author:  Nicholas Vollmer
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:
(require 'elpaca)
(declare-function url-filename "url-parse")
(defgroup elpaca-git nil "Elpaca Git Repo Support." :group 'elpaca :prefix "elpaca-git-")
(defcustom elpaca-git-default-build-steps `(elpaca-git--set-src-dir
                                            elpaca-git--clone
                                            elpaca-git--congifure-remotes
                                            elpaca-git--checkout-ref
                                            ,@elpaca-build-steps)
  "List of steps which are run when installing/building a package."
  :type '(repeat function))

(defsubst elpaca-git--mono-repo (id repo)
  "Return previously queued E with REPO other than ID."
  (cl-loop for (_ . e) in (reverse (elpaca--queued)) thereis
           (and (not (eq (elpaca<-id e) id))
                (equal repo (elpaca<-src-dir e)))))

(defsubst elpaca-git--repo-name (string)
  "Return repo name portion of STRING."
  (setq string (directory-file-name string)) ;; remove external :repo trailing slash
  (file-name-base (substring string (- (or (string-match-p "/" (reverse string))
                                           (error "Invalid repo name %S" string))))))

(defsubst elpaca-git--repo-user (string)
  "Return user name portion of STRING."
  (substring string 0 (string-match-p "/" string)))

(defun elpaca-git--repo-type (string)
  "Return type of :repo STRING.
Type is `local' for a local filesystem path, `remote' for a remote URL, or nil."
  (cond ((string-match-p "^[/~]" string) 'local)
        ((string-match-p ":" string) 'remote)))

(defun elpaca-git-repo-dir (recipe)
  "Return path to repo given RECIPE."
  (let* ((url (plist-get recipe :url))
         (repo (plist-get recipe :repo))
         (remote (car-safe repo))
         (local (cdr-safe repo))
         (pkg (plist-get recipe :package))
         (host (or (plist-get recipe :host) (plist-get recipe :fetcher)))
         (hostname (and host (prin1-to-string host 'noescape)))
         (user nil)
         (info (concat url (or remote repo) hostname))
         (key (or (and info (> (length info) 0) (intern info))
                  (error "Cannot determine URL from recipe: %S" recipe)))
         (mono-repo (elpaca-alist-get key elpaca--src-dirs))
         (dirs (and (not mono-repo) (mapcar #'cdr elpaca--src-dirs)))
         (name (cond
                (local
                 (if-let* ((owner (assoc local dirs)))
                     (error "Local repo %S owned by %s" local (cdr owner))
                   local))
                (mono-repo (car mono-repo))
                (url
                 (unless (featurep 'url-parse) (require 'url-parse))
                 (file-name-base (directory-file-name (url-filename
                                                       (url-generic-parse-url url)))))
                (repo (if-let* ((r (or remote repo))
                                ((eq (elpaca-git--repo-type r) 'local)))
                          (if local
                              (file-name-base (directory-file-name local))
                            r)
                        (when host (setq user (elpaca-git--repo-user r)))
                        (elpaca-git--repo-name (or local r))))
                (pkg pkg)
                (t (error "Unable to determine repo name"))))
         (dir (if (assoc name dirs)
                  (string-join (list name hostname user) ".")
                (and name (replace-regexp-in-string "\\.el$" "" name)))))
    (unless mono-repo (push (cons key (cons dir pkg)) elpaca--src-dirs))
    (file-name-as-directory (expand-file-name dir elpaca-repos-directory))))

(defun elpaca-git--repo-uri (recipe)
  "Return repo URI from RECIPE."
  (cl-destructuring-bind (&key (protocol 'https)
                               url
                               fetcher
                               (host fetcher)
                               (repo url) &allow-other-keys)
      recipe
    (when (consp repo) (setq repo (car repo))) ; Handle :repo rename
    (pcase (elpaca-git--repo-type (or repo (error "Unable to determine recipe URL")))
      ('remote repo)
      ('local  (expand-file-name repo))
      (_ (let ((p (pcase protocol
                    ('https '("https://" . "/"))
                    ('ssh   '("git@" . ":"))
                    (_      (signal 'wrong-type-argument `((https ssh) ,protocol)))))
               (h (pcase host
                    ('github       "github.com")
                    ('gitlab       "gitlab.com")
                    ('codeberg     "codeberg.org")
                    ('sourcehut    "git.sr.ht")
                    ((pred stringp) host)
                    (_ (signal 'wrong-type-argument
                               `(:host (github gitlab codeberg sourcehut stringp)
                                       ,host ,recipe))))))
           (concat (car p) h (cdr p) (when (eq host 'sourcehut) "~") repo
                   (unless (eq host 'sourcehut) ".git")))))))

(defun elpaca-git--set-src-dir (e)
  "Set E's src dir."
  (if-let* ((repo-dir (elpaca-git-repo-dir (elpaca<-recipe e))))
      (progn (setf (elpaca<-src-dir e) repo-dir)
             (elpaca--continue-build e))
    (error "Unable to determine source dir for %S" (elpaca<-id e))))

(defun elpaca-git--checkout-ref (e)
  "Check out E's ref."
  (let* ((recipe (elpaca<-recipe e))
         (default-directory (elpaca<-src-dir e))
         (remotes (plist-get recipe :remotes))
         (remote (let ((default (car remotes)))
                   (when (listp default) (setq recipe (elpaca-merge-plists recipe (cdr default))))
                   default))
         (ref    (plist-get recipe :ref))
         (tag    (plist-get recipe :tag))
         (branch (plist-get recipe :branch))
         (target (or ref tag branch)))
    (when-let* ((name    (car-safe remote))
                (default (elpaca-process-output "git" "rev-parse" "--abbrev-ref" "HEAD")))
      (elpaca--call-with-log e 1 "git" "checkout" "--detach")
      (elpaca--call-with-log e 1 "git" "branch"   "--delete" (string-trim default))
      (elpaca--call-with-log e 1 "git" "config"   "checkout.defaultRemote" name)
      (when-let* (((not branch))
                  (default-branch
                   (condition-case err ;;@FIX: will this stop if we fail elpaca?
                       (elpaca--remote-default-branch name)
                     (t (elpaca--fail e (format "Remote default branch err: %S" err))))))
        (setq branch default-branch target branch)))
    (if (null target)
        (unless (eq (elpaca--status e) 'failed)
          (elpaca--continue-build e nil 'ref-checked-out))
      (cond
       ((and ref (or branch tag))
        (elpaca--signal
         e (format ":ref %S overriding %S %S" ref (if branch :branch :tag) (or branch tag))))
       ((and tag branch)
        (elpaca--fail e (format "Ambiguous ref: :tag %S, :branch %S" tag branch))))
      (elpaca--signal e (concat "Checking out " target) 'checking-out-ref)
      (unless (eq (elpaca--status e) 'failed)
        (elpaca--make-process e
          :name "checkout-ref"
          :command
          `("git" "-c" "advice.detachedHead=false" ;ref, tag may detach HEAD
            ,@(cond
               (ref    (list "checkout" ref))
               (tag    (list "checkout" (concat "tags/" tag)))
               (branch (list "checkout" "-B" branch ; "--no-guess"?
                             (concat (or (elpaca--first remote)
                                         elpaca-default-remote-name)
                                     "/" branch)))))
          :sentinel (lambda (process event)
                      (elpaca--process-sentinel (concat target " checked out") 'ref-checked-out process event)))))))

(defun elpaca-git--congifure-remotes (e)
  "Add and/or rename E's repo remotes."
  (let ((fetchp nil))
    (when-let* ((default-directory (elpaca<-src-dir e))
                (recipe            (elpaca<-recipe   e))
                (remotes           (plist-get recipe :remotes)))
      (elpaca--signal e "Configuring Remotes" 'adding-remotes)
      (cl-loop with renamed for spec in remotes do
               (if (stringp spec)
                   (if renamed
                       (elpaca--signal e (format "ignoring :remotes rename %S" spec))
                     (unless (equal spec elpaca-default-remote-name)
                       (elpaca--call-with-log
                        e 1 "git" "remote" "rename" elpaca-default-remote-name spec))
                     (setq renamed spec))
                 (when-let* ((remote    (car spec))
                             (props     (cdr spec))
                             (inherited (elpaca-merge-plists recipe props))
                             (URI       (elpaca-git--repo-uri inherited)))
                   (setq fetchp t)
                   (elpaca-with-process
                       (elpaca--call-with-log e 1 "git" "remote" "add" remote URI)
                     (unless success (elpaca--fail e stderr)))))))
    (when fetchp (push #'elpaca--initial-fetch (elpaca<-build-steps e))))
  (elpaca--continue-build e))

(defun elpaca-git--clone-process-sentinel (process _event)
  "Sentinel for clone PROCESS."
  (if-let* ((e (process-get process :elpaca))
            (success (= (process-exit-status process) 0)))
      (progn (elpaca--propertize-subprocess process)
             (elpaca--continue-build e))
    (if (or (memq 'reclone (elpaca<-statuses e))
            (not (plist-get (elpaca<-recipe e) :depth)))
        (elpaca--fail e (nth 2 (car (elpaca<-log e))))
      (setf (elpaca<-recipe e) (plist-put (elpaca<-recipe e) :depth nil))
      (elpaca--signal e "Re-cloning with recipe :depth nil" 'reclone)
      (push #'elpaca-git--clone (elpaca<-build-steps e))
      (elpaca--continue-build e))))

(defun elpaca-git--clone (e)
  "Clone E's repo to `elpaca-directory'."
  (elpaca--signal e "Cloning" 'cloning)
  (let* ((recipe  (elpaca<-recipe   e))
         (remotes (plist-get recipe :remotes))
         (remote  (and remotes (car remotes)))
         (repodir (elpaca<-src-dir e))
         (URI     (elpaca-git--repo-uri recipe))
         (default-directory elpaca-directory)
         (command
          `("git" "clone"
            ;;@TODO: Some refs will need a full clone or specific branch.
            ,@(when-let* ((depth (plist-get recipe :depth)))
                (cond
                 ((plist-get recipe :ref) (elpaca--signal e "Ignoring :depth in favor of :ref"))
                 ((numberp depth) `("--depth" ,(number-to-string depth)))
                 ((memq depth '(treeless blobless))
                  (cond ((consp remote)
                         (setf (elpaca<-recipe e) (plist-put recipe :depth nil))
                         (elpaca--signal e ":remotes incompatible with treeless, blobless clones; using :depth nil"))
                        ((eq depth 'treeless) '("--filter=tree:0"))
                        ((eq depth 'blobless) '("--filter=blob:none"))))))
            ;;@FIX: allow override
            ,@(when-let* ((ref (or (plist-get recipe :branch) (plist-get recipe :tag))))
                `("--single-branch" "--branch" ,ref))
            ,@(unless (or (null remote) (stringp remote)) '("--no-checkout"))
            ,URI ,repodir)))
    (if (file-exists-p repodir)
        (progn (elpaca--signal e (format "%s exists. Skipping clone." repodir))
               (elpaca--continue-build e))
      (elpaca--make-process e
        :name "clone" :command command :connection-type 'pty
        :sentinel #'elpaca-git--clone-process-sentinel))))

;;;###autoload
(defun elpaca-git-build-steps (e)
  "Return a contextual list of build steps if E is a git type repository."
  (when (equal (plist-get (elpaca<-recipe e) :type) 'git)
    (cond
     ((eq this-command 'elpaca)
      (if (file-exists-p (elpaca<-build-dir e))
          `(elpaca-git--set-src-dir ,@elpaca--pre-built-steps)
        elpaca-git-default-build-steps))
     ((eq this-command 'elpaca-try) elpaca-git-default-build-steps)
     ((eq this-command 'elpaca-rebuild)
      (cl-set-difference elpaca-build-steps
                         '(elpaca--queue-dependencies elpaca--activate-package)))
     (t (elpaca--fail e (format "%S command not implemented" this-command))))))

(provide 'elpaca-git)
;;; elpaca-git.el ends here
