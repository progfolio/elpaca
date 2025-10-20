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
(or (executable-find "git") (error "Elpaca unable to find git executable"))
(declare-function url-filename "url-parse")
(defgroup elpaca-git nil "Elpaca Git Repo Support." :group 'elpaca :prefix "elpaca-git-")
(defcustom elpaca-git-default-build-steps '(elpaca-git--set-src-dir
                                            elpaca-git--clone
                                            elpaca-git--congifure-remotes
                                            elpaca-git--checkout-ref)
  "List of steps which are run when installing/building a package."
  :type '(repeat function))

(defsubst elpaca-git--mono-repo (id repo)
  "Return previously queued E with REPO other than ID."
  (cl-loop for (_ . e) in (reverse (elpaca--queued)) thereis
           (and (not (eq (elpaca<-id e) id))
                (equal repo (elpaca<-source-dir e)))))

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
         (mono-repo (elpaca-alist-get key elpaca--source-dirs))
         (dirs (and (not mono-repo) (mapcar #'cdr elpaca--source-dirs)))
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
    (unless mono-repo (push (cons key (cons dir pkg)) elpaca--source-dirs))
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
      (progn (setf (elpaca<-source-dir e) repo-dir)
             (elpaca--continue-build e))
    (error "Unable to determine source dir for %S" (elpaca<-id e))))

(defun elpaca-git--remote-default-branch (remote)
  "Return REMOTE's \"default\" branch.
This is the branch that would be checked out upon cloning."
  (elpaca-process-cond ("git" "remote" "show" remote)
    ((and success (string-match "\\(?:[^z-a]*HEAD branch:[[:space:]]+\\([^z-a]*?\\)$\\)"
                                stdout))
     (match-string 1 stdout))
    (invoked (error "Remote default branch error: %S" stderr))
    (t (error "Unable to determine remote default branch: %S" result))))

(defun elpaca-git--checkout-ref (e)
  "Check out E's ref."
  (let* ((recipe (elpaca<-recipe e))
         (default-directory (elpaca<-source-dir e))
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
                       (elpaca-git--remote-default-branch name)
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

(defvar elpaca-git--tag-regexp
  "\\`\\(?:\\|[RVrv]\\|release[/-]v?\\)?\\(?1:[0-9]+\\(\\.[0-9]+\\)*\\)\\'")

(defmacro elpaca-git--without-config (&rest body)
  "Eval BODY with user Git config ignored."
  `(let ((process-environment (append '("GIT_CONFIG_SYSTEM=/dev/null"
                                        "GIT_CONFIG_GLOBAL=/dev/null")
                                      process-environment)))
     ,@body))

(defun elpaca-git--commit-date (e)
  "Return date of E's checked out commit with FORMAT spec."
  (let ((default-directory (elpaca<-source-dir e)))
    (elpaca-git--without-config
     (elpaca-with-process-call ("git" "log" "-n" "1" "--format=%cd" "--date=unix")
       (if (not success) (elpaca--fail e stderr)
         (seconds-to-time (string-to-number stdout)))))))

(defun elpaca-git--fetch (e &rest command)
  "Fetch E's remotes' commits.
COMMAND must satisfy `elpaca--make-process' :command SPEC arg, which see."
  (elpaca--signal e "Fetching remotes" 'fetching-remotes)
  (let ((default-directory (elpaca<-source-dir e)))
    (elpaca--make-process e
      :name "fetch"
      :command  (or command '("git" "fetch" "--all" "-v"))
      :sentinel (lambda (process event) (elpaca--process-sentinel "Remotes fetched" nil process event)))))

(defun elpaca-git--merge-process-sentinel (process _event)
  "Handle PROCESS EVENT."
  (if-let* ((e (process-get process :elpaca))
            ((= (process-exit-status process) 0))
            (repo (elpaca<-source-dir e))
            (default-directory repo))
      (progn (when (equal (elpaca-process-output "git" "rev-parse" "HEAD")
                          (process-get process :elpaca-git-rev))
               (cl-loop for (_ . d) in (elpaca--queued)
                        when (equal (elpaca<-source-dir d) repo) do
                        (setf (elpaca<-build-steps d) nil)))
             (elpaca--propertize-subprocess process)
             (elpaca--continue-build e))
    (elpaca--fail e "Merge failed")))

(defun elpaca-git--merge (e)
  "Merge E's fetched commits."
  (let* ((default-directory (elpaca<-source-dir e))
         (rev (elpaca-process-output "git" "rev-parse" "HEAD")))
    (process-put (elpaca--make-process e
                   :name "merge"
                   :command  '("git" "merge" "--ff-only")
                   :sentinel #'elpaca-git--merge-process-sentinel)
                 :elpaca-git-rev rev)
    (elpaca--signal e "Merging updates" 'merging)))

(defun elpaca-git--initial-fetch (e)
  "Perform initial fetch for E, respecting :remotes recipe inheritance."
  (let* ((recipe (copy-tree (elpaca<-recipe e)))
         (remotes (plist-get recipe :remotes)))
    (setf recipe (elpaca-merge-plists recipe '(:remotes nil)))
    (cl-loop for remote in remotes
             for opts = (elpaca-merge-plists recipe (cdr-safe remote))
             for command = `("git" "fetch" ,@(when-let* ((depth (plist-get opts :depth))
                                                         ((numberp depth)))
                                               (list "--depth" (format "%s" depth)))
                             ,(or (car-safe remote) remote))
             for fn = `(lambda (e) (elpaca-git--fetch e ,@command))
             do (push fn (elpaca<-build-steps e))
             finally (elpaca--continue-build e))))

(defun elpaca-git--congifure-remotes (e)
  "Add and/or rename E's repo remotes."
  (let ((fetchp nil))
    (when-let* ((default-directory (elpaca<-source-dir e))
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
    (when fetchp (push #'elpaca-git--initial-fetch (elpaca<-build-steps e))))
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
         (repodir (elpaca<-source-dir e))
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

(defun elpaca-git-worktree-dirty-p (id)
  "Return t if ID's associated repository has a dirty worktree, nil otherwise."
  (when-let* ((e (elpaca-get id))
              (recipe (elpaca<-recipe e))
              (source-dir (elpaca<-source-dir e))
              ((file-exists-p source-dir))
              (default-directory source-dir))
    (not (string-empty-p (elpaca-process-output
                          "git" "-c" "status.branch=false" "status" "--short")))))

(cl-defmethod elpaca-ref ((e (elpaca git)))
  "Return E :type git ref."
  (elpaca-with-dir e source
    (elpaca-with-process-call ("git" "rev-parse" "HEAD")
      (if success (string-trim stdout)
        (error "Unable to write lock-file: %s %S" (elpaca<-id e) stderr)))))

(defun elpaca-git-latest-tag (e)
  "Return E's merged tag matching :version-regexp or `elpaca-git--tag-regexp'."
  (when-let* ((default-directory (elpaca<-source-dir e))
              (recipe (elpaca<-recipe e))
              (regexp (or (plist-get recipe :version-regexp) elpaca-git--tag-regexp))
              (tags (elpaca-with-process
                        (elpaca-process-call "git" "tag" "--sort=-creatordate" "--merged")
                      (and success stdout (split-string stdout "\n" 'omit-nulls)))))
    (cl-loop for tag in tags when (string-match regexp tag)
             return (or (match-string 1 tag) (match-string 0 tag)))))

(cl-defmethod elpaca--version ((e (elpaca git)) &optional context)
  "Return :type git E's version for CONTEXT."
  (pcase context
    (:date (elpaca-git--commit-date e))
    (:alternative (elpaca-git-latest-tag e))))

(cl-defmethod elpaca-build-steps ((e (elpaca git)) &optional context)
  "Return :type git E's build steps for CONTEXT."
  (pcase context
    ('nil `(:first ,@(if (elpaca<-builtp e)
                         (list 'elpaca-git--set-src-dir)
                       elpaca-git-default-build-steps)))))

(defvar elpaca-ui-search-tags)
(with-eval-after-load 'elpaca-ui
  (defun elpaca-ui--tag-dirty (entries)
    "Return ENTRIES for packages with a dirty worktree."
    (cl-remove-if-not #'elpaca-git-worktree-dirty-p entries :key #'caar))
  (unless (alist-get 'dirty elpaca-ui-search-tags)
    (setf (alist-get 'dirty elpaca-ui-search-tags) 'elpaca-ui--tag-dirty)))

(provide 'elpaca-git)
;;; elpaca-git.el ends here
