;;; elpaca-watcher.el --- filesystem watcher  -*- lexical-binding: t; -*-

;;; Commentary:
;;
(require 'filenotify)
(require 'elpaca)

;;; Code:
;;;; Variables
(defvar elpaca-watcher-file (expand-file-name "watcher.eld" elpaca-cache-directory)
  "File name for the watcher's repo contents cache.")
(defvar eplaca--watchers nil "List of registered notification objects.")
(defvar elpaca-watcher--items nil "Alist of observed repositories.")
(defvar elpaca-watcher--timers nil "REPO -> timer alist.")

;;;; Customizations
(defcustom elpaca-watcher-debounce-interval 0.5
  "Length of time to wait before registering a change for a repo.
See `run-at-time' for acceptable values."
  :group 'elpaca :type 'number)

;;;; Functions
(defun elpaca-watcher--hash-files (item &optional build)
  "Hash ITEM's :files. If BUILD is non-nil hash build files isntead of repo."
  (when-let ((e (elpaca-get item))
             (files (mapcar (if build #'cdr #'car) (elpaca--files e))))
    (secure-hash 'md5 (with-temp-buffer
                        (dolist (file files (buffer-string))
                          (if (file-directory-p file)
                              (dolist (f (directory-files-recursively file ".*"))
                                (ignore-errors (insert-file-contents-literally f)))
                            (ignore-errors (insert-file-contents-literally file))))))))

(defun elpaca-watcher--load-repos ()
  "Read `elpaca-watcher-file' into `elpaca-watcher--items'."
  (setq elpaca-watcher--items (elpaca--read-file elpaca-watcher-file)))

(defun elpaca-watcher--add-watches (files callback)
  "Add file system watchers to FILES.
CALLBACK is called with a `file-notify' event as its sole argument."
  (dolist (f files) (push (file-notify-add-watch f '(change) callback) eplaca--watchers)))

(defun elpaca-watcher-register-change (item)
  "Register ITEM change event."
  (message "%s %s changed" (format-time-string "[%Y-%m-%d %H:%M:%S]") item)
  (setf (alist-get item elpaca-watcher--timers nil t) nil)
  (when (elpaca-watcher-repo-modified-p item)
    (setf (alist-get item elpaca-watcher--items) (elpaca-watcher--hash-files item))
    (elpaca--write-file elpaca-watcher-file (print elpaca-watcher--items))))

(defun elpaca-watcher--register-change-maybe (event)
  "Set up `elpaca-watcher-register-change' for proper EVENTs."
  (when-let (((eq (nth 1 event) 'changed))
             (path (nth 2 event))
             (dir (file-name-directory path))
             (queued (cl-find dir (elpaca--queued)
                              :key (lambda (qd) (elpaca<-repo-dir (cdr qd)))
                              :test #'equal))
             ((member path (mapcar #'car (elpaca--files (cdr queued)))))
             (item (car queued)))
    (when-let ((timer (alist-get item elpaca-watcher--timers))) (cancel-timer timer))
    (setf (alist-get item elpaca-watcher--timers)
          (run-at-time elpaca-watcher-debounce-interval nil
                       #'elpaca-watcher-register-change item))))

(defun elpaca-watcher-repo-modified-p (item)
  "Return t if ITEM's repo hash does not match last known hash."
  (when-let ((e (elpaca-get item)))
    (not (equal (elpaca-watcher--hash-files item 'build)
                (elpaca-watcher--hash-files item)))))

(defun elpaca-watcher-modified-elpacas ()
  "Return a list of elpacas which have changed since last build."
  (cl-loop for (id . e) in (elpaca--queued)
           when (elpaca-watcher-repo-modified-p id) collect e))

;;@INCOMPLETE: Implement child process run in server mode?
(define-minor-mode elpaca-watcher-mode
  "Monitors elpaca package store for changes to automatically rebuild packages."
  :global t :group 'elpaca :lighter "elpaca-watcher "
  (if elpaca-watcher-mode
      (elpaca-watcher--add-watches
       (cl-loop for (_ . e) in (elpaca--queued) collect (elpaca<-repo-dir e))
       #'elpaca-watcher--register-change-maybe)
    (while eplaca--watchers (file-notify-rm-watch (pop eplaca--watchers)))))

(provide 'elpaca-watcher)

;;; elpaca-watcher.el ends here
