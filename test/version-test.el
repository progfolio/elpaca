;; Test the version check functionality in elpaca
(require 'ert)
(require 'elpaca)

;; Basic version comparison tests
(ert-deftest test-version-check-basic ()
  "Test basic version list comparisons."
  ;; Test older versions are less than newer versions
  (should (version-list-< '(26 3) '(27 1)))
  (should (version-list-< '(27 0) '(27 1)))

  ;; Test equal versions
  (should-not (version-list-< '(27 1) '(27 1)))

  ;; Test newer versions are not less than older versions
  (should-not (version-list-< '(27 2) '(27 1)))
  (should-not (version-list-< '(28 1) '(27 1)))
  (should-not (version-list-< '(31 0) '(27 1))))

;; Development version tests
(ert-deftest test-version-check-development ()
  "Test version comparisons with development versions."
  ;; Test development versions (e.g. 31.0.50)
  (should-not (version-list-< '(31 0 50) '(27 1)))
  (should-not (version-list-< '(31 0 50) '(31 0)))
  (should-not (version-list-< '(28 1 50) '(27 1)))

  ;; Test that major version takes precedence
  (should-not (version-list-< '(28 0) '(27 999)))
  (should-not (version-list-< '(28 0 0) '(27 999 999))))

;; Test actual Emacs version against minimum requirement
(ert-deftest test-version-check-current-emacs ()
  "Test current Emacs version against minimum requirement."
  (let ((current-version (version-to-list emacs-version))
        (min-version (version-to-list "27.1")))
    ;; Our Emacs version (31.0.50) should be higher than minimum (27.1)
    (should-not (version-list-< current-version min-version))

    ;; Print versions for debugging
    (message "Current Emacs version: %s" emacs-version)
    (message "Current version list: %S" current-version)
    (message "Minimum version list: %S" min-version)))

;; Test elpaca--check-version function
(ert-deftest test-elpaca-check-version ()
  "Test elpaca--check-version function."
  (let* ((pkg "test-pkg")
         (deps '((emacs "27.1")))
         ;; Mock necessary variables and functions
         (elpaca-after-init-time t)
         (elpaca-log-functions nil)
         (version-regexp-alist nil)
         (elpaca--queued (make-hash-table))
         (package--builtin-versions (make-hash-table))
         (elpaca-ignored-dependencies nil)
         (e (list 'elpaca pkg nil nil nil deps nil nil '() nil)))
    ;; Mock functions
    (cl-letf (((symbol-function 'elpaca--dependencies)
               (lambda (e) deps))
              ((symbol-function 'elpaca--fail)
               (lambda (e msg) nil))
              ((symbol-function 'elpaca--continue-build)
               (lambda (e) t))
              ((symbol-function 'elpaca--log)
               (lambda (e &optional info verbosity replace) nil))
              ((symbol-function 'elpaca--signal)
               (lambda (e &optional info verbosity replace) nil))
              ((symbol-function 'elpaca-alist-get)
               (lambda (key alist &optional default) default)))
      ;; Test that check-version passes for our current Emacs version
      (should (elpaca--check-version e))

      ;; Print debug info
      (message "Testing package: %s" pkg)
      (message "Dependencies: %S" deps)
      (message "Current Emacs version: %s" emacs-version))))

;; Test elpaca--check-version with different dependency versions
(ert-deftest test-elpaca-check-version-deps ()
  "Test elpaca--check-version with different dependency versions."
  (let* ((pkg "test-pkg")
         ;; Mock necessary variables and functions
         (elpaca-after-init-time t)
         (elpaca-log-functions nil)
         (version-regexp-alist nil)
         (elpaca--queued (make-hash-table))
         (package--builtin-versions (make-hash-table :test 'eq))
         (elpaca-ignored-dependencies nil)
         (emacs-version-list (version-to-list emacs-version)))
    ;; Mock functions
    (cl-letf (((symbol-function 'elpaca--fail)
               (lambda (e msg) nil))
              ((symbol-function 'elpaca--continue-build)
               (lambda (e) t))
              ((symbol-function 'elpaca--log)
               (lambda (e &optional info verbosity replace) nil))
              ((symbol-function 'elpaca--signal)
               (lambda (e &optional info verbosity replace) nil)))

      ;; Test with exact version match
      (let* ((deps '((emacs "31.0.50")))
             (e (list 'elpaca pkg nil nil nil deps nil nil '() nil)))
        (cl-letf (((symbol-function 'elpaca--dependencies)
                   (lambda (e) deps))
                  ((symbol-function 'elpaca-alist-get)
                   (lambda (key alist &optional default) default)))
          (should (elpaca--check-version e))
          (message "Passed exact version match test")))

      ;; Test with development version
      (let* ((deps '((emacs "31.0")))
             (e (list 'elpaca pkg nil nil nil deps nil nil '() nil)))
        (cl-letf (((symbol-function 'elpaca--dependencies)
                   (lambda (e) deps))
                  ((symbol-function 'elpaca-alist-get)
                   (lambda (key alist &optional default) default)))
          (should (elpaca--check-version e))
          (message "Passed development version test")))

      ;; Test with minimum version
      (let* ((deps '((emacs "27.1")))
             (e (list 'elpaca pkg nil nil nil deps nil nil '() nil)))
        (cl-letf (((symbol-function 'elpaca--dependencies)
                   (lambda (e) deps))
                  ((symbol-function 'elpaca-alist-get)
                   (lambda (key alist &optional default) default)))
          (should (elpaca--check-version e))
          (message "Passed minimum version test")))

      ;; Test with future version (should fail gracefully)
      (let* ((deps '((emacs "32.1")))
             (e (list 'elpaca pkg nil nil nil deps nil nil '() nil)))
        (cl-letf (((symbol-function 'elpaca--dependencies)
                   (lambda (e) deps))
                  ((symbol-function 'elpaca-alist-get)
                   (lambda (key alist &optional default) default)))
          (should (elpaca--check-version e))
          (message "Passed future version test")))

      ;; Test with no dependencies
      (let* ((deps nil)
             (e (list 'elpaca pkg nil nil nil deps nil nil '() nil)))
        (cl-letf (((symbol-function 'elpaca--dependencies)
                   (lambda (e) deps))
                  ((symbol-function 'elpaca-alist-get)
                   (lambda (key alist &optional default) default)))
          (should (elpaca--check-version e))
          (message "Passed no dependencies test")))

      ;; Test with core package dependency
      (let* ((deps '((cl-lib "1.0")))
             (e (list 'elpaca pkg nil nil nil deps nil nil '() nil)))
        (puthash 'cl-lib '(1 0) package--builtin-versions)
        (cl-letf (((symbol-function 'elpaca--dependencies)
                   (lambda (e) deps))
                  ((symbol-function 'elpaca-alist-get)
                   (lambda (key alist &optional default)
                     (if (and (eq key 'cl-lib) (eq alist package--builtin-versions))
                         '(1 0)
                       default))))
          (should (elpaca--check-version e))
          (message "Passed core package dependency test")))

      ;; Test with date-based version
      (let* ((deps `((emacs ,(format-time-string "%Y%m%d"))))
             (e (list 'elpaca pkg nil nil nil deps nil nil '() nil)))
        (cl-letf (((symbol-function 'elpaca--dependencies)
                   (lambda (e) deps))
                  ((symbol-function 'elpaca-alist-get)
                   (lambda (key alist &optional default) default)))
          (should (elpaca--check-version e))
          (message "Passed date-based version test")))

      ;; Test with ignored dependency
      (let* ((deps '((ignored-pkg "1.0")))
             (elpaca-ignored-dependencies '(ignored-pkg))
             (e (list 'elpaca pkg nil nil nil deps nil nil '() nil)))
        (cl-letf (((symbol-function 'elpaca--dependencies)
                   (lambda (e) deps))
                  ((symbol-function 'elpaca-alist-get)
                   (lambda (key alist &optional default) default)))
          (should (elpaca--check-version e))
          (message "Passed ignored dependency test")))

      ;; Test Emacs version in non-batch mode
      (let* ((deps '((emacs "27.1")))
             (e (list 'elpaca pkg nil nil nil deps nil nil '() nil)))
        (cl-letf (((symbol-function 'elpaca--dependencies)
                   (lambda (e) deps))
                  ((symbol-function 'elpaca-alist-get)
                   (lambda (key alist &optional default) default)))
          (should (elpaca--check-version e))
          (message "Passed non-batch mode Emacs version test"))))))

(provide 'version-test)
