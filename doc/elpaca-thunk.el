;;; Example use of `elpaca-thunk' -*- lexical-binding: t; -*-

;; Assuming that Elpaca has been bootstrapped
(require 'elpaca)

;; Compile-time dependency
(eval-when-compile
  ;; Built-in library
  (require 'generator)
  ;; Macro definition
  (defmacro noop-macro (&rest body)
    "Do nothing to BODY."
    (declare (indent 0))
    (macroexp-progn body))
  ;; Inline function definition
  (defsubst noop-fn (arg)
    "Do nothing to ARG."
    arg))

;; Unlike `elpaca', `elpaca-thunk' wraps BODY directly in a thunk
;; (nullary function), which means macros and inline functions are
;; expanded at compile time.  Conversely, this also means that
;; bootstrapping can create dependency problem.  Don't use it unless
;; you're sure what you're doing!
(elpaca-thunk nil
  ;; This shouldn't lead to void-function
  (noop-macro
    (message (noop-fn "first"))
    (noop-fn (iter-lambda () nil))
    (message (noop-fn "second"))))

;; Process the above order
(elpaca-process-queues)

;; Local Variables:
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
