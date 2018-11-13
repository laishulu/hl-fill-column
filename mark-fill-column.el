;;; mark-fill-column.el --- Provides syntax text objects. -*- lexical-binding: t; -*-

;; `define-namespace' is autoloaded, so there's no need to require
;; `names'. However, requiring it here means it will also work for
;; people who don't install through package.el.
(eval-when-compile (require 'names))

(define-namespace mark-fill-column-

(defcustom col 80
  "column to be marked"
  :type 'integer
  :group 'mark-fill-column)

(defvar keyword ()
  "Font lock keyword for fill column"
  :group 'mark-fill-column)

(defface face '((t (:background "brightblack" :foreground "white")))
  "Face used to mark fill column"
  :group 'mark-fill-column)

(defun find (end)
  "Defines a function to locate a character in column COL.
Returns the function symbol, named `column-marker-move-to-COL'."
  (let ((start (point)))
    (when (> end (point-max)) (setq end (point-max)))

    ;; Try to keep `move-to-column' from going backward, though it still can.
    (unless (< (current-column) col) (forward-line 1))

    ;; Again, don't go backward.  Try to move to correct column.
    (when (< (current-column) col) (move-to-column col))

    ;; If not at target column, try to move to it.
    (while (and (< (current-column) col) (< (point) end)
                (= 0 (+ (forward-line 1) (current-column)))) ; Should be bol.
      (move-to-column col))

    ;; If at target column, not past end, and not prior to start,
    ;; then set match data and return t.  Otherwise go to start
    ;; and return nil.
    (if (and (= col (current-column)) (<= (point) end) (> (point) start))
        (progn (set-match-data (list (1- (point)) (point)))
               t)            ; Return t.
      (goto-char start)
      nil)))                ; Return nil.

(define-minor-mode mode
  "Fill column marker mode."
  :init-value nil
  (if mode
      (progn (set 'keyword '((find (0 'face prepend t))))
             (font-lock-add-keywords nil keyword t)
             (font-lock-fontify-buffer))
    (font-lock-remove-keywords nil keyword)
    (set 'keyword nil)))

;; end of namespace
)

(provide 'mark-fill-column)
;;; mark-fill-column.el ends here
