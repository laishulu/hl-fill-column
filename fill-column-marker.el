;;; fill-column-marker.el --- Provides syntax text objects. -*- lexical-binding: t; -*-

;; `define-namespace' is autoloaded, so there's no need to require
;; `names'. However, requiring it here means it will also work for
;; people who don't install through package.el.
(eval-when-compile (require 'names))

(define-namespace fill-column-

(defcustom column 80
  "column to be marked"
  :type 'integer
  :group 'fill-column-marker)

(defface marker '((t (:background "darkblack" :foreground "white")))
  :group 'fill-column-marker)

(defvar marker-face 'marker
  "Face used for a column marker.  Usually a background color.
Changing this directly affects only new markers.")

(make-variable-buffer-local 'marker-face) ; Buffer local in all buffers.

(defun find (end)
  "Defines a function to locate a character in column COL.
Returns the function symbol, named `column-marker-move-to-COL'."
  (let ((start (point)))
    (when (> end (point-max)) (setq end (point-max)))

    ;; Try to keep `move-to-column' from going backward, though it still can.
    (unless (< (current-column) column) (forward-line 1))

    ;; Again, don't go backward.  Try to move to correct column.
    (when (< (current-column) column) (move-to-column column))

    ;; If not at target column, try to move to it.
    (while (and (< (current-column) column) (< (point) end)
                (= 0 (+ (forward-line 1) (current-column)))) ; Should be bol.
      (move-to-column column))

    ;; If at target column, not past end, and not prior to start,
    ;; then set match data and return t.  Otherwise go to start
    ;; and return nil.
    (if (and (= column (current-column)) (<= (point) end) (> (point) start))
        (progn (set-match-data (list (1- (point)) (point)))
               t)            ; Return t.
      (goto-char start)
      nil)))                ; Return nil.

(define-minor-mode mode
  "Fill column marker mode."
  :init-value nil
  :global t
  (if mode
      (progn (set 'marker '((find (0 'marker-face prepend t))))
             (font-lock-add-keywords nil marker t)
             (font-lock-fontify-buffer))
    (font-lock-remove-keywords nil marker)
    (set 'marker nil)))

;; end of namespace
)

(provide 'fill-column-marker)
;;; fill-column-marker.el ends here
