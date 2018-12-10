;;; hl-fill-column.el --- Highlight fill column. -*- lexical-binding: t; -*-

;; URL: https://github.com/laishulu/hl-fill-column
;; Created: November 1, 2018
;; Keywords: fill column, faces
;; Package-Requires: ((names "0.5") (emacs "24"))
;; Version: 0.1

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; This package provide modes to highlight fill column.
;; For more information see the README in the GitHub repo.

;;; Code:

;; `define-namespace' is autoloaded, so there's no need to require `names'.
;; However, requiring it here means it will also work for people who don't
;; install through package.el.
(eval-when-compile (require 'names))

(define-namespace hl-fill-column-

(defvar -keywords ()
  "Font lock keywords for fill column.")
(make-variable-buffer-local (quote -keywords))

(defface face '((t (:background "brightblack" :foreground "white")))
  "Face used to highlight fill column"
  :group 'hl-fill-column)

(defun -find (end)
  "Function to locate a character in fill column.
Look through END when provided."
  (let ((start (point)))
    (when (> end (point-max)) (setq end (point-max)))

    ;; Try to keep `move-to-column' from going backward, though it still can.
    (unless (< (current-column) fill-column) (forward-line 1))

    ;; Again, don't go backward.  Try to move to correct column.
    (when (< (current-column) fill-column) (move-to-column fill-column))

    ;; If not at target column, try to move to it.
    (while (and (< (current-column) fill-column) (< (point) end)
                (= 0 (+ (forward-line 1) (current-column)))) ; Should be bol.
      (move-to-column fill-column))

    ;; If at target column, not past end, and not prior to start, then set match
    ;; data and return t.  Otherwise go to start and return nil.
    (if (and (= fill-column (current-column))
             (<= (point) end)
             (> (point) start))
        (progn (set-match-data (list (1- (point)) (point)))
               t)                       ; Return t.
      (goto-char start)
      nil)))                            ; Return nil.

(define-minor-mode mode
  "Highlight fill column mode"
  :init-value nil
  (if mode
      (progn
        (setq -keywords
              '((hl-fill-column--find (0 'hl-fill-column-face prepend t))))
        (font-lock-add-keywords nil -keywords t))
    (font-lock-remove-keywords nil -keywords)
    (setq -keywords nil))
  (font-lock-fontify-buffer))

;; end of namespace
)

;;;###autoload
(define-globalized-minor-mode
  global-hl-fill-column-mode
  hl-fill-column-mode
  (lambda () (hl-fill-column-mode t))
  :group 'hl-fill-column)

(provide 'hl-fill-column)
;;; hl-fill-column.el ends here
