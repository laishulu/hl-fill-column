;;; fill-column-marker-mode.el --- Provides syntax text objects. -*- lexical-binding: t; -*-

(defcustom fill-column-marker-column 80
  "column to be marked"
  :type 'integer
  :group 'fill-column-marker)

;; hightlight fill column
(use-package column-marker
  :config
  (define-minor-mode fill-column-marker-mode
    "Fill column marker mode."
    ;; The initial value.
    :init-value t
    (if fill-column-marker-mode
        (progn
          (set-face-attribute 'column-marker-1 nil
                              :foreground "magenta")
          (column-marker-1 fill-column-marker-column))
      ;; C-u     M-x display-prefix  -| (4)
      (column-marker-1 '(4))))

  (add-hook 'after-change-major-mode-hook 'fill-column-marker-mode))

(provide 'fill-column-marker-mode)
;;; fill-column-marker-mode.el ends here
