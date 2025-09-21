;;; order.el --- Panel ordering configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Defines custom ordering for panels. Panels listed here will appear first
;; in the specified order. Any panels not listed will appear after in
;; alphabetical order.

;;; Code:

(defvar vibe-panel-order '("System")
  "List of panel names in desired order.
Panels not listed here will appear after in alphabetical order.")

(provide 'order)
;;; order.el ends here