;;; order.el --- Terminal ordering configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Defines custom ordering for terminals. Terminals listed here will appear first
;; in the specified order. Any terminals not listed will appear after in
;; alphabetical order.

;;; Code:

(defvar vibe-terminal-order '("Main" "Docker" "Shell" "EShell")
  "List of terminal names in desired order.
Terminals not listed here will appear after in alphabetical order.")

(provide 'order)
;;; order.el ends here