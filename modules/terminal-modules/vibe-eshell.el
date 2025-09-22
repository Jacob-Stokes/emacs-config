;;; vibe-eshell.el --- Eshell terminal module -*- lexical-binding: t; -*-

;;; Commentary:
;; Eshell terminal - Emacs's built-in Lisp shell.

;;; Code:

;; Terminal metadata
(defvar vibe-eshell-terminal-name "EShell"
  "Display name for this terminal.")

(defvar vibe-eshell-terminal-key "e"
  "Keybinding suffix for this terminal (C-c a + this key).")

(defvar vibe-eshell-terminal-color "#e74c3c"
  "Color for this terminal's tab and indicators.")

(defvar vibe-eshell-terminal-buffer nil
  "Buffer for eshell terminal.")

(defun vibe-eshell-setup-terminal ()
  "Set up the eshell terminal."
  (unless (and vibe-eshell-terminal-buffer (buffer-live-p vibe-eshell-terminal-buffer))
    (let ((default-directory "/root/"))
      (setq vibe-eshell-terminal-buffer (eshell 99))  ; 99 is just a unique number
      (with-current-buffer vibe-eshell-terminal-buffer
        (rename-buffer "*vibe-eshell-terminal*" t))))
  vibe-eshell-terminal-buffer)

(defun vibe-eshell-switch-to-terminal ()
  "Switch to eshell terminal."
  (interactive)
  (if (and vibe-eshell-terminal-buffer (buffer-live-p vibe-eshell-terminal-buffer))
      vibe-eshell-terminal-buffer
    (vibe-eshell-setup-terminal)))

(defun vibe-eshell-is-ready-p ()
  "Check if eshell terminal is ready."
  (and vibe-eshell-terminal-buffer (buffer-live-p vibe-eshell-terminal-buffer)))

;; Register this terminal (metadata for auto-discovery)
(defvar vibe-eshell-terminal-config
  `(:name ,vibe-eshell-terminal-name
    :key ,vibe-eshell-terminal-key
    :color ,vibe-eshell-terminal-color
    :setup-function vibe-eshell-setup-terminal
    :switch-function vibe-eshell-switch-to-terminal
    :ready-function vibe-eshell-is-ready-p))

(provide 'vibe-eshell)
;;; vibe-eshell.el ends here