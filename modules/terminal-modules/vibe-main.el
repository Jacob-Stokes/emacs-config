;;; vibe-main.el --- Main terminal module -*- lexical-binding: t; -*-

;;; Commentary:
;; Main terminal configuration - the default bash terminal.

;;; Code:

;; Terminal metadata
(defvar vibe-main-terminal-name "Main"
  "Display name for this terminal.")

(defvar vibe-main-terminal-key "m"
  "Keybinding suffix for this terminal (C-c a + this key).")

(defvar vibe-main-terminal-color "#00ff00"
  "Color for this terminal's tab and indicators.")

(defvar vibe-main-terminal-buffer nil
  "Buffer for main terminal.")

(defun vibe-main-setup-terminal ()
  "Set up the main terminal."
  (unless (and vibe-main-terminal-buffer (buffer-live-p vibe-main-terminal-buffer))
    (setq vibe-main-terminal-buffer (ansi-term "/bin/bash" "vibe-main-terminal")))
  vibe-main-terminal-buffer)

(defun vibe-main-switch-to-terminal ()
  "Switch to main terminal."
  (interactive)
  (if (and vibe-main-terminal-buffer (buffer-live-p vibe-main-terminal-buffer))
      vibe-main-terminal-buffer
    (vibe-main-setup-terminal)))

(defun vibe-main-is-ready-p ()
  "Check if main terminal is ready."
  (and vibe-main-terminal-buffer (buffer-live-p vibe-main-terminal-buffer)))

;; Register this terminal (metadata for auto-discovery)
(defvar vibe-main-terminal-config
  `(:name ,vibe-main-terminal-name
    :key ,vibe-main-terminal-key
    :color ,vibe-main-terminal-color
    :setup-function vibe-main-setup-terminal
    :switch-function vibe-main-switch-to-terminal
    :ready-function vibe-main-is-ready-p))

(provide 'vibe-main)
;;; vibe-main.el ends here