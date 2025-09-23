;;; vibe-shell.el --- Shell mode terminal module -*- lexical-binding: t; -*-

;;; Commentary:
;; Shell mode terminal - runs Emacs shell-mode for better integration.

;;; Code:

(require 'comint)

;; Terminal metadata
(defvar vibe-shell-terminal-name "Shell"
  "Display name for this terminal.")

(defvar vibe-shell-terminal-key "s"
  "Keybinding suffix for this terminal (C-c a + this key).")

(defvar vibe-shell-terminal-color "#3498db"
  "Color for this terminal's tab and indicators.")

(defvar vibe-shell-terminal-buffer nil
  "Buffer for shell terminal.")

(defun vibe-shell--process-alive-p ()
  "Return non-nil when the shell terminal buffer has a live process."
  (and vibe-shell-terminal-buffer
       (buffer-live-p vibe-shell-terminal-buffer)
       (comint-check-proc vibe-shell-terminal-buffer)))

(defun vibe-shell-setup-terminal ()
  "Create the shell terminal buffer without disrupting the current window."
  (let ((buffer-name "*vibe-shell-terminal*"))
    (unless (vibe-shell--process-alive-p)
      (when (and vibe-shell-terminal-buffer
                 (buffer-live-p vibe-shell-terminal-buffer))
        (kill-buffer vibe-shell-terminal-buffer))
      (let ((default-directory (or (and (boundp 'default-directory)
                                        default-directory)
                                   "/root/")))
        (setq vibe-shell-terminal-buffer
              (save-window-excursion
                (shell buffer-name)))))
    (setq vibe-shell-terminal-buffer (get-buffer buffer-name)))
  vibe-shell-terminal-buffer)

(defun vibe-shell-switch-to-terminal ()
  "Switch to shell terminal."
  (interactive)
  (if (and vibe-shell-terminal-buffer (buffer-live-p vibe-shell-terminal-buffer))
      vibe-shell-terminal-buffer
    (vibe-shell-setup-terminal)))

(defun vibe-shell-is-ready-p ()
  "Check if shell terminal is ready."
  (and vibe-shell-terminal-buffer (buffer-live-p vibe-shell-terminal-buffer)))

;; Register this terminal (metadata for auto-discovery)
(defvar vibe-shell-terminal-config
  `(:name ,vibe-shell-terminal-name
    :key ,vibe-shell-terminal-key
    :color ,vibe-shell-terminal-color
    :setup-function vibe-shell-setup-terminal
    :switch-function vibe-shell-switch-to-terminal
    :ready-function vibe-shell-is-ready-p))

(provide 'vibe-shell)
;;; vibe-shell.el ends here
