;;; screenfetch.el --- Screenfetch system info panel module -*- lexical-binding: t; -*-

;;; Commentary:
;; Screenfetch system information panel configuration and management.

;;; Code:

;; Panel metadata
(defvar screenfetch-panel-name "System"
  "Display name for this panel.")

(defvar screenfetch-panel-key "s"
  "Keybinding suffix for this panel (C-c + this key).")

(defvar screenfetch-panel-color "#FF6B35"
  "Color for this panel's tab and indicators.")

(defvar screenfetch-panel-command "screenfetch"
  "Command to run in terminal for this panel.")

(defvar screenfetch-terminal-buffer nil
  "Buffer for Screenfetch terminal.")

(defun screenfetch-setup-panel ()
  "Set up the Screenfetch terminal."
  (setq screenfetch-terminal-buffer (ansi-term "/bin/bash" "screenfetch-terminal"))
  (with-current-buffer screenfetch-terminal-buffer
    (sleep-for 0.2)
    (term-send-string (get-buffer-process (current-buffer)) (concat screenfetch-panel-command "\n")))
  screenfetch-terminal-buffer)

(defun screenfetch-switch-to-panel ()
  "Switch to Screenfetch panel."
  (interactive)
  (when (and screenfetch-terminal-buffer (buffer-live-p screenfetch-terminal-buffer))
    screenfetch-terminal-buffer))

(defun screenfetch-is-ready-p ()
  "Check if Screenfetch panel is ready."
  (and screenfetch-terminal-buffer (buffer-live-p screenfetch-terminal-buffer)))

;; Register this panel (metadata for auto-discovery)
(defvar screenfetch-panel-config
  `(:name ,screenfetch-panel-name
    :key ,screenfetch-panel-key
    :color ,screenfetch-panel-color
    :setup-function screenfetch-setup-panel
    :switch-function screenfetch-switch-to-panel
    :ready-function screenfetch-is-ready-p
    :assistant-mode "system"))

(provide 'screenfetch)
;;; screenfetch.el ends here