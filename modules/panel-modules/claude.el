;;; claude.el --- Claude AI panel module -*- lexical-binding: t; -*-

;;; Commentary:
;; Claude AI panel configuration and management.

;;; Code:

;; Panel metadata
(defvar claude-panel-name "Claude"
  "Display name for this panel.")

(defvar claude-panel-key "c"
  "Keybinding suffix for this panel (C-c + this key).")

(defvar claude-panel-color "#48dbfb"
  "Color for this panel's tab and indicators.")

(defvar claude-panel-command "claude"
  "Command to run in terminal for this panel.")

(defvar claude-terminal-buffer nil
  "Buffer for Claude terminal.")

(defun claude-setup-panel ()
  "Set up the Claude terminal."
  (setq claude-terminal-buffer (ansi-term "/bin/bash" "claude-terminal"))
  (with-current-buffer claude-terminal-buffer
    (sleep-for 0.2)
    (term-send-string (get-buffer-process (current-buffer)) (concat claude-panel-command "\n")))
  claude-terminal-buffer)

(defun claude-switch-to-panel ()
  "Switch to Claude panel."
  (interactive)
  (when (and claude-terminal-buffer (buffer-live-p claude-terminal-buffer))
    claude-terminal-buffer))

(defun claude-is-ready-p ()
  "Check if Claude panel is ready."
  (and claude-terminal-buffer (buffer-live-p claude-terminal-buffer)))

;; Register this panel (metadata for auto-discovery)
(defvar claude-panel-config
  `(:name ,claude-panel-name
    :key ,claude-panel-key
    :color ,claude-panel-color
    :setup-function claude-setup-panel
    :switch-function claude-switch-to-panel
    :ready-function claude-is-ready-p
    :assistant-mode "claude"))

(provide 'claude)
;;; claude.el ends here