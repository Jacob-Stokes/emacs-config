;;; gemini.el --- Gemini AI panel module -*- lexical-binding: t; -*-

;;; Commentary:
;; Gemini AI panel configuration and management.

;;; Code:

;; Panel metadata
(defvar gemini-panel-name "Gemini"
  "Display name for this panel.")

(defvar gemini-panel-key "j"
  "Keybinding suffix for this panel (C-c + this key).")

(defvar gemini-panel-color "#4CAF50"
  "Color for this panel's tab and indicators.")

(defvar gemini-panel-command "gemini"
  "Command to run in terminal for this panel.")

(defvar gemini-terminal-buffer nil
  "Buffer for Gemini terminal.")

(defun gemini-setup-panel ()
  "Set up the Gemini terminal."
  (setq gemini-terminal-buffer (ansi-term "/bin/bash" "gemini-terminal"))
  (with-current-buffer gemini-terminal-buffer
    (sleep-for 0.2)
    (term-send-string (get-buffer-process (current-buffer)) (concat gemini-panel-command "\n")))
  gemini-terminal-buffer)

(defun gemini-switch-to-panel ()
  "Switch to Gemini panel."
  (interactive)
  (when (and gemini-terminal-buffer (buffer-live-p gemini-terminal-buffer))
    gemini-terminal-buffer))

(defun gemini-is-ready-p ()
  "Check if Gemini panel is ready."
  (and gemini-terminal-buffer (buffer-live-p gemini-terminal-buffer)))

;; Register this panel (metadata for auto-discovery)
(defvar gemini-panel-config
  `(:name ,gemini-panel-name
    :key ,gemini-panel-key
    :color ,gemini-panel-color
    :setup-function gemini-setup-panel
    :switch-function gemini-switch-to-panel
    :ready-function gemini-is-ready-p
    :assistant-mode "gemini"))

(provide 'gemini)
;;; gemini.el ends here