;;; codex.el --- Codex AI panel module -*- lexical-binding: t; -*-

;;; Commentary:
;; Codex/GPT AI panel configuration and management.

;;; Code:

;; Panel metadata
(defvar codex-panel-name "Codex"
  "Display name for this panel.")

(defvar codex-panel-key "g"
  "Keybinding suffix for this panel (C-c + this key).")

(defvar codex-panel-color "#a29bfe"
  "Color for this panel's tab and indicators.")

(defvar codex-panel-command "codex"
  "Command to run in terminal for this panel.")

(defvar codex-terminal-buffer nil
  "Buffer for Codex terminal.")

(defun codex-setup-panel ()
  "Set up the Codex terminal."
  (setq codex-terminal-buffer (ansi-term "/bin/bash" "codex-terminal"))
  (with-current-buffer codex-terminal-buffer
    (sleep-for 0.2)
    (term-send-string (get-buffer-process (current-buffer)) (concat codex-panel-command "\n")))
  codex-terminal-buffer)

(defun codex-switch-to-panel ()
  "Switch to Codex panel."
  (interactive)
  (when (and codex-terminal-buffer (buffer-live-p codex-terminal-buffer))
    codex-terminal-buffer))

(defun codex-is-ready-p ()
  "Check if Codex panel is ready."
  (and codex-terminal-buffer (buffer-live-p codex-terminal-buffer)))

;; Register this panel (metadata for auto-discovery)
(defvar codex-panel-config
  `(:name ,codex-panel-name
    :key ,codex-panel-key
    :color ,codex-panel-color
    :setup-function codex-setup-panel
    :switch-function codex-switch-to-panel
    :ready-function codex-is-ready-p
    :assistant-mode "gpt"))

(provide 'codex)
;;; codex.el ends here