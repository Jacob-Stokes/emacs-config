;;; vibes-ai-terminals.el --- AI terminal management -*- lexical-binding: t; -*-

;;; Commentary:
;; This module manages AI terminal buffers for Claude, Codex, and Gemini.

;;; Code:

(defvar claude-terminal-buffer nil "Buffer for Claude terminal")
(defvar gpt-terminal-buffer nil "Buffer for GPT terminal") 
(defvar gemini-terminal-buffer nil "Buffer for Gemini terminal")
(defvar right-pane-window nil "The window displaying the right pane terminals")
(defvar right-pane-header-buffer nil "Buffer for the right pane header")
(defvar right-pane-active-terminal "claude" "Currently active terminal in right pane")

(defun update-right-pane-header ()
  "Update the right pane header to show terminal tabs"
  (when (and right-pane-header-buffer (buffer-live-p right-pane-header-buffer))
    (with-current-buffer right-pane-header-buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "  ")
        ;; Claude tab
        (insert (if (string= right-pane-active-terminal "claude")
                    (propertize "Claude" 'face '(:foreground "#48dbfb" :weight bold :underline t))
                  (propertize "Claude" 'face '(:foreground "#888888"))))
        (insert " | ")
        ;; Codex tab
        (insert (if (string= right-pane-active-terminal "gpt")
                    (propertize "Codex" 'face '(:foreground "#a29bfe" :weight bold :underline t))
                  (propertize "Codex" 'face '(:foreground "#888888"))))
        (insert " | ")
        ;; Gemini tab
        (insert (if (string= right-pane-active-terminal "gemini")
                    (propertize "Gemini" 'face '(:foreground "#4CAF50" :weight bold :underline t))
                  (propertize "Gemini" 'face '(:foreground "#888888"))))
        (insert "  (C-c c/g/j)"))
      (read-only-mode 1))))

(defun switch-right-terminal (terminal-type)
  "Switch the right pane to show specified terminal"
  (interactive)
  (if right-pane-window
      (progn
        (let ((target-buffer
               (cond
                ((string= terminal-type "claude") claude-terminal-buffer)
                ((string= terminal-type "gpt") gpt-terminal-buffer)
                ((string= terminal-type "gemini") gemini-terminal-buffer))))
          (when target-buffer
            (set-window-buffer right-pane-window target-buffer)
            (setq right-pane-active-terminal terminal-type)
            (setq current-assistant-mode terminal-type)
            (update-right-pane-header)
            (select-window right-pane-window)
            (cond
             ((string= terminal-type "claude")
              (message "Switched to Claude terminal (C-c g for Codex, C-c j for Gemini)"))
             ((string= terminal-type "gpt")
              (message "Switched to Codex terminal (C-c c for Claude, C-c j for Gemini)"))
             ((string= terminal-type "gemini")
              (message "Switched to Gemini terminal (C-c c for Claude, C-c g for Codex)"))))))
    (message "Right pane not found. Run setup-vscode-layout first.")))

(provide 'vibes-ai-terminals)
;;; vibes-ai-terminals.el ends here
