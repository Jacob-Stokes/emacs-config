;;; vibes-layout.el --- VS Code-like layout management -*- lexical-binding: t; -*-

;;; Commentary:
;; Window layout management for VS Code-like experience.

;;; Code:

(require 'vibes-ai-terminals)
(require 'vibes-animations)

(defun reset-vscode-layout ()
  "Reset to VSCode-like layout from any state"
  (interactive)
  (delete-other-windows)
  (treemacs)
  (other-window 1))

(defun setup-vscode-layout ()
  "Set up VS Code-like layout with three panels"
  (interactive)
  (if (and claude-terminal-buffer (buffer-live-p claude-terminal-buffer))
      (reset-vscode-layout)
    (progn
      ;; Clean slate
      (delete-other-windows)
      
      ;; Left panel - Treemacs file explorer
      (unless (get-buffer-window "*treemacs*")
        (treemacs))
      (other-window 1)
      
      ;; Center panel - Editor (show dashboard initially)
      (dashboard-refresh-buffer)
      (start-rainbow-animation)
      
      ;; Right panel for AI terminals (35% width)
      (split-window-horizontally (- (floor (* (window-width) 0.35))))
      (other-window 1)
      
      ;; Create header buffer for tabs
      (setq right-pane-header-buffer (get-buffer-create "*Right Pane Tabs*"))
      (with-current-buffer right-pane-header-buffer
        (display-line-numbers-mode -1)
        (setq mode-line-format nil)
        (setq window-size-fixed t))
      
      ;; Display header
      (switch-to-buffer right-pane-header-buffer)
      (let ((header-window (selected-window)))
        (set-window-dedicated-p header-window t)
        (set-window-parameter header-window 'no-other-window t))
      
      ;; Split for terminal below header
      (split-window-vertically 2)
      (other-window 1)
      
      ;; Store this window as the right pane
      (setq right-pane-window (selected-window))
      
      ;; Create GPT/Codex terminal
      (setq gpt-terminal-buffer (ansi-term "/bin/bash" "gpt-terminal"))
      (with-current-buffer gpt-terminal-buffer
        (sleep-for 0.2)
        (term-send-string (get-buffer-process (current-buffer)) "codex\n"))
      
      ;; Now create claude terminal
      (setq claude-terminal-buffer (ansi-term "/bin/bash" "claude-terminal"))
      (with-current-buffer claude-terminal-buffer
        (sleep-for 0.2)
        (term-send-string (get-buffer-process (current-buffer)) "claude\n"))
      
      ;; Create Gemini terminal
      (setq gemini-terminal-buffer (ansi-term "/bin/bash" "gemini-terminal"))
      (with-current-buffer gemini-terminal-buffer
        (sleep-for 0.2)
        (term-send-string (get-buffer-process (current-buffer)) "gemini\n"))
      
      ;; Make sure claude is displayed by default
      (set-window-buffer right-pane-window claude-terminal-buffer)
      (setq right-pane-active-terminal "claude")
      (update-right-pane-header)
      
      ;; Go back to middle window - need to go through treemacs
      (other-window 1)  ; Back to header
      (other-window 1)  ; To treemacs
      (other-window 1)  ; To center editor
      
      ;; Bottom section - split horizontally
      (let* ((height (window-height))
             (editor-height (floor (* height 0.8))))
        (split-window-vertically editor-height))
      
      ;; Move to bottom
      (other-window 1)
      
      ;; Split bottom: terminal (70%) and animation (30%)
      (split-window-horizontally (floor (* (window-width) 0.7)))
      
      ;; Open standard terminal at bottom left
      (ansi-term "/bin/bash" "bottom-terminal")
      
      ;; Move to bottom right for matrix rain
      (other-window 1)
      (create-matrix-rain-buffer)
      (switch-to-buffer matrix-rain-buffer)
      (init-matrix-rain)
      (start-matrix-rain-animation)
      (start-animation-switcher)
      (set-window-dedicated-p (selected-window) t)
      (set-window-parameter (selected-window) 'no-other-window t)
      
      ;; Focus back on top middle editor window
      (other-window -1))))

(provide 'vibes-layout)
;;; vibes-layout.el ends here
