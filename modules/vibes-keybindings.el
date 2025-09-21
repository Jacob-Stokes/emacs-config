;;; vibes-keybindings.el --- All keybindings -*- lexical-binding: t; -*-

;;; Commentary:
;; Central location for all VibEmacs keybindings.

;;; Code:

(require 'vibes-ai-terminals)
(require 'vibes-layout)
(require 'vibes-animations)

;; Create keybindings for quick terminal switching
(global-set-key (kbd "C-c c") (lambda () (interactive) (switch-right-terminal "claude")))
(global-set-key (kbd "C-c g") (lambda () (interactive) (switch-right-terminal "gpt")))
(global-set-key (kbd "C-c j") (lambda () (interactive) (switch-right-terminal "gemini")))

;; Keybinding to manually switch animation
(global-set-key (kbd "C-c n") 'switch-animation-mode)

;; Layout management
(global-set-key (kbd "C-c l") 'setup-vscode-layout)
(global-set-key (kbd "C-c t") (lambda () (interactive) (ansi-term "/bin/bash")))

;; Fun features
(global-set-key (kbd "C-c f") 'fireplace)
(global-set-key (kbd "C-c i") 'imenu-list-smart-toggle)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Better window switching
(global-set-key (kbd "M-[") 'previous-buffer)
(global-set-key (kbd "M-]") 'next-buffer)

(provide 'vibes-keybindings)
;;; vibes-keybindings.el ends here
