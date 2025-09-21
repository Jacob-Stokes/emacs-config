;;; vibes-theme.el --- Theme and appearance configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2024
;; Author: VibEmacs Configuration

;;; Commentary:
;; This module handles all theme and appearance settings including
;; transparent background, color theme, custom faces, and modeline.

;;; Code:

;; Set transparent background
(defun set-background-transparent ()
  "Make the background transparent"
  (set-face-background 'default "unspecified-bg" (selected-frame))
  (set-face-background 'line-number "unspecified-bg" (selected-frame))
  (set-face-background 'line-number-current-line "unspecified-bg" (selected-frame)))

;; Apply transparency after frame is created
(add-hook 'after-make-frame-functions
          (lambda (frame)
            (select-frame frame)
            (set-background-transparent)))

;; Also set it for the initial frame
(add-hook 'window-setup-hook 'set-background-transparent)

;; Use a color theme that works well with transparency
(load-theme 'tango t)  ; Good contrast for transparent background

;; Custom face colors for better visibility on transparent background
(custom-set-faces
 '(default ((t (:foreground "#E0E0E0"))))
 '(font-lock-comment-face ((t (:foreground "#75715e"))))
 '(font-lock-function-name-face ((t (:foreground "#fd971f"))))
 '(font-lock-keyword-face ((t (:foreground "#66d9ef"))))
 '(font-lock-string-face ((t (:foreground "#a6e22e"))))
 '(hl-line ((t (:background "unspecified" :underline t)))))

;; Modeline (status bar)
(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-height 25
        doom-modeline-icon nil  ; Disable icons in terminal
        doom-modeline-unicode-fallback t  ; Use unicode symbols instead
        doom-modeline-buffer-state-icon nil  ; Remove lock icon for read-only buffers
        doom-modeline-modal nil  ; Remove modal editing states
        doom-modeline-buffer-modification-icon nil))  ; Remove modification indicator

;; Line numbers - only for programming/text modes
(setq display-line-numbers-type 'relative)

;; Hook to enable line numbers only in programming and text modes
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'text-mode-hook 'display-line-numbers-mode)
(add-hook 'conf-mode-hook 'display-line-numbers-mode)

;; Disable line numbers in specific modes
(dolist (mode '(dashboard-mode-hook
                treemacs-mode-hook
                term-mode-hook
                eshell-mode-hook
                shell-mode-hook
                help-mode-hook
                Man-mode-hook
                woman-mode-hook
                dired-mode-hook
                imenu-list-major-mode-hook
                ansi-term-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Highlight current line
(global-hl-line-mode 1)

(provide 'vibes-theme)
;;; vibes-theme.el ends here