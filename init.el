;;; init.el --- VS Code-like Emacs Development Environment -*- lexical-binding: t; -*-

;; Copyright (C) 2024
;; Author: VibEmacs Configuration

;;; Commentary:
;; This is the main configuration file that loads all VibEmacs modules.
;; The configuration is split into focused modules for better organization.

;;; Code:

;; Add modules directory to load path
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

;; Performance optimizations during startup
(setq gc-cons-threshold (* 100 1024 1024))  ; 100MB
(setq read-process-output-max (* 1024 1024))  ; 1MB

;; Load modules in specific order (dependencies matter!)
(require 'vibes-core)          ; Package management and core settings
(require 'vibes-theme)         ; Theme and appearance
(require 'vibes-packages)      ; Third-party package configurations
(require 'vibes-aesthetic)     ; Visual enhancements and vibe features
(require 'vibes-dashboard)     ; Dashboard and welcome screen
(require 'vibes-animations)    ; Animation system (matrix, aquarium, etc.)
(require 'vibes-ai-terminals)  ; AI terminal management (Claude, Codex, Gemini)
(require 'vibes-layout)        ; VS Code-like layout management
(require 'vibes-keybindings)   ; All keybindings (load last as it references other modules)

;; Load custom file if it exists
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; Reset garbage collection after startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 2 1024 1024))  ; 2MB
            (message "VibEmacs loaded in %.2f seconds with %d garbage collections"
                     (float-time (time-subtract after-init-time before-init-time))
                     gcs-done)))

;; Automatically launch the VS Code layout on startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (when (display-graphic-p)
              (setup-vscode-layout))))

;; For terminal mode, setup layout after a short delay
(unless (display-graphic-p)
  (run-with-timer 0.5 nil 'setup-vscode-layout))

(provide 'init)
;;; init.el ends here