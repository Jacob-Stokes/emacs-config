;;; vibes-core.el --- Core settings and package management -*- lexical-binding: t; -*-

;; Copyright (C) 2024
;; Author: VibEmacs Configuration

;;; Commentary:
;; This module provides core Emacs settings, package management setup,
;; and basic configuration that other modules depend on.

;;; Code:

;; Package management setup
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
(package-initialize)

;; Bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

;; Define custom group for configuration
(defgroup my/vscode-emacs nil
  "VS Code-like layout helpers."
  :group 'convenience
  :prefix "my/")

(defcustom my/setup-vscode-layout-on-startup t
  "When non-nil, run `setup-vscode-layout` automatically after startup in GUI sessions."
  :type 'boolean
  :group 'my/vscode-emacs)

;; Disable startup messages
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)

;; UI Cleanup
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; Better scrolling
(setq scroll-margin 3)
(setq scroll-conservatively 9999)
(setq scroll-step 1)

;; Show matching parentheses
(show-paren-mode 1)

;; Auto-save and backup settings
(setq auto-save-default t)
(setq make-backup-files t)
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-saves/" t)))

;; Create backup directories if they don't exist
(make-directory "~/.emacs.d/backups" t)
(make-directory "~/.emacs.d/auto-saves" t)

;; Set default directory
(setq default-directory "~/")

;; Exclude certain files from recent files
(setq recentf-exclude '("/tmp/" "/ssh:" "/sudo:" "COMMIT_EDITMSG"))

;; Enable automatic window resizing
(setq window-combination-resize t)

;; Better defaults
(setq-default indent-tabs-mode nil)  ; Use spaces instead of tabs
(setq-default tab-width 4)           ; Set tab width to 4 spaces
(setq require-final-newline t)       ; Add newline at end of files
(setq visible-bell t)                ; Flash instead of beep
(global-auto-revert-mode 1)          ; Auto-refresh buffers when files change

(provide 'vibes-core)
;;; vibes-core.el ends here