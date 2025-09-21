;;; vibes-packages.el --- Third-party package configurations -*- lexical-binding: t; -*-

;;; Commentary:
;; Configuration for all third-party packages.

;;; Code:

;; Icons (required for treemacs and doom-modeline)
(use-package all-the-icons
  :if (display-graphic-p))

;; Treemacs - File explorer sidebar
(use-package treemacs
  :defer t
  :config
  (progn
    (setq treemacs-width 30
          treemacs-is-never-other-window t
          treemacs-follow-after-init t
          treemacs-expand-added-projects t
          treemacs-display-current-project-exclusively t)
    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)
    (treemacs-git-mode 'simple))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t d"   . treemacs-select-directory)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

;; Treemacs projectile integration
(use-package treemacs-projectile
  :after (treemacs projectile))

;; Projectile - Project management
(use-package projectile
  :init
  (projectile-mode +1)
  :config
  (setq projectile-completion-system 'default)
  :bind-keymap
  ("C-c p" . projectile-command-map))

;; Company - Autocomplete
(use-package company
  :init (global-company-mode)
  :config
  (setq company-minimum-prefix-length 2
        company-idle-delay 0.1))

;; Which-key - Shows available keybindings
(use-package which-key
  :init (which-key-mode)
  :config
  (setq which-key-idle-delay 0.5))

;; Ace-window - Jump to any window quickly
(use-package ace-window
  :bind ("M-o" . ace-window)
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
        aw-scope 'frame
        aw-background t))

(provide 'vibes-packages)
;;; vibes-packages.el ends here
