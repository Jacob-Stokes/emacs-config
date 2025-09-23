;; VS Code-like Emacs Development Environment

;; Add modules directory to load path
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

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

;; Load modules
(require 'vibe-theme)
(require 'vibe-animations)
(require 'vibe-panel)
(require 'vibe-terminal)
(require 'vibe-tabs)  ; Custom buffer tabs for main editor window
(require 'vibe-lockdown)  ; Layout protection system
(require 'vibe-layout)
(require 'vibe-welcome)

;; Disable startup messages
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)

;; UI Cleanup
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

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

;; Icons (required for treemacs and doom-modeline)
(use-package all-the-icons
  :if (display-graphic-p))

;; Treemacs - File explorer sidebar
(use-package treemacs
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-width 30
          treemacs-is-never-other-window nil  ; Allow in other-window cycling for layout setup
          treemacs-show-hidden-files t
          treemacs-git-mode 'simple
          treemacs-follow-mode t
          treemacs-filewatch-mode t
          treemacs-no-png-images t)  ; Use text icons in terminal
    ;; Suppress image resize warnings in terminal
    (advice-add 'treemacs--should-use-png-icons
                :override (lambda () nil))
    ;; Make treemacs background transparent too
    (set-face-background 'treemacs-root-face "unspecified-bg")
    (set-face-background 'treemacs-file-face "unspecified-bg")
    (set-face-background 'treemacs-directory-face "unspecified-bg")

    ;; Custom folder colors for vibe
    (set-face-foreground 'treemacs-directory-face "#66d9ef")  ; Cyan folders
    (set-face-foreground 'treemacs-root-face "#ff6b9d")       ; Pink root
    (set-face-foreground 'treemacs-file-face "#a6e22e")       ; Green files
    (set-face-foreground 'treemacs-git-modified-face "#fd971f") ; Orange modified
    (set-face-foreground 'treemacs-git-untracked-face "#75715e") ; Gray untracked

    ;; Custom dotfile/dotfolder highlighting
    (defun treemacs-custom-face-function (file)
      "Custom face function for dotfiles and dotfolders."
      (let ((filename (file-name-nondirectory file)))
        (cond
         ;; Dotfolders - purple
         ((and (file-directory-p file) (string-prefix-p "." filename))
          'treemacs-dotfolder-face)
         ;; Dotfiles - dimmed purple
         ((string-prefix-p "." filename)
          'treemacs-dotfile-face)
         ;; Regular folders - cyan (default)
         ((file-directory-p file)
          'treemacs-directory-face)
         ;; Regular files - green (default)
         (t 'treemacs-file-face))))

    ;; Define custom faces for dotfiles/dotfolders
    (defface treemacs-dotfolder-face
      '((t :foreground "#a29bfe" :weight bold))  ; Purple dotfolders
      "Face for dotfolders in treemacs.")

    (defface treemacs-dotfile-face
      '((t :foreground "#8e7cc3"))  ; Dimmed purple dotfiles
      "Face for dotfiles in treemacs.")

    ;; Apply custom face function (if treemacs supports it)
    (when (boundp 'treemacs-file-face-functions)
      (add-to-list 'treemacs-file-face-functions 'treemacs-custom-face-function))

    ;; Add C-c o to treemacs keymap for consistent navigation
    (define-key treemacs-mode-map (kbd "C-c o") 'other-window))
  :bind
  (:map global-map
        ("C-c T"     . treemacs-select-window)  ; Changed from C-c v t to avoid conflict
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
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (setq projectile-project-search-path '("~/"))
  (setq projectile-switch-project-action #'projectile-dired))

;; Company - Autocomplete
(use-package company
  :config
  (global-company-mode t)
  (setq company-idle-delay 0.2
        company-minimum-prefix-length 1))

;; Which-key - Shows available keybindings (DISABLED)
;; (use-package which-key
;;   :init (which-key-mode)
;;   :diminish which-key-mode
;;   :config
;;   (setq which-key-idle-delay 0.5))

;; VIBE FEATURES

;; Beacon - Flash cursor when switching windows (DISABLED)
;; (use-package beacon
;;   :config
;;   (beacon-mode 1)
;;   (setq beacon-color "#00ff00"
;;         beacon-blink-duration 0.3
;;         beacon-blink-delay 0.1
;;         beacon-size 20))

;; Rainbow-mode - Colorize color codes
(use-package rainbow-mode
  :hook (prog-mode . rainbow-mode))

;; Highlight indent guides - Visual indent lines
(use-package highlight-indent-guides
  :hook (prog-mode . highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'character
        highlight-indent-guides-character ?\│
        highlight-indent-guides-responsive 'top))

;; Nyan Mode - Nyan cat progress in modeline
(use-package nyan-mode
  :config
  (nyan-mode 1)
  (setq nyan-wavy-trail t
        nyan-animation-frame-interval 0.1))

;; Rainbow delimiters - Colorful parentheses
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Fireplace - Cozy fireplace
(use-package fireplace
  :commands fireplace)

;; Restart-emacs - Restart emacs from within emacs
(use-package restart-emacs
  :commands restart-emacs
  :config
  (setq restart-emacs-restore-frames t))

;; Imenu-list - Better than minimap for terminal (shows function list)
(use-package imenu-list
  :commands imenu-list-smart-toggle
  :config
  (setq imenu-list-focus-after-activation t
        imenu-list-size 30))

;; Ace-window - Jump to any window quickly
(use-package ace-window
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
        aw-background nil)
  (set-face-attribute 'aw-leading-char-face nil
                      :foreground "#ff6b9d"
                      :weight 'bold
                      :height 3.0))

(setq display-line-numbers-type t)  ; Use absolute line numbers (change to 'relative for vim-style)
(setq display-line-numbers-width-start t)  ; Reserve space for line numbers
(setq display-line-numbers-width 3)        ; Minimum width for line numbers

;; Add visual separation between line numbers and content
(set-face-attribute 'line-number nil
                    :background "unspecified-bg"
                    :foreground "#666666")
(set-face-attribute 'line-number-current-line nil
                    :background "unspecified-bg"
                    :foreground "#00ff00"
                    :weight 'bold)

;; Add a space after line numbers using fringe
(setq-default left-fringe-width 8)  ; Add small left fringe for spacing

;; Hook to enable line numbers only in programming and text modes
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'text-mode-hook 'display-line-numbers-mode)
(add-hook 'conf-mode-hook 'display-line-numbers-mode)

;; Disable line numbers in specific modes
(add-hook 'treemacs-mode-hook (lambda () (display-line-numbers-mode -1)))
(add-hook 'term-mode-hook (lambda ()
            (display-line-numbers-mode -1)
            (setq-local left-margin-width 2)
            (set-window-buffer (selected-window) (current-buffer))))
(add-hook 'ansi-term-mode-hook (lambda ()
            (display-line-numbers-mode -1)
            (setq-local left-margin-width 2)
            (set-window-buffer (selected-window) (current-buffer))))
(add-hook 'eshell-mode-hook (lambda ()
            (display-line-numbers-mode -1)
            (setq-local left-margin-width 2)
            (set-window-buffer (selected-window) (current-buffer))))
(add-hook 'shell-mode-hook (lambda ()
            (display-line-numbers-mode -1)
            (setq-local left-margin-width 2)
            (set-window-buffer (selected-window) (current-buffer))))

;; Highlight current line
(global-hl-line-mode 1)

;; Show matching parentheses
(show-paren-mode 1)

;; Better scrolling (conservative scrolling disabled for terminal performance)
(setq scroll-margin 3
      scroll-conservatively 0)

;; Auto-save and backup settings
(setq backup-directory-alist '(("." . "~/.emacs.d/backups"))
      auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-saves/" t)))

;; Create backup directories if they don't exist
(make-directory "~/.emacs.d/backups" t)
(make-directory "~/.emacs.d/auto-saves" t)

;; Set default directory
(setq default-directory "/root/")

;; Exclude certain files from recent files
(with-eval-after-load 'recentf
  (add-to-list 'recentf-exclude ".*/\\.treemacs-persist")
  (add-to-list 'recentf-exclude ".*/\\.treemacs-persist\\'")
  (add-to-list 'recentf-exclude "treemacs-persist"))

;; Animation variables are now in vibe-animations module

;; Animation functions are now in vibe-animations module
;; The following functions have been moved to modules/vibe-animations.el:
;; update-rainbow-logo, start-rainbow-animation, stop-rainbow-animation,
;; init-matrix-rain, create-matrix-rain-buffer, update-matrix-rain,
;; start-matrix-rain-animation, stop-matrix-rain-animation, init-aquarium,
;; update-aquarium, start-aquarium-animation, stop-aquarium-animation,
;; switch-animation-mode, start-animation-switcher, stop-animation-switcher

;; [Animation code removed - now in vibe-animations.el]

;; Enable tab-bar-mode for workspace tabs
(when (fboundp 'tab-bar-mode)
  (tab-bar-mode 1)
  (setq tab-bar-show 1)  ; Always show tab bar
  (setq tab-bar-new-tab-choice "*dashboard*"))  ; Default new tab

;; Configure tab-line-mode for buffer tabs (works better in terminal)
(require 'tab-line)

;; Customize tab-line appearance
(setq tab-line-new-button-show nil)  ; Hide new button
(setq tab-line-close-button-show t)  ; Show close button
(setq tab-line-separator " │ ")      ; Separator between tabs

;; Custom function to filter buffers for tab-line
(defun vibe-tab-line-buffer-filter ()
  "Filter buffers to show only real files in tab-line."
  (let ((filtered-buffers '()))
    (dolist (buf (buffer-list))
      (let ((name (buffer-name buf)))
        ;; Include file buffers and docker management buffers
        (when (and (or (buffer-file-name buf)  ; Has an associated file
                       (string-match "\\*docker-" name)  ; Docker management buffers
                       (string-match "\\*dashboard\\*" name))  ; Dashboard
                   (not (string-match "^ " name))     ; Not a hidden buffer
                   (not (string-match "\\*treemacs\\*\\|\\*Treemacs" name))
                   (not (string-match "terminal\\|Terminal" name))
                   (not (string-match "\\*scratch\\*\\|\\*Messages\\*" name)))
          (push buf filtered-buffers))))
    (reverse filtered-buffers)))

;; Set the buffer filter for tab-line
(setq tab-line-tabs-function 'vibe-tab-line-buffer-filter)

;; Custom faces for tab-line (terminal-friendly)
(set-face-attribute 'tab-line nil
                    :background "brightblack"  ; Light gray background like tab-bar
                    :foreground "black"
                    :height 1.0)
(set-face-attribute 'tab-line-tab nil
                    :background "brightblack"
                    :foreground "black")
(set-face-attribute 'tab-line-tab-current nil
                    :background "brightblack"
                    :foreground "green"
                    :weight 'bold
                    :underline t)
(set-face-attribute 'tab-line-tab-inactive nil
                    :background "brightblack"
                    :foreground "black")  ; Make inactive tabs visible
(set-face-attribute 'tab-line-highlight nil
                    :background "white"
                    :foreground "black")

;; Function to enable tab-line only in file buffers and docker buffers
(defun vibe-enable-tab-line-selectively ()
  "Enable tab-line-mode only in file buffers and docker management buffers."
  (if (and (or (not (string-match "^\\*" (buffer-name)))  ; Regular files
               (string-match "\\*docker-" (buffer-name))  ; Docker buffers
               (string-match "\\*dashboard\\*" (buffer-name)))  ; Dashboard
           (not (string-match "\\*treemacs\\*\\|\\*Treemacs" (buffer-name)))
           (not (eq major-mode 'term-mode))
           (not (eq major-mode 'ansi-term-mode)))
      (tab-line-mode 1)
    (tab-line-mode -1)))

;; Add hook to enable tab-line in appropriate buffers
(add-hook 'after-change-major-mode-hook 'vibe-enable-tab-line-selectively)
(add-hook 'find-file-hook 'vibe-enable-tab-line-selectively)

;; Browser-friendly keybindings for tab navigation
(global-set-key (kbd "C-c ]") 'tab-line-switch-to-next-tab)     ; Next tab
(global-set-key (kbd "C-c [") 'tab-line-switch-to-prev-tab)     ; Previous tab

;; Quick restart Emacs
(global-set-key (kbd "C-c r") 'restart-emacs)

;; Keybinding to manually switch animation
(global-set-key (kbd "C-c n") 'switch-animation-mode)

;; Enable automatic window resizing
(setq window-combination-resize t)

;; Zone.el - Built-in screensavers
(require 'zone)
(setq zone-timer (run-with-idle-timer 300 t 'zone))  ; Auto-zone after 5 minutes idle

;; Custom keybindings
;; Terminal keybindings now handled by vibe-terminal module
(global-set-key (kbd "C-c f") 'fireplace)  ; Easy fireplace access
(global-set-key (kbd "C-c i") 'imenu-list-smart-toggle)  ; Easy imenu toggle
(global-set-key (kbd "C-c v z") 'zone)  ; Instant screensaver/boss mode
(global-set-key (kbd "C-x C-b") 'dashboard-refresh-buffer)  ; Quick return to dashboard

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(all-the-icons beacon company dashboard docker doom-modeline
		   doom-themes fireplace highlight-indent-guides
		   imenu-list nyan-mode rainbow-delimiters
		   rainbow-mode restart-emacs treemacs-projectile
		   vterm)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:foreground "#E0E0E0"))))
 '(font-lock-comment-face ((t (:foreground "#75715e"))))
 '(font-lock-function-name-face ((t (:foreground "#fd971f"))))
 '(font-lock-keyword-face ((t (:foreground "#66d9ef"))))
 '(font-lock-string-face ((t (:foreground "#a6e22e"))))
 '(hl-line ((t (:background "unspecified" :underline t)))))
