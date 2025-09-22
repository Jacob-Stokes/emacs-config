;; VS Code-like Emacs Development Environment

;; Add modules directory to load path
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

;; Load modules
(require 'vibe-animations)
(require 'vibe-panel)
(require 'vibe-terminal)

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

;; Theme and appearance - using transparent background
;; Comment out doom-themes for transparency
;; (use-package doom-themes
;;   :config
;;   (load-theme 'doom-one t)
;;   (doom-themes-treemacs-config))

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

;; Dashboard - Better startup screen
(use-package dashboard
  :config
  (setq dashboard-banner-logo-title "[ Vibes + Emacs = Maximum Flow ]"
        dashboard-startup-banner 'logo
        dashboard-center-content t
        dashboard-items '((recents  . 5)
                          (projects . 5))
        dashboard-set-heading-icons nil
        dashboard-set-file-icons nil  ; Terminal friendly
        dashboard-week-agenda nil
        dashboard-filter-agenda-entry 'dashboard-no-filter-agenda)

  ;; Custom ASCII banner
  (setq dashboard-footer-messages '("Ready to vibe? 🌈"))

  ;; Override the banner with our VIBEMACS logo
  (defun dashboard-insert-custom-banner ()
    "Insert our custom VIBEMACS banner with rainbow effect"
    (insert "\n\n\n")
    (dashboard-insert-center
     "██╗   ██╗██╗██████╗ ███████╗███╗   ███╗ █████╗  ██████╗███████╗\n"
     "██║   ██║██║██╔══██╗██╔════╝████╗ ████║██╔══██╗██╔════╝██╔════╝\n"
     "██║   ██║██║██████╔╝█████╗  ██╔████╔██║███████║██║     ███████╗\n"
     "╚██╗ ██╔╝██║██╔══██╗██╔══╝  ██║╚██╔╝██║██╔══██║██║     ╚════██║\n"
     " ╚████╔╝ ██║██████╔╝███████╗██║ ╚═╝ ██║██║  ██║╚██████╗███████║\n"
     "  ╚═══╝  ╚═╝╚═════╝ ╚══════╝╚═╝     ╚═╝╚═╝  ╚═╝ ╚═════╝╚══════╝")
    (insert "\n"))

  (advice-add 'dashboard-insert-banner :override #'dashboard-insert-custom-banner)

  ;; Initialize dashboard
  (dashboard-setup-startup-hook)

  ;; Hook to start rainbow animation when dashboard loads
  (add-hook 'dashboard-after-initialize-hook 'start-rainbow-animation))

;; We'll use built-in ansi-term instead of vterm for better compatibility

;; Line numbers - only for programming/text modes
(setq display-line-numbers-type 'relative)

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

;; Function to create welcome buffer
(defun create-welcome-buffer ()
  "Create a welcome buffer with instructions"
  (let ((buf (get-buffer-create "*Welcome*")))
    (with-current-buffer buf
      (erase-buffer)
      (insert "\n")
      (insert "   ██╗   ██╗██╗██████╗ ███████╗███╗   ███╗ █████╗  ██████╗███████╗\n")
      (insert "   ██║   ██║██║██╔══██╗██╔════╝████╗ ████║██╔══██╗██╔════╝██╔════╝\n")
      (insert "   ██║   ██║██║██████╔╝█████╗  ██╔████╔██║███████║██║     ███████╗\n")
      (insert "   ╚██╗ ██╔╝██║██╔══██╗██╔══╝  ██║╚██╔╝██║██╔══██║██║     ╚════██║\n")
      (insert "    ╚████╔╝ ██║██████╔╝███████╗██║ ╚═╝ ██║██║  ██║╚██████╗███████║\n")
      (insert "     ╚═══╝  ╚═╝╚═════╝ ╚══════╝╚═╝     ╚═╝╚═╝  ╚═╝ ╚═════╝╚══════╝\n")
      (insert "\n")
      (insert "  " (propertize "[ Vibes + Emacs = Maximum Flow ]" 'face '(:foreground "cyan" :weight bold)))
      (insert "\n\n")
      (insert "  Quick Keys:\n")
      (insert "  ─────────────────────────────────────────────────────────────\n")
      (insert "    C-x t t     Open/Close file tree (Treemacs)\n")
      (insert "    C-c T       Quick jump to Treemacs window\n")
      (insert "    C-x C-f     Open file\n")
      (insert "    C-x b       Switch buffer\n")
      (insert "    C-x 3       Split vertically\n")
      (insert "    C-x 2       Split horizontally\n")
      (insert "    C-x o       Switch windows\n")
      (insert "    C-x 1       Maximize current window\n")
      (insert "    M-x vterm   Open terminal\n\n")
      (insert "  Project Management:\n")
      (insert "  ─────────────────────────────────────────────────────────────\n")
      (insert "    C-c p p     Switch project\n")
      (insert "    C-c p f     Find file in project\n")
      (insert "    C-c p s g   Search in project\n\n")
      (insert "  File Tree (Treemacs):\n")
      (insert "  ─────────────────────────────────────────────────────────────\n")
      (insert "    TAB         Expand/Collapse\n")
      (insert "    RET         Open file\n")
      (insert "    cf          Create file\n")
      (insert "    cd          Create directory\n")
      (insert "    d           Delete\n")
      (insert "    R           Rename\n\n")
      (insert "  Terminal:\n")
      (insert "  ─────────────────────────────────────────────────────────────\n")
      (insert "    Type 'claude' in terminal to start Claude\n")
      (insert "    C-c a m     Main terminal\n")
      (insert "    C-c a s     Shell terminal\n")
      (insert "    C-c a e     EShell terminal\n")
      (insert "    C-c a d     Docker container manager\n\n")
      (insert "  Vibe Features:\n")
      (insert "  ─────────────────────────────────────────────────────────────\n")
      (insert "    M-x fireplace        Start cozy fireplace\n")
      (insert "    M-x imenu-list       Toggle function outline\n")
      (insert "    🌈 Colors auto-preview in code\n")
      (insert "    🐱 Nyan cat in modeline shows position\n")
      (goto-char (point-min))  ; Move cursor to beginning of buffer
      (display-line-numbers-mode -1)  ; Disable line numbers in welcome buffer
      (read-only-mode 1))
    buf))

;; Enable tab-bar-mode for tabs
(when (fboundp 'tab-bar-mode)
  (tab-bar-mode 1)
  (setq tab-bar-show 1)  ; Always show tab bar
  (setq tab-bar-new-tab-choice "*dashboard*"))  ; Default new tab

;; Store the right pane window for easy reference
(defvar right-pane-window nil "The window containing the right pane terminals")
(defvar right-pane-header-buffer nil "Buffer for right pane tab header")

;; Function to create/update right pane header
;; Note: Right pane header now handled by vibe-update-panel-header in vibe-panel.el

;; Note: Terminal switching now handled by new panel system
;; Keybindings are automatically set up by vibe-panel.el

;; Keybinding to manually switch animation
(global-set-key (kbd "C-c n") 'switch-animation-mode)

;; Enable automatic window resizing
(setq window-combination-resize t)

;; Function to reset layout (for resizing)
(defun reset-vscode-layout ()
  "Reset the VS Code layout maintaining existing buffers"
  (interactive)
  ;; Store current buffers if they exist
  (let ((has-treemacs (get-buffer-window "*treemacs*"))
        (current-buffer (current-buffer)))
    ;; Start fresh
    (delete-other-windows)

    ;; Restore treemacs
    (when has-treemacs
      (treemacs))

    ;; If treemacs wasn't open but we want it
    (unless (get-buffer-window "*treemacs*")
      (treemacs))

    ;; Go to main window
    (other-window 1)

    ;; Restore the buffer that was active or show dashboard
    (if (and current-buffer (not (equal (buffer-name current-buffer) " *Treemacs-Scoped-Buffer-Perspective 1*")))
        (switch-to-buffer current-buffer)
      (dashboard-refresh-buffer))

    ;; Re-create the right pane with proper proportions
    (split-window-horizontally (- (floor (* (window-width) 0.35))))
    (other-window 1)

    ;; Restore right pane if panels exist
    (when (and vibe-panels vibe-active-panel)
      (vibe-switch-to-panel vibe-active-panel))

    ;; Go back to main window
    (other-window -1)))

;; Function to setup VS Code-like layout with multiple terminals
(defun setup-vscode-layout ()
  "Setup VS Code-like three-panel layout"
  (interactive)
  ;; Initialize panel system if not already done
  (unless (and vibe-panels (> (length vibe-panels) 0))
    (vibe-initialize-panel-system))

  ;; If already set up, just reset the layout
  (if (and (boundp 'right-pane-window) right-pane-window)
      (reset-vscode-layout)
    ;; Otherwise do full setup
    (progn
      ;; Delete other windows
      (delete-other-windows)

      ;; Open treemacs on the left (only if not already visible)
      (unless (get-buffer-window "*treemacs*")
        (treemacs))

      ;; Move to main editor window
      (other-window 1)
      (dashboard-refresh-buffer)
      (start-rainbow-animation)  ; Start the rainbow effect for logo
      (vibe-start-animations)    ; Start the matrix/aquarium animations

      ;; First split vertically for right column (65% editor, 35% right pane)
      ;; Use negative value to make it proportional
      (split-window-horizontally (- (floor (* (window-width) 0.35))))
      (other-window 1)

      ;; Use new panel system for right pane
      ;; Display panel header at top of right pane
      (when (and vibe-panel-header-buffer (buffer-live-p vibe-panel-header-buffer))
        (switch-to-buffer vibe-panel-header-buffer)
        (let ((header-window (selected-window)))
          (set-window-dedicated-p header-window t)
          (set-window-parameter header-window 'no-other-window t))
        (split-window-vertically 2)  ; Small window for header
        (other-window 1)

        ;; Store this window as the panel window
        (setq vibe-panel-window (selected-window))
        (setq right-pane-window (selected-window))  ; Keep old variable for compatibility

        ;; Switch to first panel
        (when vibe-panels
          (vibe-switch-to-panel (caar vibe-panels))))

      ;; Go back to middle window - need to go through treemacs
      (other-window 1)  ; This should go to treemacs
      (other-window 1)  ; This should go to middle window

      ;; Now split the middle window for bottom terminal
      (let* ((height (window-height))
             (editor-height (floor (* height 0.75))))  ; 75% for editor, 25% for terminal
        (split-window-vertically editor-height))

      (other-window 1)

      ;; Split bottom area horizontally - left for terminal, right for ASCII face
      (split-window-horizontally (floor (* (window-width) 0.7)))

      ;; Initialize and create terminal window
      (vibe-initialize-terminal-system)
      (vibe-create-terminal-window (selected-window))

      ;; Move to bottom right for animation display
      (other-window 1)
      ;; Animation already started by vibe-start-animations above
      (when (and matrix-rain-buffer (buffer-live-p matrix-rain-buffer))
        (switch-to-buffer matrix-rain-buffer))
      (set-window-dedicated-p (selected-window) t)
      (set-window-parameter (selected-window) 'no-other-window t)

      ;; Focus back on top middle editor window
      (other-window -1))))

;; Initialize panel system and setup layout on startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (when my/setup-vscode-layout-on-startup
              (vibe-initialize-panel-system)
              (run-at-time "2 sec" nil #'setup-vscode-layout))))

;; Zone.el - Built-in screensavers
(require 'zone)
(setq zone-timer (run-with-idle-timer 300 t 'zone))  ; Auto-zone after 5 minutes idle

;; Custom keybindings
(global-set-key (kbd "C-c l") 'setup-vscode-layout)
;; Terminal keybindings now handled by vibe-terminal module
(global-set-key (kbd "C-c f") 'fireplace)  ; Easy fireplace access
(global-set-key (kbd "C-c i") 'imenu-list-smart-toggle)  ; Easy imenu toggle
(global-set-key (kbd "C-c v z") 'zone)  ; Instant screensaver/boss mode
;; Smart window cycling that skips Treemacs
(defun vibe-other-window ()
  "Cycle through windows, skipping Treemacs."
  (interactive)
  (let ((start-window (selected-window))
        (tried-windows 0)
        (max-tries 10))
    (other-window 1)
    (while (and (< tried-windows max-tries)
                (string-match "\\*treemacs\\*" (buffer-name)))
      (other-window 1)
      (setq tried-windows (1+ tried-windows))
      (when (eq (selected-window) start-window)
        (break)))))

(global-set-key (kbd "C-c o") 'vibe-other-window)  ; Cycle through main windows
(global-set-key (kbd "M-o") 'ace-window)  ; Quick window jumping
(global-set-key (kbd "C-x C-b") 'dashboard-refresh-buffer)  ; Quick return to dashboard
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages nil))
