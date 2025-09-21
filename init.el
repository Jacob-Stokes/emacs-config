;; VS Code-like Emacs Development Environment

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
 '(default ((t (:foreground "#E0E0E0"))))
 '(font-lock-comment-face ((t (:foreground "#75715e"))))
 '(font-lock-keyword-face ((t (:foreground "#66d9ef"))))
 '(font-lock-string-face ((t (:foreground "#a6e22e"))))
 '(font-lock-function-name-face ((t (:foreground "#fd971f"))))
 '(hl-line ((t (:background "unspecified" :underline t)))))

;; Modeline (status bar)
(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-height 25))

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
          treemacs-is-never-other-window nil
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
    (set-face-background 'treemacs-directory-face "unspecified-bg"))
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

;; Which-key - Shows available keybindings
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.5))

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

;; Better scrolling
(setq scroll-margin 3
      scroll-conservatively 10000)

;; Auto-save and backup settings
(setq backup-directory-alist '(("." . "~/.emacs.d/backups"))
      auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-saves/" t)))

;; Create backup directories if they don't exist
(make-directory "~/.emacs.d/backups" t)
(make-directory "~/.emacs.d/auto-saves" t)

;; Set default directory
(setq default-directory "/root/")

;; Function to create welcome buffer
(defun create-welcome-buffer ()
  "Create a welcome buffer with instructions"
  (let ((buf (get-buffer-create "*Welcome*")))
    (with-current-buffer buf
      (erase-buffer)
      (insert "\n")
      (insert "  ╔══════════════════════════════════════════════════════════╗\n")
      (insert "  ║           VS Code-like Emacs Development Environment!     ║\n")
      (insert "  ╚══════════════════════════════════════════════════════════╝\n\n")
      (insert "  Quick Keys:\n")
      (insert "  ─────────────────────────────────────────────────────────────\n")
      (insert "    C-x t t     Open/Close file tree (Treemacs)\n")
      (insert "    C-x C-f     Open file\n")
      (insert "    C-x b       Switch buffer\n")
      (insert "    C-x 3       Split vertically\n")
      (insert "    C-x 2       Split horizontally\n")
      (insert "    C-x o       Switch windows\n")
      (insert "    C-x 1       Maximize current window\n")
      (insert "    M-x vterm   Open terminal\n\n")
      (insert "  Tab Management:\n")
      (insert "  ─────────────────────────────────────────────────────────────\n")
      (insert "    C-<tab>     Next tab\n")
      (insert "    C-S-<tab>   Previous tab\n")
      (insert "    C-t         New tab\n")
      (insert "    C-w         Close tab\n\n")
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
      (insert "    C-c C-t     New terminal in split\n")
      (insert "    C-c c       Switch right pane to Claude terminal\n")
      (insert "    C-c g       Switch right pane to GPT terminal\n")
      (goto-char (point-min))  ; Move cursor to beginning of buffer
      (display-line-numbers-mode -1)  ; Disable line numbers in welcome buffer
      (read-only-mode 1))
    buf))

;; Enable tab-bar-mode for tabs in main editor
(when (fboundp 'tab-bar-mode)
  (tab-bar-mode 1)
  (setq tab-bar-show 1)  ; Always show tab bar
  (setq tab-bar-new-tab-choice "*Welcome*"  ; Default new tab
        tab-bar-close-button-show t
        tab-bar-new-button-show t
        tab-bar-format '(tab-bar-format-tabs
                        tab-bar-separator
                        tab-bar-format-align-right
                        tab-bar-format-global))
  ;; Better tab appearance
  (custom-set-faces
   '(tab-bar ((t (:background "unspecified-bg" :foreground "#E0E0E0"))))
   '(tab-bar-tab ((t (:background "#3a3a3a" :foreground "#E0E0E0" :box (:line-width 1 :style released-button)))))
   '(tab-bar-tab-inactive ((t (:background "unspecified-bg" :foreground "#808080"))))))

;; Store the right pane window for easy reference
(defvar right-pane-window nil "The window containing the right pane terminals")
(defvar claude-terminal-buffer nil "Buffer for Claude terminal")
(defvar gpt-terminal-buffer nil "Buffer for GPT terminal")
(defvar right-pane-header-buffer nil "Buffer for right pane tab header")
(defvar right-pane-active-terminal "claude" "Currently active terminal in right pane")

;; Function to create/update right pane header
(defun update-right-pane-header ()
  "Update the right pane header to show active tab"
  (when (and right-pane-header-buffer (buffer-live-p right-pane-header-buffer))
    (with-current-buffer right-pane-header-buffer
      (read-only-mode -1)
      (erase-buffer)
      (insert " ")
      ;; Claude tab
      (if (string= right-pane-active-terminal "claude")
          (insert (propertize "[Claude]" 'face '(:foreground "cyan" :weight bold :underline t)))
        (insert (propertize " Claude " 'face '(:foreground "gray"))))
      (insert " ")
      ;; GPT/Codex tab
      (if (string= right-pane-active-terminal "gpt")
          (insert (propertize "[Codex]" 'face '(:foreground "green" :weight bold :underline t)))
        (insert (propertize " Codex " 'face '(:foreground "gray"))))
      (insert "  ")
      (insert (propertize "(C-c c/g)" 'face '(:foreground "dark gray" :height 0.9)))
      (read-only-mode 1))))

;; Function to switch between terminals in right pane
(defun switch-right-terminal (terminal-type)
  "Switch the right pane to show a specific terminal buffer"
  (interactive)
  (let ((current-window (selected-window)))
    (if (and right-pane-window (window-live-p right-pane-window))
        (progn
          ;; Switch to the right window
          (select-window right-pane-window)
          ;; Switch to the appropriate buffer
          (cond
           ((equal terminal-type "claude")
            (if (and claude-terminal-buffer (buffer-live-p claude-terminal-buffer))
                (progn
                  (switch-to-buffer claude-terminal-buffer)
                  (setq right-pane-active-terminal "claude")
                  (update-right-pane-header)
                  (message "Switched to Claude terminal (C-c g for Codex)"))
              (message "Claude terminal not found")))
           ((equal terminal-type "gpt")
            (if (and gpt-terminal-buffer (buffer-live-p gpt-terminal-buffer))
                (progn
                  (switch-to-buffer gpt-terminal-buffer)
                  (setq right-pane-active-terminal "gpt")
                  (update-right-pane-header)
                  (message "Switched to Codex terminal (C-c c for Claude)"))
              (message "Codex terminal not found"))))
          ;; Stay in the right pane terminal window (don't return)
          )
      (message "Right pane not found. Run setup-vscode-layout first."))))

;; Create keybindings for quick terminal switching
(global-set-key (kbd "C-c c") (lambda () (interactive) (switch-right-terminal "claude")))
(global-set-key (kbd "C-c g") (lambda () (interactive) (switch-right-terminal "gpt")))

;; Function to setup VS Code-like layout with multiple terminals
(defun setup-vscode-layout ()
  "Setup VS Code-like three-panel layout"
  (interactive)
  ;; Delete other windows
  (delete-other-windows)

  ;; Open treemacs on the left
  (treemacs)

  ;; Create main editor window in center
  (other-window 1)
  (switch-to-buffer (create-welcome-buffer))

  ;; Calculate window widths for 60/40 split
  (let* ((total-width (- (window-width) treemacs-width))
         (editor-width (floor (* total-width 0.6)))
         (right-width (- total-width editor-width)))
    ;; First split vertically for right column
    (split-window-horizontally (- editor-width)))
  (other-window 1)

  ;; Create header buffer for right pane
  (setq right-pane-header-buffer (get-buffer-create "*Right Pane Tabs*"))
  (with-current-buffer right-pane-header-buffer
    (display-line-numbers-mode -1)
    (setq mode-line-format nil)  ; Hide modeline for header
    (setq window-size-fixed t))  ; Prevent resizing

  ;; Display header at top of right pane
  (switch-to-buffer right-pane-header-buffer)
  (let ((header-window (selected-window)))
    (set-window-dedicated-p header-window t)  ; Dedicate window to this buffer
    (set-window-parameter header-window 'no-other-window t))  ; Skip in window cycling
  (split-window-vertically 2)  ; Small window for header
  (other-window 1)

  ;; Store this window as the right pane
  (setq right-pane-window (selected-window))

  ;; Create GPT terminal first
  (setq gpt-terminal-buffer (ansi-term "/bin/bash" "gpt-terminal"))
  (with-current-buffer gpt-terminal-buffer
    (sleep-for 0.2)
    ;; You can replace this with actual GPT/Codex command if you have one
    (term-send-string (get-buffer-process (current-buffer)) "echo 'GPT Terminal Ready'\n")
    (term-send-string (get-buffer-process (current-buffer)) "echo 'Install your GPT CLI tool or type: openai api completions.create ...'\n")
    (term-send-string (get-buffer-process (current-buffer)) "echo 'Press C-c c for Claude, C-c g for GPT'\n"))

  ;; Now create claude terminal
  (setq claude-terminal-buffer (ansi-term "/bin/bash" "claude-terminal"))
  (with-current-buffer claude-terminal-buffer
    (sleep-for 0.2)
    (term-send-string (get-buffer-process (current-buffer)) "claude\n"))

  ;; Make sure claude is displayed by default
  (set-window-buffer right-pane-window claude-terminal-buffer)
  (setq right-pane-active-terminal "claude")
  (update-right-pane-header)

  ;; Go back to middle window - need to go through treemacs
  (other-window 1)  ; This should go to treemacs
  (other-window 1)  ; This should go to middle window

  ;; Now split the middle window for bottom terminal
  (let ((height (window-height)))
    (split-window-vertically (- height (/ height 3))))  ; Split at 2/3 point

  (other-window 1)
  ;; Open standard terminal at bottom middle
  (ansi-term "/bin/bash" "bottom-terminal")

  ;; Focus back on top middle editor window
  (other-window -1))

;; Setup layout on startup (with delay for package installation)
(add-hook 'emacs-startup-hook
          (lambda ()
            (when my/setup-vscode-layout-on-startup
              (run-at-time "2 sec" nil #'setup-vscode-layout))))

;; Custom keybindings
(global-set-key (kbd "C-c l") 'setup-vscode-layout)
(global-set-key (kbd "C-c t") (lambda () (interactive) (ansi-term "/bin/bash")))

;; Tab management keybindings
(global-set-key (kbd "C-<tab>") 'tab-next)
(global-set-key (kbd "C-S-<tab>") 'tab-previous)
(global-set-key (kbd "C-t") 'tab-new)
(global-set-key (kbd "C-w") 'tab-close)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(which-key company projectile treemacs-projectile treemacs all-the-icons doom-modeline)))