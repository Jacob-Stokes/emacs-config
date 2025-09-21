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

;; VIBE FEATURES

;; Beacon - Flash cursor when switching windows
(use-package beacon
  :config
  (beacon-mode 1)
  (setq beacon-color "#00ff00"
        beacon-blink-duration 0.3
        beacon-blink-delay 0.1
        beacon-size 20))

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
    (let* ((logo-width 68)  ; Actual width of the VIBEMACS logo
           (window-width (window-width))
           (padding (max 0 (/ (- window-width logo-width) 2)))
           (pad-str (make-string padding ?\s)))
      (insert "\n")
      ;; Insert each line with calculated padding
      (insert pad-str "██╗   ██╗██╗██████╗ ███████╗███╗   ███╗ █████╗  ██████╗███████╗\n")
      (insert pad-str "██║   ██║██║██╔══██╗██╔════╝████╗ ████║██╔══██╗██╔════╝██╔════╝\n")
      (insert pad-str "██║   ██║██║██████╔╝█████╗  ██╔████╔██║███████║██║     ███████╗\n")
      (insert pad-str "╚██╗ ██╔╝██║██╔══██╗██╔══╝  ██║╚██╔╝██║██╔══██║██║     ╚════██║\n")
      (insert pad-str " ╚████╔╝ ██║██████╔╝███████╗██║ ╚═╝ ██║██║  ██║╚██████╗███████║\n")
      (insert pad-str "  ╚═══╝  ╚═╝╚═════╝ ╚══════╝╚═╝     ╚═╝╚═╝  ╚═╝ ╚═════╝╚══════╝\n")
      (insert "\n")))

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

;; Exclude certain files from recent files
(with-eval-after-load 'recentf
  (add-to-list 'recentf-exclude ".*/\\.treemacs-persist")
  (add-to-list 'recentf-exclude ".*/\\.treemacs-persist\\'")
  (add-to-list 'recentf-exclude "treemacs-persist"))

;; Rainbow animation variables - muted/pastel rainbow
(defvar rainbow-colors '("#ff6b9d" "#feca57" "#c7ecee" "#48dbfb" "#0abde3" "#667eea" "#a29bfe"))
(defvar rainbow-timer nil)
(defvar rainbow-index 0)

;; Function to update rainbow colors - works with both Welcome and Dashboard
(defun update-rainbow-logo ()
  "Update the VIBEMACS logo with rainbow effect"
  (let ((buffer (or (get-buffer "*dashboard*") (get-buffer "*Welcome*"))))
    (when buffer
      (with-current-buffer buffer
        (let ((was-readonly buffer-read-only))
          (when was-readonly (read-only-mode -1))
          (save-excursion
            (goto-char (point-min))
            ;; Find the logo start (look for the first ██)
            (when (search-forward "██╗" nil t)
              (beginning-of-line)
              (let ((colors (append (nthcdr rainbow-index rainbow-colors)
                                   (cl-subseq rainbow-colors 0 rainbow-index))))
                ;; Update each line with shifted colors
                (dotimes (i 6)
                  (let ((line-start (point))
                        (line-end (progn (end-of-line) (point))))
                    (add-text-properties line-start line-end
                                       `(face (:foreground ,(nth i colors) :weight bold)))
                    (forward-line 1))))))
          (when was-readonly (read-only-mode 1)))))
    (setq rainbow-index (mod (1+ rainbow-index) (length rainbow-colors)))))

;; Function to start rainbow animation
(defun start-rainbow-animation ()
  "Start the rainbow animation for VIBEMACS logo"
  (when rainbow-timer
    (cancel-timer rainbow-timer))
  (setq rainbow-timer (run-at-time "0 sec" 0.15 'update-rainbow-logo)))

;; Function to stop rainbow animation
(defun stop-rainbow-animation ()
  "Stop the rainbow animation"
  (when rainbow-timer
    (cancel-timer rainbow-timer)
    (setq rainbow-timer nil)))

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
      (insert "    C-c C-t     New terminal in split\n")
      (insert "    C-c c       Switch right pane to Claude terminal\n")
      (insert "    C-c g       Switch right pane to GPT terminal\n\n")
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

    ;; Restore right pane if terminals exist
    (when (and claude-terminal-buffer (buffer-live-p claude-terminal-buffer))
      (set-window-buffer (selected-window) claude-terminal-buffer))

    ;; Go back to main window
    (other-window -1)))

;; Function to setup VS Code-like layout with multiple terminals
(defun setup-vscode-layout ()
  "Setup VS Code-like three-panel layout"
  (interactive)
  ;; If already set up, just reset the layout
  (if (and claude-terminal-buffer (buffer-live-p claude-terminal-buffer))
      (reset-vscode-layout)
    ;; Otherwise do full setup
    (progn
      ;; Delete other windows
      (delete-other-windows)

      ;; Open treemacs on the left (only if not already visible)
      (unless (get-buffer-window "*treemacs*")
        (treemacs))

      ;; Create main editor window in center
      (other-window 1)
      (dashboard-refresh-buffer)
      (start-rainbow-animation)  ; Start the rainbow effect

      ;; First split vertically for right column (65% editor, 35% right pane)
      ;; Use negative value to make it proportional
      (split-window-horizontally (- (floor (* (window-width) 0.35))))
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
      (let* ((height (window-height))
             (editor-height (floor (* height 0.8))))  ; 80% for editor, 20% for terminal
        (split-window-vertically editor-height))

      (other-window 1)
      ;; Open standard terminal at bottom middle
      (ansi-term "/bin/bash" "bottom-terminal")

      ;; Focus back on top middle editor window
      (other-window -1))))

;; Setup layout on startup (with delay for package installation)
(add-hook 'emacs-startup-hook
          (lambda ()
            (when my/setup-vscode-layout-on-startup
              (run-at-time "2 sec" nil #'setup-vscode-layout))))

;; Custom keybindings
(global-set-key (kbd "C-c l") 'setup-vscode-layout)
(global-set-key (kbd "C-c t") (lambda () (interactive) (ansi-term "/bin/bash")))
(global-set-key (kbd "C-c f") 'fireplace)  ; Easy fireplace access
(global-set-key (kbd "C-c i") 'imenu-list-smart-toggle)  ; Easy imenu toggle
(global-set-key (kbd "M-o") 'ace-window)  ; Quick window jumping
(global-set-key (kbd "C-x C-b") 'dashboard-refresh-buffer)  ; Quick return to dashboard
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages nil))
