;;; vibe-magit.el --- Magit terminal module for Git management -*- lexical-binding: t; -*-

;;; Commentary:
;; Git terminal module using magit for comprehensive Git workflow management.
;; Provides a clean interface to Git functionality within the terminal system.

;;; Code:

(require 'magit nil t) ; Optional require - will install if needed

;; Terminal metadata
(defvar vibe-magit-terminal-name "Git"
  "Display name for this terminal.")

(defvar vibe-magit-terminal-key "g"
  "Keybinding suffix for this terminal.")

(defvar vibe-magit-terminal-color "#f14e32"
  "Color for this terminal's tab and indicators (Git red).")

(defvar vibe-magit-terminal-buffer nil
  "Buffer for Git terminal.")

(defun vibe-magit-ensure-package ()
  "Ensure magit package is installed."
  (unless (featurep 'magit)
    (condition-case err
        (when (y-or-n-p "Magit package not found. Install it? ")
          (if (fboundp 'package-install)
              (progn
                (message "Installing magit and dependencies...")
                (unless package-archive-contents
                  (package-refresh-contents))
                ;; Install transient first (magit dependency)
                (unless (package-installed-p 'transient)
                  (package-install 'transient))
                ;; Then install magit
                (unless (package-installed-p 'magit)
                  (package-install 'magit))
                (require 'magit)
                (message "Magit installed successfully"))
            (message "Please install magit package manually")))
      (error
       (message "Failed to install magit: %s. Try manual installation with M-x package-install RET magit"
               (error-message-string err)))))

  ;; Advice to force all magit buffers to open in main editor panel
  (when (featurep 'magit)
    (advice-add 'display-buffer :around #'vibe-magit-display-buffer-advice)))

(defun vibe-magit-display-buffer-advice (orig-fun buffer-or-name &rest args)
  "Advice to force magit buffers to display in main editor window."
  (let ((buffer (get-buffer buffer-or-name)))
    (if (and buffer
             (string-match "\\*magit\\|magit:" (buffer-name buffer))
             (boundp 'vibe-main-editor-window)
             (window-live-p vibe-main-editor-window))
        ;; Force magit buffers to display in main editor window
        (progn
          (set-window-buffer vibe-main-editor-window buffer)
          (select-window vibe-main-editor-window)
          ;; Enable tab-line and refresh tabs
          (with-current-buffer buffer
            (when (fboundp 'tab-line-mode)
              (tab-line-mode 1)))
          vibe-main-editor-window)
      ;; For non-magit buffers, use original behavior
      (apply orig-fun buffer-or-name args))))

(defun vibe-magit-with-main-context (magit-function)
  "Execute MAGIT-FUNCTION using the main editor window's buffer context."
  (if (and (boundp 'vibe-main-editor-window)
           (window-live-p vibe-main-editor-window))
      (let* ((main-buffer (window-buffer vibe-main-editor-window))
             (main-directory (with-current-buffer main-buffer default-directory)))
        ;; Temporarily switch to main editor context
        (with-current-buffer main-buffer
          (let ((default-directory main-directory))
            (message "Running Git command from: %s" main-directory)
            (funcall magit-function))))
    ;; Fallback to current context if main window not available
    (funcall magit-function)))

(defun vibe-magit-setup-terminal ()
  "Set up the Git terminal buffer."
  (condition-case err
      (progn
        (vibe-magit-ensure-package)
  (unless (and vibe-magit-terminal-buffer
               (buffer-live-p vibe-magit-terminal-buffer))
    (setq vibe-magit-terminal-buffer
          (get-buffer-create "*Git Manager*"))
    (with-current-buffer vibe-magit-terminal-buffer
      (erase-buffer)
      (insert (propertize "Git Manager" 'face '(:foreground "#f14e32" :weight bold)) "\n")
      (insert (propertize "═══════════" 'face '(:foreground "#444444")) "\n")

      ;; Git status line (line 3) - check main editor context
      (condition-case err
          (let* ((main-buffer (if (and (boundp 'vibe-main-editor-window)
                                       (window-live-p vibe-main-editor-window))
                                  (window-buffer vibe-main-editor-window)
                                (current-buffer)))
                 (main-directory (with-current-buffer main-buffer default-directory)))
            (with-current-buffer main-buffer
              (let ((default-directory main-directory))
                (if (and (featurep 'magit) (magit-toplevel))
                    (let* ((repo-name (file-name-nondirectory (directory-file-name (magit-toplevel))))
                           (branch (magit-get-current-branch))
                           (staged (magit-anything-staged-p))
                           (unstaged (magit-anything-unstaged-p)))
                      (insert (format "%s │ %s │ " repo-name (or branch "detached")))
                      (insert (cond
                               (staged (propertize "staged ●" 'face '(:foreground "#ffff00")))
                               (unstaged (propertize "modified ●" 'face '(:foreground "#ff6600")))
                               (t (propertize "clean ✓" 'face '(:foreground "#00ff00")))) "\n"))
                  ;; Not in a git repo - show current directory
                  (let ((current-dir (file-name-nondirectory (directory-file-name main-directory))))
                    (insert (format "%s │ " current-dir))
                    (insert (propertize "not a git repo" 'face '(:foreground "#ff6600")) "\n"))))))
        (error
         (insert (propertize "⚠ Git error" 'face '(:foreground "#ff6600")) "\n")))

      ;; Empty line (line 4)
      (insert "\n")

      ;; Quick actions (lines 5-6)
      (insert (propertize "[s]" 'face '(:foreground "#ffff00" :weight bold)) " Status     ")
      (insert (propertize "[l]" 'face '(:foreground "#ffff00" :weight bold)) " Log\n")
      (insert (propertize "[b]" 'face '(:foreground "#ffff00" :weight bold)) " Branches   ")
      (insert (propertize "[d]" 'face '(:foreground "#ffff00" :weight bold)) " Diff\n")

      ;; Empty line (line 7)
      (insert "\n")

      ;; Additional actions (line 8)
      (insert (propertize "[c]" 'face '(:foreground "#ffff00" :weight bold)) " Commit     ")
      (insert (propertize "[p]" 'face '(:foreground "#ffff00" :weight bold)) " Push\n")

      ;; Set up local keybindings - open magit in main editor panel
      (use-local-map (make-sparse-keymap))
      (local-set-key (kbd "s") (lambda () (interactive) (vibe-magit-with-main-context 'magit-status)))
      (local-set-key (kbd "l") (lambda () (interactive) (vibe-magit-with-main-context 'magit-log-current)))
      (local-set-key (kbd "b") (lambda () (interactive) (vibe-magit-with-main-context 'magit-branch-manager)))
      (local-set-key (kbd "d") (lambda () (interactive) (vibe-magit-with-main-context 'magit-diff-dwim)))
      (local-set-key (kbd "c") (lambda () (interactive) (vibe-magit-with-main-context 'magit-commit)))
      (local-set-key (kbd "p") (lambda () (interactive) (vibe-magit-with-main-context 'magit-push-current-to-upstream)))
      (local-set-key (kbd "r") (lambda () (interactive)
                                  (message "Refreshing git status from: %s" default-directory)
                                  (vibe-magit-setup-terminal)))
      (local-set-key (kbd "q") (lambda () (interactive) (message "Press C-c b [num] to switch terminals")))
      (local-set-key (kbd "RET") (lambda () (interactive) (magit-status)))

      (goto-char (point-min))
      (setq buffer-read-only t)))

    vibe-magit-terminal-buffer)
    (error
     (message "Error setting up git terminal: %s" (error-message-string err))
     (get-buffer-create "*Git Manager*"))))

(defun vibe-magit-switch-to-terminal ()
  "Switch to Git terminal."
  (interactive)
  (if (and vibe-magit-terminal-buffer (buffer-live-p vibe-magit-terminal-buffer))
      vibe-magit-terminal-buffer
    (vibe-magit-setup-terminal)))

(defun vibe-magit-is-ready-p ()
  "Check if Git terminal is ready."
  (and vibe-magit-terminal-buffer (buffer-live-p vibe-magit-terminal-buffer)))

;; Register this terminal (metadata for auto-discovery)
(defvar vibe-magit-terminal-config
  `(:name ,vibe-magit-terminal-name
    :key ,vibe-magit-terminal-key
    :color ,vibe-magit-terminal-color
    :setup-function vibe-magit-setup-terminal
    :switch-function vibe-magit-switch-to-terminal
    :ready-function vibe-magit-is-ready-p))

(provide 'vibe-magit)
;;; vibe-magit.el ends here