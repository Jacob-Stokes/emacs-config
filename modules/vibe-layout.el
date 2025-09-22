;;; vibe-layout.el --- VS Code style layout orchestration -*- lexical-binding: t; -*-

;;; Commentary:
;; Centralizes window layout helpers for VibEmacs so init.el can stay lean.

;;; Code:

(require 'cl-lib)
(require 'vibe-panel)
(require 'vibe-terminal)
(require 'vibe-animations)

(defgroup my/vscode-emacs nil
  "VS Code-like layout helpers for VibEmacs."
  :group 'convenience
  :prefix "my/")

(defcustom my/setup-vscode-layout-on-startup t
  "When non-nil, run `setup-vscode-layout` automatically after startup."
  :type 'boolean
  :group 'my/vscode-emacs)

(defvar right-pane-window nil
  "The window containing the right pane terminals.")

(defvar right-pane-header-buffer nil
  "Buffer for right pane tab header (kept for backwards compatibility).")

(defvar vibe-main-editor-window nil
  "Primary editor window in the VS Code layout.")

(defun reset-vscode-layout ()
  "Reset the VS Code layout maintaining existing buffers."
  (interactive)
  (let ((has-treemacs (get-buffer-window "*treemacs*"))
        (current-buffer (current-buffer)))
    (delete-other-windows)
    (when has-treemacs
      (treemacs))
    (unless (get-buffer-window "*treemacs*")
      (treemacs))
    (other-window 1)
    (if (and current-buffer
             (not (equal (buffer-name current-buffer)
                         " *Treemacs-Scoped-Buffer-Perspective 1*")))
        (switch-to-buffer current-buffer)
      (if (fboundp 'dashboard-refresh-buffer)
          (dashboard-refresh-buffer)
        (switch-to-buffer "*scratch*")))
    (split-window-horizontally (- (floor (* (window-width) 0.35))))
    (other-window 1)
    (when (and vibe-panels vibe-active-panel)
      (vibe-switch-to-panel vibe-active-panel))
    (other-window -1)
    (setq vibe-main-editor-window (selected-window))))

(defun setup-vscode-layout ()
  "Setup VS Code-like three-panel layout."
  (interactive)
  (unless (and vibe-panels (> (length vibe-panels) 0))
    (vibe-initialize-panel-system))
  (if (and (boundp 'right-pane-window) right-pane-window)
      (reset-vscode-layout)
    (progn
      (delete-other-windows)
      (unless (get-buffer-window "*treemacs*")
        (treemacs))
      (other-window 1)
      (if (fboundp 'dashboard-refresh-buffer)
          (dashboard-refresh-buffer)
        (switch-to-buffer "*scratch*"))
      (start-rainbow-animation)
      (start-animation-system)
      (split-window-horizontally (- (floor (* (window-width) 0.35))))
      (other-window 1)
      (when (and vibe-panel-header-buffer (buffer-live-p vibe-panel-header-buffer))
        (switch-to-buffer vibe-panel-header-buffer)
        (let ((header-window (selected-window)))
          (set-window-dedicated-p header-window t)
          (set-window-parameter header-window 'no-other-window t))
        (split-window-vertically 2)
        (other-window 1)
        (setq vibe-panel-window (selected-window))
        (setq right-pane-window (selected-window))
        (when vibe-panels
          (vibe-switch-to-panel (caar vibe-panels))))
      (other-window 1)
      (other-window 1)
      (setq vibe-main-editor-window (selected-window))
      (let* ((height (window-height))
             (editor-height (floor (* height 0.75))))
        (split-window-vertically editor-height))
      (other-window 1)
      (split-window-horizontally (floor (* (window-width) 0.7)))
      (vibe-initialize-terminal-system)
      (vibe-create-terminal-window (selected-window))
      (other-window 1)
      (when (and matrix-rain-buffer (buffer-live-p matrix-rain-buffer))
        (switch-to-buffer matrix-rain-buffer))
      (set-window-dedicated-p (selected-window) t)
      (set-window-parameter (selected-window) 'no-other-window t)
      (other-window -1)
      (when (window-live-p vibe-main-editor-window)
        (select-window vibe-main-editor-window)))))

(defun vibe-swap-terminal-and-main ()
  "Swap positions of the vibe terminal panel and the main editor pane."
  (interactive)
  (let ((main-window vibe-main-editor-window)
        (terminal-window vibe-terminal-window))
    (cond
     ((not (window-live-p main-window))
      (message "Main editor window not available. Run `setup-vscode-layout` first."))
     ((not (window-live-p terminal-window))
      (message "Terminal window not available. Run `setup-vscode-layout` first."))
     (t
      (let ((header-window (and (boundp 'vibe-terminal-header-window)
                                (window-live-p vibe-terminal-header-window)
                                vibe-terminal-header-window)))
        (when header-window
          (delete-window header-window)
          (setq vibe-terminal-header-window nil))
        (window-swap-states main-window terminal-window)
        (setq vibe-main-editor-window terminal-window)
        (setq vibe-terminal-window main-window)
        (when (and (buffer-live-p vibe-terminal-header-buffer)
                   (window-live-p vibe-terminal-window))
          (with-selected-window vibe-terminal-window
            (when (< (window-total-height (selected-window)) 4)
              (enlarge-window (- 4 (window-total-height (selected-window)))))
            (let ((new-header (split-window (selected-window) 2 'above)))
              (set-window-buffer new-header vibe-terminal-header-buffer)
              (set-window-dedicated-p new-header t)
              (set-window-parameter new-header 'no-other-window t)
              (setq vibe-terminal-header-window new-header))
            (select-window vibe-terminal-window)))
        (when (fboundp 'vibe-update-terminal-header)
          (vibe-update-terminal-header))
        (when (window-live-p vibe-main-editor-window)
          (select-window vibe-main-editor-window))
        (message "Swapped terminal and main panels."))))))

(defun vibe-other-window (&optional arg)
  "Cycle through windows, skipping Treemacs and animation panels.
Optional ARG behaves like `other-window'."
  (interactive "p")
  (let ((count (or arg 1)))
    (dotimes (_ (abs count))
      (let ((start-window (selected-window))
            (tries 0)
            (max-tries 10))
        (other-window (if (< count 0) -1 1))
        (while (and (< tries max-tries)
                    (let ((buf (buffer-name)))
                      (or (string-match "\\*treemacs\\*\\|\\*Treemacs" buf)
                          (string-match "\\*Matrix Rain\\*\\|\\*Animation\\*" buf)
                          (string-match "\\*Panel Tabs\\*\\|\\*Terminal Tabs\\*\\|\\*Buffer Tabs\\*" buf))))
          (other-window (if (< count 0) -1 1))
          (setq tries (1+ tries))
          (when (eq (selected-window) start-window)
            (setq tries max-tries)))))))

(defun vibe-find-file-advice (orig-fun &rest args)
  "Open files in the main editor window, regardless of current window.
ORIG-FUN and ARGS mirror `find-file'."
  (let ((current-window (selected-window))
        (current-buffer-name (buffer-name))
        (mode major-mode))
    (if (or (string-match "\\*treemacs\\*\\|Treemacs" current-buffer-name)
            (memq mode '(term-mode ansi-term-mode eshell-mode shell-mode))
            (string-match "terminal\\|docker-\\|Matrix Rain\\|Animation\\|Panel Tabs\\|Terminal Tabs"
                          current-buffer-name)
            (string-match "^\\*" current-buffer-name))
        (let ((main-window nil))
          (dolist (win (window-list))
            (with-selected-window win
              (unless (or (string-match "\\*treemacs\\*\\|Treemacs" (buffer-name))
                          (memq major-mode '(term-mode ansi-term-mode eshell-mode shell-mode))
                          (string-match "terminal\\|docker-\\|Matrix Rain\\|Animation\\|Panel Tabs\\|Terminal Tabs"
                                         (buffer-name))
                          (and (string-match "^\\*" (buffer-name))
                               (not (string-match "\\*dashboard\\*\\|\\*scratch\\*" (buffer-name)))))
                (setq main-window win))))
          (if main-window
              (progn
                (select-window main-window)
                (apply orig-fun args)
                (message "Opened file in main editor window"))
            (apply orig-fun args)))
      (apply orig-fun args))))

;;;###autoload
(defun vibe-layout-initialize ()
  "Initialize layout helpers and keybindings."
  (advice-add 'find-file :around #'vibe-find-file-advice)
  (global-set-key (kbd "C-c l") #'setup-vscode-layout)
  (global-set-key (kbd "C-c w s") #'vibe-swap-terminal-and-main)
  (setq vibe-terminal-swap-key "C-c w s")
  (global-set-key (kbd "C-x o") #'vibe-other-window)
  (global-set-key (kbd "M-o") #'ace-window)
  (add-hook 'emacs-startup-hook
            (lambda ()
              (when my/setup-vscode-layout-on-startup
                (vibe-initialize-panel-system)
                (run-at-time "2 sec" nil #'setup-vscode-layout)))))

(vibe-layout-initialize)

(provide 'vibe-layout)
;;; vibe-layout.el ends here
