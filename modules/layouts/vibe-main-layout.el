;;; vibe-main-layout.el --- Default VS Code-style layout -*- lexical-binding: t; -*-

;;; Commentary:
;; Implements the original VibEmacs treemacs + panel + terminal layout and
;; registers it with the generic layout manager.

;;; Code:

(require 'cl-lib)
(require 'vibe-panel)
(require 'vibe-terminal)
(require 'vibe-animations)
(require 'vibe-lockdown)

(declare-function treemacs-get-local-window "treemacs")

(declare-function vibe-register-layout "vibe-layout" (symbol plist))

(defvar right-pane-window nil
  "The window containing the right pane panels.")

(defvar right-pane-header-buffer nil
  "Buffer for right pane tab header (kept for backwards compatibility).")

(defvar vibe-main-editor-window nil
  "Primary editor window in the VS Code layout.")

(defvar vibe-main-layout--initialized nil)

(defun vibe-main-layout--valid-editor-buffer-p (buffer)
  "Return non-nil when BUFFER is safe to display in the editor window."
  (and buffer
       (buffer-live-p buffer)
       (let ((name (buffer-name buffer)))
         (and name
              (not (string-match "\\*treemacs\\*\\|Treemacs" name))
               (not (string-match "^ " name))))
        (with-current-buffer buffer
          (or buffer-file-name
              (derived-mode-p 'text-mode 'prog-mode 'org-mode 'markdown-mode 'conf-mode)
              (memq major-mode '(fundamental-mode))))))

(defun vibe-main-layout--configure-right-pane ()
  "Create the right panel column and restore the active panel."
  (let ((main-window (selected-window)))
    (split-window-horizontally (- (floor (* (window-width main-window) 0.35))))
    (other-window 1)
    (when (and vibe-panel-header-buffer (buffer-live-p vibe-panel-header-buffer))
      (switch-to-buffer vibe-panel-header-buffer)
      (let ((header-window (selected-window)))
        (set-window-dedicated-p header-window t)
        (set-window-parameter header-window 'no-other-window t))
      (split-window-vertically 2)
      (other-window 1))
    (setq right-pane-window (selected-window))
    (setq vibe-panel-window (selected-window))
    (cond
     ((and vibe-panels vibe-active-panel)
      (vibe-switch-to-panel vibe-active-panel))
     (vibe-panels
      (vibe-switch-to-panel (caar vibe-panels))))
    (select-window main-window)))

(defun vibe-main-layout--configure-bottom-pane ()
  "Create the terminal row beneath the main editor and the animation window."
  (let* ((main-window (selected-window))
         (editor-height (max 10 (floor (* (window-height main-window) 0.75))))
         (bottom-window (split-window main-window editor-height 'below)))
    (select-window bottom-window)
    (let ((ascii-window (split-window bottom-window
                                      (floor (* (window-width bottom-window) 0.7))
                                      'right)))
      (select-window bottom-window)
      (vibe-initialize-terminal-system)
      (vibe-create-terminal-window bottom-window)
      (when (window-live-p ascii-window)
        (select-window ascii-window)
        (when (and (boundp 'matrix-rain-buffer)
                   matrix-rain-buffer
                   (buffer-live-p matrix-rain-buffer))
          (switch-to-buffer matrix-rain-buffer))
        (set-window-dedicated-p ascii-window t)
        (set-window-parameter ascii-window 'no-other-window t)))
    (select-window main-window)
    (setq vibe-main-editor-window main-window)))

(defun vibe-main-layout--maybe-current-buffer ()
  "Return the current buffer when it is suitable for the editor pane."
  (let ((buf (current-buffer)))
    (when (vibe-main-layout--valid-editor-buffer-p buf)
      buf)))

(defun vibe-main-layout--build (preferred-buffer)
  "Construct the full VS Code-style layout.
PREFERRED-BUFFER is restored in the main editor window when possible."
  (let* ((preferred-buffer (and preferred-buffer
                                (vibe-main-layout--valid-editor-buffer-p preferred-buffer)
                                preferred-buffer))
         (fallback-buffer (or preferred-buffer
                               (vibe-main-layout--maybe-current-buffer)
                               (get-buffer-create "*scratch*")))
         (treemacs-window (and (fboundp 'treemacs-get-local-window)
                               (treemacs-get-local-window))))
    (when (and treemacs-window (eq (selected-window) treemacs-window))
      ;; Prevent toggling treemacs off when rebuilding from its window.
      (switch-to-buffer fallback-buffer))
    (setq right-pane-window nil
          vibe-panel-window nil
          vibe-main-editor-window nil)
    (delete-other-windows)
    (treemacs)
    (other-window 1)
    (cond
     ((and preferred-buffer
           (vibe-main-layout--valid-editor-buffer-p preferred-buffer))
      (switch-to-buffer preferred-buffer))
     ((fboundp 'dashboard-refresh-buffer)
      (dashboard-refresh-buffer))
     (t
      (switch-to-buffer fallback-buffer)))
    (start-rainbow-animation)
    (start-animation-system)
    (vibe-main-layout--configure-right-pane)
    (vibe-main-layout--configure-bottom-pane)
    (when (window-live-p vibe-main-editor-window)
      (select-window vibe-main-editor-window))
    ;; Enable layout lockdown after setup is complete
    (run-with-timer 1.0 nil 'vibe-lockdown-enable)))

(defun vibe-main-layout--ensure-setup ()
  "Install global helpers used by the main layout."
  (unless vibe-main-layout--initialized
    (advice-add 'find-file :around #'vibe-find-file-advice)
    (global-set-key (kbd "C-c w s") #'vibe-swap-terminal-and-main)
    (setq vibe-terminal-swap-key "C-c w s")
    (global-set-key (kbd "C-x o") #'vibe-other-window)
    (global-set-key (kbd "M-o") #'ace-window)
    (setq vibe-main-layout--initialized t)))

(defun vibe-main-layout-reset ()
  "Reset the VS Code layout while preserving the active editor buffer."
  (interactive)
  (vibe-main-layout--ensure-setup)
  (let ((buffer (current-buffer)))
    (vibe-main-layout-apply buffer)))

(defun vibe-main-layout-apply (&optional preferred-buffer)
  "Apply the default VS Code-style layout.
When PREFERRED-BUFFER is non-nil, display it in the main editor pane."
  (interactive)
  (vibe-main-layout--ensure-setup)
  (unless (and vibe-panels (> (length vibe-panels) 0))
    (vibe-initialize-panel-system))
  (vibe-main-layout--build (or preferred-buffer
                               (vibe-main-layout--maybe-current-buffer))))

(defun vibe-swap-terminal-and-main ()
  "Swap positions of the vibe terminal panel and the main editor pane."
  (interactive)
  (condition-case err
      (let* ((main-window (or vibe-main-editor-window
                              ;; Try to find main editor window dynamically
                              (cl-find-if
                               (lambda (w)
                                 (let ((buf (window-buffer w)))
                                   (and buf
                                        (not (string-match "\\*treemacs\\*\\|\\*Treemacs" (buffer-name buf)))
                                        (not (string-match "\\*Panel Tabs\\*\\|\\*Terminal Tabs\\*\\|\\*Buffer Tabs\\*" (buffer-name buf)))
                                        (not (string-match "\\*Animation\\*\\|\\*Matrix Rain\\*" (buffer-name buf)))
                                        (not (eq (buffer-local-value 'major-mode buf) 'term-mode))
                                        (not (eq (buffer-local-value 'major-mode buf) 'ansi-term-mode)))))
                               (window-list))))
             (terminal-window (or vibe-terminal-window
                                  ;; Try to find terminal window dynamically
                                  (cl-find-if
                                   (lambda (w)
                                     (let ((buf (window-buffer w)))
                                       (and buf
                                            (or (eq (buffer-local-value 'major-mode buf) 'term-mode)
                                                (eq (buffer-local-value 'major-mode buf) 'ansi-term-mode)
                                                (string-match "\\*.*terminal\\*" (buffer-name buf))))))
                                   (window-list))))
             (lockdown-was-enabled (and (boundp 'vibe-lockdown-enabled) vibe-lockdown-enabled)))

        (cond
         ((not (window-live-p main-window))
          (message "Main editor window not available (from %s). Run `setup-vscode-layout` first."
                   (if (current-buffer) (buffer-name) "unknown")))
         ((not (window-live-p terminal-window))
          (message "Terminal window not available (from %s). Run `setup-vscode-layout` first."
                   (if (current-buffer) (buffer-name) "unknown")))
         (t
          ;; Temporarily disable lockdown to allow window changes
          (when lockdown-was-enabled
            (vibe-lockdown-disable))

          ;; Safely handle terminal header window - only delete if it's actually a terminal header
          (let ((header-window (and (boundp 'vibe-terminal-header-window)
                                    (window-live-p vibe-terminal-header-window)
                                    vibe-terminal-header-window)))
            (when header-window
              ;; Validate this is actually a terminal header before deleting
              (let ((header-buffer (window-buffer header-window)))
                (when (and header-buffer
                           (string-match "\\*Terminal Tabs\\*" (buffer-name header-buffer)))
                  (delete-window header-window)
                  (setq vibe-terminal-header-window nil)))))

          ;; Perform the window swap
          (window-swap-states main-window terminal-window)

          ;; Update window references
          (setq vibe-main-editor-window terminal-window)
          (setq vibe-terminal-window main-window)

          ;; Recreate terminal header in new location
          (when (and (boundp 'vibe-terminal-header-buffer)
                     (buffer-live-p vibe-terminal-header-buffer)
                     (window-live-p vibe-terminal-window))
            (with-selected-window vibe-terminal-window
              (condition-case header-err
                  (progn
                    (when (< (window-total-height) 4)
                      (enlarge-window (- 4 (window-total-height))))
                    (let ((new-header (split-window nil 2 'above)))
                      (set-window-buffer new-header vibe-terminal-header-buffer)
                      (set-window-dedicated-p new-header t)
                      (set-window-parameter new-header 'no-other-window t)
                      (setq vibe-terminal-header-window new-header)))
                (error
                 (message "Warning: Could not recreate terminal header: %s" header-err)))))

          ;; Update terminal header display
          (when (fboundp 'vibe-update-terminal-header)
            (vibe-update-terminal-header))

          ;; Re-enable lockdown if it was enabled
          (when lockdown-was-enabled
            (run-with-timer 0.5 nil 'vibe-lockdown-enable))

          ;; Return to main editor window regardless of where we started
          (when (window-live-p vibe-main-editor-window)
            (select-window vibe-main-editor-window))

          (message "Swapped terminal and main panels.")))

    (error
     (message "Panel swap failed: %s" (error-message-string err))))))

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

(defun vibe-main-layout--startup ()
  "Apply the main layout shortly after startup to allow packages to load."
  (vibe-main-layout--ensure-setup)
  (run-at-time "2 sec" nil #'vibe-main-layout-apply))

(vibe-register-layout
 'vibe-main-layout
 '(:title "VS Code Panels"
   :description "Treemacs left, panels right, terminals below."
   :apply vibe-main-layout-apply
   :startup vibe-main-layout--startup
   :init vibe-main-layout--ensure-setup))

(defalias 'setup-vscode-layout #'vibe-main-layout-apply)
(defalias 'reset-vscode-layout #'vibe-main-layout-reset)

(provide 'vibe-main-layout)
;;; vibe-main-layout.el ends here
