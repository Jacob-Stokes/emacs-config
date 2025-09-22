;;; vibe-tabs.el --- Buffer tab system for main editor window -*- lexical-binding: t; -*-

;;; Commentary:
;; Provides VS Code-style buffer tabs for the main editor window.
;; Shows all open file buffers as clickable tabs.

;;; Code:

(require 'cl-lib)

;; Configuration variables
(defvar vibe-tabs-height 2
  "Height of the tab bar in lines.")

(defvar vibe-tabs-max-width 25
  "Maximum width of each tab.")

(defvar vibe-tabs-separator " │ "
  "Separator between tabs.")

(defvar vibe-tabs-buffer nil
  "Buffer displaying the tabs.")

(defvar vibe-tabs-window nil
  "Window displaying the tabs.")

(defvar vibe-tabs-update-timer nil
  "Timer for updating tabs.")

(defface vibe-tabs-current-tab
  '((t :foreground "#00ff00" :weight bold :underline t))
  "Face for current tab.")

(defface vibe-tabs-other-tab
  '((t :foreground "#888888"))
  "Face for other tabs.")

(defface vibe-tabs-modified-tab
  '((t :foreground "#fd971f"))
  "Face for modified buffer tabs.")

(defface vibe-tabs-special-tab
  '((t :foreground "#66d9ef"))
  "Face for special buffer tabs.")

(defun vibe-tabs-get-file-buffers ()
  "Get list of buffers to show in tabs."
  (cl-remove-if
   (lambda (buf)
     (let ((name (buffer-name buf)))
       (or (string-match "^\\*" name)  ; Skip special buffers
           (string-match "^ " name)     ; Skip internal buffers
           (string-match "\\*treemacs\\*\\|\\*Treemacs" name)
           (string-match "\\*Matrix Rain\\*\\|\\*Animation\\*" name)
           (string-match "\\*Panel Tabs\\*\\|\\*Terminal Tabs\\*\\|\\*Buffer Tabs\\*" name)
           (eq (buffer-local-value 'major-mode buf) 'term-mode)
           (eq (buffer-local-value 'major-mode buf) 'ansi-term-mode))))
   (buffer-list)))

(defun vibe-tabs-truncate-name (name max-width)
  "Truncate NAME to MAX-WIDTH with ellipsis if needed."
  (if (<= (length name) max-width)
      name
    (concat (substring name 0 (- max-width 3)) "...")))

(defun vibe-tabs-create-tab-line ()
  "Create the tab line string."
  (let* ((buffers (vibe-tabs-get-file-buffers))
         (current-buf (current-buffer))
         (tabs '()))

    ;; Always include dashboard if it exists
    (when (get-buffer "*dashboard*")
      (unless (member (get-buffer "*dashboard*") buffers)
        (setq buffers (cons (get-buffer "*dashboard*") buffers))))

    (dolist (buf buffers)
      (let* ((name (buffer-name buf))
             (modified (and (buffer-modified-p buf)
                           (not (string-match "\\*dashboard\\*" name))))
             (current (eq buf current-buf))
             (display-name (vibe-tabs-truncate-name name vibe-tabs-max-width))
             (face (cond
                    (current 'vibe-tabs-current-tab)
                    (modified 'vibe-tabs-modified-tab)
                    ((string-match "\\*.*\\*" name) 'vibe-tabs-special-tab)
                    (t 'vibe-tabs-other-tab))))

        ;; Add modified indicator
        (when modified
          (setq display-name (concat display-name "*")))

        (push (propertize display-name
                         'face face
                         'buffer buf
                         'mouse-face 'highlight
                         'help-echo (format "Click to switch to %s" name)
                         'local-map (let ((map (make-sparse-keymap)))
                                     (define-key map [mouse-1]
                                       `(lambda ()
                                          (interactive)
                                          (switch-to-buffer ,buf)))
                                     map))
              tabs)))

    (mapconcat 'identity (reverse tabs) vibe-tabs-separator)))

(defun vibe-tabs-update ()
  "Update the tab display."
  (condition-case err
      (when (and vibe-tabs-buffer (buffer-live-p vibe-tabs-buffer)
                 vibe-tabs-window (window-live-p vibe-tabs-window))
        (with-current-buffer vibe-tabs-buffer
          (let ((inhibit-read-only t))
            (erase-buffer)
            (let ((tab-line (vibe-tabs-create-tab-line)))
              (when tab-line
                (insert " " tab-line " "))
              (goto-char (point-min))))))
    (error
     (message "Error updating tabs: %s" err))))

(defun vibe-tabs-smart-update ()
  "Smart update that recreates window if needed."
  (condition-case err
      (progn
        ;; Check if window is missing and recreate if needed
        (when (and vibe-tabs-update-timer  ; Only if tabs are supposed to be enabled
                   (or (not vibe-tabs-window)
                       (not (window-live-p vibe-tabs-window))))
          (vibe-tabs-create-window))

        ;; Do regular update
        (vibe-tabs-update))
    (error
     (message "Error in smart tabs update: %s" err))))

(defun vibe-tabs-create-window ()
  "Create the tab window at the top of the main editor."
  (condition-case err
      (progn
        ;; Find the main editor window
        (let ((main-window (or (cl-find-if
                                (lambda (win)
                                  (with-selected-window win
                                    (let ((buf-name (buffer-name)))
                                      (and (not (string-match "\\*treemacs\\*\\|\\*Treemacs" buf-name))
                                           (not (eq major-mode 'term-mode))
                                           (not (eq major-mode 'ansi-term-mode))
                                           (not (string-match "\\*.*terminal\\*" buf-name))
                                           (not (string-match "\\*Matrix Rain\\*\\|\\*Animation\\*" buf-name))
                                           (not (string-match "\\*Panel Tabs\\*\\|\\*Terminal Tabs\\*\\|\\*Buffer Tabs\\*" buf-name))))))
                                (window-list))
                               (selected-window))))

          ;; Ensure we have a valid window and it's large enough
          (when (and main-window (>= (window-height main-window) (+ vibe-tabs-height 3)))
            (with-selected-window main-window
              ;; Create buffer for tabs
              (unless (and vibe-tabs-buffer (buffer-live-p vibe-tabs-buffer))
                (setq vibe-tabs-buffer (get-buffer-create "*Buffer Tabs*"))
                (with-current-buffer vibe-tabs-buffer
                  (setq mode-line-format nil)
                  (setq cursor-type nil)
                  (setq buffer-read-only t)
                  (setq truncate-lines t)
                  (setq line-spacing 0)))

              ;; Create window at top if doesn't exist
              (unless (and vibe-tabs-window (window-live-p vibe-tabs-window))
                (condition-case split-err
                    (let ((new-window (split-window main-window vibe-tabs-height 'above)))
                      (setq vibe-tabs-window new-window)
                      (set-window-buffer new-window vibe-tabs-buffer)
                      (set-window-dedicated-p new-window t)
                      ;; Don't allow C-x o to select this window
                      (set-window-parameter new-window 'no-other-window t)
                      (message "Vibe tabs window created successfully"))
                  (error
                   (message "Failed to split window for tabs: %s" split-err)
                   (setq vibe-tabs-window nil))))

              ;; Update tabs if window was created successfully
              (when (and vibe-tabs-window (window-live-p vibe-tabs-window))
                (vibe-tabs-update))))))
    (error
     (message "Error in vibe-tabs-create-window: %s" err)
     (setq vibe-tabs-window nil
           vibe-tabs-buffer nil))))

(defun vibe-tabs-next-buffer ()
  "Switch to next buffer in tab list."
  (interactive)
  (let* ((buffers (vibe-tabs-get-file-buffers))
         (current (current-buffer))
         (index (cl-position current buffers))
         (next-index (if index
                        (mod (1+ index) (length buffers))
                      0)))
    (when buffers
      (switch-to-buffer (nth next-index buffers))
      (vibe-tabs-update))))

(defun vibe-tabs-previous-buffer ()
  "Switch to previous buffer in tab list."
  (interactive)
  (let* ((buffers (vibe-tabs-get-file-buffers))
         (current (current-buffer))
         (index (cl-position current buffers))
         (prev-index (if index
                        (mod (1- index) (length buffers))
                      (1- (length buffers)))))
    (when buffers
      (switch-to-buffer (nth prev-index buffers))
      (vibe-tabs-update))))

(defun vibe-tabs-close-current-buffer ()
  "Close current buffer and switch to next."
  (interactive)
  (let ((buffers (vibe-tabs-get-file-buffers))
        (current (current-buffer)))
    (when (> (length buffers) 1)
      (vibe-tabs-next-buffer))
    (kill-buffer current)
    (vibe-tabs-update)))

(defun vibe-tabs-enable ()
  "Enable buffer tabs."
  (interactive)
  (message "Enabling vibe tabs...")

  ;; Clean up any existing setup first
  (vibe-tabs-disable)

  ;; Create initial window with a small delay to ensure layout is stable
  (run-with-timer 0.5 nil
    (lambda ()
      (vibe-tabs-create-window)

      ;; Set up hooks to keep tabs updated
      (add-hook 'buffer-list-update-hook 'vibe-tabs-update)
      (add-hook 'window-configuration-change-hook 'vibe-tabs-update)

      ;; Start update timer
      (when vibe-tabs-update-timer
        (cancel-timer vibe-tabs-update-timer))
      (setq vibe-tabs-update-timer
            (run-with-timer 2 2 'vibe-tabs-smart-update))

      (message "Buffer tabs enabled successfully")))

  (message "Vibe tabs initialization started..."))

(defun vibe-tabs-disable ()
  "Disable buffer tabs."
  (interactive)
  ;; Remove window
  (when (and vibe-tabs-window (window-live-p vibe-tabs-window))
    (delete-window vibe-tabs-window))

  ;; Remove hooks
  (remove-hook 'buffer-list-update-hook 'vibe-tabs-update)
  (remove-hook 'window-configuration-change-hook 'vibe-tabs-update)

  ;; Cancel timer
  (when vibe-tabs-update-timer
    (cancel-timer vibe-tabs-update-timer)
    (setq vibe-tabs-update-timer nil))

  ;; Kill buffer
  (when (and vibe-tabs-buffer (buffer-live-p vibe-tabs-buffer))
    (kill-buffer vibe-tabs-buffer))

  ;; Reset variables
  (setq vibe-tabs-window nil
        vibe-tabs-buffer nil)

  (message "Buffer tabs disabled"))

(defun vibe-tabs-recreate ()
  "Recreate the tabs window if it was deleted."
  (interactive)
  (when (or (not vibe-tabs-window)
            (not (window-live-p vibe-tabs-window)))
    (message "Recreating tabs window...")
    (vibe-tabs-create-window)))

(defun vibe-tabs-toggle ()
  "Toggle buffer tabs on/off."
  (interactive)
  (if (and vibe-tabs-window (window-live-p vibe-tabs-window))
      (vibe-tabs-disable)
    (vibe-tabs-enable)))

(provide 'vibe-tabs)
;;; vibe-tabs.el ends here