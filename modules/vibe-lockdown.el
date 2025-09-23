;;; vibe-lockdown.el --- Layout protection and lockdown system -*- lexical-binding: t; -*-

;;; Commentary:
;; Prevents users from accidentally breaking the vibemacs layout by:
;; - Disabling window splitting/deletion keybindings
;; - Disabling buffer switching except through tabs
;; - Protecting dedicated windows

;;; Code:

(require 'cl-lib)

(defvar vibe-lockdown-enabled nil
  "Whether layout lockdown is currently active.")

(defvar vibe-lockdown-protected-keys
  '("C-x 2"        ; split-window-below
    "C-x 3"        ; split-window-right
    "C-x 0"        ; delete-window
    "C-x 1"        ; delete-other-windows
    "C-x b"        ; switch-to-buffer
    "C-x C-b"      ; list-buffers
    "C-x <left>"   ; previous-buffer
    "C-x <right>"  ; next-buffer
    "C-x 4 b"      ; switch-to-buffer-other-window
    "C-x 4 f"      ; find-file-other-window
    "C-x 5 b"      ; switch-to-buffer-other-frame
    "C-x 5 f")     ; find-file-other-frame
  "List of keybindings to disable when lockdown is active.")

(defvar vibe-lockdown-original-bindings nil
  "Stores original keybindings to restore when lockdown is disabled.")

(defun vibe-lockdown-blocked-action ()
  "Display message when a blocked action is attempted."
  (interactive)
  (message "Layout modification blocked. Use tabs to switch buffers."))

(defun vibe-lockdown-save-original-bindings ()
  "Save original keybindings before disabling them."
  (setq vibe-lockdown-original-bindings nil)
  (dolist (key vibe-lockdown-protected-keys)
    (let ((binding (key-binding (kbd key))))
      (when binding
        (push (cons key binding) vibe-lockdown-original-bindings)))))

(defun vibe-lockdown-disable-keys ()
  "Disable window management and buffer switching keys."
  (vibe-lockdown-save-original-bindings)
  (dolist (key vibe-lockdown-protected-keys)
    (global-set-key (kbd key) 'vibe-lockdown-blocked-action))
  (message "Layout lockdown enabled - window management keys disabled"))

(defun vibe-lockdown-restore-keys ()
  "Restore original keybindings."
  (dolist (binding vibe-lockdown-original-bindings)
    (let ((key (car binding))
          (original-func (cdr binding)))
      (global-set-key (kbd key) original-func)))
  (setq vibe-lockdown-original-bindings nil)
  (message "Layout lockdown disabled - window management keys restored"))

(defun vibe-lockdown-protect-dedicated-windows ()
  "Ensure all special windows remain dedicated."
  (dolist (window (window-list))
    (let ((buffer (window-buffer window)))
      (with-current-buffer buffer
        (let ((name (buffer-name)))
          (when (or (string-match "\\*treemacs\\*\\|\\*Treemacs" name)
                    (string-match "\\*Panel Tabs\\*\\|\\*Terminal Tabs\\*\\|\\*Buffer Tabs\\*" name)
                    (string-match "\\*Animation\\*\\|\\*Matrix Rain\\*" name))
            (set-window-dedicated-p window t)
            (set-window-parameter window 'no-other-window t)))))))

(defun vibe-lockdown-enable ()
  "Enable layout lockdown protection."
  (interactive)
  (unless vibe-lockdown-enabled
    (setq vibe-lockdown-enabled t)
    (vibe-lockdown-disable-keys)
    (vibe-lockdown-protect-dedicated-windows)

    ;; Only add hook to maintain dedicated windows
    (add-hook 'buffer-list-update-hook 'vibe-lockdown-protect-dedicated-windows)

    (message "VibEmacs layout lockdown enabled")))

(defun vibe-lockdown-disable ()
  "Disable layout lockdown protection."
  (interactive)
  (when vibe-lockdown-enabled
    (setq vibe-lockdown-enabled nil)
    (vibe-lockdown-restore-keys)

    ;; Remove hook
    (remove-hook 'buffer-list-update-hook 'vibe-lockdown-protect-dedicated-windows)

    (message "VibEmacs layout lockdown disabled")))

(defun vibe-lockdown-toggle ()
  "Toggle layout lockdown on/off."
  (interactive)
  (if vibe-lockdown-enabled
      (vibe-lockdown-disable)
    (vibe-lockdown-enable)))

(defun vibe-lockdown-status ()
  "Show current lockdown status."
  (interactive)
  (message "Layout lockdown: %s" (if vibe-lockdown-enabled "ENABLED" "DISABLED")))

;; Set up keybindings
(global-set-key (kbd "C-c w L") 'vibe-lockdown-toggle)
(global-set-key (kbd "C-c w l") 'vibe-lockdown-status)

(provide 'vibe-lockdown)
;;; vibe-lockdown.el ends here