;;; vibe-docker.el --- Docker terminal module using docker.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Docker terminal that uses the docker.el package for container management.
;; Provides a clean interface to docker functionality within the terminal system.

;;; Code:

(require 'docker nil t) ; Optional require - will install if needed

;; Terminal metadata
(defvar vibe-docker-terminal-name "Docker"
  "Display name for this terminal.")

(defvar vibe-docker-terminal-key "d"
  "Keybinding suffix for this terminal.")

(defvar vibe-docker-terminal-color "#0db7ed"
  "Color for this terminal's tab and indicators (Docker blue).")

(defvar vibe-docker-terminal-buffer nil
  "Buffer for Docker terminal.")

(defun vibe-docker-ensure-package ()
  "Ensure docker.el package is installed."
  (unless (featurep 'docker)
    (when (y-or-n-p "Docker.el package not found. Install it? ")
      (if (fboundp 'package-install)
          (progn
            (unless package-archive-contents
              (package-refresh-contents))
            (package-install 'docker)
            (require 'docker))
        (message "Please install docker.el package manually"))))

  ;; Advice to force all docker buffers to open in main editor panel
  (when (featurep 'docker)
    (advice-add 'display-buffer :around #'vibe-docker-display-buffer-advice)))

(defun vibe-docker-display-buffer-advice (orig-fun buffer-or-name &rest args)
  "Advice to force docker buffers to display in main editor window."
  (let ((buffer (get-buffer buffer-or-name)))
    (if (and buffer
             (string-match "\\*docker" (buffer-name buffer))
             (boundp 'vibe-main-editor-window)
             (window-live-p vibe-main-editor-window))
        ;; Force docker buffers to display in main editor window
        (progn
          (set-window-buffer vibe-main-editor-window buffer)
          (select-window vibe-main-editor-window)
          ;; Enable tab-line and refresh tabs
          (with-current-buffer buffer
            (when (fboundp 'tab-line-mode)
              (tab-line-mode 1)))
          vibe-main-editor-window)
      ;; For non-docker buffers, use original behavior
      (apply orig-fun buffer-or-name args))))

(defun vibe-docker-setup-terminal ()
  "Set up the Docker terminal buffer."
  (vibe-docker-ensure-package)
  (unless (and vibe-docker-terminal-buffer
               (buffer-live-p vibe-docker-terminal-buffer))
    (setq vibe-docker-terminal-buffer
          (get-buffer-create "*Docker Manager*"))
    (with-current-buffer vibe-docker-terminal-buffer
      (erase-buffer)
      (insert (propertize "Docker Manager" 'face '(:foreground "#0db7ed" :weight bold)) "\n")
      (insert (propertize "══════════════" 'face '(:foreground "#444444")) "\n")

      ;; Status line (line 3)
      (when (featurep 'docker)
        (condition-case err
            (let* ((containers-output (shell-command-to-string "docker ps --format '{{.Names}}'"))
                   (containers (length (split-string containers-output "\n" t)))
                   (images-output (shell-command-to-string "docker images --format '{{.Repository}}'"))
                   (images (length (split-string images-output "\n" t))))
              (insert (format "%d containers │ %d images │ " containers images))
              (insert (propertize "CLI ✓" 'face '(:foreground "#00ff00")) "\n"))
          (error
           (insert (propertize "⚠ Docker daemon error" 'face '(:foreground "#ff6600")) "\n"))))

      ;; Empty line (line 4)
      (insert "\n")

      ;; Quick actions (lines 5-6)
      (insert (propertize "[c]" 'face '(:foreground "#ffff00" :weight bold)) " Containers  ")
      (insert (propertize "[i]" 'face '(:foreground "#ffff00" :weight bold)) " Images\n")
      (insert (propertize "[n]" 'face '(:foreground "#ffff00" :weight bold)) " Networks   ")
      (insert (propertize "[v]" 'face '(:foreground "#ffff00" :weight bold)) " Volumes\n")

      ;; Empty line (line 7)
      (insert "\n")

      ;; Additional actions (line 8)
      (insert (propertize "[r]" 'face '(:foreground "#ffff00" :weight bold)) " Refresh    ")
      (insert (propertize "[q]" 'face '(:foreground "#ffff00" :weight bold)) " Quit\n")

      ;; Set up local keybindings
      (use-local-map (make-sparse-keymap))
      (local-set-key (kbd "c") (lambda () (interactive) (docker-containers)))
      (local-set-key (kbd "i") (lambda () (interactive) (docker-images)))
      (local-set-key (kbd "n") (lambda () (interactive) (docker-networks)))
      (local-set-key (kbd "v") (lambda () (interactive) (docker-volumes)))
      (local-set-key (kbd "r") (lambda () (interactive) (vibe-docker-setup-terminal)))
      (local-set-key (kbd "q") (lambda () (interactive) (message "Press C-c b [num] to switch terminals")))
      (local-set-key (kbd "RET") (lambda () (interactive) (docker)))

      (goto-char (point-min))
      (setq buffer-read-only t)))

  vibe-docker-terminal-buffer)

(defun vibe-docker-switch-to-terminal ()
  "Switch to Docker terminal."
  (interactive)
  (if (and vibe-docker-terminal-buffer (buffer-live-p vibe-docker-terminal-buffer))
      vibe-docker-terminal-buffer
    (vibe-docker-setup-terminal)))

(defun vibe-docker-is-ready-p ()
  "Check if Docker terminal is ready."
  (and vibe-docker-terminal-buffer (buffer-live-p vibe-docker-terminal-buffer)))

;; Register this terminal (metadata for auto-discovery)
(defvar vibe-docker-terminal-config
  `(:name ,vibe-docker-terminal-name
    :key ,vibe-docker-terminal-key
    :color ,vibe-docker-terminal-color
    :setup-function vibe-docker-setup-terminal
    :switch-function vibe-docker-switch-to-terminal
    :ready-function vibe-docker-is-ready-p))

(provide 'vibe-docker)
;;; vibe-docker.el ends here