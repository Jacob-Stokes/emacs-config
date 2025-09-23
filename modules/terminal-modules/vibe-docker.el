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
        (message "Please install docker.el package manually")))))

(defun vibe-docker-setup-terminal ()
  "Set up the Docker terminal buffer."
  (vibe-docker-ensure-package)
  (unless (and vibe-docker-terminal-buffer
               (buffer-live-p vibe-docker-terminal-buffer))
    (setq vibe-docker-terminal-buffer
          (get-buffer-create "*Docker Manager*"))
    (with-current-buffer vibe-docker-terminal-buffer
      (erase-buffer)
      (insert (propertize "Docker Management Interface\n"
                          'face '(:foreground "#0db7ed" :weight bold :height 1.2)))
      (insert (propertize "════════════════════════════\n\n"
                          'face '(:foreground "#444444")))

      (insert "Available Commands:\n\n")
      (insert (propertize "M-x docker" 'face '(:foreground "#00ff00" :weight bold))
              " - Open Docker main interface\n")
      (insert (propertize "M-x docker-containers" 'face '(:foreground "#00ff00" :weight bold))
              " - Manage containers\n")
      (insert (propertize "M-x docker-images" 'face '(:foreground "#00ff00" :weight bold))
              " - Manage images\n")
      (insert (propertize "M-x docker-networks" 'face '(:foreground "#00ff00" :weight bold))
              " - Manage networks\n")
      (insert (propertize "M-x docker-volumes" 'face '(:foreground "#00ff00" :weight bold))
              " - Manage volumes\n\n")

      (insert "Quick Actions:\n")
      (insert (propertize "[c]" 'face '(:foreground "#ffff00" :weight bold))
              " - Containers  ")
      (insert (propertize "[i]" 'face '(:foreground "#ffff00" :weight bold))
              " - Images  ")
      (insert (propertize "[n]" 'face '(:foreground "#ffff00" :weight bold))
              " - Networks  ")
      (insert (propertize "[v]" 'face '(:foreground "#ffff00" :weight bold))
              " - Volumes\n")
      (insert (propertize "[r]" 'face '(:foreground "#ffff00" :weight bold))
              " - Refresh  ")
      (insert (propertize "[q]" 'face '(:foreground "#ffff00" :weight bold))
              " - Quit\n\n")

      (when (featurep 'docker)
        (insert (propertize "Status: " 'face '(:weight bold))
                (propertize "Docker.el package loaded ✓\n" 'face '(:foreground "#00ff00")))

        ;; Try to get docker info
        (condition-case err
            (let ((containers (ignore-errors (docker-container-names)))
                  (images (ignore-errors (docker-image-names))))
              (insert (format "Containers: %s\n"
                             (if containers (length containers) "Unable to connect")))
              (insert (format "Images: %s\n"
                             (if images (length images) "Unable to connect"))))
          (error
           (insert (propertize "Warning: " 'face '(:foreground "#ff6600" :weight bold))
                   (format "Could not connect to Docker daemon\n")))))

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