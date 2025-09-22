;;; vibe-docker.el --- Docker container terminal module -*- lexical-binding: t; -*-

;;; Commentary:
;; Docker terminal that lists running containers and allows shell access.

;;; Code:

(require 'ansi-color)

;; Terminal metadata
(defvar vibe-docker-terminal-name "Docker"
  "Display name for this terminal.")

(defvar vibe-docker-terminal-key "d"
  "Keybinding suffix for this terminal (C-c a + this key).")

(defvar vibe-docker-terminal-color "#0db7ed"
  "Color for this terminal's tab and indicators (Docker blue).")

(defvar vibe-docker-terminal-buffer nil
  "Buffer for Docker terminal.")

(defvar vibe-docker-selected-container nil
  "Currently selected Docker container.")

(defvar vibe-docker-menu-buffer nil
  "Buffer for Docker menu.")

(defun vibe-docker-get-running-containers ()
  "Get list of running Docker containers."
  (let ((output (shell-command-to-string "docker ps --format '{{.Names}}'"))
        (containers '()))
    (dolist (line (split-string output "\n" t))
      (push (string-trim line) containers))
    (reverse containers)))

(defun vibe-docker-get-container-info (container)
  "Get info for CONTAINER."
  (let ((info-cmd (format "docker ps --filter name=%s --format '{{.Status}}|{{.Image}}|{{.Ports}}'" container)))
    (split-string (string-trim (shell-command-to-string info-cmd)) "|")))

(defun vibe-docker-select-container ()
  "Prompt user to select a Docker container."
  (let ((containers (vibe-docker-get-running-containers)))
    (if containers
        (completing-read "Select container: " containers nil t)
      (error "No running Docker containers found"))))

(defun vibe-docker-create-container-menu ()
  "Create a menu buffer for Docker container selection."
  (let ((buf (get-buffer-create "*vibe-docker-menu*"))
        (containers (vibe-docker-get-running-containers)))
    (with-current-buffer buf
      (erase-buffer)
      (if containers
          (progn
            ;; Header
            (insert (propertize (format " %-3s %-25s %-20s %-30s\n"
                                       "#" "Container" "Status" "Image")
                               'face '(:weight bold :underline t)))
            (insert (propertize "───────────────────────────────────────────────────────────────────────────────\n"
                               'face '(:foreground "gray30")))

            ;; Container list - compact single line per container
            (let ((index 1)
                  (keys "1234567890abcdefghijklmnopqrstuvwxyz"))
              (dolist (container containers)
                (when (<= index 36) ; Limit to 36 containers
                  (let* ((info (vibe-docker-get-container-info container))
                         (status (or (nth 0 info) "Unknown"))
                         (image (or (nth 1 info) "Unknown"))
                         ;; Truncate long strings
                         (container-display (truncate-string-to-width container 25 nil nil "..."))
                         (status-display (truncate-string-to-width status 20 nil nil "..."))
                         (image-display (truncate-string-to-width image 30 nil nil "..."))
                         ;; Get key for this index
                         (key-char (substring keys (1- index) index)))

                    ;; Color code based on status
                    (let ((status-face (cond
                                       ((string-match "healthy" status) '(:foreground "#00ff00"))
                                       ((string-match "unhealthy" status) '(:foreground "#ff0000"))
                                       (t '(:foreground "#ffff00")))))
                      (insert (format " [%s] " key-char))
                      (insert (propertize (format "%-25s " container-display) 'face '(:foreground "#00ff00" :weight bold)))
                      (insert (propertize (format "%-20s " status-display) 'face status-face))
                      (insert (propertize (format "%-30s" image-display) 'face '(:foreground "gray")))
                      (insert "\n"))))
                (setq index (1+ index))))

            (insert "\n")
            (insert (propertize "Commands: " 'face '(:weight bold)))
            (insert "[1-9,0,a-z] Connect | [r] Refresh | [l] Logs | [s] Stats | [q] Quit\n"))

        (insert "No running Docker containers found.\n\n")
        (insert "Start containers with:\n")
        (insert "  docker start <container-name>\n")
        (insert "  docker-compose up -d\n")))

    ;; Set up keybindings BEFORE making buffer read-only
    (use-local-map (make-sparse-keymap))
    (local-set-key (kbd "RET") 'vibe-docker-handle-menu-input)
    (local-set-key (kbd "r") 'vibe-docker-refresh-menu)
    (local-set-key (kbd "q") 'vibe-docker-quit)
    (local-set-key (kbd "l") 'vibe-docker-logs-prompt)
    (local-set-key (kbd "s") 'vibe-docker-show-container-stats)

    ;; Number keys for ALL containers (0-9 gives us 10, then use letters a-z for more)
    (let ((keys "1234567890abcdefghijklmnopqrstuvwxyz")
          (max-containers (min (length containers) 36))) ; Support up to 36 containers
      (dotimes (i max-containers)
        (let ((key (substring keys i (1+ i)))
              (container-num (1+ i)))
          (local-set-key (kbd key)
                        `(lambda () (interactive)
                           (vibe-docker-select-by-number ,container-num))))))

    ;; NOW make buffer read-only
    (setq buffer-read-only t)
    (read-only-mode 1)

    ;; Move cursor to beginning of buffer
    (goto-char (point-min))
    buf))

(defun vibe-docker-select-by-number (num)
  "Select container by number NUM."
  (let ((containers (vibe-docker-get-running-containers)))
    (when (and (> num 0) (<= num (length containers)))
      (let ((container (nth (1- num) containers)))
        (setq vibe-docker-selected-container container)
        (vibe-docker-connect-to-container container)))))

(defun vibe-docker-refresh-menu ()
  "Refresh the Docker menu."
  (interactive)
  (vibe-docker-create-container-menu)
  (goto-char (point-min))
  (message "Container list refreshed"))

(defun vibe-docker-quit ()
  "Quit Docker menu."
  (interactive)
  (kill-buffer (current-buffer)))

(defun vibe-docker-logs-prompt ()
  "Prompt for container and show logs."
  (interactive)
  (let ((container (vibe-docker-select-container)))
    (vibe-docker-show-container-logs container)))

(defun vibe-docker-handle-menu-input ()
  "Handle input in Docker menu."
  (interactive)
  (let ((containers (vibe-docker-get-running-containers))
        (input (read-string "Enter selection: ")))
    (cond
     ;; Number selection - connect to container
     ((string-match "^[0-9]+$" input)
      (vibe-docker-select-by-number (string-to-number input)))
     ;; Refresh
     ((string= input "r")
      (vibe-docker-refresh-menu))
     ;; Show logs
     ((string= input "l")
      (vibe-docker-logs-prompt))
     ;; Show stats
     ((string= input "s")
      (vibe-docker-show-container-stats))
     ;; Quit
     ((string= input "q")
      (vibe-docker-quit))
     (t
      (message "Invalid selection")))))

(defun vibe-docker-connect-to-container (container)
  "Connect to CONTAINER's shell."
  (let ((shell-cmd (format "docker exec -it %s /bin/bash 2>/dev/null || docker exec -it %s /bin/sh; echo 'DOCKER_EXIT_MARKER'; exit"
                          container container))
        (buffer-name (format "*docker-%s*" container)))

    ;; Store menu buffer before switching
    (setq vibe-docker-menu-buffer (current-buffer))

    ;; Create new terminal for container
    (setq vibe-docker-terminal-buffer (ansi-term "/bin/bash" buffer-name))

    (with-current-buffer vibe-docker-terminal-buffer
      ;; Add a process filter to detect when docker exits
      (let ((proc (get-buffer-process (current-buffer))))
        (set-process-filter proc
                           (lambda (process output)
                             ;; First, display the output normally
                             (term-emulate-terminal process output)
                             ;; Check if docker has exited
                             (when (string-match "DOCKER_EXIT_MARKER" output)
                               ;; Kill the terminal and return to menu
                               (kill-buffer (process-buffer process))
                               (when (buffer-live-p vibe-docker-menu-buffer)
                                 (switch-to-buffer vibe-docker-menu-buffer)
                                 (setq vibe-docker-selected-container nil)
                                 (message "Container session ended. Back to Docker menu.")))))
        ;; Send the docker exec command
        (sleep-for 0.2)
        (term-send-string proc (concat shell-cmd "\n"))))

    (message "Connected to container: %s (type 'exit' to return to menu)" container)))

(defun vibe-docker-show-container-logs (container)
  "Show logs for CONTAINER."
  (let ((buf (get-buffer-create (format "*docker-logs-%s*" container))))
    (with-current-buffer buf
      (erase-buffer)
      (insert (format "=== Logs for %s ===\n\n" container))
      (insert (shell-command-to-string (format "docker logs --tail 100 %s" container)))
      (goto-char (point-min))
      (ansi-color-apply-on-region (point-min) (point-max)))
    (switch-to-buffer buf)))

(defun vibe-docker-show-container-stats ()
  "Show stats for all containers."
  (interactive)
  (ansi-term "/bin/bash" "docker-stats")
  (with-current-buffer "*docker-stats*"
    (sleep-for 0.2)
    (term-send-string (get-buffer-process (current-buffer)) "docker stats\n")))

(defun vibe-docker-setup-terminal ()
  "Set up the Docker terminal."
  (vibe-docker-switch-to-terminal))

(defun vibe-docker-switch-to-terminal ()
  "Switch to Docker terminal or show menu."
  (interactive)
  (if vibe-docker-selected-container
      ;; If we have a selected container, return to its terminal
      (if (and vibe-docker-terminal-buffer (buffer-live-p vibe-docker-terminal-buffer))
          vibe-docker-terminal-buffer
        ;; Container buffer died, go back to menu
        (progn
          (setq vibe-docker-selected-container nil)
          (vibe-docker-create-container-menu)))
    ;; Otherwise show/create the menu
    (if (and vibe-docker-menu-buffer (buffer-live-p vibe-docker-menu-buffer))
        (switch-to-buffer vibe-docker-menu-buffer)
      (vibe-docker-create-container-menu))))

(defun vibe-docker-is-ready-p ()
  "Check if Docker terminal is ready."
  (or (and vibe-docker-terminal-buffer (buffer-live-p vibe-docker-terminal-buffer))
      (and vibe-docker-menu-buffer (buffer-live-p vibe-docker-menu-buffer))))

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