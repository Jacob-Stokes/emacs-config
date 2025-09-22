;;; vibe-animations.el --- Auto-discoverable animation system -*- lexical-binding: t; -*-

;;; Commentary:
;; Provides a fully auto-discoverable animation system.
;; Drop an animation module in animation-modules/ and it auto-registers!

;;; Code:

(require 'cl-lib)

;; Core animation variables
(defvar animation-modules-dir (expand-file-name "animation-modules" (file-name-directory load-file-name))
  "Directory containing animation modules.")

(defvar vibe-animations nil
  "Alist of discovered animations: ((name . config-plist) ...)")

(defvar current-animation-mode nil
  "Currently active animation mode.")

(defvar animation-switch-timer nil
  "Timer for automatic animation switching.")

(defvar animation-switch-interval 5
  "Seconds between animation switches.")

;; Shared variables for all animations
(defvar animation-buffer nil
  "Shared buffer for all animations.")

(defvar animation-buffer-name "*Animation*"
  "Name for the animation buffer.")

(defvar animation-width 80
  "Width of animation display.")

(defvar animation-height 24
  "Height of animation display.")

;; Legacy aliases for backward compatibility
(defvaralias 'matrix-rain-buffer 'animation-buffer)
(defvaralias 'matrix-width 'animation-width)
(defvaralias 'matrix-height 'animation-height)

;; Timer for updating the mode-line time
(defvar vibe-animation-time-timer nil
  "Timer for updating the time display.")

(defun vibe-animation-update-mode-line ()
  "Update the mode-line for the animation buffer."
  (when (and animation-buffer (buffer-live-p animation-buffer))
    (with-current-buffer animation-buffer
      (let* ((anim (assoc current-animation-mode vibe-animations))
             (config (cdr anim))
             (name (or (plist-get config :name) current-animation-mode)))
        (setq-local mode-line-format
                    (list
                     ;; Left side - animation name (darker gray color)
                     (propertize (format " %s " name)
                                 'face '(:foreground "#555555"))
                     ;; Middle - fill with spaces
                     '(:eval (propertize " " 'display `((space :align-to (- right 15)))))
                     ;; Right side - current date and time
                     '(:eval (propertize (format-time-string "%b %d %H:%M ")
                                         'face '(:foreground "#666666")))))
        (force-mode-line-update)))))

;; Function to discover animation modules
(defun vibe-discover-animations ()
  "Discover and load all animation modules."
  (setq vibe-animations nil)

  ;; Load order configuration if it exists
  (let ((order-file (expand-file-name "order.el" animation-modules-dir)))
    (when (file-exists-p order-file)
      (load order-file nil t)))

  ;; Discover all animation modules
  (dolist (file (directory-files animation-modules-dir t "\\.el$"))
    (let ((module-name (file-name-sans-extension (file-name-nondirectory file))))
      (unless (string= module-name "order")
        ;; Load the module
        (load file nil t)

        ;; Look for animation config
        (let ((config-var (intern (format "%s-animation-config" module-name))))
          (when (boundp config-var)
            (let ((config (symbol-value config-var)))
              (push (cons module-name config) vibe-animations)))))))

  ;; Sort animations according to vibe-animation-cycle if defined
  (when (boundp 'vibe-animation-cycle)
    (let ((ordered-anims nil)
          (remaining-anims vibe-animations))
      ;; First add animations in the specified order
      (dolist (name vibe-animation-cycle)
        (let ((anim (assoc name vibe-animations)))
          (when anim
            (push anim ordered-anims)
            (setq remaining-anims (remove anim remaining-anims)))))
      ;; Add any remaining animations not in the cycle
      (setq vibe-animations (append (reverse ordered-anims) remaining-anims))))

  (message "Animation system: %d modes available: %s"
           (length vibe-animations)
           (mapconcat 'car vibe-animations ", ")))

;; Create shared animation buffer
(defun create-animation-buffer ()
  "Create the shared animation buffer."
  (unless (and animation-buffer (buffer-live-p animation-buffer))
    (setq animation-buffer (get-buffer-create animation-buffer-name))
    (with-current-buffer animation-buffer
      ;; Keep default mode-line until we set custom one
      (setq cursor-type nil)
      (setq truncate-lines t)
      (setq buffer-read-only t))))

;; Switch animation mode
(defun switch-animation-mode ()
  "Switch to the next animation mode."
  (interactive)

  ;; Ensure animations are discovered
  (when (null vibe-animations)
    (vibe-discover-animations))

  ;; Stop current animation
  (when current-animation-mode
    (let* ((current-anim (assoc current-animation-mode vibe-animations))
           (config (cdr current-anim))
           (stop-func (plist-get config :stop-function)))
      (when (fboundp stop-func)
        (condition-case err
            (funcall stop-func)
          (error (message "Error stopping animation %s: %s" current-animation-mode err))))))

  ;; Move to next animation
  (let ((anim-names (mapcar 'car vibe-animations)))
    (let ((current-index (cl-position current-animation-mode anim-names :test 'string=)))
      (setq current-animation-mode
            (nth (% (1+ (or current-index -1)) (length anim-names)) anim-names))))

  ;; Ensure buffer exists
  (create-animation-buffer)

  ;; Start new animation
  (let* ((new-anim (assoc current-animation-mode vibe-animations))
         (config (cdr new-anim))
         (start-func (plist-get config :start-function)))
    (when (fboundp start-func)
      (condition-case err
          (funcall start-func)
        (error (message "Error starting animation %s: %s" current-animation-mode err)))))

  ;; Update mode-line for new animation
  (vibe-animation-update-mode-line))

;; Start animation system
(defun start-animation-system ()
  "Initialize and start the animation system."
  (interactive)

  ;; Discover available animations
  (vibe-discover-animations)

  ;; Create shared buffer
  (create-animation-buffer)

  ;; Start first animation
  (when vibe-animations
    (setq current-animation-mode (caar vibe-animations))
    (let* ((anim (assoc current-animation-mode vibe-animations))
           (config (cdr anim))
           (start-func (plist-get config :start-function)))
      (when (fboundp start-func)
        (funcall start-func)))

    ;; Set up mode-line for first animation
    (vibe-animation-update-mode-line)

    ;; Start time update timer (update every minute)
    (when vibe-animation-time-timer
      (cancel-timer vibe-animation-time-timer))
    (setq vibe-animation-time-timer
          (run-with-timer 60 60 'vibe-animation-update-mode-line)))

  ;; Start automatic switching
  (when animation-switch-timer
    (cancel-timer animation-switch-timer))
  (setq animation-switch-timer
        (run-with-timer animation-switch-interval animation-switch-interval
                        'switch-animation-mode)))

;; Stop animation system
(defun stop-animation-system ()
  "Stop the animation system."
  (interactive)

  ;; Cancel timers
  (when animation-switch-timer
    (cancel-timer animation-switch-timer)
    (setq animation-switch-timer nil))

  (when vibe-animation-time-timer
    (cancel-timer vibe-animation-time-timer)
    (setq vibe-animation-time-timer nil))

  ;; Stop current animation
  (when current-animation-mode
    (let* ((anim (assoc current-animation-mode vibe-animations))
           (config (cdr anim))
           (stop-func (plist-get config :stop-function)))
      (when (fboundp stop-func)
        (funcall stop-func)))))


;; Initialize on load
(vibe-discover-animations)

(provide 'vibe-animations)
;;; vibe-animations.el ends here