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

(defvar animation-switch-interval 30
  "Seconds between animation switches.")

(defvar current-assistant-mode "claude"
  "Current assistant mode for display.")

(defvar matrix-rain-buffer nil
  "Shared buffer for all animations.")

(defvar matrix-width 80
  "Width of animation display.")

(defvar matrix-height 24
  "Height of animation display.")

;; Function to discover animation modules
(defun vibe-discover-animations ()
  "Discover and load all animation modules."
  (setq vibe-animations nil)

  ;; Load order configuration if it exists
  (let ((order-file (expand-file-name "order.el" animation-modules-dir)))
    (when (file-exists-p order-file)
      (load order-file nil t)))

  ;; First, always load matrix-rain.el since other animations depend on it
  (let ((matrix-file (expand-file-name "matrix-rain.el" animation-modules-dir)))
    (when (file-exists-p matrix-file)
      (load matrix-file nil t)))

  ;; Discover all animation modules
  (dolist (file (directory-files animation-modules-dir t "\\.el$"))
    (let ((module-name (file-name-sans-extension (file-name-nondirectory file))))
      (unless (or (string= module-name "order")
                  (string= module-name "matrix-rain")) ; Already loaded
        ;; Load the module
        (load file nil t))

      ;; Look for animation config (including matrix-rain)
      (unless (string= module-name "order")
        (let ((config-var (intern (format "%s-animation-config" module-name))))
          (when (boundp config-var)
            (let ((config (symbol-value config-var)))
              (unless (assoc module-name vibe-animations) ; Avoid duplicates
                (push (cons module-name config) vibe-animations))))))))

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
  (unless (and matrix-rain-buffer (buffer-live-p matrix-rain-buffer))
    (setq matrix-rain-buffer (get-buffer-create "*Animation*"))
    (with-current-buffer matrix-rain-buffer
      (setq mode-line-format nil)
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
        (error (message "Error starting animation %s: %s" current-animation-mode err))))))

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
        (funcall start-func))))

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

  ;; Cancel timer
  (when animation-switch-timer
    (cancel-timer animation-switch-timer)
    (setq animation-switch-timer nil))

  ;; Stop current animation
  (when current-animation-mode
    (let* ((anim (assoc current-animation-mode vibe-animations))
           (config (cdr anim))
           (stop-func (plist-get config :stop-function)))
      (when (fboundp stop-func)
        (funcall stop-func)))))

;; Backward compatibility functions (for init.el)
(defun start-rainbow-animation ()
  "Start rainbow animation - backward compatibility."
  (setq current-animation-mode "rainbow")
  (switch-animation-mode))

;; Initialize on load
(vibe-discover-animations)

(provide 'vibe-animations)
;;; vibe-animations.el ends here