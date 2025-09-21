;;; vibe-animations.el --- Animation system manager for VibEmacs -*- lexical-binding: t; -*-

;; Copyright (C) 2024
;; Author: VibEmacs Configuration

;;; Commentary:
;; This module manages and orchestrates all animation sub-modules.
;; Individual animations are loaded from the animation-modules subfolder.

;;; Code:

;; Add animation-modules to load path
(add-to-list 'load-path (expand-file-name "animation-modules"
                                          (file-name-directory load-file-name)))

;; Auto-discover and load animation modules
(defun vibe-discover-animations ()
  "Automatically discover and load animation modules."
  (let ((anim-dir (expand-file-name "animation-modules"
                                    (file-name-directory load-file-name)))
        (discovered-modes '()))

    (when (file-directory-p anim-dir)
      (dolist (file (directory-files anim-dir nil "\\.el$"))
        (unless (string= file "order.el")  ; Skip order.el
          (let ((module-name (file-name-sans-extension file)))
            ;; Load the animation module
            (require (intern module-name))
            ;; Add to discovered modes (except rainbow which is special)
            (unless (string= module-name "rainbow")
              (push module-name discovered-modes))))))

    ;; Load custom ordering if available
    (let ((cycle-list nil))
      (when (file-exists-p (expand-file-name "order.el" anim-dir))
        (require 'order)
        (when (boundp 'vibe-animation-cycle)
          (setq cycle-list vibe-animation-cycle)))

      ;; Set animation modes using custom cycle or all discovered
      (setq animation-modes
            (or cycle-list
                (sort discovered-modes 'string<))))

    (message "Animation system: %d modes available: %s"
             (length animation-modes)
             (string-join animation-modes ", "))))

;; Discover animations on load
(vibe-discover-animations)

;; Animation system control variables
(defvar current-animation-mode (or (car animation-modes) "matrix")
  "Currently active animation mode.")

(defvar animation-switch-timer nil
  "Timer for automatic animation switching.")

(defvar animation-switch-interval 5
  "Seconds between automatic animation switches.")

;; Master animation control functions
(defun switch-animation-mode ()
  "Switch to the next animation mode"
  (interactive)
  ;; Ensure we have animation modes loaded
  (when (zerop (length animation-modes))
    (vibe-discover-animations))

  ;; Stop current animation
  (condition-case err
      (progn
        ;; Stop all animations first to avoid conflicts
        (stop-matrix-rain-animation)
        (stop-aquarium-animation)
        (stop-starfield-animation))
    (error (message "Error stopping animations: %s" err)))

  ;; Move to next mode
  (let ((current-index (cl-position current-animation-mode animation-modes :test 'string=)))
    (setq current-animation-mode
          (nth (% (1+ (or current-index 0)) (length animation-modes)) animation-modes)))

  ;; Start new animation
  (condition-case err
      (progn
        ;; Ensure shared buffer exists and is writable
        (when (not (and matrix-rain-buffer (buffer-live-p matrix-rain-buffer)))
          (create-matrix-rain-buffer))
        (with-current-buffer matrix-rain-buffer
          (when buffer-read-only (read-only-mode -1)))

        (cond
         ((string= current-animation-mode "matrix")
          (init-matrix-rain)
          (start-matrix-rain-animation))
         ((string= current-animation-mode "aquarium")
          (start-aquarium-animation))
         ((string= current-animation-mode "starfield")
          (start-starfield-animation))))
    (error (message "Error starting animation: %s" err)))

  (message "Switched to %s animation" current-animation-mode))

(defun start-animation-switcher ()
  "Start automatic animation switching"
  (when animation-switch-timer
    (cancel-timer animation-switch-timer))
  ;; Start with a delay to let the initial animation settle
  (setq animation-switch-timer
        (run-at-time animation-switch-interval
                     animation-switch-interval
                     'switch-animation-mode)))

(defun stop-animation-switcher ()
  "Stop automatic animation switching"
  (when animation-switch-timer
    (cancel-timer animation-switch-timer)
    (setq animation-switch-timer nil)))

;; Public API for easy animation management
(defun vibe-start-animations ()
  "Start the default animation system"
  (interactive)
  ;; Ensure shared buffer exists first
  (create-matrix-rain-buffer)

  ;; Start the current animation mode
  (cond
   ((string= current-animation-mode "matrix")
    (init-matrix-rain)
    (start-matrix-rain-animation))
   ((string= current-animation-mode "aquarium")
    (start-aquarium-animation))
   ((string= current-animation-mode "starfield")
    (start-starfield-animation)))
  (start-animation-switcher))

(defun vibe-stop-animations ()
  "Stop all animations"
  (interactive)
  (stop-animation-switcher)
  (stop-matrix-rain-animation)
  (stop-aquarium-animation)
  (stop-starfield-animation)
  (stop-rainbow-animation))

(defun vibe-set-animation (mode)
  "Set a specific animation MODE.
Valid modes are dynamically discovered from animation-modules."
  (interactive
   (list (completing-read "Animation mode: " animation-modes nil t)))
  (setq current-animation-mode mode)
  (switch-animation-mode))

;; The animation functions are already exported by their respective modules
;; No need for additional aliases since require already makes them available

(provide 'vibe-animations)
;;; vibe-animations.el ends here