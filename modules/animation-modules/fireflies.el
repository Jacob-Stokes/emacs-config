;;; fireflies.el --- Fireflies animation -*- lexical-binding: t; -*-

;;; Commentary:
;; Softly glowing fireflies drifting across the animation buffer.

;;; Code:

(require 'cl-lib)

(defvar fireflies-timer nil)
(defvar fireflies-particles nil)
(defvar fireflies-last-size nil)

(defun fireflies--sync-dimensions ()
  "Update shared animation dimensions from the active window."
  (let ((win (get-buffer-window matrix-rain-buffer)))
    (when win
      (setq matrix-width (max 20 (- (window-width win) 3)))
      (setq matrix-height (max 10 (- (window-height win) 1))))))

(defun init-fireflies ()
  "Initialise the fireflies animation state."
  (fireflies--sync-dimensions)
  (setq fireflies-last-size (cons matrix-width matrix-height))
  (let* ((area (max 1 (* matrix-width matrix-height)))
         (target-count (min 75 (max 15 (/ area 90)))))
    (setq fireflies-particles
          (cl-loop repeat target-count
                   collect (list :x (random (max 1 matrix-width))
                                 :y (random (max 1 matrix-height))
                                 :dir-x 0
                                 :dir-y 0
                                 :glow (random 8))))))

(defun fireflies--ensure-ready ()
  "Re-initialise particles if the usable area changed."
  (fireflies--sync-dimensions)
  (unless (and fireflies-last-size
               (= (car fireflies-last-size) matrix-width)
               (= (cdr fireflies-last-size) matrix-height)
               fireflies-particles)
    (init-fireflies)))

(defun fireflies--update-particle (particle)
  "Advance PARTICLE and refresh its glow state."
  (let ((dx (plist-get particle :dir-x))
        (dy (plist-get particle :dir-y)))
    (when (= (random 5) 0)
      (setq dx (- (random 3) 1)))
    (when (= (random 5) 0)
      (setq dy (- (random 3) 1)))
    (setq dx (max -1 (min 1 dx)))
    (setq dy (max -1 (min 1 dy)))
    (plist-put particle :dir-x dx)
    (plist-put particle :dir-y dy)

    (let* ((x (plist-get particle :x))
           (y (plist-get particle :y))
           (new-x (+ x dx))
           (new-y (+ y dy)))
      (when (or (< new-x 1) (> new-x (- matrix-width 2)))
        (setq dx (- dx))
        (setq new-x (+ x dx))
        (plist-put particle :dir-x dx))
      (when (or (< new-y 1) (> new-y (- matrix-height 2)))
        (setq dy (- dy))
        (setq new-y (+ y dy))
        (plist-put particle :dir-y dy))
      (plist-put particle :x (max 0 (min (1- matrix-width) new-x)))
      (plist-put particle :y (max 0 (min (1- matrix-height) new-y)))))

  (let ((glow (max 0 (1- (plist-get particle :glow)))))
    (when (= (random 8) 0)
      (setq glow (+ 6 (random 6))))
    (plist-put particle :glow glow))
  particle)

(defun update-fireflies ()
  "Render one frame of the fireflies animation."
  (when (and matrix-rain-buffer (buffer-live-p matrix-rain-buffer))
    (with-current-buffer matrix-rain-buffer
      (let ((was-readonly buffer-read-only))
        (when was-readonly (read-only-mode -1))
        (erase-buffer)
        (fireflies--ensure-ready)

        (dotimes (row matrix-height)
          (insert " ")
          (dotimes (_ matrix-width)
            (insert " "))
          (when (< row (1- matrix-height))
            (insert "\n")))

        (dolist (particle fireflies-particles)
          (fireflies--update-particle particle)
          (let* ((x (plist-get particle :x))
                 (y (plist-get particle :y))
                 (glow (plist-get particle :glow))
                 (color (cond
                         ((>= glow 10) "#fef08a")
                         ((>= glow 7) "#fde047")
                         ((>= glow 4) "#facc15")
                         (t "#f97316")))
                 (glyph (if (> glow 6) "*" ".")))
            (when (and (>= x 0) (< x matrix-width)
                       (>= y 0) (< y matrix-height))
              (animation-safe-draw-at x y glyph
                                      `(:foreground ,color)))))

        (when was-readonly (read-only-mode 1))))))

(defun start-fireflies-animation ()
  "Start the fireflies animation timer."
  (when fireflies-timer
    (cancel-timer fireflies-timer))
  (init-fireflies)
  (setq fireflies-timer (run-at-time "0 sec" 0.12 #'update-fireflies)))

(defun stop-fireflies-animation ()
  "Stop the fireflies animation timer."
  (when fireflies-timer
    (cancel-timer fireflies-timer)
    (setq fireflies-timer nil)))

(defvar fireflies-animation-config
  (list :name "Fireflies"
        :start-function #'start-fireflies-animation
        :stop-function #'stop-fireflies-animation)
  "Configuration for the fireflies animation.")

(provide 'fireflies)
;;; fireflies.el ends here
