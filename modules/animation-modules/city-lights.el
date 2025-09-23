;;; city-lights.el --- City skyline lights animation -*- lexical-binding: t; -*-

;;; Commentary:
;; Animated skyline with flickering windows and twinkling stars.

;;; Code:

(require 'cl-lib)

(defvar city-lights-timer nil)
(defvar city-lights-buildings nil)
(defvar city-lights-stars nil)
(defvar city-lights-last-size nil)
(defvar city-lights-phase 0)

(defun city-lights--sync-dimensions ()
  "Refresh matrix dimensions using the current animation window."
  (let ((win (get-buffer-window matrix-rain-buffer)))
    (when win
      (setq matrix-width (max 40 (- (window-width win) 3)))
      (setq matrix-height (max 14 (- (window-height win) 1))))))

(defun city-lights--make-building (start-x)
  "Create a building plist positioned at START-X."
  (let* ((remaining (max 6 (- matrix-width start-x)))
         (width (max 4 (min remaining (+ 4 (random 6)))))
         (max-height (max 6 (- matrix-height 2)))
         (height (max 5 (min max-height (+ 6 (random (max 4 (/ matrix-height 2)))))))
         (windows '()))
    (dotimes (row (max 1 (/ (- height 2) 2)))
      (let ((y-offset (+ 2 (* row 2))))
        (when (< y-offset height)
          (dotimes (col (max 1 (/ (- width 2) 2)))
            (let ((x-offset (+ 1 (* col 2))))
              (when (< x-offset (1- width))
                (push (cons x-offset y-offset) windows)))))))
    (list :x start-x
          :width width
          :height height
          :windows windows
          :lights (cl-loop for win in windows
                           when (= (random 2) 0)
                           collect win))))

(defun city-lights--generate-buildings ()
  "Populate skyline buildings across the matrix width."
  (setq city-lights-buildings '())
  (let ((x 0))
    (while (< x matrix-width)
      (let* ((building (city-lights--make-building x))
             (width (plist-get building :width))
             (gap (if (= (random 3) 0) 2 1)))
        (push building city-lights-buildings)
        (setq x (+ x width gap)))))
  (setq city-lights-buildings (nreverse city-lights-buildings)))

(defun city-lights--generate-stars ()
  "Scatter ambient stars in the upper half of the screen."
  (let* ((count (min 60 (max 12 (/ matrix-width 2))))
         (star-height (max 1 (floor (* 0.4 matrix-height)))))
    (setq city-lights-stars
          (cl-loop repeat count
                   collect (cons (random (max 1 matrix-width))
                                 (random (max 1 star-height)))))))

(defun init-city-lights ()
  "Initialise skyline, lights and ambient stars."
  (city-lights--sync-dimensions)
  (setq city-lights-last-size (cons matrix-width matrix-height))
  (city-lights--generate-buildings)
  (city-lights--generate-stars)
  (setq city-lights-phase 0))

(defun city-lights--ensure-ready ()
  "Rebuild skyline when the available area changes."
  (city-lights--sync-dimensions)
  (unless (and city-lights-last-size
               (= (car city-lights-last-size) matrix-width)
               (= (cdr city-lights-last-size) matrix-height)
               city-lights-buildings)
    (init-city-lights)))

(defun city-lights--update-lights (building)
  "Flicker windows for BUILDING to simulate life inside."
  (let ((lights (plist-get building :lights))
        (windows (plist-get building :windows))
        (updated '()))
    (dolist (win windows)
      (let ((lit (member win lights)))
        (cond
         ((and lit (> (random 100) 4))
          (push win updated))
         ((and (not lit) (< (random 100) 15))
          (push win updated)))))
    (setf (plist-get building :lights) updated)))

(defun city-lights--draw-building (building)
  "Draw BUILDING silhouette and lit windows." 
  (let ((base-x (plist-get building :x))
        (width (plist-get building :width))
        (height (plist-get building :height)))
    (dotimes (w width)
      (dotimes (h height)
        (let ((x (+ base-x w))
              (y (- matrix-height 1 h)))
          (when (and (>= x 0) (< x matrix-width)
                     (>= y 0) (< y matrix-height))
            (animation-safe-draw-at x y "#"
                                    '(:foreground "#1f2937"))))))
    ;; Soft roof glow
    (dotimes (w width)
      (let ((x (+ base-x w))
            (y (- matrix-height height)))
        (when (and (>= x 0) (< x matrix-width)
                   (>= y 0) (< y matrix-height))
          (animation-safe-draw-at x y "-"
                                  '(:foreground "#374151")))))
    ;; Highlight active windows after the silhouette
    (dolist (win (plist-get building :lights))
      (let ((x (+ base-x (car win)))
            (y (- matrix-height 1 (cdr win))))
        (when (and (>= x 0) (< x matrix-width)
                   (>= y 0) (< y matrix-height))
          (animation-safe-draw-at x y "o"
                                  '(:foreground "#fde68a" :weight bold)))))))

(defun update-city-lights ()
  "Render a frame of the city skyline animation."
  (when (and matrix-rain-buffer (buffer-live-p matrix-rain-buffer))
    (with-current-buffer matrix-rain-buffer
      (let ((was-readonly buffer-read-only))
        (when was-readonly (read-only-mode -1))
        (erase-buffer)
        (city-lights--ensure-ready)

        (dotimes (row matrix-height)
          (insert " ")
          (dotimes (_ matrix-width)
            (insert " "))
          (when (< row (1- matrix-height))
            (insert "\n")))

        ;; Twinkling stars
        (dolist (star city-lights-stars)
          (let ((x (car star))
                (y (cdr star)))
            (when (and (< (random 5) 4)
                       (>= x 0) (< x matrix-width)
                       (>= y 0) (< y matrix-height))
              (animation-safe-draw-at x y "*"
                                      '(:foreground "#e5e7eb")))))

        ;; City skyline
        (dolist (building city-lights-buildings)
          (city-lights--update-lights building)
          (city-lights--draw-building building))

        (setq city-lights-phase (1+ city-lights-phase))

        (when was-readonly (read-only-mode 1))))))

(defun start-city-lights-animation ()
  "Start the city lights animation timer."
  (when city-lights-timer
    (cancel-timer city-lights-timer))
  (init-city-lights)
  (setq city-lights-timer (run-at-time "0 sec" 0.18 #'update-city-lights)))

(defun stop-city-lights-animation ()
  "Stop the city lights animation timer."
  (when city-lights-timer
    (cancel-timer city-lights-timer)
    (setq city-lights-timer nil)))

(defvar city-lights-animation-config
  (list :name "City Lights"
        :start-function #'start-city-lights-animation
        :stop-function #'stop-city-lights-animation)
  "Configuration for the city lights animation.")

(provide 'city-lights)
;;; city-lights.el ends here
