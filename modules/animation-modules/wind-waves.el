;;; wind-waves.el --- Rolling wind waves animation -*- lexical-binding: t; -*-

;;; Commentary:
;; Gentle waves sweep across the bottom of the animation buffer.

;;; Code:

(defvar wind-waves-timer nil)
(defvar wind-waves-phase 0.0)
(defvar wind-waves-last-size nil)

(defun wind-waves--sync-dimensions ()
  "Update width and height for the shared animation buffer."
  (let ((win (get-buffer-window matrix-rain-buffer)))
    (when win
      (setq matrix-width (max 30 (- (window-width win) 3)))
      (setq matrix-height (max 12 (- (window-height win) 1))))))

(defun init-wind-waves ()
  "Initialise wind wave animation state."
  (wind-waves--sync-dimensions)
  (setq wind-waves-phase 0.0)
  (setq wind-waves-last-size (cons matrix-width matrix-height)))

(defun wind-waves--ensure-ready ()
  "Recreate cached settings if the buffer size changes."
  (wind-waves--sync-dimensions)
  (unless (and wind-waves-last-size
               (= (car wind-waves-last-size) matrix-width)
               (= (cdr wind-waves-last-size) matrix-height))
    (setq wind-waves-phase 0.0)
    (setq wind-waves-last-size (cons matrix-width matrix-height))))

(defun update-wind-waves ()
  "Render wind-driven waves near the bottom of the buffer."
  (when (and matrix-rain-buffer (buffer-live-p matrix-rain-buffer))
    (with-current-buffer matrix-rain-buffer
      (let ((was-readonly buffer-read-only))
        (when was-readonly (read-only-mode -1))
        (erase-buffer)
        (wind-waves--ensure-ready)

        (dotimes (row matrix-height)
          (insert " ")
          (dotimes (_ matrix-width)
            (insert " "))
          (when (< row (1- matrix-height))
            (insert "\n")))

        (let* ((usable-width (max 1 matrix-width))
               (rows (min (max 4 (floor (/ matrix-height 2))) (- matrix-height 1)))
               (start-row (max 0 (- matrix-height rows))))
          (dotimes (row rows)
            (let* ((actual-row (+ start-row row))
                   (angle (+ wind-waves-phase (* (float row) 0.35)))
                   (wave (sin angle))
                   (offset (mod (round (+ (* wave 4.0) (* row 2))) usable-width))
                   (col 0))
              (while (< col matrix-width)
                (let* ((x (mod (+ offset col) usable-width))
                       (crest (mod (1+ x) usable-width)))
                  (when (and (>= actual-row 0) (< actual-row matrix-height))
                    (animation-safe-draw-at x actual-row "~"
                                            '(:foreground "#38bdf8")))
                  (when (> actual-row 0)
                    (animation-safe-draw-at crest (1- actual-row) "`"
                                            '(:foreground "#bae6fd")))
                  (setq col (+ col 3))))))

        (setq wind-waves-phase (+ wind-waves-phase 0.12))

        (when was-readonly (read-only-mode 1)))))))

(defun start-wind-waves-animation ()
  "Start the wind waves animation timer."
  (when wind-waves-timer
    (cancel-timer wind-waves-timer))
  (init-wind-waves)
  (setq wind-waves-timer (run-at-time "0 sec" 0.12 #'update-wind-waves)))

(defun stop-wind-waves-animation ()
  "Stop the wind waves animation timer."
  (when wind-waves-timer
    (cancel-timer wind-waves-timer)
    (setq wind-waves-timer nil)))

(defvar wind-waves-animation-config
  (list :name "Wind Waves"
        :start-function #'start-wind-waves-animation
        :stop-function #'stop-wind-waves-animation)
  "Configuration for the wind waves animation.")

(provide 'wind-waves)
;;; wind-waves.el ends here
