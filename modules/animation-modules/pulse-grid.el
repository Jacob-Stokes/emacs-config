;;; pulse-grid.el --- Audio-style pulse grid animation -*- lexical-binding: t; -*-

;;; Commentary:
;; Animated vertical bars pulsing like a spectrum analyser.

;;; Code:

(defvar pulse-grid-timer nil)
(defvar pulse-grid-phase 0.0)
(defvar pulse-grid-last-size nil)

(defun pulse-grid--sync-dimensions ()
  "Bring matrix dimensions in line with the active animation window."
  (let ((win (get-buffer-window matrix-rain-buffer)))
    (when win
      (setq matrix-width (max 30 (- (window-width win) 3)))
      (setq matrix-height (max 12 (- (window-height win) 1))))))

(defun pulse-grid--ensure-ready ()
  "Reset phase when the drawable region changes."
  (pulse-grid--sync-dimensions)
  (unless (and pulse-grid-last-size
               (= (car pulse-grid-last-size) matrix-width)
               (= (cdr pulse-grid-last-size) matrix-height))
    (setq pulse-grid-phase 0.0)
    (setq pulse-grid-last-size (cons matrix-width matrix-height))))

(defun pulse-grid--color (intensity)
  "Return a colour string matched to INTENSITY in the range 0..1."
  (cond
   ((> intensity 0.85) "#c084fc")
   ((> intensity 0.7) "#a855f7")
   ((> intensity 0.55) "#7c3aed")
   ((> intensity 0.35) "#6366f1")
   (t "#3b82f6")))

(defun init-pulse-grid ()
  "Initialise pulse grid animation state."
  (pulse-grid--sync-dimensions)
  (setq pulse-grid-phase 0.0)
  (setq pulse-grid-last-size (cons matrix-width matrix-height)))

(defun update-pulse-grid ()
  "Render one frame of the pulse grid animation."
  (when (and matrix-rain-buffer (buffer-live-p matrix-rain-buffer))
    (with-current-buffer matrix-rain-buffer
      (let ((was-readonly buffer-read-only))
        (when was-readonly (read-only-mode -1))
        (erase-buffer)
        (pulse-grid--ensure-ready)

        (dotimes (row matrix-height)
          (insert " ")
          (dotimes (_ matrix-width)
            (insert " "))
          (when (< row (1- matrix-height))
            (insert "\n")))

        (let* ((height (float matrix-height))
               (base (* 0.25 height))
               (amplitude (* 0.65 height)))
          (dotimes (col matrix-width)
            (let* ((angle (+ pulse-grid-phase (* (float col) 0.25)))
                   (wave (sin angle))
                   (intensity (/ (+ wave 1.0) 2.0))
                   (bar-height (max 1 (round (+ base (* amplitude intensity))))))
              (dotimes (offset (min bar-height matrix-height))
                (let ((y (- matrix-height 1 offset)))
                  (when (>= y 0)
                    (animation-safe-draw-at col y "|"
                                            `(:foreground ,(pulse-grid--color intensity))))))))

        (setq pulse-grid-phase (+ pulse-grid-phase 0.22))

        (when was-readonly (read-only-mode 1)))))))

(defun start-pulse-grid-animation ()
  "Start the pulse grid animation."
  (when pulse-grid-timer
    (cancel-timer pulse-grid-timer))
  (init-pulse-grid)
  (setq pulse-grid-timer (run-at-time "0 sec" 0.08 #'update-pulse-grid)))

(defun stop-pulse-grid-animation ()
  "Stop the pulse grid animation."
  (when pulse-grid-timer
    (cancel-timer pulse-grid-timer)
    (setq pulse-grid-timer nil)))

(defvar pulse-grid-animation-config
  (list :name "Pulse Grid"
        :start-function #'start-pulse-grid-animation
        :stop-function #'stop-pulse-grid-animation)
  "Configuration for the pulse grid animation.")

(provide 'pulse-grid)
;;; pulse-grid.el ends here
