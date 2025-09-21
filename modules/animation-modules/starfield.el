;;; starfield.el --- Starfield animation -*- lexical-binding: t; -*-

;;; Commentary:
;; Provides a moving starfield animation with stars traveling at different speeds.

;;; Code:

(require 'cl-lib)
(require 'matrix-rain)  ; For shared buffer

;; Starfield animation variables
(defvar starfield-timer nil)
(defvar starfield-stars nil)
(defvar starfield-num-stars 30)

(cl-defstruct star
  x y speed char)

(defun init-starfield ()
  "Initialize the starfield with random stars"
  ;; Get actual window dimensions if buffer is displayed
  (let ((win (get-buffer-window matrix-rain-buffer)))
    (when win
      (setq matrix-width (- (window-width win) 2))
      (setq matrix-height (- (window-height win) 1))))

  ;; Create stars at random positions
  (setq starfield-stars '())
  (dotimes (i starfield-num-stars)
    (push (make-star :x (random matrix-width)
                     :y (random matrix-height)
                     :speed (+ 1 (random 3))  ; Speed 1-3
                     :char (nth (random 4) '("." "·" "*" "+")))
          starfield-stars)))

;; Note: Starfield now uses shared matrix-rain-buffer

(defun update-starfield ()
  "Update starfield animation frame"
  (when (and matrix-rain-buffer (buffer-live-p matrix-rain-buffer))
    (with-current-buffer matrix-rain-buffer
      (let ((was-readonly buffer-read-only))
        (when was-readonly (read-only-mode -1))
        (erase-buffer)

        ;; Create empty grid
        (dotimes (row matrix-height)
          (dotimes (col matrix-width)
            (insert " "))
          (insert "\n"))

        ;; Move and draw stars
        (dolist (star starfield-stars)
          (setf (star-x star) (+ (star-x star) (star-speed star)))
          ;; Wrap around when star goes off screen
          (when (>= (star-x star) matrix-width)
            (setf (star-x star) 0)
            (setf (star-y star) (random matrix-height)))

          ;; Draw star at position
          (let ((pos (+ (* (star-y star) (+ matrix-width 1)) (star-x star) 1)))
            (when (and (>= pos 1) (<= pos (point-max)))
              (goto-char pos)
              (delete-char 1)
              (insert (propertize (star-char star)
                                 'face '(:foreground "white"))))))

        ;; Starfield doesn't show assistant mode indicator
        ;; Just show stars without text

        (when was-readonly (read-only-mode 1))))))

(defun start-starfield-animation ()
  "Start the starfield animation"
  (when starfield-timer
    (cancel-timer starfield-timer))
  (init-starfield)
  (setq starfield-timer (run-at-time "0 sec" 0.2 'update-starfield)))

(defun stop-starfield-animation ()
  "Stop the starfield animation"
  (when starfield-timer
    (cancel-timer starfield-timer)
    (setq starfield-timer nil)))

(provide 'starfield)
;;; starfield.el ends here