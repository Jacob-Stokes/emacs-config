;;; comet-trail.el --- Comet trails animation -*- lexical-binding: t; -*-

;;; Commentary:
;; Streaking comets leave glowing trails across the dashboard buffer.

;;; Code:

(require 'cl-lib)

(defvar comet-trail-timer nil)
(defvar comet-trail-comets nil)
(defvar comet-trail-last-size nil)

(defconst comet-trail-count 4
  "Number of simultaneous comets to render.")

(defconst comet-trail-tail-length 12
  "Maximum length of the trail behind each comet.")

(defconst comet-trail-tail-palette
  '("#ffe5b4" "#ffd27f" "#ffb347" "#ff9f1c" "#f97316" "#ea580c")
  "Colour gradient used for comet tails from head to tail end.")

(defun comet-trail--sync-dimensions ()
  "Refresh shared animation dimensions from the live window."
  (let ((win (get-buffer-window matrix-rain-buffer)))
    (when win
      (setq matrix-width (max 30 (- (window-width win) 3)))
      (setq matrix-height (max 12 (- (window-height win) 1))))))

(defun comet-trail--make-comet ()
  "Create a fresh comet plist ready for animation."
  (list :x (- (random 25))
        :y (random (max 1 matrix-height))
        :vx (+ 1 (random 2))
        :vy (- (random 3) 1)
        :trail nil))

(defun comet-trail--reset-comet (comet)
  "Reset COMET back to the left edge with a new trajectory."
  (setf (plist-get comet :x) (- (random 25))
        (plist-get comet :y) (random (max 1 matrix-height))
        (plist-get comet :vx) (+ 1 (random 2))
        (plist-get comet :vy) (- (random 3) 1)
        (plist-get comet :trail) nil)
  comet)

(defun init-comet-trail ()
  "Initialise comet state for the animation."
  (comet-trail--sync-dimensions)
  (setq comet-trail-last-size (cons matrix-width matrix-height))
  (setq comet-trail-comets
        (cl-loop repeat comet-trail-count
                 collect (comet-trail--make-comet))))

(defun comet-trail--ensure-ready ()
  "Rebuild comet state if the drawable region changed."
  (comet-trail--sync-dimensions)
  (unless (and comet-trail-last-size
               (= (car comet-trail-last-size) matrix-width)
               (= (cdr comet-trail-last-size) matrix-height)
               comet-trail-comets)
    (init-comet-trail)))

(defun comet-trail--advance (comet)
  "Advance COMET one step, updating trail and velocity."
  (let* ((prev-x (plist-get comet :x))
         (prev-y (plist-get comet :y))
         (vx (plist-get comet :vx))
         (vy (plist-get comet :vy))
         (trail (plist-get comet :trail)))
    (setq trail (cons (cons (round prev-x) (round prev-y)) trail))
    (when (> (length trail) comet-trail-tail-length)
      (let ((cut (nthcdr (1- comet-trail-tail-length) trail)))
        (when cut (setcdr cut nil))))
    (setf (plist-get comet :trail) trail)

    (when (= (random 30) 0)
      (setq vy (+ vy (- (random 3) 1)))
      (setq vy (max -1 (min 1 vy))))

    (let ((new-x (+ prev-x vx))
          (new-y (+ prev-y vy)))
      (when (or (< new-y 1) (> new-y (- matrix-height 2)))
        (setq vy (- vy))
        (setq new-y (+ prev-y vy))
        (setf (plist-get comet :vy) vy))
      (setf (plist-get comet :x) new-x)
      (setf (plist-get comet :y) (max 0 (min (1- matrix-height) new-y)))))

  (when (> (plist-get comet :x) (+ matrix-width 12))
    (comet-trail--reset-comet comet))
  comet)

(defun update-comet-trail ()
  "Render a single frame of the comet trail animation."
  (when (and matrix-rain-buffer (buffer-live-p matrix-rain-buffer))
    (with-current-buffer matrix-rain-buffer
      (let ((was-readonly buffer-read-only))
        (when was-readonly (read-only-mode -1))
        (erase-buffer)
        (comet-trail--ensure-ready)

        (dotimes (row matrix-height)
          (insert " ")
          (dotimes (_ matrix-width)
            (insert " "))
          (when (< row (1- matrix-height))
            (insert "\n")))

        (dolist (comet comet-trail-comets)
          (comet-trail--advance comet)
          (let ((head-x (round (plist-get comet :x)))
                (head-y (round (plist-get comet :y))))
            (when (and (>= head-x 0) (< head-x matrix-width)
                       (>= head-y 0) (< head-y matrix-height))
              (animation-safe-draw-at head-x head-y "@"
                                      '(:foreground "#ffd166" :weight bold))))
          (let ((index 0))
            (dolist (segment (plist-get comet :trail))
              (let ((seg-x (car segment))
                    (seg-y (cdr segment)))
                (when (and (>= seg-x 0) (< seg-x matrix-width)
                           (>= seg-y 0) (< seg-y matrix-height))
                  (let* ((palette-index (min index (1- (length comet-trail-tail-palette))))
                         (color (nth palette-index comet-trail-tail-palette)))
                    (when color
                      (animation-safe-draw-at seg-x seg-y "="
                                              `(:foreground ,color))))))
              (setq index (1+ index)))))

        (when was-readonly (read-only-mode 1))))))

(defun start-comet-trail-animation ()
  "Start the comet trail animation timer."
  (when comet-trail-timer
    (cancel-timer comet-trail-timer))
  (init-comet-trail)
  (setq comet-trail-timer (run-at-time "0 sec" 0.1 #'update-comet-trail)))

(defun stop-comet-trail-animation ()
  "Stop the comet trail animation timer."
  (when comet-trail-timer
    (cancel-timer comet-trail-timer)
    (setq comet-trail-timer nil)))

(defvar comet-trail-animation-config
  (list :name "Comet Trails"
        :start-function #'start-comet-trail-animation
        :stop-function #'stop-comet-trail-animation)
  "Configuration for the comet trail animation.")

(provide 'comet-trail)
;;; comet-trail.el ends here
