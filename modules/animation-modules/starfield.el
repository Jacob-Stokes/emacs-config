;;; starfield.el --- Starfield animation -*- lexical-binding: t; -*-

;;; Commentary:
;; Provides a moving starfield animation with stars traveling at different speeds.

;;; Code:

(require 'cl-lib)

;; Starfield animation variables
(defvar starfield-timer nil)
(defvar starfield-buffer nil)
(defvar starfield-stars nil)
(defvar starfield-width 40)
(defvar starfield-height 15)
(defvar starfield-num-stars 30)

(cl-defstruct star
  x y speed char)

(defun init-starfield ()
  "Initialize the starfield with random stars"
  ;; Get actual window dimensions if buffer is displayed
  (let ((win (get-buffer-window starfield-buffer)))
    (when win
      (setq starfield-width (- (window-width win) 2))
      (setq starfield-height (- (window-height win) 1))))

  ;; Create stars at random positions
  (setq starfield-stars '())
  (dotimes (i starfield-num-stars)
    (push (make-star :x (random starfield-width)
                     :y (random starfield-height)
                     :speed (+ 1 (random 3))  ; Speed 1-3
                     :char (nth (random 4) '("." "·" "*" "+")))
          starfield-stars)))

(defun create-starfield-buffer ()
  "Create buffer for starfield animation"
  (setq starfield-buffer (get-buffer-create "*Starfield*"))
  (with-current-buffer starfield-buffer
    (erase-buffer)
    (display-line-numbers-mode -1)
    (setq mode-line-format nil)
    (setq cursor-type nil)
    (read-only-mode 1)))

(defun update-starfield ()
  "Update starfield animation frame"
  (when (and starfield-buffer (buffer-live-p starfield-buffer))
    (with-current-buffer starfield-buffer
      (let ((was-readonly buffer-read-only))
        (when was-readonly (read-only-mode -1))
        (erase-buffer)

        ;; Create empty grid
        (dotimes (row starfield-height)
          (dotimes (col starfield-width)
            (insert " "))
          (insert "\n"))

        ;; Move and draw stars
        (dolist (star starfield-stars)
          (setf (star-x star) (+ (star-x star) (star-speed star)))
          ;; Wrap around when star goes off screen
          (when (>= (star-x star) starfield-width)
            (setf (star-x star) 0)
            (setf (star-y star) (random starfield-height)))

          ;; Draw star at position
          (let ((pos (+ (* (star-y star) (+ starfield-width 1)) (star-x star) 1)))
            (when (and (>= pos 1) (<= pos (point-max)))
              (goto-char pos)
              (delete-char 1)
              (insert (propertize (star-char star)
                                 'face '(:foreground "white"))))))

        ;; Add assistant mode indicator in corner
        (goto-char (point-min))
        (end-of-line)
        (backward-char 8)
        (let ((mode-text (cond
                         ((string= current-assistant-mode "claude") "[CLAUDE]")
                         ((string= current-assistant-mode "codex") "[CODEX]")
                         ((string= current-assistant-mode "gemini") "[GEMINI]")
                         ((string= current-assistant-mode "system") "[SYSTEM]")
                         (t "[STARS]"))))
          (delete-region (point) (line-end-position))
          (insert (propertize mode-text 'face '(:foreground "cyan" :weight bold))))

        (when was-readonly (read-only-mode 1))))))

(defun start-starfield-animation ()
  "Start the starfield animation"
  (when starfield-timer
    (cancel-timer starfield-timer))
  (create-starfield-buffer)
  (init-starfield)
  (setq starfield-timer (run-at-time "0 sec" 0.2 'update-starfield)))

(defun stop-starfield-animation ()
  "Stop the starfield animation"
  (when starfield-timer
    (cancel-timer starfield-timer)
    (setq starfield-timer nil)))

(provide 'starfield)
;;; starfield.el ends here