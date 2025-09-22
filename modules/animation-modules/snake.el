;;; snake.el --- Snake game animation -*- lexical-binding: t; -*-

;;; Commentary:
;; Provides a snake game animation that runs automatically.

;;; Code:

(require 'cl-lib)

;; Snake animation variables
(defvar snake-timer nil)
(defvar snake-body nil)
(defvar snake-direction nil)
(defvar snake-food nil)
(defvar snake-score 0)
(defvar snake-game-over nil)

(defun init-snake ()
  "Initialize the snake game."
  ;; Get actual window dimensions if buffer is displayed
  (let ((win (get-buffer-window matrix-rain-buffer)))
    (when win
      (setq matrix-width (max 30 (- (window-width win) 2)))
      (setq matrix-height (max 15 (- (window-height win) 1)))))

  ;; Initialize snake in center
  (setq snake-body (list (cons (/ matrix-width 2) (/ matrix-height 2))
                         (cons (- (/ matrix-width 2) 1) (/ matrix-height 2))
                         (cons (- (/ matrix-width 2) 2) (/ matrix-height 2))))
  (setq snake-direction 'right)
  (setq snake-game-over nil)
  (setq snake-score 0)

  ;; Place initial food
  (snake-place-food))

(defun snake-place-food ()
  "Place food at a random location not occupied by snake."
  (let ((x (random matrix-width))
        (y (random matrix-height)))
    (while (member (cons x y) snake-body)
      (setq x (random matrix-width))
      (setq y (random matrix-height)))
    (setq snake-food (cons x y))))

(defun snake-move ()
  "Move the snake in the current direction."
  (when (not snake-game-over)
    (let* ((head (car snake-body))
           (new-head (cond
                      ((eq snake-direction 'right)
                       (cons (1+ (car head)) (cdr head)))
                      ((eq snake-direction 'left)
                       (cons (1- (car head)) (cdr head)))
                      ((eq snake-direction 'up)
                       (cons (car head) (1- (cdr head))))
                      ((eq snake-direction 'down)
                       (cons (car head) (1+ (cdr head)))))))

      ;; Check wall collision
      (when (or (< (car new-head) 0)
                (>= (car new-head) matrix-width)
                (< (cdr new-head) 0)
                (>= (cdr new-head) matrix-height))
        (setq snake-game-over t)
        (init-snake)  ; Restart game
        (return-from snake-move))

      ;; Check self collision
      (when (member new-head snake-body)
        (setq snake-game-over t)
        (init-snake)  ; Restart game
        (return-from snake-move))

      ;; Add new head
      (push new-head snake-body)

      ;; Check food collision
      (if (equal new-head snake-food)
          (progn
            (setq snake-score (1+ snake-score))
            (snake-place-food))
        ;; Remove tail if no food eaten
        (setq snake-body (butlast snake-body))))))

(defun snake-auto-turn ()
  "AI to automatically turn the snake towards food."
  (when (and snake-food (not snake-game-over))
    (let* ((head (car snake-body))
           (food-x (car snake-food))
           (food-y (cdr snake-food))
           (head-x (car head))
           (head-y (cdr head))
           (dx (- food-x head-x))
           (dy (- food-y head-y)))

      ;; Simple AI: move towards food, avoiding immediate self-collision
      (cond
       ;; Prioritize horizontal movement
       ((and (/= dx 0)
             (not (and (> dx 0) (eq snake-direction 'left)))
             (not (and (< dx 0) (eq snake-direction 'right))))
        (if (> dx 0)
            (unless (member (cons (1+ head-x) head-y) snake-body)
              (setq snake-direction 'right))
          (unless (member (cons (1- head-x) head-y) snake-body)
            (setq snake-direction 'left))))
       ;; Then vertical
       ((and (/= dy 0)
             (not (and (> dy 0) (eq snake-direction 'up)))
             (not (and (< dy 0) (eq snake-direction 'down))))
        (if (> dy 0)
            (unless (member (cons head-x (1+ head-y)) snake-body)
              (setq snake-direction 'down))
          (unless (member (cons head-x (1- head-y)) snake-body)
            (setq snake-direction 'up))))
       ;; Avoid hitting walls if no good direction to food
       ((= head-x 0) (setq snake-direction 'right))
       ((= head-x (1- matrix-width)) (setq snake-direction 'left))
       ((= head-y 0) (setq snake-direction 'down))
       ((= head-y (1- matrix-height)) (setq snake-direction 'up))))))

(defun update-snake ()
  "Update snake animation frame."
  (when (and matrix-rain-buffer (buffer-live-p matrix-rain-buffer))
    (with-current-buffer matrix-rain-buffer
      (let ((was-readonly buffer-read-only))
        (when was-readonly (read-only-mode -1))
        (erase-buffer)

        ;; Ensure proper window sizing
        (let ((win (get-buffer-window matrix-rain-buffer)))
          (when win
            (setq matrix-width (max 30 (- (window-width win) 2)))
            (setq matrix-height (max 15 (- (window-height win) 1)))))

        ;; AI control
        (snake-auto-turn)

        ;; Move snake
        (snake-move)

        ;; Create empty grid
        (dotimes (row matrix-height)
          (insert " ")  ; Left padding
          (dotimes (col matrix-width)
            (insert " "))
          (when (< row (1- matrix-height))
            (insert "\n")))

        ;; Draw food
        (when snake-food
          (let ((pos (+ (* (cdr snake-food) (+ matrix-width 1))
                        (car snake-food) 1)))
            (when (and (>= pos 1) (<= pos (point-max)))
              (goto-char pos)
              (delete-char 1)
              (insert (propertize "●" 'face '(:foreground "#fd971f"))))))

        ;; Draw snake
        (dolist (segment snake-body)
          (let ((pos (+ (* (cdr segment) (+ matrix-width 1))
                        (car segment) 1)))
            (when (and (>= pos 1) (<= pos (point-max)))
              (goto-char pos)
              (delete-char 1)
              (if (eq segment (car snake-body))
                  ;; Head
                  (insert (propertize "█" 'face '(:foreground "#00ff00" :weight bold)))
                ;; Body
                (insert (propertize "▪" 'face '(:foreground "#48dbfb")))))))

        ;; Show score and mode
        (goto-char (point-max))
        (insert (format "\n Score: %d  " snake-score))
        (let ((color (cond
                      ((string= current-assistant-mode "claude") "#48dbfb")
                      ((string= current-assistant-mode "gpt") "#a29bfe")
                      ((string= current-assistant-mode "gemini") "#4CAF50")
                      (t "#888888")))
              (label (cond
                      ((string= current-assistant-mode "claude") "[CLAUDE]")
                      ((string= current-assistant-mode "gpt") "[CODEX]")
                      ((string= current-assistant-mode "gemini") "[GEMINI]")
                      (t "[UNKNOWN]"))))
          (insert (propertize label 'face `(:foreground ,color :weight bold))))

        (when was-readonly (read-only-mode 1))))))

(defun start-snake-animation ()
  "Start the snake animation."
  (when snake-timer
    (cancel-timer snake-timer))
  (init-snake)
  (setq snake-timer (run-at-time "0 sec" 0.15 'update-snake)))

(defun stop-snake-animation ()
  "Stop the snake animation."
  (when snake-timer
    (cancel-timer snake-timer)
    (setq snake-timer nil)))

;; Animation configuration for auto-discovery
(defvar snake-animation-config
  (list :name "Snake Game"
        :start-function 'start-snake-animation
        :stop-function 'stop-snake-animation)
  "Configuration for snake animation.")

(provide 'snake)
;;; snake.el ends here