;;; aquarium.el --- Aquarium animation with fish and bubbles -*- lexical-binding: t; -*-

;;; Commentary:
;; Provides aquarium animation with swimming fish and rising bubbles.

;;; Code:

;; Need matrix-rain for shared buffer and dimensions
(require 'matrix-rain)

;; Aquarium animation variables
(defvar aquarium-timer nil)
(defvar aquarium-fishes nil)
(defvar aquarium-bubbles nil)

(defun init-aquarium ()
  "Initialize the aquarium with fish and bubbles"
  (let ((win (get-buffer-window matrix-rain-buffer)))
    (when win
      (setq matrix-width (- (window-width win) 2))
      (setq matrix-height (- (window-height win) 1))))
  ;; Create fish with random positions and directions
  (setq aquarium-fishes
        (list (list :x 5 :y 3 :dir 1 :type "<><")
              (list :x 15 :y 5 :dir -1 :type "><>")
              (list :x 10 :y 7 :dir 1 :type "<°))))><")))
  ;; Create bubbles
  (setq aquarium-bubbles
        (list (list :x 8 :y (- matrix-height 2))
              (list :x 20 :y (- matrix-height 4))
              (list :x 14 :y (- matrix-height 1)))))

(defun update-aquarium ()
  "Update the aquarium animation"
  (when (and matrix-rain-buffer (buffer-live-p matrix-rain-buffer))
    (with-current-buffer matrix-rain-buffer
      (read-only-mode -1)
      (erase-buffer)
      ;; Draw water background
      (dotimes (y matrix-height)
        (insert " ")  ; Add left padding
        (dotimes (x matrix-width)
          (insert (if (= (random 20) 0) "~" " ")))
        (insert "\n"))
      ;; Draw fish
      (dolist (fish aquarium-fishes)
        (let ((x (plist-get fish :x))
              (y (plist-get fish :y))
              (dir (plist-get fish :dir))
              (type (plist-get fish :type)))
          ;; Move fish
          (plist-put fish :x (+ x dir))
          ;; Reverse direction at edges
          (when (or (<= (plist-get fish :x) 0)
                    (>= (plist-get fish :x) (- matrix-width (length type))))
            (plist-put fish :dir (- dir)))
          ;; Draw fish at position
          (when (and (>= y 0) (< y matrix-height)
                     (>= x 0) (< x matrix-width))
            (goto-char (+ (* y (1+ matrix-width)) x 1))
            (delete-char (length type))
            (insert (propertize type 'face '(:foreground "orange"))))))
      ;; Draw bubbles
      (dolist (bubble aquarium-bubbles)
        (let ((x (plist-get bubble :x))
              (y (plist-get bubble :y)))
          ;; Move bubble up
          (plist-put bubble :y (1- y))
          ;; Reset bubble at bottom when it reaches top
          (when (< (plist-get bubble :y) 0)
            (plist-put bubble :y (- matrix-height 1))
            (plist-put bubble :x (random matrix-width)))
          ;; Draw bubble
          (when (and (>= y 0) (< y matrix-height)
                     (>= x 0) (< x matrix-width))
            (goto-char (+ (* y (1+ matrix-width)) x 1))
            (delete-char 1)
            (insert (propertize "o" 'face '(:foreground "cyan"))))))
      ;; Add mode indicator
      (goto-char (point-max))
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
      (read-only-mode 1))))

(defun start-aquarium-animation ()
  "Start the aquarium animation"
  (when aquarium-timer
    (cancel-timer aquarium-timer))
  (init-aquarium)
  (setq aquarium-timer (run-at-time "0 sec" 0.2 'update-aquarium)))

(defun stop-aquarium-animation ()
  "Stop the aquarium animation"
  (when aquarium-timer
    (cancel-timer aquarium-timer)
    (setq aquarium-timer nil)))

(provide 'aquarium)
;;; aquarium.el ends here