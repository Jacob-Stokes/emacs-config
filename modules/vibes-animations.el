;;; vibes-animations.el --- Animation system for VibEmacs -*- lexical-binding: t; -*-

;; Copyright (C) 2024
;; Author: VibEmacs Configuration

;;; Commentary:
;; This module provides the animation system including matrix rain,
;; aquarium, rainbow effects, and animation switching.

;;; Code:

;; Rainbow animation variables - muted/pastel rainbow
(defvar rainbow-colors '("#ff6b9d" "#feca57" "#c7ecee" "#48dbfb" "#0abde3" "#667eea" "#a29bfe"))
(defvar rainbow-timer nil)
(defvar rainbow-index 0)

;; Animation system variables
(defvar animation-modes '("matrix" "aquarium"))  ; Available animation modes
(defvar current-animation-mode "matrix")  ; Current animation mode
(defvar animation-switch-timer nil)  ; Timer for automatic switching
(defvar animation-switch-interval 30)  ; Seconds between switches

;; Matrix rain animation variables
(defvar matrix-rain-timer nil)
(defvar matrix-rain-buffer nil)
(defvar matrix-columns nil)
(defvar matrix-chars "01ｦｱｳｴｵｶｷｹｺｻｼｽｾｿﾀﾂﾃﾅﾆﾇﾈﾊﾋﾎﾏﾐﾑﾒﾓﾔﾕﾗﾘﾜﾝ")
(defvar matrix-width 20)  ; Width of the matrix display
(defvar matrix-height 10) ; Height of the matrix display
(defvar current-assistant-mode "claude")  ; Track which assistant is active

;; Aquarium animation variables
(defvar aquarium-timer nil)
(defvar aquarium-fishes nil)
(defvar aquarium-bubbles nil)

;; Function to update rainbow colors - works with both Welcome and Dashboard
(defun update-rainbow-logo ()
  "Update the VIBEMACS logo with rainbow effect"
  (let ((buffer (or (get-buffer "*dashboard*") (get-buffer "*Welcome*"))))
    (when buffer
      (with-current-buffer buffer
        (let ((was-readonly buffer-read-only))
          (when was-readonly (read-only-mode -1))
          (save-excursion
            (goto-char (point-min))
            ;; Find the logo start (look for the first ██)
            (when (search-forward "██╗" nil t)
              (beginning-of-line)
              (let ((colors (append (nthcdr rainbow-index rainbow-colors)
                                   (cl-subseq rainbow-colors 0 rainbow-index))))
                ;; Update each line with shifted colors
                (dotimes (i 6)
                  (let ((line-start (point))
                        (line-end (progn (end-of-line) (point))))
                    (add-text-properties line-start line-end
                                       `(face (:foreground ,(nth i colors) :weight bold)))
                    (forward-line 1))))))
          (when was-readonly (read-only-mode 1)))))
    (setq rainbow-index (mod (1+ rainbow-index) (length rainbow-colors)))))

;; Function to start rainbow animation
(defun start-rainbow-animation ()
  "Start the rainbow animation for VIBEMACS logo"
  (when rainbow-timer
    (cancel-timer rainbow-timer))
  (setq rainbow-timer (run-at-time "0 sec" 0.15 'update-rainbow-logo)))

;; Function to stop rainbow animation
(defun stop-rainbow-animation ()
  "Stop the rainbow animation"
  (when rainbow-timer
    (cancel-timer rainbow-timer)
    (setq rainbow-timer nil)))

;; Function to initialize matrix rain
(defun init-matrix-rain ()
  "Initialize the matrix rain columns"
  ;; Get actual window dimensions
  (let ((win (get-buffer-window matrix-rain-buffer)))
    (when win
      (setq matrix-width (- (window-width win) 2))  ; Account for margins
      (setq matrix-height (- (window-height win) 1))))  ; Account for mode line
  (setq matrix-columns (make-vector matrix-width nil))
  (dotimes (i matrix-width)
    (aset matrix-columns i (cons (random matrix-height) (random 256)))))

;; Function to create matrix rain buffer
(defun create-matrix-rain-buffer ()
  "Create buffer with matrix rain effect"
  (setq matrix-rain-buffer (get-buffer-create "*Matrix Rain*"))
  (with-current-buffer matrix-rain-buffer
    (erase-buffer)
    (display-line-numbers-mode -1)  ; No line numbers
    (setq cursor-type nil)  ; Hide cursor
    ;; Initialize with default size, will be updated when displayed
    (setq matrix-width 40)
    (setq matrix-height 15)
    (read-only-mode 1)))

;; Function to update matrix rain
(defun update-matrix-rain ()
  "Update the matrix rain animation"
  (when (and matrix-rain-buffer (buffer-live-p matrix-rain-buffer))
    (with-current-buffer matrix-rain-buffer
      ;; Update dimensions if window size changed
      (let ((win (get-buffer-window matrix-rain-buffer)))
        (when win
          (let ((new-width (- (window-width win) 2))  ; Account for margins
                (new-height (- (window-height win) 1)))  ; Account for mode line
            (when (or (/= new-width matrix-width)
                      (/= new-height matrix-height))
              (setq matrix-width new-width)
              (setq matrix-height new-height)
              (init-matrix-rain)))))
      (read-only-mode -1)
      (erase-buffer)
      ;; Create the matrix display
      (let ((display (make-vector matrix-height nil)))
        ;; Initialize empty lines
        (dotimes (y matrix-height)
          (aset display y (make-string matrix-width ? )))
        ;; Draw the matrix columns
        (dotimes (x matrix-width)
          (let* ((col-data (aref matrix-columns x))
                 (y (car col-data))
                 (intensity (cdr col-data)))
            ;; Draw character if within bounds
            (when (and (>= y 0) (< y matrix-height))
              (let* ((line (aref display y))
                     (char (aref matrix-chars (random (length matrix-chars))))
                     (color (cond
                            ((> intensity 200) "#00ff00")  ; Bright green
                            ((> intensity 150) "#00cc00")  ; Medium green
                            ((> intensity 100) "#009900")  ; Dark green
                            (t "#006600"))))              ; Very dark green
                (aset line x char)
                ;; Store the colored line back
                (aset display y line)))
            ;; Update column position and intensity
            (setcar col-data (if (>= y matrix-height)
                                (- (random 5) 10)  ; Reset to above screen
                              (1+ y)))  ; Move down
            (setcdr col-data (max 0 (- intensity 15)))))  ; Fade
        ;; Randomly boost some columns
        (when (< (random 100) 30)
          (let ((boost-col (random matrix-width)))
            (setcdr (aref matrix-columns boost-col) 255)))
        ;; Display the matrix
        (dotimes (y matrix-height)
          (let ((line (aref display y)))
            (dotimes (x matrix-width)
              (let* ((char (aref line x))
                     (col-data (aref matrix-columns x))
                     (col-y (car col-data))
                     (intensity (cdr col-data)))
                (if (= char ? )
                    (insert " ")
                  (let ((color (cond
                               ((= y col-y) "#00ff00")      ; Leading edge - bright
                               ((= y (1- col-y)) "#00cc00")  ; Second - medium
                               ((< y col-y) "#006600")       ; Trail - dark
                               (t "#004400"))))              ; Very dark
                    (insert (propertize (string char) 'face `(:foreground ,color)))))))
            (insert "\n")))
        ;; Add mode indicator at bottom
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
          (insert (propertize label 'face `(:foreground ,color :weight bold)))))
      (read-only-mode 1))))

;; Function to start matrix rain animation
(defun start-matrix-rain-animation ()
  "Start the matrix rain animation"
  (when matrix-rain-timer
    (cancel-timer matrix-rain-timer))
  (setq matrix-rain-timer (run-at-time "0 sec" 0.1 'update-matrix-rain)))

;; Function to stop matrix rain animation
(defun stop-matrix-rain-animation ()
  "Stop the matrix rain animation"
  (when matrix-rain-timer
    (cancel-timer matrix-rain-timer)
    (setq matrix-rain-timer nil)))

;; Aquarium animation functions
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

;; Master animation control
(defun switch-animation-mode ()
  "Switch to the next animation mode"
  ;; Stop current animation
  (cond
   ((string= current-animation-mode "matrix")
    (stop-matrix-rain-animation))
   ((string= current-animation-mode "aquarium")
    (stop-aquarium-animation)))
  ;; Move to next mode
  (let ((current-index (cl-position current-animation-mode animation-modes :test 'string=)))
    (setq current-animation-mode
          (nth (% (1+ current-index) (length animation-modes)) animation-modes)))
  ;; Start new animation
  (cond
   ((string= current-animation-mode "matrix")
    (init-matrix-rain)
    (start-matrix-rain-animation))
   ((string= current-animation-mode "aquarium")
    (start-aquarium-animation)))
  (message "Switched to %s animation" current-animation-mode))

(defun start-animation-switcher ()
  "Start automatic animation switching"
  (when animation-switch-timer
    (cancel-timer animation-switch-timer))
  (setq animation-switch-timer
        (run-at-time animation-switch-interval animation-switch-interval 'switch-animation-mode)))

(defun stop-animation-switcher ()
  "Stop automatic animation switching"
  (when animation-switch-timer
    (cancel-timer animation-switch-timer)
    (setq animation-switch-timer nil)))

(provide 'vibes-animations)
;;; vibes-animations.el ends here