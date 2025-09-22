;;; matrix-rain.el --- Matrix rain animation -*- lexical-binding: t; -*-

;;; Commentary:
;; Provides matrix rain animation effect.

;;; Code:

;; Matrix rain animation variables
(defvar matrix-rain-timer nil)
(defvar matrix-rain-buffer nil)
(defvar matrix-columns nil)
(defvar matrix-chars "01ｦｱｳｴｵｶｷｹｺｻｼｽｾｿﾀﾂﾃﾅﾆﾇﾈﾊﾋﾎﾏﾐﾑﾒﾓﾔﾕﾗﾘﾜﾝ")
(defvar matrix-width 20)  ; Width of the matrix display
(defvar matrix-height 10) ; Height of the matrix display
;; Assistant mode display removed - cleaner animations

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
            (insert " ")  ; Add left padding
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
        ;; Animation complete
        (read-only-mode 1)))))

(defun start-matrix-rain-animation ()
  "Start the matrix rain animation"
  (when matrix-rain-timer
    (cancel-timer matrix-rain-timer))
  (init-matrix-rain)  ; Initialize the matrix columns
  (setq matrix-rain-timer (run-at-time "0 sec" 0.1 'update-matrix-rain)))

(defun stop-matrix-rain-animation ()
  "Stop the matrix rain animation"
  (when matrix-rain-timer
    (cancel-timer matrix-rain-timer)
    (setq matrix-rain-timer nil)))

;; Animation configuration for auto-discovery
(defvar matrix-rain-animation-config
  (list :name "Matrix Rain"
        :start-function 'start-matrix-rain-animation
        :stop-function 'stop-matrix-rain-animation)
  "Configuration for matrix rain animation.")

(provide 'matrix-rain)
;;; matrix-rain.el ends here