;;; scroll-logo.el --- Scrolling VIBEMACS logo animation -*- lexical-binding: t; -*-

;;; Commentary:
;; Provides a scrolling VIBEMACS logo animation across the animation buffer.

;;; Code:

;; Animation uses shared buffer from vibe-animations

;; Scrolling logo animation variables
(defvar scroll-logo-timer nil)
(defvar scroll-logo-position 0)
(defvar scroll-logo-colors '("#ff6b9d" "#feca57" "#c7ecee" "#48dbfb" "#0abde3" "#667eea" "#a29bfe"))
(defvar scroll-logo-color-index 0)

;; VIBEMACS logo pattern (6 lines)
(defvar vibemacs-logo-lines
  '("██╗   ██╗██╗██████╗ ███████╗███╗   ███╗ █████╗  ██████╗███████╗"
    "██║   ██║██║██╔══██╗██╔════╝████╗ ████║██╔══██╗██╔════╝██╔════╝"
    "██║   ██║██║██████╔╝█████╗  ██╔████╔██║███████║██║     ███████╗"
    "╚██╗ ██╔╝██║██╔══██╗██╔══╝  ██║╚██╔╝██║██╔══██║██║     ╚════██║"
    " ╚████╔╝ ██║██████╔╝███████╗██║ ╚═╝ ██║██║  ██║╚██████╗███████║"
    "  ╚═══╝  ╚═╝╚═════╝ ╚══════╝╚═╝     ╚═╝╚═╝  ╚═╝ ╚═════╝╚══════╝"))

(defun update-scroll-logo ()
  "Update scrolling VIBEMACS logo animation"
  (when (and matrix-rain-buffer (buffer-live-p matrix-rain-buffer))
    (with-current-buffer matrix-rain-buffer
      (let ((was-readonly buffer-read-only))
        (when was-readonly (read-only-mode -1))
        (erase-buffer)

        ;; Ensure proper window sizing
        (let ((win (get-buffer-window matrix-rain-buffer)))
          (when win
            (setq matrix-width (max 80 (- (window-width win) 3)))  ; Account for padding
            (setq matrix-height (max 15 (- (window-height win) 1)))))

        ;; Calculate logo positioning
        (let* ((logo-width 65)  ; Width of VIBEMACS logo
               (logo-height 6)   ; Height of VIBEMACS logo
               (scroll-range (+ matrix-width logo-width))
               (logo-x (mod scroll-logo-position scroll-range))
               (logo-start-y (max 0 (/ (- matrix-height logo-height) 2))))  ; Center vertically

          ;; Create empty grid
          (dotimes (row matrix-height)
            (insert " ")  ; Add left padding
            (dotimes (col matrix-width)
              (insert " "))
            (when (< row (1- matrix-height))
              (insert "\n")))

          ;; Draw the scrolling logo
          (when (and (>= logo-x (- logo-width)) (< logo-x matrix-width))
            (dotimes (line-idx logo-height)
              (let* ((target-row (+ logo-start-y line-idx))
                     (logo-line (nth line-idx vibemacs-logo-lines))
                     (color (nth (mod (+ scroll-logo-color-index line-idx)
                                     (length scroll-logo-colors))
                                scroll-logo-colors)))
                (when (and (>= target-row 0) (< target-row matrix-height))
                  ;; Position cursor at the target row (accounting for padding)
                  (goto-char (1+ (* target-row (+ matrix-width 2))))  ; +2 for padding + newline
                  (forward-char (max 0 (+ logo-x 1)))  ; +1 for left padding

                  ;; Draw visible portion of logo line
                  (let* ((logo-line-len (length logo-line))
                         (start-char (max 0 (- logo-x)))
                         (end-char (min logo-line-len (+ start-char (- matrix-width (max 0 logo-x))))))
                    (when (and (< start-char end-char)
                               (>= start-char 0)
                               (<= start-char logo-line-len)
                               (>= end-char 0)
                               (<= end-char logo-line-len))
                      (let ((visible-text (substring logo-line start-char end-char)))
                        (delete-region (point) (min (+ (point) (length visible-text))
                                                   (line-end-position)))
                        (insert (propertize visible-text
                                           'face `(:foreground ,color :weight bold))))))))))

          ;; Update position and color cycling
          (setq scroll-logo-position (mod (+ scroll-logo-position 2) scroll-range))
          (when (= (mod scroll-logo-position 20) 0)  ; Change colors periodically
            (setq scroll-logo-color-index (mod (1+ scroll-logo-color-index)
                                              (length scroll-logo-colors)))))

        (when was-readonly (read-only-mode 1))))))

(defun start-scroll-logo-animation ()
  "Start the scrolling logo animation"
  (when scroll-logo-timer
    (cancel-timer scroll-logo-timer))
  (setq scroll-logo-position 0)
  (setq scroll-logo-color-index 0)
  (setq scroll-logo-timer (run-at-time "0 sec" 0.1 'update-scroll-logo)))

(defun stop-scroll-logo-animation ()
  "Stop the scrolling logo animation"
  (when scroll-logo-timer
    (cancel-timer scroll-logo-timer)
    (setq scroll-logo-timer nil)))

;; Animation configuration for auto-discovery
(defvar scroll-logo-animation-config
  (list :name "Scrolling Logo"
        :start-function 'start-scroll-logo-animation
        :stop-function 'stop-scroll-logo-animation)
  "Configuration for scrolling logo animation.")

(provide 'scroll-logo)
;;; scroll-logo.el ends here