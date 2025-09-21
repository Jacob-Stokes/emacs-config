;;; rainbow.el --- Rainbow logo animation -*- lexical-binding: t; -*-

;;; Commentary:
;; Provides rainbow animation for the VIBEMACS logo in dashboard.

;;; Code:

;; Rainbow animation variables
(defvar rainbow-colors '("#ff6b9d" "#feca57" "#c7ecee" "#48dbfb" "#0abde3" "#667eea" "#a29bfe"))
(defvar rainbow-timer nil)
(defvar rainbow-index 0)

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

(defun start-rainbow-animation ()
  "Start the rainbow animation for VIBEMACS logo"
  (when rainbow-timer
    (cancel-timer rainbow-timer))
  (setq rainbow-timer (run-at-time "0 sec" 0.15 'update-rainbow-logo)))

(defun stop-rainbow-animation ()
  "Stop the rainbow animation"
  (when rainbow-timer
    (cancel-timer rainbow-timer)
    (setq rainbow-timer nil)))

(provide 'rainbow)
;;; rainbow.el ends here