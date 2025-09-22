;;; vibe-testing-layout.el --- Simple testing layout -*- lexical-binding: t; -*-

;;; Commentary:
;; Provides a lightweight two-pane layout useful for running tests or quick checks.

;;; Code:

(require 'cl-lib)

(declare-function vibe-register-layout "vibe-layout" (symbol plist))

(defun vibe-testing-layout--select-buffer (preferred)
  "Return a reasonable buffer to show in the main window.
PREFERRED is used when non-nil and live."
  (cond
   ((and preferred (buffer-live-p preferred)) preferred)
   ((cl-find-if (lambda (buf)
                 (let ((name (buffer-name buf)))
                   (and name
                        (not (string-prefix-p " *" name))
                        (not (string-match "\\*treemacs\\*\\|Treemacs" name)))))
               (buffer-list)))
   (t (get-buffer-create "*scratch*"))))

(defun vibe-testing-layout-apply (&optional preferred-buffer)
  "Apply the testing layout.
Displays the main buffer above and a shared test buffer below."
  (interactive)
  (let* ((main-buffer (vibe-testing-layout--select-buffer preferred-buffer))
         (test-buffer (get-buffer-create "*Vibe Test*")))
    (delete-other-windows)
    (switch-to-buffer main-buffer)
    (let* ((main-window (selected-window))
           (bottom-window (split-window main-window (max 12 (floor (* (window-height main-window) 0.55))) 'below)))
      (when (window-live-p bottom-window)
        (select-window bottom-window)
        (switch-to-buffer test-buffer)
        (display-line-numbers-mode -1)
        (setq mode-line-format '(" Vibe Test "))
        (select-window main-window)))))

(vibe-register-layout
 'vibe-testing-layout
 '(:title "Testing"
   :description "Two-pane layout: code above, *Vibe Test* buffer below."
   :apply vibe-testing-layout-apply))

(provide 'vibe-testing-layout)
;;; vibe-testing-layout.el ends here
