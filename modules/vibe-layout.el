;;; vibe-layout.el --- Layout management for VibEmacs -*- lexical-binding: t; -*-

;;; Commentary:
;; Provides a registry and selection utilities for VibEmacs window layouts.
;; Individual layouts live under modules/layouts/ and call `vibe-register-layout`.

;;; Code:

(require 'cl-lib)

(defgroup vibe-layout nil
  "Layout selection and orchestration for VibEmacs."
  :group 'convenience
  :prefix "vibe-layout-")

(defconst vibe-layout-directory
  (expand-file-name "modules/layouts" user-emacs-directory)
  "Directory containing VibEmacs layout definition files.")

;; Ensure layout modules are discoverable via `require`.
(add-to-list 'load-path vibe-layout-directory)

(defvar vibe-layout-definitions nil
  "Alist mapping layout symbols to metadata plists.")

(defvar vibe-current-layout nil
  "Symbol for the currently active VibEmacs layout.")

(defcustom vibe-layout-default 'vibe-main-layout
  "Default VibEmacs layout to apply with `vibe-layout-apply-current`."
  :type 'symbol
  :group 'vibe-layout
  :set (lambda (sym value)
         (set-default sym value)
         (when (featurep 'vibe-layout)
           (setq vibe-current-layout value))))

(defcustom vibe-layout-apply-default-on-startup t
  "When non-nil, apply the default layout automatically after startup."
  :type 'boolean
  :group 'vibe-layout)

(define-obsolete-variable-alias 'my/setup-vscode-layout-on-startup
  'vibe-layout-apply-default-on-startup "2024-05-15")

(defun vibe-register-layout (symbol plist)
  "Register a VibEmacs layout SYMBOL with metadata PLIST.
PLIST must include :title (string) and :apply (function). Optionally
include :description (string), :init (function executed at registration), and
optionally :startup (function used during automatic startup application)."
  (let ((title (plist-get plist :title))
        (apply-fn (plist-get plist :apply))
        (init-fn (plist-get plist :init)))
    (unless (and title (stringp title))
      (error "Layout %s missing :title" symbol))
    (unless (functionp apply-fn)
      (error "Layout %s missing :apply function" symbol))
    (setq vibe-layout-definitions (assq-delete-all symbol vibe-layout-definitions))
    (push (cons symbol plist) vibe-layout-definitions)
    (when (functionp init-fn)
      (funcall init-fn))
    (when (and (null vibe-current-layout)
               (eq symbol vibe-layout-default))
      (setq vibe-current-layout symbol))))

(defun vibe-layout--entries ()
  "Return registered layout entries."
  (nreverse vibe-layout-definitions))

(defun vibe-layout--ensure-discovered ()
  "Load all layout modules from `vibe-layout-directory`."
  (when (and (file-directory-p vibe-layout-directory)
             (null vibe-layout-definitions))
    (dolist (file (directory-files vibe-layout-directory nil "\\.el$"))
      (require (intern (file-name-sans-extension file))))))

(defun vibe-layout--entry (layout)
  "Return metadata entry for LAYOUT symbol."
  (assoc layout vibe-layout-definitions))

(defun vibe-layout--call-apply (entry)
  "Invoke the :apply function from layout ENTRY."
  (let ((apply-fn (plist-get (cdr entry) :apply)))
    (funcall apply-fn)))

(defun vibe-layout-available-layouts ()
  "Return a list of registered layout symbols."
  (mapcar #'car (vibe-layout--entries)))

(defun vibe-layout-apply (layout)
  "Apply VibEmacs LAYOUT (symbol)."
  (interactive
   (list (vibe-layout--read-layout "Apply layout")))
  (let ((entry (vibe-layout--entry layout)))
    (unless entry
      (user-error "Unknown layout: %s" layout))
    (setq vibe-current-layout layout)
    (vibe-layout--call-apply entry)
    (message "Loaded layout: %s" (plist-get (cdr entry) :title))))

(defun vibe-layout-apply-current (&optional force-default)
  "Reapply the current layout.
When FORCE-DEFAULT is non-nil, apply `vibe-layout-default` instead."
  (interactive "P")
  (let ((layout (if force-default
                    vibe-layout-default
                  (or vibe-current-layout vibe-layout-default))))
    (unless layout
      (user-error "No layout configured"))
    (vibe-layout-apply layout)))

(defun vibe-layout-switch ()
  "Prompt for a layout and apply it."
  (interactive)
  (vibe-layout-apply (vibe-layout--read-layout "Switch to layout")))

(defun vibe-layout-describe ()
  "Display available layouts in the echo area."
  (interactive)
  (if (null vibe-layout-definitions)
      (message "No layouts registered")
    (let ((descriptions
           (mapcar (lambda (entry)
                     (let* ((sym (car entry))
                            (meta (cdr entry))
                            (title (plist-get meta :title))
                            (desc (plist-get meta :description)))
                       (format "%s (%s)%s" title sym
                               (if desc (concat " – " desc) ""))))
                   (vibe-layout--entries))))
      (message "%s" (string-join descriptions " | ")))))

(defun vibe-layout--read-layout (prompt)
  "Read a layout symbol from the minibuffer using PROMPT."
  (vibe-layout--ensure-discovered)
  (let* ((entries (vibe-layout--entries))
         (collection
          (mapcar (lambda (entry)
                    (let* ((sym (car entry))
                           (meta (cdr entry))
                           (title (plist-get meta :title))
                           (desc (plist-get meta :description))
                           (display (if desc
                                        (format "%s (%s) – %s" title sym desc)
                                      (format "%s (%s)" title sym))))
                      (cons display sym)))
                  entries))
         (choice (completing-read prompt collection nil t)))
    (or (cdr (assoc choice collection))
        vibe-layout-default)))

(defun vibe-layout-initialize ()
  "Discover layouts and install keybindings."
  (vibe-layout--ensure-discovered)
  (setq vibe-current-layout (or vibe-current-layout vibe-layout-default))
  (unless (key-binding (kbd "C-c l"))
    (global-set-key (kbd "C-c l") #'vibe-layout-apply-current))
  (global-set-key (kbd "C-c w l") #'vibe-layout-switch)
  (add-hook 'emacs-startup-hook #'vibe-layout--maybe-apply-default))

(defun vibe-layout--maybe-apply-default ()
  "Apply the default layout after startup when configured to do so."
  (remove-hook 'emacs-startup-hook #'vibe-layout--maybe-apply-default)
  (when (and vibe-layout-apply-default-on-startup vibe-layout-default)
    (let ((entry (vibe-layout--entry vibe-layout-default)))
      (when entry
        (let ((startup-fn (plist-get (cdr entry) :startup)))
          (setq vibe-current-layout vibe-layout-default)
          (if (functionp startup-fn)
              (funcall startup-fn)
            (vibe-layout--call-apply entry)))))))

(vibe-layout-initialize)

(provide 'vibe-layout)
;;; vibe-layout.el ends here
