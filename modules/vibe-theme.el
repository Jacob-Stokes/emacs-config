;;; vibe-theme.el --- Theme management for VibEmacs -*- lexical-binding: t; -*-

;;; Commentary:
;; Central registry and helper utilities for VibEmacs theme variants.
;; Themes can be added by dropping definition files into modules/themes/
;; and calling `vibe-register-theme` inside them.

;;; Code:

(require 'cl-lib)

(defgroup vibe-theme nil
  "Appearance customization for VibEmacs."
  :group 'faces
  :prefix "vibe-theme-")

(defconst vibe-theme-directory
  (expand-file-name "modules/themes" user-emacs-directory)
  "Directory containing VibEmacs theme definition files.")

;; Ensure theme modules can be required.
(add-to-list 'load-path vibe-theme-directory)

(defvar vibe-theme-definitions nil
  "Alist mapping theme symbols to their metadata plists.")

(defvar vibe-current-theme nil
  "Currently active VibEmacs theme symbol.")

(defvar vibe-theme--transparency-hooks-installed nil
  "Whether transparency hooks have been installed.")

(defun vibe-theme-apply-transparency (&optional frame)
  "Apply transparent background tweaks to FRAME (or the selected frame)."
  (let ((frame (or frame (selected-frame))))
    (dolist (face '(default line-number line-number-current-line))
      (when (facep face)
        (set-face-attribute face frame :background "unspecified-bg")))))

(defun vibe-theme-ensure-transparency-hooks ()
  "Install hooks that keep transparency in sync across frames."
  (unless vibe-theme--transparency-hooks-installed
    (add-hook 'after-make-frame-functions #'vibe-theme-apply-transparency)
    (add-hook 'window-setup-hook #'vibe-theme-apply-transparency)
    (setq vibe-theme--transparency-hooks-installed t)))

(defun vibe-register-theme (symbol plist)
  "Register a VibEmacs theme SYMBOL with metadata PLIST.
PLIST must include at least :title (string) and :apply (function)."
  (let ((title (plist-get plist :title))
        (apply-fn (plist-get plist :apply)))
    (unless (and title (stringp title))
      (error "Theme %s missing :title" symbol))
    (unless (functionp apply-fn)
      (error "Theme %s missing :apply function" symbol))
    (setq vibe-theme-definitions (assq-delete-all symbol vibe-theme-definitions))
    (push (cons symbol plist) vibe-theme-definitions)))

(defun vibe-theme--available-symbols ()
  "Return list of registered theme symbols."
  (mapcar #'car vibe-theme-definitions))

(defcustom vibe-theme-default 'vibe-tango-glass
  "Default VibEmacs theme to load during startup."
  :type 'symbol
  :group 'vibe-theme
  :set (lambda (sym value)
         (set-default sym value)
         (when (featurep 'vibe-theme)
           (vibe-apply-theme value))))

(defun vibe-apply-theme (&optional theme)
  "Apply VibEmacs THEME (symbol). Defaults to `vibe-theme-default'."
  (let* ((theme (or theme vibe-theme-default))
         (entry (assoc theme vibe-theme-definitions)))
    (unless entry
      (error "Unknown VibEmacs theme: %s" theme))
    ;; Disable any enabled custom themes to ensure a clean slate.
    (mapc #'disable-theme custom-enabled-themes)
    (let ((apply-fn (plist-get (cdr entry) :apply)))
      (funcall apply-fn))
    (setq vibe-current-theme theme)
    (message "Loaded VibEmacs theme: %s" (plist-get (cdr entry) :title))))

(defun vibe-discover-themes ()
  "Discover and load theme definition modules from `vibe-theme-directory'."
  (when (file-directory-p vibe-theme-directory)
    (dolist (file (directory-files vibe-theme-directory nil "\\.el$"))
      (let ((module-name (intern (file-name-sans-extension file))))
        (require module-name))))
  (when (null vibe-theme-definitions)
    (message "No VibEmacs themes registered. Add files to %s" vibe-theme-directory)))

;; Discover bundled themes and apply the default immediately.
(vibe-discover-themes)
(vibe-apply-theme vibe-theme-default)

(provide 'vibe-theme)
;;; vibe-theme.el ends here
