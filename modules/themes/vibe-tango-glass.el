;;; vibe-tango-glass.el --- Default Tango glass theme for VibEmacs -*- lexical-binding: t; -*-

;;; Commentary:
;; Provides the default semi-transparent Tango-based theme used by VibEmacs.

;;; Code:

(defun vibe-theme--tango-glass-apply-treemacs-faces ()
  "Tint Treemacs faces to match the Tango glass palette."
  (dolist (spec '((treemacs-root-face        :foreground "#ff6b9d" :background "unspecified-bg")
                  (treemacs-file-face        :foreground "#a6e22e" :background "unspecified-bg")
                  (treemacs-directory-face   :foreground "#66d9ef" :background "unspecified-bg")
                  (treemacs-git-modified-face :foreground "#fd971f")
                  (treemacs-git-untracked-face :foreground "#75715e")))
    (apply #'set-face-attribute (car spec) nil (cdr spec)))
  ;; Custom faces for dotfiles/dotfolders if they exist.
  (when (facep 'treemacs-dotfolder-face)
    (set-face-attribute 'treemacs-dotfolder-face nil :foreground "#a29bfe" :weight 'bold))
  (when (facep 'treemacs-dotfile-face)
    (set-face-attribute 'treemacs-dotfile-face nil :foreground "#8e7cc3")))

(defun vibe-theme--apply-tango-glass ()
  "Internal helper that configures the Tango glass theme."
  (load-theme 'tango t)
  (vibe-theme-ensure-transparency-hooks)
  (vibe-theme-apply-transparency)
  (custom-theme-set-faces
   'user
   '(default ((t (:foreground "#E0E0E0"))))
   '(font-lock-comment-face ((t (:foreground "#75715e"))))
   '(font-lock-function-name-face ((t (:foreground "#fd971f"))))
   '(font-lock-keyword-face ((t (:foreground "#66d9ef"))))
   '(font-lock-string-face ((t (:foreground "#a6e22e"))))
   '(hl-line ((t (:background "unspecified" :underline t)))))
  ;; Apply Treemacs tint immediately if available and also after it loads.
  (when (featurep 'treemacs)
    (vibe-theme--tango-glass-apply-treemacs-faces))
  (with-eval-after-load 'treemacs
    (vibe-theme--tango-glass-apply-treemacs-faces)))

(vibe-register-theme
 'vibe-tango-glass
 '(:title "Tango Glass"
   :description "Tango base with neon accents and transparent background."
   :apply vibe-theme--apply-tango-glass))

(provide 'vibe-tango-glass)
;;; vibe-tango-glass.el ends here
