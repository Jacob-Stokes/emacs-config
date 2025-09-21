;;; vibes-dashboard.el --- Dashboard and welcome screen -*- lexical-binding: t; -*-

;;; Commentary:
;; Dashboard configuration and welcome buffer.

;;; Code:

(require 'vibes-animations)

;; Dashboard - Better startup screen
(use-package dashboard
  :ensure t
  :config
  (setq dashboard-banner-logo-title "[ Vibes + Emacs = Maximum Flow ]"
        dashboard-startup-banner 'logo
        dashboard-center-content t
        dashboard-show-shortcuts t
        dashboard-set-heading-icons nil
        dashboard-set-file-icons nil
        dashboard-items '((recents  . 10)
                         (projects . 5)
                         (agenda . 5))
        dashboard-week-agenda t
        dashboard-filter-agenda-entry 'dashboard-no-filter-agenda)
  
  ;; Custom ASCII art banner
  (setq dashboard-banner-logo-title nil)
  (setq dashboard-startup-banner nil)
  
  ;; Custom footer
  (setq dashboard-footer-messages 
        '("Happy coding! 🚀"
          "Code with passion! ✨"
          "Build something amazing! 💫"
          "Stay curious! 🌟"))
  
  ;; Initialize dashboard
  (dashboard-setup-startup-hook)
  
  ;; Override the banner function to use our custom ASCII art
  (defun dashboard-insert-banner ()
    "Insert custom VIBEMACS ASCII art banner."
    (goto-char (point-max))
    (insert "\n")
    (insert "   ██╗   ██╗██╗██████╗ ███████╗███╗   ███╗ █████╗  ██████╗███████╗\n")
    (insert "   ██║   ██║██║██╔══██╗██╔════╝████╗ ████║██╔══██╗██╔════╝██╔════╝\n")
    (insert "   ██║   ██║██║██████╔╝█████╗  ██╔████╔██║███████║██║     ███████╗\n")
    (insert "   ╚██╗ ██╔╝██║██╔══██╗██╔══╝  ██║╚██╔╝██║██╔══██║██║     ╚════██║\n")
    (insert "    ╚████╔╝ ██║██████╔╝███████╗██║ ╚═╝ ██║██║  ██║╚██████╗███████║\n")
    (insert "     ╚═══╝  ╚═╝╚═════╝ ╚══════╝╚═╝     ╚═╝╚═╝  ╚═╝ ╚═════╝╚══════╝\n")
    (insert "\n")
    (insert "  " (propertize "[ Vibes + Emacs = Maximum Flow ]" 'face '(:foreground "cyan" :weight bold)))
    (insert "\n\n")))

;; Function to create welcome buffer
(defun create-welcome-buffer ()
  "Create a welcome buffer with instructions"
  (let ((buf (get-buffer-create "*Welcome*")))
    (with-current-buffer buf
      (erase-buffer)
      (insert "\n")
      (insert "   ██╗   ██╗██╗██████╗ ███████╗███╗   ███╗ █████╗  ██████╗███████╗\n")
      (insert "   ██║   ██║██║██╔══██╗██╔════╝████╗ ████║██╔══██╗██╔════╝██╔════╝\n")
      (insert "   ██║   ██║██║██████╔╝█████╗  ██╔████╔██║███████║██║     ███████╗\n")
      (insert "   ╚██╗ ██╔╝██║██╔══██╗██╔══╝  ██║╚██╔╝██║██╔══██║██║     ╚════██║\n")
      (insert "    ╚████╔╝ ██║██████╔╝███████╗██║ ╚═╝ ██║██║  ██║╚██████╗███████║\n")
      (insert "     ╚═══╝  ╚═╝╚═════╝ ╚══════╝╚═╝     ╚═╝╚═╝  ╚═╝ ╚═════╝╚══════╝\n")
      (insert "\n")
      (insert "  " (propertize "[ Vibes + Emacs = Maximum Flow ]" 'face '(:foreground "cyan" :weight bold)))
      (insert "\n\n")
      (insert "  Quick Keys:\n")
      (insert "  ─────────────────────────────────────────────────────────────\n")
      (insert "  " (propertize "C-c l" 'face 'font-lock-keyword-face) "        → Launch VS Code Layout\n")
      (insert "  " (propertize "C-c c/g/j" 'face 'font-lock-keyword-face) "    → Switch AI terminals (Claude/Codex/Gemini)\n")
      (insert "  " (propertize "C-c n" 'face 'font-lock-keyword-face) "        → Switch animation mode\n")
      (insert "  " (propertize "C-c t" 'face 'font-lock-keyword-face) "        → Open new terminal\n")
      (insert "  " (propertize "C-c f" 'face 'font-lock-keyword-face) "        → Fireplace mode\n")
      (insert "  " (propertize "M-o" 'face 'font-lock-keyword-face) "          → Jump to any window\n")
      (insert "  " (propertize "C-c p" 'face 'font-lock-keyword-face) "        → Projectile commands\n")
      (insert "  ─────────────────────────────────────────────────────────────\n")
      (read-only-mode 1))
    buf))

(provide 'vibes-dashboard)
;;; vibes-dashboard.el ends here
