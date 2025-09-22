;;; vibe-welcome.el --- Startup dashboard and welcome buffer helpers -*- lexical-binding: t; -*-

;;; Commentary:
;; Extracted dashboard configuration and welcome buffer rendering for VibEmacs.

;;; Code:

(require 'use-package)
(require 'vibe-animations)

(use-package dashboard
  :config
  (setq dashboard-banner-logo-title "[ Vibes + Emacs = Maximum Flow ]"
        dashboard-startup-banner 'logo
        dashboard-center-content t
        dashboard-items '((recents  . 5)
                          (projects . 5))
        dashboard-set-heading-icons nil
        dashboard-set-file-icons nil
        dashboard-week-agenda nil
        dashboard-filter-agenda-entry 'dashboard-no-filter-agenda
        dashboard-footer-messages '("Ready to vibe? 🌈"))

  (defun dashboard-insert-custom-banner ()
    "Insert the VibEmacs ASCII banner with rainbow effect."
    (insert "\n\n\n")
    (dashboard-insert-center
     "██╗   ██╗██╗██████╗ ███████╗███╗   ███╗ █████╗  ██████╗███████╗\n"
     "██║   ██║██║██╔══██╗██╔════╝████╗ ████║██╔══██╗██╔════╝██╔════╝\n"
     "██║   ██║██║██████╔╝█████╗  ██╔████╔██║███████║██║     ███████╗\n"
     "╚██╗ ██╔╝██║██╔══██╗██╔══╝  ██║╚██╔╝██║██╔══██║██║     ╚════██║\n"
     " ╚████╔╝ ██║██████╔╝███████╗██║ ╚═╝ ██║██║  ██║╚██████╗███████║\n"
     "  ╚═══╝  ╚═╝╚═════╝ ╚══════╝╚═╝     ╚═╝╚═╝  ╚═╝ ╚═════╝╚══════╝")
    (insert "\n"))

  (advice-add 'dashboard-insert-banner :override #'dashboard-insert-custom-banner)
  (dashboard-setup-startup-hook)
  (add-hook 'dashboard-after-initialize-hook #'start-rainbow-animation))

(defun create-welcome-buffer ()
  "Create a welcome buffer with VibEmacs tips."
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
      (insert "    C-x t t     Open/Close file tree (Treemacs)\n")
      (insert "    C-c T       Quick jump to Treemacs window\n")
      (insert "    C-x C-f     Open file\n")
      (insert "    C-x b       Switch buffer\n")
      (insert "    C-x 3       Split vertically\n")
      (insert "    C-x 2       Split horizontally\n")
      (insert "    C-x o       Switch windows\n")
      (insert "    C-x 1       Maximize current window\n")
      (insert "    M-x vterm   Open terminal\n\n")
      (insert "  Project Management:\n")
      (insert "  ─────────────────────────────────────────────────────────────\n")
      (insert "    C-c p p     Switch project\n")
      (insert "    C-c p f     Find file in project\n")
      (insert "    C-c p s g   Search in project\n\n")
      (insert "  File Tree (Treemacs):\n")
      (insert "  ─────────────────────────────────────────────────────────────\n")
      (insert "    TAB         Expand/Collapse\n")
      (insert "    RET         Open file\n")
      (insert "    cf          Create file\n")
      (insert "    cd          Create directory\n")
      (insert "    d           Delete\n")
      (insert "    R           Rename\n\n")
      (insert "  Terminal:\n")
      (insert "  ─────────────────────────────────────────────────────────────\n")
      (insert "    Type 'claude' in terminal to start Claude\n")
      (insert "    C-c a m     Main terminal\n")
      (insert "    C-c a s     Shell terminal\n")
      (insert "    C-c a e     EShell terminal\n")
      (insert "    C-c a d     Docker container manager\n")
      (insert "    C-c w s     Swap editor <-> terminal pane\n\n")
      (insert "  Vibe Features:\n")
      (insert "  ─────────────────────────────────────────────────────────────\n")
      (insert "    M-x fireplace        Start cozy fireplace\n")
      (insert "    M-x imenu-list       Toggle function outline\n")
      (insert "    🌈 Colors auto-preview in code\n")
      (insert "    🐱 Nyan cat in modeline shows position\n")
      (goto-char (point-min))
      (display-line-numbers-mode -1)
      (read-only-mode 1))
    buf))

(provide 'vibe-welcome)
;;; vibe-welcome.el ends here
