;;; vibes-aesthetic.el --- Visual enhancements and vibe features -*- lexical-binding: t; -*-

;;; Commentary:
;; Fun visual enhancements for a better coding vibe.

;;; Code:

;; Beacon - Flash cursor when switching windows
(use-package beacon
  :config
  (beacon-mode 1)
  (setq beacon-blink-when-window-scrolls t
        beacon-blink-when-window-changes t
        beacon-blink-when-point-moves nil
        beacon-blink-duration 0.3
        beacon-size 20
        beacon-color "#ff6b9d"))

;; Rainbow-mode - Colorize color codes
(use-package rainbow-mode
  :hook (prog-mode . rainbow-mode))

;; Highlight indent guides - Visual indent lines
(use-package highlight-indent-guides
  :hook (prog-mode . highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'character
        highlight-indent-guides-character ?\│
        highlight-indent-guides-responsive 'stack))

;; Nyan Mode - Nyan cat progress in modeline
(use-package nyan-mode
  :config
  (nyan-mode 1)
  (setq nyan-wavy-trail t
        nyan-animate-nyancat t))

;; Rainbow delimiters - Colorful parentheses
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Fireplace - Cozy fireplace
(use-package fireplace
  :commands fireplace)

;; Imenu-list - Better than minimap for terminal
(use-package imenu-list
  :config
  (setq imenu-list-focus-after-activation t
        imenu-list-auto-resize t
        imenu-list-size 0.15))

(provide 'vibes-aesthetic)
;;; vibes-aesthetic.el ends here
