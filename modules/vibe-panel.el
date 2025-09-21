;;; vibe-panel.el --- Auto-discovering AI panel system -*- lexical-binding: t; -*-

;; Copyright (C) 2024
;; Author: VibEmacs Configuration

;;; Commentary:
;; This module automatically discovers and manages AI panel modules.
;; Just drop a new .el file in panel-modules/ and it becomes available!

;;; Code:

(require 'vibe-animations)  ; For current-assistant-mode

;; Panel system variables
(defvar vibe-panels nil
  "List of discovered panel configurations.")

(defvar vibe-active-panel nil
  "Currently active panel name.")

(defvar vibe-panel-window nil
  "Window displaying the panel.")

(defvar vibe-panel-header-buffer nil
  "Buffer for panel header tabs.")

;; Panel sorting function
(defun vibe-sort-panels-with-order (panels order-list)
  "Sort PANELS using ORDER-LIST, with unlisted panels alphabetical at end."
  (let ((ordered-panels '())
        (remaining-panels panels))

    ;; First, add panels in the specified order
    (dolist (panel-name order-list)
      (let ((panel-entry (cl-find-if (lambda (p)
                                       (string= (plist-get (cdr p) :name) panel-name))
                                     remaining-panels)))
        (when panel-entry
          (push panel-entry ordered-panels)
          (setq remaining-panels (remove panel-entry remaining-panels)))))

    ;; Then add remaining panels alphabetically
    (let ((sorted-remaining (sort remaining-panels
                                  (lambda (a b)
                                    (string< (plist-get (cdr a) :name)
                                            (plist-get (cdr b) :name))))))
      (append (reverse ordered-panels) sorted-remaining))))

;; Auto-discovery functions
(defun vibe-discover-panels ()
  "Automatically discover all panel modules."
  (let ((panel-dir (expand-file-name "modules/panel-modules" user-emacs-directory))
        (panels '()))

    ;; Add panel-modules to load path
    (add-to-list 'load-path panel-dir)

    ;; Find all .el files in panel-modules
    (when (file-directory-p panel-dir)
      (dolist (file (directory-files panel-dir nil "\\.el$"))
        (let* ((module-name (file-name-sans-extension file))
               (config-var (intern (concat module-name "-panel-config"))))

          ;; Load the module
          (require (intern module-name))

          ;; Get its configuration
          (when (boundp config-var)
            (let ((config (symbol-value config-var)))
              (push (cons module-name config) panels))))))

    ;; Load custom ordering if available
    (let ((order-list nil))
      (when (file-exists-p (expand-file-name "order.el" panel-dir))
        (require 'order)
        (when (boundp 'vibe-panel-order)
          (setq order-list vibe-panel-order)))

      ;; Sort panels using custom order, falling back to alphabetical
      (setq vibe-panels
            (if order-list
                (vibe-sort-panels-with-order panels order-list)
              (sort panels (lambda (a b)
                             (string< (plist-get (cdr a) :name)
                                     (plist-get (cdr b) :name)))))))

    (message "Discovered %d panels: %s"
             (length vibe-panels)
             (mapconcat (lambda (p) (plist-get (cdr p) :name)) vibe-panels ", "))))

(defun vibe-setup-panel-keybindings ()
  "Set up keybindings for all discovered panels."
  (dolist (panel vibe-panels)
    (let* ((config (cdr panel))
           (key (plist-get config :key))
           (switch-func (plist-get config :switch-function)))
      (when (and key switch-func)
        (global-set-key (kbd (concat "C-c " key))
                        `(lambda ()
                           (interactive)
                           (vibe-switch-to-panel ,(car panel))))))))

(defun vibe-create-panel-header ()
  "Create the panel header with tabs showing all panels."
  (setq vibe-panel-header-buffer (get-buffer-create "*Panel Tabs*"))
  (with-current-buffer vibe-panel-header-buffer
    (display-line-numbers-mode -1)
    (setq mode-line-format nil)
    (setq window-size-fixed t)
    (read-only-mode 1)))

(defun vibe-update-panel-header ()
  "Update the panel header to show all available panels."
  (when (and vibe-panel-header-buffer (buffer-live-p vibe-panel-header-buffer))
    (with-current-buffer vibe-panel-header-buffer
      (read-only-mode -1)
      (erase-buffer)
      (insert "  ")

      ;; Build tab string dynamically
      (let ((tab-strings '())
            (key-strings '()))

        (dolist (panel vibe-panels)
          (let* ((config (cdr panel))
                 (name (plist-get config :name))
                 (key (plist-get config :key))
                 (color (plist-get config :color))
                 (is-active (string= (car panel) vibe-active-panel)))

            ;; Create tab display
            (push (if is-active
                      (propertize name 'face `(:foreground ,color :weight bold :underline t))
                    (propertize name 'face '(:foreground "gray")))
                  tab-strings)

            ;; Collect keybinding for display
            (when key
              (push key key-strings))))

        ;; Insert tabs separated by |
        (insert (string-join (reverse tab-strings) " | "))
        (insert "  ")

        ;; Show keybindings
        (insert (propertize
                 (concat "(C-c " (string-join (reverse key-strings) "/") ")")
                 'face '(:foreground "dark gray" :height 0.9))))

      (read-only-mode 1))))

(defun vibe-switch-to-panel (panel-name)
  "Switch to the specified PANEL-NAME."
  (when vibe-panel-window
    (let* ((panel-entry (assoc panel-name vibe-panels))
           (config (cdr panel-entry)))

      (when config
        (let ((switch-func (plist-get config :switch-function))
              (assistant-mode (plist-get config :assistant-mode)))

          ;; Get or create the panel buffer
          (let ((buffer (funcall switch-func)))
            (when buffer
              (set-window-buffer vibe-panel-window buffer)
              (setq vibe-active-panel panel-name)

              ;; Update animation mode indicator
              (when assistant-mode
                (setq current-assistant-mode assistant-mode))

              ;; Update header
              (vibe-update-panel-header)

              ;; Focus on panel
              (select-window vibe-panel-window)

              (message "Switched to %s panel (%s)"
                       (plist-get config :name)
                       (mapconcat (lambda (p)
                                    (concat "C-c " (plist-get (cdr p) :key) "=" (plist-get (cdr p) :name)))
                                  (remove panel-entry vibe-panels) ", ")))))))))

(defun vibe-setup-all-panels ()
  "Set up all discovered panels."
  (dolist (panel vibe-panels)
    (let* ((config (cdr panel))
           (setup-func (plist-get config :setup-function)))
      (when setup-func
        (funcall setup-func))))

  ;; Set first panel as active by default
  (when vibe-panels
    (setq vibe-active-panel (caar vibe-panels))))

(defun vibe-initialize-panel-system ()
  "Initialize the entire panel system."
  (interactive)
  (vibe-discover-panels)
  (vibe-setup-panel-keybindings)
  (vibe-create-panel-header)
  (vibe-setup-all-panels)
  (vibe-update-panel-header)
  (message "Panel system initialized with %d panels" (length vibe-panels)))

;; Public API functions
(defun vibe-add-panel-example ()
  "Show example of how to add a new panel."
  (interactive)
  (message "To add a new panel:
1. Create panel-modules/yourpanel.el
2. Define yourpanel-panel-config with :name, :key, :color, :setup-function, :switch-function
3. Restart Emacs - new panel auto-appears!"))

(provide 'vibe-panel)
;;; vibe-panel.el ends here