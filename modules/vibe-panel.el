;;; vibe-panel.el --- Auto-discovering AI panel system -*- lexical-binding: t; -*-

;; Copyright (C) 2024
;; Author: VibEmacs Configuration

;;; Commentary:
;; This module automatically discovers and manages AI panel modules.
;; Just drop a new .el file in panel-modules/ and it becomes available!

;;; Code:

(require 'cl-lib)
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
  "Set up keybindings for all discovered panels using numeric shortcuts."
  ;; Clear legacy bindings that used direct C-c X mappings.
  (dolist (panel vibe-panels)
    (let* ((config (cdr panel))
           (legacy-key (plist-get config :key)))
      (when legacy-key
        (global-unset-key (kbd (concat "C-c " legacy-key))))))

  ;; Create prefix map for the new C-c c shortcuts.
  (let ((prefix-map (make-sparse-keymap)))
    (define-key global-map (kbd "C-c c") prefix-map)
    (cl-loop for panel in vibe-panels
             for index from 1
             for panel-name = (car panel)
             do (define-key prefix-map (kbd (number-to-string index))
                             `(lambda ()
                                (interactive)
                                (vibe-switch-to-panel ,panel-name))))))

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

      ;; Build tab string dynamically with numeric labels
      (let ((tab-strings '()))
        (cl-loop for panel in vibe-panels
                 for index from 1
                 do (let* ((config (cdr panel))
                           (name (plist-get config :name))
                           (color (plist-get config :color))
                           (label (format "(%d) %s" index name))
                           (is-active (string= (car panel) vibe-active-panel)))
                      (push (if is-active
                                (propertize label
                                            'face `(:foreground ,(or color "#00ff00")
                                                                :weight bold
                                                                :underline t))
                              (propertize label 'face '(:foreground "gray")))
                            tab-strings)))

        ;; Insert tabs separated by |
        (when tab-strings
          (insert (string-join (nreverse tab-strings) " | "))))

      (insert "  ")

      ;; Show keybinding hint
      (insert (propertize "(C-c c [NUM])" 'face '(:foreground "dark gray" :height 0.9)))

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

              (let ((panel-index (1+ (or (cl-position panel-entry vibe-panels :test #'eq) -1))))
                (message "Switched to %s panel (C-c c %d)"
                         (plist-get config :name)
                         panel-index)))))))))

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
