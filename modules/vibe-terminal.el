;;; vibe-terminal.el --- Auto-discovering terminal system -*- lexical-binding: t; -*-

;; Copyright (C) 2024
;; Author: VibEmacs Configuration

;;; Commentary:
;; This module automatically discovers and manages terminal modules.
;; Just drop a new .el file in terminal-modules/ and it becomes available!

;;; Code:

;; Terminal system variables
(defvar vibe-terminals nil
  "List of discovered terminal configurations.")

(defvar vibe-active-terminal nil
  "Currently active terminal name.")

(defvar vibe-terminal-window nil
  "Window displaying the terminal.")

(defvar vibe-terminal-header-buffer nil
  "Buffer for terminal header tabs.")

;; Terminal sorting function
(defun vibe-sort-terminals-with-order (terminals order-list)
  "Sort TERMINALS using ORDER-LIST, with unlisted terminals alphabetical at end."
  (let ((ordered-terminals '())
        (remaining-terminals terminals))

    ;; First, add terminals in the specified order
    (dolist (terminal-name order-list)
      (let ((terminal-entry (cl-find-if (lambda (t)
                                          (string= (plist-get (cdr t) :name) terminal-name))
                                        remaining-terminals)))
        (when terminal-entry
          (push terminal-entry ordered-terminals)
          (setq remaining-terminals (remove terminal-entry remaining-terminals)))))

    ;; Then add remaining terminals alphabetically
    (let ((sorted-remaining (sort remaining-terminals
                                  (lambda (a b)
                                    (string< (plist-get (cdr a) :name)
                                            (plist-get (cdr b) :name))))))
      (append (reverse ordered-terminals) sorted-remaining))))

;; Auto-discovery functions
(defun vibe-discover-terminals ()
  "Automatically discover all terminal modules."
  (let ((terminal-dir (expand-file-name "modules/terminal-modules" user-emacs-directory))
        (terminals '()))

    ;; Add terminal-modules to load path
    (add-to-list 'load-path terminal-dir)

    ;; Find all .el files in terminal-modules
    (when (file-directory-p terminal-dir)
      (dolist (file (directory-files terminal-dir nil "\\.el$"))
        (unless (string= file "order.el")  ; Skip order.el
          (let* ((module-name (file-name-sans-extension file))
                 (config-var (intern (concat module-name "-terminal-config"))))

            ;; Load the module
            (require (intern module-name))

            ;; Get its configuration
            (when (boundp config-var)
              (let ((config (symbol-value config-var)))
                (push (cons module-name config) terminals)))))))

    ;; Load custom ordering if available
    (let ((order-list nil)
          (order-file (expand-file-name "order.el" terminal-dir)))
      (when (file-exists-p order-file)
        (load order-file)
        (when (boundp 'vibe-terminal-order)
          (setq order-list vibe-terminal-order)))

      ;; Sort terminals using custom order, falling back to alphabetical
      (setq vibe-terminals
            (if order-list
                (vibe-sort-terminals-with-order terminals order-list)
              (sort terminals (lambda (a b)
                               (string< (plist-get (cdr a) :name)
                                       (plist-get (cdr b) :name)))))))

    (message "Discovered %d terminals: %s"
             (length vibe-terminals)
             (mapconcat (lambda (t) (plist-get (cdr t) :name)) vibe-terminals ", "))))

(defun vibe-setup-terminal-keybindings ()
  "Set up keybindings for all discovered terminals."
  (dolist (terminal vibe-terminals)
    (let* ((config (cdr terminal))
           (key (plist-get config :key))
           (switch-func (plist-get config :switch-function)))
      (when (and key switch-func)
        (global-set-key (kbd (concat "C-c a " key))
                        `(lambda ()
                           (interactive)
                           (vibe-switch-to-terminal ,(car terminal))))))))

(defun vibe-create-terminal-header ()
  "Create the terminal header with tabs showing all terminals."
  (setq vibe-terminal-header-buffer (get-buffer-create "*Terminal Tabs*"))
  (with-current-buffer vibe-terminal-header-buffer
    (display-line-numbers-mode -1)
    (setq mode-line-format nil)
    (setq window-size-fixed t)
    (read-only-mode 1)))

(defun vibe-update-terminal-header ()
  "Update the terminal header to show all available terminals."
  (when (and vibe-terminal-header-buffer (buffer-live-p vibe-terminal-header-buffer))
    (with-current-buffer vibe-terminal-header-buffer
      (read-only-mode -1)
      (erase-buffer)
      (insert "  ")

      ;; Build tab string dynamically
      (let ((tab-strings '())
            (key-strings '()))

        (dolist (terminal vibe-terminals)
          (let* ((config (cdr terminal))
                 (name (plist-get config :name))
                 (key (plist-get config :key))
                 (color (plist-get config :color))
                 (is-active (string= (car terminal) vibe-active-terminal)))

            ;; Create tab display
            (push (if is-active
                      (propertize name 'face `(:foreground ,color :weight bold :underline t))
                    (propertize name 'face '(:foreground "gray")))
                  tab-strings)

            ;; Collect keybinding for display
            (when key
              (push (concat "a " key) key-strings))))

        ;; Insert tabs separated by |
        (insert (string-join (reverse tab-strings) " | "))
        (insert "  ")

        ;; Show keybindings
        (insert (propertize
                 (concat "(C-c " (string-join (reverse key-strings) "/") ")")
                 'face '(:foreground "dark gray" :height 0.9))))

      (read-only-mode 1))))

(defun vibe-switch-to-terminal (terminal-name)
  "Switch to the specified TERMINAL-NAME."
  (let* ((terminal-entry (assoc terminal-name vibe-terminals))
         (config (cdr terminal-entry)))

    (when config
      (let ((switch-func (plist-get config :switch-function)))

        ;; Get or create the terminal buffer
        (let ((buffer (funcall switch-func)))
          (when buffer
            ;; If no terminal window exists, just switch to buffer
            (if vibe-terminal-window
                (progn
                  (set-window-buffer vibe-terminal-window buffer)
                  (select-window vibe-terminal-window))
              ;; No terminal window, just switch to buffer in current window
              (switch-to-buffer buffer))

            (setq vibe-active-terminal terminal-name)

            ;; Update header if it exists
            (when vibe-terminal-header-buffer
              (vibe-update-terminal-header))

            (message "Switched to %s terminal"
                     (plist-get config :name))))))))

(defun vibe-setup-all-terminals ()
  "Set up all discovered terminals."
  (dolist (terminal vibe-terminals)
    (let* ((config (cdr terminal))
           (setup-func (plist-get config :setup-function)))
      (when setup-func
        (funcall setup-func))))

  ;; Set first terminal as active by default
  (when vibe-terminals
    (setq vibe-active-terminal (caar vibe-terminals))))

(defun vibe-create-terminal-window (parent-window)
  "Create terminal window with tabs in PARENT-WINDOW."
  ;; Split for terminal header
  (select-window parent-window)
  (let ((terminal-area-window (selected-window)))
    ;; First create header at top
    (when (and vibe-terminal-header-buffer (buffer-live-p vibe-terminal-header-buffer))
      (switch-to-buffer vibe-terminal-header-buffer)
      (let ((header-window (selected-window)))
        (set-window-dedicated-p header-window t)
        (set-window-parameter header-window 'no-other-window t))
      (split-window-vertically 2)  ; Small window for header
      (other-window 1)

      ;; Store this window as the terminal window
      (setq vibe-terminal-window (selected-window))

      ;; Switch to first terminal
      (when vibe-terminals
        (vibe-switch-to-terminal (caar vibe-terminals))))))

(defun vibe-initialize-terminal-system ()
  "Initialize the entire terminal system."
  (interactive)
  (vibe-discover-terminals)
  (vibe-setup-terminal-keybindings)
  (vibe-create-terminal-header)
  (vibe-setup-all-terminals)
  (vibe-update-terminal-header)
  (message "Terminal system initialized with %d terminals" (length vibe-terminals)))

;; Public API functions
(defun vibe-add-terminal-example ()
  "Show example of how to add a new terminal."
  (interactive)
  (message "To add a new terminal:
1. Create terminal-modules/yourterminal.el
2. Define yourterminal-terminal-config with :name, :key, :color, :setup-function, :switch-function
3. Restart Emacs - new terminal auto-appears!"))

;; Initialize the terminal system on module load
;; This discovers terminals and sets up keybindings
(vibe-discover-terminals)
(vibe-setup-terminal-keybindings)
(vibe-create-terminal-header)
(message "Terminal system loaded with %d terminals" (length vibe-terminals))

(provide 'vibe-terminal)
;;; vibe-terminal.el ends here