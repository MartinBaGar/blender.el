;;; blender.el --- Send commands and control Blender -*- lexical-binding: t; -*-

;; Author: Martin Bari Garnier <martbari.g@gmail.com>
;; Maintainer: Martin Bari Garnier <martbari.g@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "27.1"))
;; Keywords: tools, 3D, blender
;; URL: https://github.com/MartinBaGar/blender.el

;;; Commentary:

;; blender.el provides integration between Emacs and Blender. It allows
;; you to send Python code from Emacs directly to Blender for execution,
;; control the Blender Python environment, and automate workflows.
;; Note: This is designed to work with Emacs running in WSL and Blender running on Windows.
;; Ensure Blender paths are converted using `wslpath -w` for compatibility.

;;; Code:
(defgroup blender nil
  "Blender development integration for Emacs."
  :group 'tools
  :prefix "blender-")

(defcustom blender-executable nil
  "Absolute path to the Blender executable.
Required for starting Blender from within Emacs.
Use `wslpath` if using WSL and pointing to a Windows path."
  :type '(choice (const :tag "Unset" nil)
          (file :must-match t))
  :group 'blender)

(defcustom blender-bridge-script
  (let ((base-dir (file-name-directory (or load-file-name (buffer-file-name)))))
    (expand-file-name "blender-emacs_bridge.py" base-dir))
  "Path to the Python bridge script for Blender."
  :type 'file
  :group 'blender)

(defcustom blender-external-python nil
  "Path to external Python interpreter for heavy processing.
Set to nil to use only Blender's Python."
  :type '(choice (const :tag "Use Blender's Python only" nil)
          (string :tag "Python executable path"))
  :group 'blender)

(defcustom blender-addon-directory nil
  "Absolute path to the directory containing your add-on.
Required for starting Blender from within Emacs.
Use `wslpath` if using WSL and pointing to a Windows path."
  :type 'directory
  :group 'blender)

(defcustom blender-default-addon nil
  "Default addon name for development."
  :type 'string
  :group 'blender)

(defvar blender-process nil
  "The Blender process handle.")

(defvar blender-current-addon nil
  "Currently active add-on name.")

(defun blender-convert-path (path)
  "Convert WSL PATH to WINDOWS PATH using wslpath."
  (let* ((wsl-path (expand-file-name path))
         (windows-path (string-trim
                        (shell-command-to-string
                         (concat "wslpath -w " (shell-quote-argument wsl-path))))))
    windows-path))

(defun blender-build-startup-args ()
  "Construct Blender startup arguments.
Optionally injecting external Python site-packages."
  (unless blender-executable
    (user-error "`blender-executable` is not set. Please customize it"))

  (let* ((external-lib
          (when blender-external-python
            (string-trim
             (shell-command-to-string
              (format "\"%s\" -c \"import site; print([p for p in site.getsitepackages() if 'site-packages' in p][0])\""
                      blender-external-python)))))

         (paths
          (when (or blender-addon-directory external-lib)
            (if external-lib
                (if blender-addon-directory
                    (list blender-addon-directory external-lib)
                  (list external-lib))
              (list blender-addon-directory))))

         (python-expr
          (when paths
            (format "import sys; sys.path.extend([%s])"
                    (mapconcat #'prin1-to-string paths ", ")))))

    (append
     (when python-expr (list "--python-expr" python-expr))
     (list "--python" (blender-convert-path blender-bridge-script))
     ;; Only pass default addon if it's set
     (when (and blender-default-addon (not (string-empty-p blender-default-addon)))
       (list "--" blender-default-addon)))))

(defun blender-start ()
  "Start Blender with bridge and optional external Python injection."
  (interactive)
  (if (process-live-p blender-process)
      (message "Blender is already running")
    (progn
      (message "Starting Blender...")
      (setq blender-process
            (apply #'start-process
                   "blender"
                   "*blender*"
                   blender-executable
                   (blender-build-startup-args)))
      (set-process-query-on-exit-flag blender-process nil)
      ;; Set current addon to default if available
      (when (and blender-default-addon (not (string-empty-p blender-default-addon)))
        (setq blender-current-addon blender-default-addon))
      (blender-mode 1)
      (message "Blender launched with bridge. Current addon: %s"
               (or blender-current-addon "None")))))

(defun blender-send-command (json-string)
  "Send a raw JSON-STRING to the running Blender process."
  (interactive "sJSON Command: ")
  (unless (process-live-p blender-process)
    (error "Blender process is not running. Use M-x blender-start first"))
  (process-send-string blender-process (concat json-string "\n")))

(defun blender-run-file (path &optional external)
  "Send a command to Blender to run a Python file at PATH.
If EXTERNAL is non-nil and BLENDER-EXTERNAL-PYTHON is set,
run in external Python environment."
  (interactive "fScript to run in Blender: ")
  (let ((windows-path (blender-convert-path path)))
    (if (and external blender-external-python)
        (blender-send-command
         (json-encode `(("cmd" . "run")
                        ("path" . ,windows-path)
                        ("external" . t)
                        ("python_env" . ,blender-external-python))))
      (blender-send-command
       (json-encode `(("cmd" . "run")
                      ("path" . ,windows-path)))))))

(defun blender-run-current-buffer ()
  "Save and run the current buffer in Blender's Python."
  (interactive)
  (when buffer-file-name
    (save-buffer)
    (blender-run-file buffer-file-name)))

(defun blender-run-current-buffer-external ()
  "Save and run the current buffer in external Python environment."
  (interactive)
  (unless blender-external-python
    (error "Blender-external-python is not set. Configure it first"))
  (when buffer-file-name
    (save-buffer)
    (blender-run-file buffer-file-name t)))

(defun blender-reload-addon ()
  "Reload the current addon by disabling and re-enabling it."
  (interactive)
  (let ((addon-to-reload (or blender-current-addon blender-default-addon)))
    (unless addon-to-reload
      (error "No current addon set. Use blender-set-addon first or set blender-default-addon"))
    (blender-send-command
     (json-encode `(("cmd" . "reload_addon")
                    ("addon" . ,addon-to-reload))))))

(defun blender-set-default-addon-from-buffer ()
  "Set BLENDER-DEFAULT-ADDON to the name of the current buffer's parent directory."
  (interactive)
  (when buffer-file-name
    (let* ((dir (file-name-directory buffer-file-name))
           (addon-name (file-name-nondirectory (directory-file-name dir))))
      (setq blender-default-addon addon-name)
      (message "blender-default-addon set to: %s" blender-default-addon))))

(defun blender-set-addon (name)
  "Set the active addon NAME for development and reloading."
  (interactive "sAddon name: ")
  (setq blender-current-addon name)
  (blender-send-command
   (json-encode `(("cmd" . "set_addon")
                  ("name" . ,name))))
  (message "Active addon set to: %s" name))

(defun blender-save-and-reload ()
  "Save current buffer and reload the addon."
  (interactive)
  (when buffer-file-name
    (save-buffer)
    (blender-reload-addon)))

(defun blender-eval (expression)
  "Evaluate a Python EXPRESSION in Blender and show result."
  (interactive "sExpression: ")
  (blender-send-command
   (json-encode `(("cmd" . "eval")
                  ("expr" . ,expression)))))

(defun blender-stop ()
  "Stop the running Blender process."
  (interactive)
  (when (process-live-p blender-process)
    (kill-process blender-process)
    (setq blender-current-addon nil)
    (blender-mode -1)
    (message "Blender stopped.")))

(defun blender-show-config ()
  "Show current Blender development configuration."
  (interactive)
  (with-output-to-temp-buffer "*Blender Config*"
    (princ "=== Blender Development Configuration ===\n\n")
    (princ (format "Blender Executable: %s\n" blender-executable))
    (princ (format "Bridge Script: %s\n" blender-bridge-script))
    (princ (format "Addon Directory: %s\n" blender-addon-directory))
    (princ (format "Current Addon: %s\n" (or blender-current-addon "None")))
    (princ (format "External Python: %s\n" (or blender-external-python "Not configured")))
    (princ (format "Process Running: %s\n" (if (process-live-p blender-process) "Yes" "No")))))

;; Auto-reload setup for addon files
(defun blender-setup-auto-reload ()
  "Set up auto-reload for addon files in the current buffer."
  (when (and buffer-file-name
             (string-match-p (regexp-quote blender-addon-directory) buffer-file-name))
    (add-hook 'after-save-hook 'blender-reload-addon nil t)
    (message "Auto-reload enabled for this addon file")))

(defvar blender-popup-window nil)
(defvar blender-popup-height 0.3
  "Height ratio for blender popup window.")

(defun blender-toggle-popup ()
  "Toggle blender buffer as popup."
  (interactive)
  (if (blender-popup-visible-p)
      (blender-dismiss-popup)
    (blender-show-popup)))

(defun blender-show-popup ()
  "Show blender buffer as popup."
  (let ((buffer (cond
                 ((and blender-process (process-live-p blender-process))
                  (process-buffer blender-process))
                 (t nil))))
    (if buffer
        (setq blender-popup-window
              (display-buffer buffer
                              `((display-buffer-at-bottom)
                                (window-height . 0.3)
                                (dedicated . t))))
      (message "Blender is not running. Start it with `blender-start`."))))

(defun blender-dismiss-popup ()
  "Dismiss the blender popup window."
  (interactive)
  (when (blender-popup-visible-p)
    (delete-window blender-popup-window)
    (setq blender-popup-window nil)))

(defun blender-popup-visible-p ()
  "Check if blender popup is currently visible."
  (and blender-popup-window
       (window-live-p blender-popup-window)))

;; Define minor mode and keymap
(defvar blender-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-b s") 'blender-start)
    (define-key map (kbd "C-c C-b r") 'blender-run-current-buffer)
    (define-key map (kbd "C-c C-b e") 'blender-run-current-buffer-external)
    (define-key map (kbd "C-c C-b l") 'blender-reload-addon)
    (define-key map (kbd "C-c C-b a") 'blender-set-addon)
    (define-key map (kbd "C-c C-b q") 'blender-stop)
    (define-key map (kbd "C-c C-b c") 'blender-show-config)
    (define-key map (kbd "C-c C-b v") 'blender-eval)
    (define-key map (kbd "C-c C-b d") 'blender-set-default-addon-from-buffer)
    (define-key map (kbd "C-c C-b p") 'blender-toggle-popup) ; Add popup toggle
    ;; Process buffer specific keys (only active when in *blender* buffer)
    (define-key map (kbd "q") 'blender-dismiss-popup)
    (define-key map (kbd "C-g") 'blender-dismiss-popup)
    map)
  "Keymap for `blender-mode'.")

(define-minor-mode blender-mode
  "Minor mode for interacting with Blender."
  :lighter " 🧃"
  :keymap blender-mode-map)

(provide 'blender)

;;; blender.el ends here
