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

;;; Code:
(provide 'blender)

(defgroup blender nil
  "Blender development integration for Emacs."
  :group 'tools
  :prefix "blender-")

(defcustom blender-executable "/mnt/c/Program Files/Blender Foundation/Blender 4.4/blender.exe"
  "Path to Blender executable."
  :type 'string
  :group 'blender)

(defcustom blender-bridge-script "C:/Users/martb/Documents/Blender/blender_emacs_bridge.py"
  "Path to the Python bridge script for Blender."
  :type 'string
  :group 'blender)

(defcustom blender-external-python nil
  "Path to external Python interpreter for heavy processing.
Set to nil to use only Blender's Python."
  :type '(choice (const :tag "Use Blender's Python only" nil)
          (string :tag "Python executable path"))
  :group 'blender)

(defcustom blender-addon-directory "C:/Users/martb/Documents/Blender/my_addons"
  "Directory containing your Blender addons."
  :type 'directory
  :group 'blender)

(defcustom blender-default-addon "test"
  "Default addon name for development."
  :type 'string
  :group 'blender)

(defvar blender-process nil
  "The Blender process handle.")

(defvar blender-current-addon nil
  "Currently active addon name.")

(defun blender-build-startup-args ()
  "Construct Blender startup arguments, optionally injecting external Python site-packages."
  (let* ((external-lib
          (when blender-external-python
            (string-trim
             (shell-command-to-string
              (format "\"%s\" -c \"import site; print([p for p in site.getsitepackages() if 'site-packages' in p][0])\"" blender-external-python)))))
         (python-expr
          (when external-lib
            (format "import sys; sys.path.append(r'%s')" external-lib))))
    (append
     (when python-expr (list "--python-expr" python-expr))
     (list "--python" blender-bridge-script
           "--" blender-default-addon))))

(defun blender-start ()
  "Start Blender with bridge and optional external Python injection."
  (interactive)
  (when (process-live-p blender-process)
    (message "Blender is already running.")
    (return))
  (setq blender-process
        (apply #'start-process
               "blender"
               "*blender*"
               blender-executable
               (blender-build-startup-args)))
  (set-process-query-on-exit-flag blender-process nil)
  (setq blender-current-addon blender-default-addon)
  (message "Blender launched with bridge. Current addon: %s" blender-current-addon))


(defun blender-send-command (json-string)
  "Send a raw JSON string to the running Blender process."
  (interactive "sJSON Command: ")
  (unless (process-live-p blender-process)
    (error "Blender process is not running. Use M-x blender-start first"))
  (process-send-string blender-process (concat json-string "\n")))

(defun blender-convert-path (path)
  "Convert WSL path to Windows path using wslpath."
  (let* ((wsl-path (expand-file-name path))
         (windows-path (string-trim
                        (shell-command-to-string
                         (concat "wslpath -w " (shell-quote-argument wsl-path))))))
    windows-path))

(defun blender-run-file (path &optional external)
  "Send a command to Blender to run a Python file at PATH.
If EXTERNAL is non-nil and blender-external-python is set, 
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
    (error "blender-external-python is not set. Configure it first"))
  (when buffer-file-name
    (save-buffer)
    (blender-run-file buffer-file-name t)))

(defun blender-reload-addon ()
  "Reload the current addon by disabling and re-enabling it."
  (interactive)
  (unless blender-current-addon
    (error "No current addon set. Use blender-set-addon first"))
  (blender-send-command
   (json-encode `(("cmd" . "reload_addon")
                  ("addon" . ,blender-current-addon)))))

(defun blender-set-default-addon-from-buffer ()
  "Set `blender-default-addon` to the name of the current buffer's parent directory."
  (interactive)
  (when buffer-file-name
    (let* ((dir (file-name-directory buffer-file-name))
           (addon-name (file-name-nondirectory (directory-file-name dir))))
      (setq blender-default-addon addon-name)
      (message "blender-default-addon set to: %s" blender-default-addon))))

(defun blender-set-addon (name)
  "Set the active addon name for development and reloading."
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
  "Evaluate a Python expression in Blender and show result."
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

;; Optional: Add to python-mode-hook for automatic setup
;; (add-hook 'python-mode-hook 'blender-setup-auto-reload)

;; Key bindings (optional - users can customize)
(defvar blender-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c b s") 'blender-start)
    (define-key map (kbd "C-c b r") 'blender-run-current-buffer)
    (define-key map (kbd "C-c b e") 'blender-run-current-buffer-external)
    (define-key map (kbd "C-c b l") 'blender-reload-addon)
    (define-key map (kbd "C-c b a") 'blender-set-addon)
    (define-key map (kbd "C-c b q") 'blender-stop)
    (define-key map (kbd "C-c b c") 'blender-show-config)
    (define-key map (kbd "C-c b v") 'blender-eval)
    (define-key map (kbd "C-c b d") 'blender-set-default-addon-from-buffer)
    map)
  "Keymap for Blender development commands.")

(define-minor-mode blender-mode
  "Minor mode for Blender addon development."
  :lighter " Blender"
  :keymap blender-mode-map)

(provide 'blender)

;;; blender.el ends here
