;;; ee-eglot.el --- Eglot LSP configuration for eemacs -*- lexical-binding: t; -*-

(use-package eglot
  :straight t
  :commands eglot eglot-ensure
  :config
  (setq eglot-autoshutdown t
        eglot-send-changes-idle-time 0.5
        eglot-connect-timeout 10
        eglot-ignored-server-capabilities '(:inlayHintProvider)
        read-process-output-max (* 1024 1024))

  (defun my-eglot-server-available-p (server-command)
    "Check if SERVER-COMMAND is available in PATH."
    (executable-find (car server-command)))

  (defun my-eglot-format-on-save ()
    "Format buffer on save if LSP supports it."
    (when (eglot-managed-p)
      (eglot-format-buffer)))

  (add-hook 'before-save-hook #'my-eglot-format-on-save)

  (setq eglot-events-buffer-size 0)
  (add-to-list 'eglot-stay-out-of 'flymake)

  (defun my-add-eglot-server (mode server-command)
    "Add SERVER-COMMAND for MODE if the server is available."
    (when (my-eglot-server-available-p server-command)
      (add-to-list 'eglot-server-programs `(,mode . ,server-command))))

  (setq eglot-server-programs '()))

(provide 'ee-eglot)
