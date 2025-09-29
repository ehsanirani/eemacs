;;; ee-evil.el --- Evil mode configuration for eemacs -*- lexical-binding: t; -*-

;; Define functions before using them
(defun ehsan/setup-leader-key ()
  "Set up space as leader key with basic bindings."
  (interactive)
  (when (bound-and-true-p evil-mode)
    (message "Setting up SPC leader key...")

    ;; Create a prefix keymap for SPC
    (define-prefix-command 'ehsan/leader-map)

    ;; Bind the prefix map to SPC in normal state
    (define-key evil-normal-state-map (kbd "SPC") 'ehsan/leader-map)

    ;; Add commands to the leader map
    (define-key ehsan/leader-map (kbd "f") 'find-file)
    (define-key ehsan/leader-map (kbd "b") 'switch-to-buffer)
    (define-key ehsan/leader-map (kbd "s") 'save-buffer)
    (define-key ehsan/leader-map (kbd "q") 'save-buffers-kill-terminal)
    (define-key ehsan/leader-map (kbd "h") 'help-command)
    (define-key ehsan/leader-map (kbd "g") 'swiper)
    (define-key ehsan/leader-map (kbd "t") 'display-line-numbers-mode)

    (message "SPC leader key setup complete. Try pressing SPC now!")))

;; Function to test if SPC key is bound
(defun ehsan/test-spc-key ()
  "Test what SPC key is bound to in different states."
  (interactive)
  (message "SPC in normal state: %s" (lookup-key evil-normal-state-map (kbd "SPC")))
  (message "SPC f in normal state: %s" (lookup-key evil-normal-state-map (kbd "SPC f")))
  (message "Evil mode active: %s" (if evil-mode "Yes" "No"))
  (message "Current evil state: %s" evil-state)
  (message "SPC leader map bound: %s" (if (boundp 'ehsan/leader-map) "Yes" "No")))

;; Install evil mode
(use-package evil
  :straight t
  :init
  (setq evil-want-keybinding nil)
  (setq evil-want-integration t)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-w-in-emacs-state t)
  (setq evil-respect-visual-line-mode t)
  :config
  (evil-mode 1)

  ;; Make escape quit everything
  (define-key evil-normal-state-map [escape] 'keyboard-quit)
  (define-key evil-visual-state-map [escape] 'keyboard-quit)
  (define-key evil-insert-state-map [escape] 'evil-normal-state)
  (define-key evil-replace-state-map [escape] 'evil-normal-state)

  ;; Use j/k to move by visual lines
  (define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
  (define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)

  ;; Better window navigation
  (define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
  (define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
  (define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
  (define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)

  ;; Make Y behave like D and C
  (define-key evil-normal-state-map (kbd "Y") 'evil-yank-line)

  ;; Ensure C-w behaves as expected in insert mode
  (define-key evil-insert-state-map (kbd "C-w") 'evil-delete-backward-word)

  ;; Make movement keys work like visual lines
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  ;; Set up basic leader key after evil loads
  (when (fboundp 'ehsan/setup-leader-key)
    (ehsan/setup-leader-key)))

;; Install general for better keybinding management (optional for now)
(use-package general
  :straight t
  :after evil
  :config
  (general-evil-setup)
  (message "General package loaded successfully"))

;; Install evil-collection for better integration
(use-package evil-collection
  :straight t
  :after evil
  :config
  (evil-collection-init))

;; Install which-key for better key help
(use-package which-key
  :straight t
  :defer nil
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.5))

;; Variable to track editing style for mode line
(defvar editing-style-indicator "Vim"
  "Current editing style indicator for mode line.")

;; Function to toggle between emacs and vim styles
(defun toggle-editing-style ()
  "Toggle between Emacs and Vim editing styles."
  (interactive)
  (if evil-mode
      (progn
        (evil-mode -1)
        (setq editing-style-indicator "Emacs")
        (message "Switched to Emacs editing style"))
    (evil-mode 1)
    (setq editing-style-indicator "Vim")
    (message "Switched to Vim editing style"))
  ;; Force mode line update
  (force-mode-line-update))

;; Add editing style indicator to mode line
(setq-default mode-line-format
              (cons '(:eval (propertize (format " [%s] " editing-style-indicator)
                                       'face 'font-lock-keyword-face))
                    mode-line-format))

;; Keybinding to toggle styles - changed to C-c e
(global-set-key (kbd "C-c e") 'toggle-editing-style)

;; Keybinding to manually set up leader key if needed
(global-set-key (kbd "C-c l") 'ehsan/setup-leader-key)
(global-set-key (kbd "C-c k") 'ehsan/test-spc-key)

;; Make sure we can use the toggle in evil normal state
(with-eval-after-load 'evil
  (define-key evil-normal-state-map (kbd "C-c e") 'toggle-editing-style))

(provide 'ee-evil)
;;; ee-evil.el ends here