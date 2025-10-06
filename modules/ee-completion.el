;;; ee-completion.el --- Completion configuration for eemacs -*- lexical-binding: t; -*-

;; Minibuffer completion
(use-package vertico
  :straight (:repo "minad/vertico" :files (:defaults "extensions/*.el"))
  :init
  (vertico-mode 1)
  (vertico-mouse-mode 1)
  (vertico-indexed-mode 1)
  :hook
  (rfn-eshadow-update-overlay . vertico-directory-tidy)
  (vertico-mode . vertico-multiform-mode)
  (minibuffer-setup . vertico-repeat-save)
  :bind
  (("M-R" . vertico-repeat)
   :map vertico-map
   ("RET" . vertico-directory-enter)
   ("DEL" . vertico-directory-delete-char)
   ("M-DEL" . vertico-directory-delete-word)
   ("C-l" . vertico-directory-up))
  :custom
  (vertico-cycle t)
  (vertico-count 12)
  (vertico-resize nil)
  (vertico-multiform-categories '((embark-keybinding grid))))

;; Annotations
(use-package marginalia
  :straight t
  :after vertico
  :init (marginalia-mode)
  :config
  (add-to-list 'marginalia-annotator-registry '(file marginalia-annotate-file nil)))

(use-package nerd-icons-completion
  :straight t
  :after marginalia
  :demand t
  :config
  (nerd-icons-completion-marginalia-setup)
  (nerd-icons-completion-mode 1))

;; Fuzzy matching
(use-package orderless
  :straight t
  :init
  (setq completion-styles '(orderless basic)
        completion-category-overrides
        '((file (styles orderless basic partial-completion))
          (command (styles orderless)))))

;; In-buffer completion
(use-package corfu
  :straight (:repo "minad/corfu" :files (:defaults "extensions/*.el"))
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode)
  (corfu-history-mode)
  (corfu-indexed-mode)
  :hook
  ((eshell-mode shell-mode) . +corfu-less-intrusive-h)
  (minibuffer-setup . +corfu-enable-in-minibuffer-h)
  :bind
  (:map corfu-map
        ("M-n" . corfu-popupinfo-scroll-up)
        ("M-p" . corfu-popupinfo-scroll-down)
        ("M-d" . corfu-popupinfo-toggle))
  :custom
  (corfu-auto t)
  (corfu-cycle t)
  (corfu-preview-current nil)
  (corfu-min-width 25)
  (corfu-popupinfo-delay '(1.0 . 0.5))
  :config
  (with-eval-after-load 'savehist
    (add-to-list 'savehist-additional-variables 'corfu-history))

  (defun +corfu-less-intrusive-h ()
    (setq-local corfu-quit-at-boundary t
                corfu-quit-no-match t
                corfu-auto nil)
    (corfu-mode 1))

  (defun +corfu-enable-in-minibuffer-h ()
    (when (local-variable-p 'completion-at-point-functions)
      (setq-local corfu-auto t
                  corfu-popupinfo-delay nil)
      (corfu-mode 1)))

  (use-package nerd-icons-corfu
    :straight t
    :after corfu
    :init
    (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter)))

;; Completion backends
(use-package cape
  :straight t
  :bind (("C-c p p" . completion-at-point)
         ("C-c p f" . cape-file)
         ("C-c p d" . cape-dabbrev)
         ("C-c p h" . cape-history)
         ("C-c p k" . cape-keyword)
         ("C-c p s" . cape-elisp-symbol)
         ("C-c p e" . cape-elisp-block)
         ("C-c p a" . cape-abbrev)
         ("C-c p l" . cape-line)
         ("C-c p :" . cape-emoji))
  :init
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)
  (dolist (f '(comint-completion-at-point
               eglot-completion-at-point
               pcomplete-completions-at-point))
    (advice-add f :around #'cape-wrap-nonexclusive))

  (defun me-cape-add ()
    (add-hook 'completion-at-point-functions #'cape-file nil t)
    (add-hook 'completion-at-point-functions #'cape-keyword nil t)
    (when (< emacs-major-version 31)
      (add-hook 'completion-at-point-functions #'cape-dict nil t)))
  (add-hook 'prog-mode-hook #'me-cape-add)
  (add-hook 'emacs-lisp-mode-hook
            (lambda () (add-hook 'completion-at-point-functions #'cape-elisp-symbol nil t)))
  (add-hook 'org-mode-hook
            (lambda () (add-hook 'completion-at-point-functions #'cape-elisp-block nil t))))

;; Search and navigation
(use-package consult
  :straight t
  :bind (([remap switch-to-buffer] . consult-buffer)
         ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
         ([remap recentf-open-files] . consult-recent-file)
         ([remap bookmark-jump] . consult-bookmark)
         ("M-g o" . consult-outline)
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ([remap imenu] . consult-imenu)
         ([remap goto-line] . consult-goto-line)
         ("M-s l" . consult-line)
         ("M-s r" . consult-ripgrep)
         ("M-s d" . consult-find)
         ("M-s g" . consult-grep))
  :init
  (setq xref-show-definitions-function #'consult-xref)
  :config
  ;; Add preview delay for theme switching to avoid errors
  (consult-customize consult-theme :preview-key '(:debounce 0.2 any)))

;; Actions
(use-package embark
  :straight t
  :bind (("C-." . embark-act)
         ("C-;" . embark-dwim))
  :init (setq prefix-help-command #'embark-prefix-help-command))

(use-package embark-consult
  :straight t
  :after (embark consult))

(provide 'ee-completion)
