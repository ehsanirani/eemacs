;;; ee-prog.el --- Programming configuration for eemacs -*- lexical-binding: t; -*-

;; LSP performance
(use-package eglot-booster
  :straight (:host github :repo "jdtsmith/eglot-booster")
  :custom (eglot-booster-io-only (>= emacs-major-version 30)))

;; Eglot integration
(use-package consult-eglot
  :straight t
  :after (consult eglot)
  :config
  (consult-customize
   consult-eglot-symbols
   :initial (or (thing-at-point 'region t)
                (thing-at-point 'symbol t))))

;; Code formatting
(use-package apheleia
  :straight t
  :hook (prog-mode . apheleia-mode)
  :custom
  (apheleia-remote-algorithm 'local)
  :config
  (cl-callf append apheleia-mode-alist
            '((nxml-mode . xmllint)
              (protobuf-mode . clang-format)
              (protobuf-ts-mode . clang-format)))
  (let ((clang (assq 'clang-format apheleia-formatters)))
    (setcdr clang (cons "clang-format"
                        (append (cddr clang)
                                '((apheleia-formatters-locate-file
                                   ".clang-format" "_clang-format")))))))

;; Jump to definition
(use-package dumb-jump
  :straight t
  :custom (dumb-jump-selector 'completing-read)
  :init (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

;; Multiple xref backends
(use-package xref-union
  :straight t
  :after xref
  :config
  (setq xref-union-excluded-backends '(etags--xref-backend))
  (xref-union-mode 1))

;; TODO highlighting
(use-package hl-todo
  :straight (:host github :repo "tarsius/hl-todo")
  :hook (prog-mode . hl-todo-mode)
  :config
  (dolist (kw '(("BUG" . "#ee5555")
                ("BUGFIX" . "#ee5555")
                ("PROJ" . "#447f44")
                ("IDEA" . "#0fa050")))
    (add-to-list 'hl-todo-keyword-faces kw)))

;; Macro expansion
(use-package macrostep
  :straight t
  :commands macrostep-expand
  :bind
  (:map emacs-lisp-mode-map ("C-c m" . macrostep-expand))
  (:map c-mode-map ("C-c m" . macrostep-expand))
  (:map c++-mode-map ("C-c m" . macrostep-expand))
  (:map c-ts-mode-map ("C-c m" . macrostep-expand))
  (:map c++-ts-mode-map ("C-c m" . macrostep-expand))
  :hook
  ((c-mode c++-mode c-ts-mode c++-ts-mode) . macrostep-c-mode-hook))

;; Breadcrumb navigation
(use-package breadcrumb
  :straight t
  :hook
  ((c-mode c++-mode c-ts-base-mode python-base-mode rust-ts-mode sh-mode bash-ts-mode)
   . breadcrumb-local-mode)
  :config
  (with-eval-after-load 'nerd-icons
    (defun my-breadcrumb-icon-only ()
      (concat " " (if buffer-file-name
                      (nerd-icons-icon-for-file buffer-file-name)
                    (nerd-icons-icon-for-mode major-mode))))
    (advice-add 'breadcrumb-project-crumbs :override #'my-breadcrumb-icon-only)))

;; C/C++ utilities
(use-package simpc-mode
  :straight (:host github :repo "rexim/simpc-mode")
  :commands simpc-mode)

(use-package cpp-func-impl
  :straight (:host github :repo "dheerajshenoy/cpp-func-impl.el")
  :commands cpp-func-impl-generate)

;; ---------------------------------------------------------------------------
;; Tree-sitter Support (Centaur-style approach)
;; ---------------------------------------------------------------------------

;; Automatic Tree-sitter grammar management
(use-package treesit-auto
  :straight t
  :hook (after-init . global-treesit-auto-mode)
  :init (setq treesit-auto-install 'prompt))

;; Code folding with built-in hs-minor-mode (more reliable than treesit-fold-indicators)
(use-package hideshow
  :ensure nil
  :hook (prog-mode . hs-minor-mode)
  :config
  (setq hs-hide-comments-when-hiding-all nil
        hs-isearch-open nil
        hs-special-modes-alist
        '((c-mode "{" "}" "/[*/]" nil nil)
          (c++-mode "{" "}" "/[*/]" nil nil)
          (java-mode "{" "}" "/[*/]" nil nil)
          (python-mode "#" "" "" nil nil)
          (js-mode "{" "}" "/[*/]" nil nil)
          (typescript-mode "{" "}" "/[*/]" nil nil)
          (rust-mode "{" "}" "/[*/]" nil nil)
          (go-mode "{" "}" "/[*/]" nil nil))))

;; Prettify Symbols (Centaur-style) - display lambda as λ, etc.
(use-package prog-mode
  :ensure nil
  :hook (prog-mode . prettify-symbols-mode)
  :init
  (setq prettify-symbols-unprettify-at-point 'right-edge))

(provide 'ee-prog)
