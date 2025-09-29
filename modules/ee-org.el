;;; ee-org.el --- Org mode configuration for eemacs -*- lexical-binding: t; -*-

;; Basic org-mode settings (matching minemacs approach)
(use-package org
  :defer t
  :commands (org-mode org-capture org-agenda)
  :init
  (setq org-directory "~/org"
        org-default-notes-file (expand-file-name "notes.org" org-directory)
        org-agenda-files (list org-directory))
  :config
  ;; Core org settings from minemacs
  (setq org-auto-align-tags nil
        org-edit-src-content-indentation 0
        org-hide-leading-stars t
        org-ellipsis " ▾"
        org-pretty-entities t
        org-pretty-entities-include-sub-superscripts t
        org-hide-emphasis-markers t
        org-fontify-quote-and-verse-blocks t
        org-fontify-whole-heading-line t
        org-highlight-latex-and-related '(latex script entities)
        org-tags-column 0
        org-catch-invisible-edits 'smart
        org-special-ctrl-a/e t
        org-special-ctrl-k t
        org-insert-heading-respect-content t
        org-startup-folded 'content
        org-startup-indented t)

  ;; Org capture templates
  (setq org-capture-templates
        '(("t" "Todo" entry (file+headline "todo.org" "Tasks")
           "* TODO %?\n  %u\n  %a")
          ("n" "Note" entry (file+headline "notes.org" "Notes")
           "* %?\n  %u\n  %a")
          ("j" "Journal" entry (file+olp+datetree "journal.org")
           "* %?\n  %U")
          ("i" "Idea" entry (file+headline "ideas.org" "Ideas")
           "* %?\n  %u")))

  ;; Org todo keywords
  (setq org-todo-keywords
        '((sequence "TODO(t)" "PROJ(p)" "STRT(s)" "WAIT(w)" "HOLD(h)" "IDEA(i)" "|" "DONE(d)" "KILL(k)")))

  ;; Org priorities
  (setq org-highest-priority ?A
        org-lowest-priority ?C
        org-default-priority ?B)

  ;; Org tags
  (setq org-tag-alist '((:startgroup . nil)
                        ("@home" . ?h) ("@work" . ?w) ("@errands" . ?e)
                        (:endgroup . nil)
                        (:startgroup . nil)
                        ("emacs" . ?e) ("linux" . ?l) ("programming" . ?p)
                        (:endgroup . nil)
                        ("IMPORTANT" . ?!)))

  ;; Org agenda settings
  (setq org-agenda-start-with-log-mode t
        org-log-done 'time
        org-log-into-drawer t
        org-agenda-span 'day
        org-agenda-include-diary t
        org-agenda-use-time-grid t
        org-agenda-time-grid '((daily today require-timed)
                               (800 1000 1200 1400 1600 1800 2000)
                               " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄"))

  ;; Org refile settings
  (setq org-refile-targets '((nil :maxlevel . 3)
                             (org-agenda-files :maxlevel . 3))
        org-outline-path-complete-in-steps nil
        org-refile-use-outline-path t)

  ;; Org archive settings
  (setq org-archive-location "archive/%s_archive::")

  ;; Babel settings
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)
     (shell . t)
     (js . t)))

  (setq org-confirm-babel-evaluate nil
        org-src-tab-acts-natively t
        org-src-window-setup 'current-window)

  ;; Org export settings
  (setq org-export-backends '(ascii html icalendar latex md))

  ;; Enhanced code block highlighting
  (setq org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-src-window-setup 'current-window)

  ;; Clean code block appearance - NO BACKGROUNDS! Theme-aware colors for dark themes
  (custom-set-faces
   '(org-block ((t (:inherit fixed-pitch :foreground "#d4d4d4"))))
   '(org-block-begin-line ((t (:inherit fixed-pitch :foreground "#8b949e"))))
   '(org-block-end-line ((t (:inherit fixed-pitch :foreground "#8b949e")))))

  ;; Professional org export settings (Centaur approach)
  (setq org-export-backends '(ascii html icalendar latex md)
        org-export-with-smart-quotes t
        org-export-with-sub-superscripts '{}
        org-export-with-tags 'not-in-toc
        org-export-with-toc t
        org-export-with-section-numbers t
        org-export-headline-levels 4
        org-export-with-drawers t
        org-export-with-properties t
        org-export-with-archived-trees nil
        org-export-with-broken-links t
        org-export-with-tasks t)

  ;; Enable org-protocol for capture from browser
  (require 'org-protocol))

;; Contributed packages to Org - exactly like minemacs
(use-package org-contrib
  :straight (:host github :repo "abougouffa/org-contrib" :branch "master"))

;; Auto-toggle Org elements - exactly like minemacs
(use-package org-appear
  :straight t
  :hook (org-mode . org-appear-mode)
  :custom
  (org-appear-inside-latex t)
  (org-appear-autokeywords t)
  (org-appear-autoentities t)
  (org-appear-autoemphasis t)
  (org-appear-autosubmarkers t)
  (org-appear-autolinks 'just-brackets)
  :config
  ;; For proper first-time setup, `org-appear--set-elements' needs to be run after other hooks have acted.
  (run-at-time nil nil #'org-appear--set-elements))

;; Modern Org style - exactly like minemacs
(use-package org-modern
  :straight t
  :after org
  :custom-face
  ;; Force monospaced font for tags
  (org-modern-tag ((t (:inherit org-verbatim :weight regular :foreground "black" :background "LightGray" :box "black"))))
  :custom
  (org-modern-table-vertical 5)
  (org-modern-table-horizontal 2)
  (org-modern-block-fringe nil)
  (org-modern-checkbox nil) ;; Not that interesting! Maybe it depends on the used font
  (org-modern-todo-faces
   ;; Tweak colors, and force it to be monospaced, useful when using `mixed-pitch-mode'.
   '(("IDEA" . (:inherit org-verbatim :weight semi-bold :foreground "white" :background "goldenrod"))
     ("NEXT" . (:inherit org-verbatim :weight semi-bold :foreground "white" :background "IndianRed1"))
     ("STRT" . (:inherit org-verbatim :weight semi-bold :foreground "white" :background "OrangeRed"))
     ("WAIT" . (:inherit org-verbatim :weight semi-bold :foreground "white" :background "coral"))
     ("KILL" . (:inherit org-verbatim :weight semi-bold :foreground "white" :background "DarkGreen"))
     ("PROJ" . (:inherit org-verbatim :weight semi-bold :foreground "white" :background "LimeGreen"))
     ("HOLD" . (:inherit org-verbatim :weight semi-bold :foreground "white" :background "orange"))
     ("DONE" . (:inherit org-verbatim :weight semi-bold :foreground "black" :background "LightGray"))))
  :init
  (global-org-modern-mode 1))

;; Automatically toggle Org mode LaTeX fragment previews - exactly like minemacs
(use-package org-fragtog
  :straight t
  :hook (org-mode . org-fragtog-mode)
  :custom
  (org-fragtog-preview-delay 0.2))

;; Org Roam for knowledge management (Centaur approach)
(use-package org-roam
  :straight t
  :after org
  :commands (org-roam-node-find org-roam-node-insert org-roam-buffer-toggle)
  :custom
  (org-roam-directory (expand-file-name "roam" org-directory))
  (org-roam-completion-everywhere t)
  (org-roam-db-gc-threshold most-positive-fixnum)
  :config
  (setq org-roam-node-display-template "${title:*} ${tags:15}")
  (org-roam-db-autosync-mode))

;; Org download for images (Centaur approach)
(use-package org-download
  :straight t
  :after org
  :hook (org-mode . org-download-enable)
  :custom
  (org-download-method 'attach)
  (org-download-image-dir "images")
  (org-download-heading-lvl nil)
  (org-download-timestamp "%Y%m%d_%H%M%S_")
  :config
  ;; Compatibility fix for url-handler-file-remote-p function
  (unless (fboundp 'url-handler-file-remote-p)
    (defun url-handler-file-remote-p (file)
      "Compatibility function for older Emacs versions."
      (and (stringp file) (file-remote-p file)))))

;; Keybindings
(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c a") 'org-agenda)
  (define-key org-mode-map (kbd "C-c c") 'org-capture)
  (define-key org-mode-map (kbd "C-c l") 'org-store-link)
  (define-key org-mode-map (kbd "C-c C-w") 'org-refile)
  (define-key org-mode-map (kbd "M-h") 'org-metaleft)
  (define-key org-mode-map (kbd "M-l") 'org-metaright)
  (define-key org-mode-map (kbd "M-j") 'org-metadown)
  (define-key org-mode-map (kbd "M-k") 'org-metaup))

(provide 'ee-org)
;;; ee-org.el ends here