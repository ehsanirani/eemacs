;;; ee-org.el --- Org mode configuration (hybrid) -*- lexical-binding: t; -*-

;; Best-of-both approach:
;; - MinEmacs: rich GTD taxonomy, academic LaTeX export, citar bibliography, org-modern faces
;; - Centaur: org-roam knowledge graph, org-pomodoro, capture with clock-in, polished agenda UI
;; - Multi-project: distributed notes/TODOs across project directories

;;;; ============================================================
;;;; Multi-project support
;;;; ============================================================

(defvar ee-org-projects-file
  (expand-file-name "org-projects.el" user-emacs-directory)
  "File storing registered project directories.")

(defvar ee-org-project-directories nil
  "List of project directories that contain org files.
Each directory may contain a TODO.org for tasks and a notes/ subdirectory
for org-roam-indexed notes. Managed via `ee-org-register-project' and
`ee-org-unregister-project'.")

(defun ee-org--load-projects ()
  "Load registered project directories from `ee-org-projects-file'."
  (when (file-exists-p ee-org-projects-file)
    (with-temp-buffer
      (insert-file-contents ee-org-projects-file)
      (setq ee-org-project-directories (read (current-buffer))))))

(defun ee-org--save-projects ()
  "Save registered project directories to `ee-org-projects-file'."
  (with-temp-file ee-org-projects-file
    (prin1 ee-org-project-directories (current-buffer))))

(defun ee-org-register-project (dir)
  "Register DIR as an org-aware project directory.
This adds DIR's TODO.org to the agenda and its notes/ folder to org-roam.
Creates TODO.org and notes/ in DIR if they don't exist."
  (interactive "DProject directory: ")
  (let ((dir (expand-file-name (file-name-as-directory dir))))
    (unless (member dir ee-org-project-directories)
      (push dir ee-org-project-directories)
      (ee-org--save-projects)
      ;; Create TODO.org if missing
      (let ((todo-file (expand-file-name "TODO.org" dir)))
        (unless (file-exists-p todo-file)
          (with-temp-file todo-file
            (insert (format "#+title: %s Tasks\n#+filetags: :project:\n\n* Tasks\n\n* Notes\n"
                            (file-name-nondirectory (directory-file-name dir)))))))
      ;; Create notes/ if missing
      (let ((notes-dir (expand-file-name "notes" dir)))
        (unless (file-exists-p notes-dir)
          (make-directory notes-dir t)))
      ;; Update agenda files
      (ee-org--update-agenda-files)
      ;; Sync org-roam db so new notes/ files are indexed
      (when (and (bound-and-true-p org-roam-directory)
                 (fboundp 'org-roam-db-sync))
        (org-roam-db-sync))
      (message "Registered project: %s" dir))))

(defun ee-org-unregister-project (dir)
  "Unregister DIR as an org-aware project directory."
  (interactive
   (list (completing-read "Unregister project: " ee-org-project-directories nil t)))
  (let ((dir (expand-file-name (file-name-as-directory dir))))
    (setq ee-org-project-directories (delete dir ee-org-project-directories))
    (ee-org--save-projects)
    (ee-org--update-agenda-files)
    ;; Sync org-roam db to remove stale entries
    (when (and (bound-and-true-p org-roam-directory)
               (fboundp 'org-roam-db-sync))
      (org-roam-db-sync))
    (message "Unregistered project: %s" dir)))

(defun ee-org-list-projects ()
  "List all registered org-aware project directories."
  (interactive)
  (if ee-org-project-directories
      (message "Registered projects:\n%s"
               (mapconcat (lambda (d) (concat "  - " d))
                          ee-org-project-directories "\n"))
    (message "No projects registered. Use M-x ee-org-register-project")))

(defun ee-org--collect-project-todo-files ()
  "Collect TODO.org files from all registered project directories."
  (let (files)
    (dolist (dir ee-org-project-directories)
      (let ((todo (expand-file-name "TODO.org" dir)))
        (when (file-exists-p todo)
          (push todo files))))
    (nreverse files)))

(defun ee-org--collect-project-notes-dirs ()
  "Collect notes/ directories from all registered project directories."
  (let (dirs)
    (dolist (dir ee-org-project-directories)
      (let ((notes-dir (expand-file-name "notes" dir)))
        (when (file-exists-p notes-dir)
          (push notes-dir dirs))))
    (nreverse dirs)))

(defun ee-org--update-agenda-files ()
  "Rebuild `org-agenda-files' from central org + project TODO files."
  (when (bound-and-true-p org-directory)
    (setq org-agenda-files
          (append
           ;; Central org files — only include those that exist
           (cl-remove-if-not
            #'file-exists-p
            (mapcar (lambda (f) (expand-file-name f org-directory))
                    '("inbox.org" "agenda.org" "projects.org")))
           ;; Project TODO.org files
           (ee-org--collect-project-todo-files)))))

(defun ee-org--file-in-project-notes-p (path)
  "Return non-nil if PATH is inside a registered project's notes/ directory."
  (when path
    (let ((path (expand-file-name path)))
      (cl-some (lambda (dir)
                 (let ((notes-dir (expand-file-name "notes" dir)))
                   (and (file-directory-p notes-dir)
                        (org-roam-descendant-of-p path notes-dir))))
               ee-org-project-directories))))

(defun ee-org--roam-file-p-advice (orig-fn &optional file)
  "Advice for `org-roam-file-p' to also accept project notes/ files.
Falls through to ORIG-FN for files under `org-roam-directory'."
  (or (funcall orig-fn file)
      (when-let* ((path (or file (buffer-file-name (buffer-base-buffer)))))
        (and (member (org-roam--file-name-extension path)
                     org-roam-file-extensions)
             (ee-org--file-in-project-notes-p path)))))

(defun ee-org--roam-list-files-advice (orig-fn)
  "Advice for `org-roam-list-files' to include project notes/ files.
Appends files from registered project notes/ directories to ORIG-FN result."
  (let ((files (funcall orig-fn)))
    (dolist (dir (ee-org--collect-project-notes-dirs))
      (setq files (append files (org-roam--list-files dir))))
    (delete-dups files)))

(defun ee-org--setup-roam-project-advice ()
  "Advise org-roam to also index project notes/ directories.
Org-roam v2 only indexes files under `org-roam-directory'.  We extend it
by advising `org-roam-file-p' and `org-roam-list-files' to also accept
files from registered project notes/ directories."
  (when (fboundp 'org-roam-file-p)
    (advice-add 'org-roam-file-p :around #'ee-org--roam-file-p-advice)
    (advice-add 'org-roam-list-files :around #'ee-org--roam-list-files-advice)))

(defun ee-org--current-project-dir ()
  "Return the current project root, or nil."
  (or (and (fboundp 'project-current)
           (when-let* ((proj (project-current nil)))
             (expand-file-name (file-name-as-directory (project-root proj)))))
      default-directory))

(defun ee-org--current-project-todo ()
  "Return TODO.org path for the current project, creating it if needed."
  (let* ((dir (ee-org--current-project-dir))
         (todo (expand-file-name "TODO.org" dir)))
    (unless (file-exists-p todo)
      (with-temp-file todo
        (insert (format "#+title: %s Tasks\n#+filetags: :project:\n\n* Tasks\n\n* Notes\n"
                        (file-name-nondirectory (directory-file-name dir))))))
    todo))

(defun ee-org--current-project-notes-dir ()
  "Return notes/ path for the current project, creating it if needed."
  (let ((dir (expand-file-name "notes" (ee-org--current-project-dir))))
    (unless (file-exists-p dir)
      (make-directory dir t))
    dir))

;; Load projects on startup
(ee-org--load-projects)

;;;; ============================================================
;;;; Core Org
;;;; ============================================================

(use-package org
  :straight (:type built-in)
  :defer t
  :commands (org-mode org-capture org-agenda)
  :bind (("C-c a" . org-agenda)
         ("C-c x" . org-capture))
  :hook (((org-babel-after-execute org-mode) . org-redisplay-inline-images))
  :custom
  ;; Directories
  (org-directory (or (bound-and-true-p org-directory) "~/org"))

  ;; Core editing
  (org-auto-align-tags nil)
  (org-edit-src-content-indentation 0)
  (org-ellipsis " ↩")
  (org-fold-catch-invisible-edits 'smart)
  (org-fontify-quote-and-verse-blocks t)
  (org-fontify-whole-heading-line t)
  (org-hide-emphasis-markers t)
  (org-highlight-latex-and-related '(native latex script entities))
  (org-insert-heading-respect-content t)
  (org-list-allow-alphabetical t)
  (org-pretty-entities t)
  (org-pretty-entities-include-sub-superscripts t)
  (org-return-follows-link t)
  (org-special-ctrl-a/e t)
  (org-special-ctrl-k t)
  (org-startup-folded 'content)
  (org-startup-indented t)
  (org-tags-column 0)
  (org-use-property-inheritance t)
  (org-use-sub-superscripts '{})

  ;; Logging
  (org-log-done 'time)
  (org-log-into-drawer t)

  ;; TODO keywords — MinEmacs rich GTD taxonomy (3 sequences)
  (org-todo-keywords
   '((sequence
      "TODO(t)"  ; A task that needs to be done
      "PROJ(p)"  ; A project, which usually contains other tasks
      "LOOP(r)"  ; A recurring task
      "STRT(s)"  ; A task that is in progress
      "WAIT(w)"  ; Something external is holding up this task
      "HOLD(h)"  ; This task is paused/on hold because of me
      "IDEA(i)"  ; An unconfirmed and unapproved task or notion
      "|"
      "DONE(d)"  ; Task successfully completed
      "KILL(k)") ; Task was cancelled, aborted or is no longer applicable
     (sequence "[ ](T)" "[-](S)" "[?](W)" "|" "[X](D)")
     (sequence "|" "OKAY(o)" "YES(y)" "NO(n)")))

  (org-todo-keyword-faces
   '(("[-]"  . +org-todo-active)
     ("STRT" . +org-todo-active)
     ("[?]"  . +org-todo-onhold)
     ("WAIT" . +org-todo-onhold)
     ("HOLD" . +org-todo-onhold)
     ("PROJ" . +org-todo-project)
     ("NO"   . +org-todo-cancel)
     ("KILL" . +org-todo-cancel)))

  ;; Priorities
  (org-highest-priority ?A)
  (org-lowest-priority ?C)
  (org-default-priority ?B)

  ;; Export — MinEmacs academic defaults
  (org-export-in-background t)
  (org-export-with-broken-links 'mark)
  (org-export-with-smart-quotes t)
  (org-export-with-sub-superscripts '{})
  (org-export-with-tags 'not-in-toc)

  :config
  ;; Custom TODO faces (from Doom Emacs, used by MinEmacs)
  (with-no-warnings
    (custom-declare-face '+org-todo-active  '((t (:inherit (bold font-lock-constant-face org-todo)))) "")
    (custom-declare-face '+org-todo-project '((t (:inherit (bold font-lock-doc-face org-todo)))) "")
    (custom-declare-face '+org-todo-onhold  '((t (:inherit (bold warning org-todo)))) "")
    (custom-declare-face '+org-todo-cancel  '((t (:inherit (bold error org-todo)))) ""))

  ;; Default notes file
  (setq org-default-notes-file (expand-file-name "inbox.org" org-directory))

  ;; Agenda files — central + project TODO files
  (ee-org--update-agenda-files)

  ;; Capture templates — central + project-aware
  (setq org-capture-templates
        `(("t" "Todo" entry (file ,(expand-file-name "inbox.org" org-directory))
           "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
          ("n" "Note" entry (file ,(expand-file-name "inbox.org" org-directory))
           "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
          ("j" "Journal" entry (file+olp+datetree
                                ,(expand-file-name "journal.org" org-directory))
           "* %^{Title} %?\n%U\n%a\n" :clock-in t :clock-resume t)
          ("i" "Idea" entry (file+headline
                             ,(expand-file-name "inbox.org" org-directory) "Ideas")
           "* IDEA %?\n%U\n")
          ("r" "Research note" entry (file ,(expand-file-name "inbox.org" org-directory))
           "* %^{Title} :research:\n%U\nSource: %a\n\n%?" :clock-in t :clock-resume t)
          ("b" "Book/Paper" entry (file+olp+datetree
                                   ,(expand-file-name "reading.org" org-directory))
           "* %^{Title} %^g\n%U\nAuthor: %^{Author}\n\n%?")

          ;; Project-local captures
          ;; NOTE: (file ee-org--current-project-todo) works because org-capture
          ;; calls functionp symbols via funcall in org-capture-target-buffer.
          ("p" "Project")
          ("pt" "Project todo" entry (file ee-org--current-project-todo)
           "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
          ("pn" "Project note" entry (file ee-org--current-project-todo)
           "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
          ("pi" "Project idea" entry (file ee-org--current-project-todo)
           "* IDEA %?\n%U\n")))

  ;; Tags — research-oriented
  (setq org-tag-alist '((:startgroup . nil)
                        ("@home" . ?h) ("@work" . ?w) ("@lab" . ?L)
                        (:endgroup . nil)
                        ("research" . ?r) ("paper" . ?p) ("experiment" . ?x)
                        ("emacs" . ?e) ("linux" . ?l) ("programming" . ?P)
                        ("IMPORTANT" . ?!) ("URGENT" . ?u)))

  ;; Refile — include project TODO files as targets
  (setq org-refile-targets '((nil :maxlevel . 3)
                             (org-agenda-files :maxlevel . 3))
        org-outline-path-complete-in-steps nil
        org-refile-use-outline-path t)

  ;; Archive
  (setq org-archive-location "archive/%s_archive::")

  ;; Babel — MinEmacs extensive language support
  (setq org-babel-load-languages
        '((emacs-lisp . t) (python . t) (shell . t) (js . t) (C . t)
          (R . t) (dot . t) (awk . t) (sed . t) (sql . t) (org . t)
          (latex . t) (sqlite . t) (scheme . t) (fortran . t)
          (gnuplot . t) (plantuml . t) (makefile . t)))
  (org-babel-do-load-languages 'org-babel-load-languages
                               org-babel-load-languages)

  (setq org-confirm-babel-evaluate nil
        org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-src-window-setup 'current-window)

  ;; Export backends
  (setq org-export-backends '(ascii html icalendar latex md odt))

  ;; Org-protocol for browser capture
  (require 'org-protocol nil t))

;;;; ============================================================
;;;; Agenda — Centaur polished UI
;;;; ============================================================

(use-package org-agenda
  :straight (:type built-in)
  :after org
  :custom
  (org-agenda-tags-column 0)
  (org-agenda-span 'day)
  (org-agenda-start-with-log-mode t)
  (org-agenda-include-diary t)
  (org-agenda-use-time-grid t)
  (org-agenda-block-separator ?─)
  (org-agenda-current-time-string
   "⭠ now ─────────────────────────────────────────────────")
  (org-agenda-time-grid '((daily today require-timed)
                          (800 1000 1200 1400 1600 1800 2000)
                          " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")))

;;;; ============================================================
;;;; Export — MinEmacs academic pipeline
;;;; ============================================================

;; LaTeX export — academic quality
(use-package ox-latex
  :straight (:type built-in)
  :after org
  :custom
  (org-latex-src-block-backend 'engraved)
  (org-latex-prefer-user-labels t)
  (org-latex-tables-booktabs t)
  (org-latex-pdf-process
   '("latexmk -c -bibtex-cond1 %f"
     "latexmk -f -pdf -%latex -shell-escape -interaction=nonstopmode -output-directory=%o %f")))

;; Additional export formats (Beamer, ODT)
(use-package ox
  :straight (:type built-in)
  :after org
  :config
  (require 'ox-odt nil t)
  (require 'ox-beamer nil t))

;; GFM export (from Centaur)
(use-package ox-gfm
  :straight t
  :after org
  :config
  (add-to-list 'org-export-backends 'gfm))

;; Citations
(use-package oc
  :straight (:type built-in)
  :after org
  :custom
  (org-cite-export-processors '((latex biblatex) (t csl)))
  :config
  (require 'oc-csl nil t)
  (require 'oc-natbib nil t)
  (require 'oc-biblatex nil t))

;;;; ============================================================
;;;; Org enhancement packages
;;;; ============================================================

;; Contributed packages to Org
(use-package org-contrib
  :straight (:host github :repo "abougouffa/org-contrib" :branch "master"))

;; Convert font-lock faces to other formats (for export)
(use-package engrave-faces
  :straight t)

;; Convenience functions for Org export (provided by org-contrib)
(use-package ox-extra
  :straight nil
  :after (ox org-contrib)
  :demand
  :config
  (ox-extras-activate '(latex-header-blocks ignore-headlines)))

;; Auto-toggle Org elements
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
  (run-at-time nil nil #'org-appear--set-elements))

;; Modern Org style — MinEmacs color-coded TODO badges
(use-package org-modern
  :straight t
  :after org
  :custom-face
  (org-modern-tag ((t (:inherit org-verbatim :weight regular :foreground "black" :background "LightGray" :box "black"))))
  :custom
  (org-modern-table-vertical 5)
  (org-modern-table-horizontal 2)
  (org-modern-block-fringe nil)
  (org-modern-checkbox nil)
  (org-modern-todo-faces
   '(("IDEA" . (:inherit org-verbatim :weight semi-bold :foreground "white" :background "goldenrod"))
     ("STRT" . (:inherit org-verbatim :weight semi-bold :foreground "white" :background "OrangeRed"))
     ("WAIT" . (:inherit org-verbatim :weight semi-bold :foreground "white" :background "coral"))
     ("KILL" . (:inherit org-verbatim :weight semi-bold :foreground "white" :background "DarkGreen"))
     ("PROJ" . (:inherit org-verbatim :weight semi-bold :foreground "white" :background "LimeGreen"))
     ("HOLD" . (:inherit org-verbatim :weight semi-bold :foreground "white" :background "orange"))
     ("LOOP" . (:inherit org-verbatim :weight semi-bold :foreground "white" :background "SteelBlue"))
     ("DONE" . (:inherit org-verbatim :weight semi-bold :foreground "black" :background "LightGray"))))
  :init
  (global-org-modern-mode 1))

;; LaTeX fragment auto-preview
(use-package org-fragtog
  :straight t
  :hook (org-mode . org-fragtog-mode)
  :custom
  (org-fragtog-preview-delay 0.2))

;;;; ============================================================
;;;; Bibliography — MinEmacs citar
;;;; ============================================================

(use-package citar
  :straight t
  :custom
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  (citar-symbol-separator "  ")
  :config
  (with-eval-after-load 'nerd-icons
    (setq citar-symbols
          `((file ,(nerd-icons-codicon "nf-cod-file_pdf" :face 'error) . " ")
            (note ,(nerd-icons-faicon "nf-fa-file_text" :face 'warning) . " ")
            (link ,(nerd-icons-mdicon "nf-md-link" :face 'org-link) . " ")))))

(use-package citar-embark
  :straight t
  :after citar embark
  :init
  (citar-embark-mode 1))

;;;; ============================================================
;;;; Knowledge management — Centaur org-roam
;;;; ============================================================

(use-package org-roam
  :straight t
  :if (and (fboundp 'sqlite-available-p) (sqlite-available-p))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ("C-c n j" . org-roam-dailies-capture-today))
  :custom
  (org-roam-directory (expand-file-name "roam" org-directory))
  (org-roam-completion-everywhere t)
  (org-roam-db-gc-threshold most-positive-fixnum)
  :config
  (setq org-roam-node-display-template
        (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (unless (file-exists-p org-roam-directory)
    (make-directory org-roam-directory t))
  ;; Advise org-roam to also index project notes/ directories
  (ee-org--setup-roam-project-advice)
  (org-roam-db-autosync-mode))

;; Interactive graph visualization
(use-package org-roam-ui
  :straight t
  :after org-roam
  :bind ("C-c n u" . org-roam-ui-mode))

;;;; ============================================================
;;;; Productivity — Centaur pomodoro
;;;; ============================================================

(use-package org-pomodoro
  :straight t
  :after org
  :custom-face
  (org-pomodoro-mode-line ((t (:inherit warning))))
  (org-pomodoro-mode-line-overtime ((t (:inherit error))))
  (org-pomodoro-mode-line-break ((t (:inherit success))))
  :bind (:map org-mode-map
         ("C-c C-x m" . org-pomodoro))
  :init
  (with-eval-after-load 'org-agenda
    (bind-keys :map org-agenda-mode-map
      ("K" . org-pomodoro)
      ("C-c C-x m" . org-pomodoro))))

;;;; ============================================================
;;;; Image & media
;;;; ============================================================

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
  (unless (fboundp 'url-handler-file-remote-p)
    (defun url-handler-file-remote-p (file)
      "Compatibility function for older Emacs versions."
      (and (stringp file) (file-remote-p file)))))

;;;; ============================================================
;;;; Keybindings
;;;; ============================================================

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c l") 'org-store-link)
  (define-key org-mode-map (kbd "C-c C-w") 'org-refile)
  (define-key org-mode-map (kbd "M-h") 'org-metaleft)
  (define-key org-mode-map (kbd "M-l") 'org-metaright)
  (define-key org-mode-map (kbd "M-j") 'org-metadown)
  (define-key org-mode-map (kbd "M-k") 'org-metaup))

;; Project management keybindings (global — work from any buffer)
(define-prefix-command 'ee-org-project-map)
(global-set-key (kbd "C-c P") 'ee-org-project-map)
(define-key ee-org-project-map (kbd "r") #'ee-org-register-project)
(define-key ee-org-project-map (kbd "u") #'ee-org-unregister-project)
(define-key ee-org-project-map (kbd "l") #'ee-org-list-projects)

(provide 'ee-org)
;;; ee-org.el ends here
