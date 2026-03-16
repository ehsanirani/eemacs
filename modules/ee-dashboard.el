;;; ee-dashboard.el --- Startup dashboard for eemacs -*- lexical-binding: t; -*-

(defvar ee-dashboard-recover-layout-p nil
  "Whether to recover window layout when quitting dashboard.")

(defun ee-dashboard-open ()
  "Open the *dashboard* buffer, remembering layout for recovery."
  (interactive)
  ;; Remember layout if multiple windows are open
  (when (length> (window-list-1) 1)
    (setq ee-dashboard-recover-layout-p t))
  (delete-other-windows)
  (dashboard-refresh-buffer)
  (dashboard-goto-recent-files))

(defun ee-dashboard-quit ()
  "Quit dashboard and recover previous window layout if needed."
  (interactive)
  (when (buffer-live-p (get-buffer dashboard-buffer-name))
    (kill-buffer dashboard-buffer-name))
  (when ee-dashboard-recover-layout-p
    (when (bound-and-true-p winner-mode)
      (winner-undo))
    (setq ee-dashboard-recover-layout-p nil)))

(defun ee-dashboard-restore-session ()
  "Restore previous desktop session."
  (interactive)
  (ee-dashboard-quit)
  (desktop-read))

(require 'transient)

;; Dashboard defines section-jump keys as anonymous lambdas at runtime.
;; We wrap them so transient can reference them by name.
(defun ee-dashboard-goto-agenda ()
  "Jump to agenda section." (interactive)
  (funcall (lookup-key dashboard-mode-map "a")))

(defun ee-dashboard-goto-recents ()
  "Jump to recent files section." (interactive)
  (funcall (lookup-key dashboard-mode-map "r")))

(defun ee-dashboard-goto-projects ()
  "Jump to projects section." (interactive)
  (funcall (lookup-key dashboard-mode-map "p")))

(defun ee-dashboard-goto-bookmarks ()
  "Jump to bookmarks section." (interactive)
  (funcall (lookup-key dashboard-mode-map "m")))

(transient-define-prefix ee-dashboard-help ()
  "Dashboard help menu."
  [:description "Dashboard"
   ["Navigate"
    ("a" "Agenda"       ee-dashboard-goto-agenda)
    ("r" "Recent files" ee-dashboard-goto-recents)
    ("p" "Projects"     ee-dashboard-goto-projects)
    ("m" "Bookmarks"    ee-dashboard-goto-bookmarks)]
   ["Actions"
    ("c" "Capture"      org-capture)
    ("A" "Open agenda"  org-agenda)
    ("f" "Find file"    project-find-file)
    ("n" "Roam find"    org-roam-node-find)]
   ["Dashboard"
    ("g" "Refresh"      dashboard-refresh-buffer)
    ("R" "Restore session" ee-dashboard-restore-session)
    ("q" "Quit"         ee-dashboard-quit)]])

(use-package dashboard
  :straight t
  :diminish
  :demand t
  :bind ("C-c d" . ee-dashboard-open)
  :custom
  ;; General
  (dashboard-banner-logo-title "eemacs")
  (dashboard-startup-banner 'logo)
  (dashboard-center-content t)
  (dashboard-vertically-center-content t)
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  (dashboard-icon-type 'nerd-icons)

  ;; Widgets to display, in order
  (dashboard-items '((agenda    . 7)
                     (recents   . 8)
                     (projects  . 5)
                     (bookmarks . 5)))

  ;; Org-agenda integration
  (dashboard-filter-agenda-entry 'dashboard-no-filter-agenda)
  (dashboard-agenda-release-buffers t)
  (dashboard-agenda-sort-strategy '(time-up))
  (dashboard-agenda-prefix-format " %i %-12:c %s ")
  (dashboard-agenda-tags-format 'ignore)

  ;; Project backend
  (dashboard-projects-backend 'project-el)

  ;; Path display
  (dashboard-path-style 'truncate-middle)
  (dashboard-path-max-length 60)

  ;; Navigation
  (dashboard-set-navigator t)

  :config
  (dashboard-setup-startup-hook)

  ;; Dashboard keybindings (set directly to override evil-mode)
  (define-key dashboard-mode-map (kbd "R") #'ee-dashboard-restore-session)
  (define-key dashboard-mode-map (kbd "q") #'ee-dashboard-quit)
  (define-key dashboard-mode-map (kbd "?") #'ee-dashboard-help)
  (with-eval-after-load 'evil
    (evil-define-key 'normal dashboard-mode-map
      (kbd "?") #'ee-dashboard-help
      (kbd "q") #'ee-dashboard-quit
      (kbd "R") #'ee-dashboard-restore-session))
  ;; Navigator buttons: quick access to common actions
  (setq dashboard-navigator-buttons
        `(((,(nerd-icons-codicon "nf-cod-new_file" :height 1.0 :v-adjust -0.1)
            "Capture" "Org capture (C-c x)"
            (lambda (&rest _) (org-capture)))
           (,(nerd-icons-codicon "nf-cod-calendar" :height 1.0 :v-adjust -0.1)
            "Agenda" "Org agenda (C-c a)"
            (lambda (&rest _) (org-agenda)))
           (,(nerd-icons-mdicon "nf-md-file_find" :height 1.0 :v-adjust -0.1)
            "Find file" "Project file"
            (lambda (&rest _) (project-find-file)))
           (,(nerd-icons-codicon "nf-cod-graph" :height 1.0 :v-adjust -0.1)
            "Roam" "Org-roam find (C-c n f)"
            (lambda (&rest _) (org-roam-node-find))))))

  ;; Footer hint — insert centered help hint after the default footer
  (advice-add 'dashboard-insert-footer :after
              (lambda (&rest _)
                (let ((hint "[Press ? for help]"))
                  (insert "\n\n")
                  (insert (make-string (max 0 (/ (- (window-width) (length hint)) 2)) ?\s))
                  (insert (propertize hint 'face 'font-lock-comment-face)))))

  ;; Ensure org-agenda-files is populated before dashboard renders agenda widget
  (with-eval-after-load 'org
    (ee-org--update-agenda-files)))

(provide 'ee-dashboard)
;;; ee-dashboard.el ends here
