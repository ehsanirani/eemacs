;;; ee-modeline.el --- Light, modern mode-line for eemacs -*- lexical-binding: t -*-

;; Copyright (C) 2023-2026  Protesilaos Stavrou
;; Copyright (C) 2025  Abdelhak Bougouffa

;; Based on: https://protesilaos.com/emacs/dotemacs
;; Adapted for eemacs from MinEmacs

;; SPDX-License-Identifier: GPL-3.0

;;; Commentary:
;; A lightweight, opinionated mode-line based on Protesilaos Stavrou's design.

;;; Code:

(require 'nerd-icons)

;; Helper function (from MinEmacs me-lib.el)
(defun +unquote (expr)
  "Return EXPR unquoted."
  (declare (pure t) (side-effect-free t))
  (while (memq (car-safe expr) '(quote function))
    (setq expr (cadr expr)))
  expr)

(defconst ee-modeline--icon-alist
  (cl-loop for glyph-set in nerd-icons-glyph-sets
           for sym = (intern (format "nerd-icons/%s-alist" glyph-set))
           for name = (caar (symbol-value sym))
           collect (cons (nth 1 (string-split name "-"))
                         (intern (format "nerd-icons-%s" glyph-set)))))

(defun ee-modeline--icon (name &rest args)
  "Generic function to get icons by NAME, with ARGS."
  (if-let* ((variant (nth 1 (string-split name "-")))
            (fn (alist-get variant ee-modeline--icon-alist nil nil #'equal)))
      (apply fn (cons name args))
    (error "Cannot detect the function which provides %S" name)))

(defgroup ee-modeline nil
  "Custom modeline that is stylistically close to the default."
  :group 'mode-line)

(defvar-local ee-modeline-disabled-sections nil)

(defmacro ee-modeline-define-section (name &rest body)
  "Define section NAME with BODY in the :eval part."
  (declare (indent 1))
  (let ((var-sym (intern (format "ee-modeline-%s" (+unquote name)))))
    `(progn
       (defvar ,var-sym
         '(:eval (when (not (memq ',(+unquote name) ee-modeline-disabled-sections)) ,@body)))
       (put ',var-sym 'risky-local-variable t))))

;;; Faces

(defface ee-modeline-inverse-video-face '((t (:inverse-video t)))
  "Inverse video face."
  :group 'ee-modeline)

;;; Keyboard macro indicator

(ee-modeline-define-section kbd-macro
  (when (and (mode-line-window-selected-p) defining-kbd-macro
             (not (bound-and-true-p kmacro-x-mc-mode)))
    (concat " " (ee-modeline--icon "nf-md-record_rec"))))

;;; Narrow indicator

(ee-modeline-define-section narrow
  (when (and (mode-line-window-selected-p)
             (buffer-narrowed-p)
             (not (derived-mode-p 'Info-mode 'help-mode 'special-mode 'message-mode 'archive-mode)))
    (concat " " (ee-modeline--icon "nf-md-arrow_collapse_vertical"))))

(ee-modeline-define-section multiple-cursors
  (when-let* ((count-face
               (cond ((and (or (bound-and-true-p iedit-mode) (bound-and-true-p iedit-rectangle-mode))
                           (bound-and-true-p iedit-occurrences-overlays))
                      (cons (length iedit-occurrences-overlays) 'nerd-icons-purple))
                     ((bound-and-true-p iedit-rectangle-mode) (bound-and-true-p iedit-occurrences-overlays))
                     ((bound-and-true-p kmacro-x-mc-mode)
                      (cons (length kmacro-x-mc-cursors) 'nerd-icons-red))
                     ((bound-and-true-p multiple-cursors-mode)
                      (cons (mc/num-cursors) 'nerd-icons-blue)))))
    (propertize (concat " " (ee-modeline--icon "nf-fa-i_cursor") (format " %d " (car count-face)))
                'face `(,(cdr count-face) ee-modeline-inverse-video-face))))

;;; Input method

(ee-modeline-define-section input-method
  (when current-input-method-title
    (propertize (format " %s " current-input-method-title)
                'face '(nerd-icons-green ee-modeline-inverse-video-face)
                'mouse-face 'mode-line-highlight)))

;;; Buffer status

(ee-modeline-define-section buffer-status
  (concat
   (when overwrite-mode
     (concat " " (ee-modeline--icon "nf-fa-pencil" :face 'nerd-icons-red)))
   (ee-modeline--remote-indicator)))

(defvar ee-modeline--remotes-cache (make-hash-table :test #'equal))

(defun ee-modeline--remote-indicator ()
  (when-let* (((file-remote-p default-directory))
              (vec (and (fboundp 'tramp-dissect-file-name) (tramp-dissect-file-name default-directory)))
              (icons
               (or (gethash vec ee-modeline--remotes-cache)
                   (puthash vec (cl-loop
                                 for file in (tramp-compute-multi-hops vec)
                                 collect
                                 (let* ((icon-face (pcase (tramp-file-name-method file)
                                                     ((rx (or "scp" "fcp" "scpx" "pscp" "ssh" "sshx" "sshfs")) '("nf-md-ssh" . nerd-icons-lred))
                                                     ("adb" "nf-dev-android")
                                                     ("mtp" "nf-fa-mobile_phone")
                                                     ("gdrive" "nf-fa-google")
                                                     ("nextcloud" "nf-fa-cloud")
                                                     ("kubernetes" "nf-dev-kubernetes")
                                                     ("ftp" "nf-md-folder_network_outline")
                                                     ("plinkx?" "nf-dev-putty")
                                                     ("p?sftp" "nf-md-folder_key_network_outline")
                                                     ("smb" '("nf-dev-windows" . nerd-icons-blue))
                                                     ("davs?" "nf-fa-globe")
                                                     ((rx (or "rsh" "rcp")) "nf-fa-freebsd")
                                                     ((rx (or "rsync" "rclone")) "nf-fa-clone")
                                                     ((rx (or "docker" "dockercp")) '("nf-fa-docker" . nerd-icons-blue))
                                                     ((rx (or "podman" "podmancp")) '("nf-dev-podman" . nerd-icons-purple))
                                                     ((rx (or "lxc" "lxd" "incus")) '("nf-oct-container" . nerd-icons-yellow))
                                                     ((rx (or "sudo" "su" "androidsu" "doas" "sudoedit")) '("nf-md-pound_box" . nerd-icons-red))
                                                     (_ "nf-md-folder_network_outline")))
                                        (icon (if (consp icon-face) (car icon-face) icon-face))
                                        (face (if (consp icon-face) (cdr icon-face) 'nerd-icons-green)))
                                   (propertize (ee-modeline--icon icon :face face)
                                               'help-echo (concat
                                                           (tramp-file-name-method file) ":"
                                                           (when-let* ((u (tramp-file-name-user file))) (concat u "@"))
                                                           (tramp-file-name-host file)))))
                            ee-modeline--remotes-cache))))
    (concat " " (string-join icons (ee-modeline--icon "nf-oct-arrow_right" :face 'nerd-icons-dsilver)))))

;;; Dedicated window

(ee-modeline-define-section window-dedicated-status
  (when (window-dedicated-p) (concat " " (ee-modeline--icon "nf-oct-pin" :face 'nerd-icons-dred))))

;;; Buffer name and modified status

(ee-modeline-define-section buffer-identification
  (concat (and buffer-read-only (concat (ee-modeline--icon "nf-fa-lock") " "))
          (propertize
           (buffer-name)
           'face (let ((file (buffer-file-name)))
                   (cond ((and (mode-line-window-selected-p) file (buffer-modified-p)) '(error italic mode-line-buffer-id))
                         ((and file (buffer-modified-p)) 'italic)
                         ((mode-line-window-selected-p) 'mode-line-buffer-id)))
           'mouse-face 'mode-line-highlight
           'help-echo (concat
                       (propertize (buffer-name) 'face 'mode-line-buffer-id) "\n"
                       (propertize
                        (or (buffer-file-name) (format "No underlying file.\nDirectory is: %s" default-directory))
                        'face 'font-lock-doc-face)))))

(ee-modeline-define-section region-size
  (when (region-active-p)
    (format " %d:%d"
            (- (line-number-at-pos (region-end)) (line-number-at-pos (region-beginning)))
            (- (region-end) (region-beginning)))))

(ee-modeline-define-section project
  (when-let* ((fname (or (buffer-file-name) default-directory))
              ((not (file-remote-p fname)))
              (proj (project-current))
              (name (project-name proj)))
    (concat
     " "
     (propertize
      (concat " " name " ")
      'face `(,(if (mode-line-window-selected-p) '(mode-line-buffer-id)) ee-modeline-inverse-video-face)
      'mouse-face 'mode-line-highlight
      'help-echo (propertize (concat "Project root: " (project-root proj)) 'face 'font-lock-doc-face))
     " ")))

;;; Major mode

(ee-modeline-define-section major-mode-icon
  (cond ((buffer-file-name)
         (nerd-icons-icon-for-file (buffer-file-name)))
        ((and (bound-and-true-p dired-mode) dired-directory)
         (nerd-icons-icon-for-dir dired-directory))
        (t (nerd-icons-icon-for-mode major-mode))))

(ee-modeline-define-section process
  (list '("" mode-line-process)))

;;; Git branch and diffstat

(declare-function vc-git--symbolic-ref "vc-git" (file))

(defvar ee-modeline-vc-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line down-mouse-1] 'vc-diff)
    (define-key map [mode-line mouse-3] 'vc-root-diff)
    map))

(defvar ee-modeline--vc-faces
  '((added . vc-locally-added-state)
    (edited . vc-edited-state)
    (removed . vc-removed-state)
    (missing . vc-missing-state)
    (conflict . vc-conflict-state)
    (locked . vc-locked-state)
    (up-to-date . vc-up-to-date-state)))

(ee-modeline-define-section vc-branch
  (when-let* (((mode-line-window-selected-p))
              (file (or buffer-file-name default-directory))
              ((not (file-remote-p file)))
              (backend (or (vc-backend file) 'Git))
              (file-state (vc-state file backend))
              (face (alist-get file-state ee-modeline--vc-faces 'vc-up-to-date-state))
              (rev (vc-working-revision file backend))
              (branch (or (vc-git--symbolic-ref file) (substring rev 0 7)))
              (help-echo-msg (format "Branch: %s\nRevision: %s\nmouse-1: `vc-diff', diff the current file\nmouse-3: `vc-root-diff', diff all project files" branch rev))
              (branch-trim (if-let* ((len (length branch))
                                     ((> len 15)))
                               (concat ".." (substring branch (- len 15) len))
                             branch)))
    (concat
     (ee-modeline--icon "nf-fa-code_branch" :face 'shadow)
     " "
     (propertize branch-trim
                 'face face
                 'mouse-face 'mode-line-highlight
                 'help-echo help-echo-msg
                 'local-map ee-modeline-vc-map))))

;;; Flymake errors, warnings, notes

(declare-function flymake--severity "flymake" (type))
(declare-function flymake-diagnostic-type "flymake" (diag))

(defun ee-modeline-flymake-counter (type)
  "Compute number of diagnostics in buffer with TYPE's severity.
TYPE is usually keyword `:error', `:warning' or `:note'."
  (let ((count 0))
    (dolist (d (flymake-diagnostics))
      (when (= (flymake--severity type) (flymake--severity (flymake-diagnostic-type d)))
        (cl-incf count)))
    (when (cl-plusp count)
      (number-to-string count))))

(defvar ee-modeline-flymake-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line down-mouse-1] 'flymake-show-buffer-diagnostics)
    (define-key map [mode-line mouse-3] 'flymake-show-project-diagnostics)
    map))

(defmacro ee-modeline-flymake-type (type icon &optional face)
  "Return function that handles Flymake TYPE with stylistic ICON and FACE."
  `(defun ,(intern (format "ee-modeline-flymake-%s" type)) ()
     (when-let* ((count (ee-modeline-flymake-counter ,(intern (format ":%s" type)))))
       (concat
        " "
        (ee-modeline--icon ,icon :face ',(or face type))
        " "
        (propertize count
                    'face ',(or face type)
                    'mouse-face 'mode-line-highlight
                    'local-map ee-modeline-flymake-map
                    'help-echo "mouse-1: buffer diagnostics\nmouse-3: project diagnostics")))))

(ee-modeline-flymake-type error "nf-cod-error" nerd-icons-red)
(ee-modeline-flymake-type warning "nf-cod-warning" nerd-icons-orange)
(ee-modeline-flymake-type note "nf-cod-info" nerd-icons-green)

(ee-modeline-define-section flymake
  (when (and (mode-line-window-selected-p) (bound-and-true-p flymake-mode))
    (list
     '(:eval (ee-modeline-flymake-error))
     '(:eval (ee-modeline-flymake-warning))
     '(:eval (ee-modeline-flymake-note)))))

;;; Eglot

(with-eval-after-load 'eglot
  (setq mode-line-misc-info
        (delete '(eglot--managed-mode (" [" eglot--mode-line-format "] ")) mode-line-misc-info)))

(ee-modeline-define-section eglot
  (when (and (mode-line-window-selected-p) (featurep 'eglot))
    '(eglot--managed-mode eglot--mode-line-format)))

;;; Compilation

(ee-modeline-define-section compile
  (when (and (mode-line-window-selected-p) (bound-and-true-p compilation-in-progress))
    (propertize (ee-modeline--icon "nf-fa-hammer" :face 'nerd-icons-red)
                'mouse-face 'mode-line-highlight
                'help-echo (mapconcat #'buffer-name (mapcar #'process-buffer compilation-in-progress) "\n"))))

;;; Miscellaneous

(ee-modeline-define-section misc-info
  (when (mode-line-window-selected-p) mode-line-misc-info))

;;; Subtle mode-line styling

(defun +subtle-mode-line (&rest _args)
  "Subtle look for the mode-line."
  (when (display-graphic-p)
    (set-face-attribute
     'mode-line-active nil
     :box `(:line-width 4 :color ,(face-attribute 'default :background nil t) :style nil)
     :overline (face-attribute 'default :foreground nil t)
     :background (face-attribute 'default :background nil t))

    (set-face-attribute
     'mode-line-inactive nil
     :box `(:line-width 4 :color ,(face-attribute 'mode-line-inactive :background nil t) :style nil)
     :overline (face-attribute 'mode-line-inactive :foreground nil t))))

;;; Mode

(defvar ee-modeline--mode-line-format-orig nil)

;;;###autoload
(define-minor-mode ee-modeline-mode
  "eemacs' custom mode-line."
  :global t
  (if ee-modeline-mode
      (progn
        (unless ee-modeline--mode-line-format-orig
          (setq ee-modeline--mode-line-format-orig (default-value 'mode-line-format)))
        (setq-default mode-line-format
                      '("%e"
                        ee-modeline-multiple-cursors
                        ee-modeline-kbd-macro
                        ee-modeline-narrow
                        ee-modeline-buffer-status
                        ee-modeline-window-dedicated-status
                        ee-modeline-input-method
                        "  "
                        ee-modeline-project
                        ee-modeline-major-mode-icon
                        " "
                        ee-modeline-buffer-identification
                        "  "
                        mode-line-position
                        ee-modeline-region-size
                        "  "
                        ee-modeline-process
                        "  "
                        ee-modeline-eglot
                        "  "
                        mode-line-format-right-align
                        "  "
                        ee-modeline-compile
                        "  "
                        ee-modeline-vc-branch
                        "  "
                        ee-modeline-flymake
                        "  "
                        ee-modeline-misc-info
                        "  "))
        (+subtle-mode-line)
        (add-hook 'server-after-make-frame-hook #'+subtle-mode-line)
        (add-hook 'enable-theme-functions #'+subtle-mode-line))
    (setq-default mode-line-format ee-modeline--mode-line-format-orig)
    (remove-hook 'server-after-make-frame-hook #'+subtle-mode-line)
    (remove-hook 'enable-theme-functions #'+subtle-mode-line))
  (force-mode-line-update t))


(provide 'ee-modeline)
;;; ee-modeline.el ends here
