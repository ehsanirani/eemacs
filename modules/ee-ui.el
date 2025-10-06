;;; ee-ui.el --- UI configuration for eemacs -*- lexical-binding: t; -*-

;; Theme packages
(use-package doom-themes
  :straight t
  :defer t
  :init
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t))

(use-package ef-themes
  :straight t
  :defer t
  :init
  (setq ef-themes-mixed-fonts t
        ef-themes-variable-pitch-ui t))

(use-package catppuccin-theme
  :straight t
  :config
  (setq catppuccin-flavor 'mocha
        catppuccin-accent 'mauve
        catppuccin-italic-comments t
        catppuccin-colored-modes t
        catppuccin-mode-line-style 'colored)
  (custom-set-faces
   `(diff-hl-delete ((t (:background unspecified :foreground ,(catppuccin-get-color 'red)))))
   `(diff-hl-insert ((t (:background unspecified :foreground ,(catppuccin-get-color 'green)))))))

;; Basic UI settings
(use-package emacs
  :init
  (setq scroll-margin 3
        scroll-conservatively 101
        scroll-up-aggressively 0.01
        scroll-down-aggressively 0.01)

  :config
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (column-number-mode 1)
  (delete-selection-mode 1)

  (set-fringe-mode 8)
  (set-display-table-slot standard-display-table 'vertical-border (make-glyph-code ?│))

  (setq x-stretch-cursor t
        blink-cursor-interval 0.5
        blink-cursor-blinks 10
        frame-resize-pixelwise t
        window-divider-default-bottom-width 2
        window-divider-default-right-width 2)

  (setq display-line-numbers-type 'relative
        display-line-numbers-width 4
        display-line-numbers-widen t)

  (global-display-line-numbers-mode 1)
  (window-divider-mode 1)

  (when (fboundp 'pixel-scroll-precision-mode)
    (pixel-scroll-precision-mode 1)
    (setq pixel-scroll-precision-use-momentum t))

  (setq mouse-wheel-scroll-amount '(2 ((shift) . hscroll))
        mouse-wheel-scroll-amount-horizontal 2))

;; Modeline
(use-package doom-modeline
  :straight t
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-height 45
        doom-modeline-bar-width 4
        doom-modeline-window-width-limit 85))

;; Icons
(use-package nerd-icons
  :straight t
  :config
  (setq nerd-icons-dired-icons-enabled t
        nerd-icons-completion-icons-enabled t)
  (add-hook 'dired-mode-hook 'nerd-icons-dired-mode))

;; Configuration variable for code block highlighting
(defvar ee-enable-code-block-highlighting nil
  "Whether to enable subtle background highlighting for code blocks.")

;; Enhanced code block highlighting - targeted approach for markdown
(defun ee-highlight-code-blocks ()
  "Add subtle background highlighting to markdown code blocks only."
  (interactive)
  (when (eq major-mode 'markdown-mode)
    (save-excursion
      (goto-char (point-min))
      ;; Clear existing overlays first
      (remove-overlays)
      (while (re-search-forward "^```.*$" nil t)
        (let ((start-pos (match-beginning 0)))
          ;; Look for the matching closing ```
          (when (re-search-forward "^```$" nil t)
            (let ((end-pos (match-end 0)))
              ;; Add subtle background overlay to the entire code block
              (let ((overlay (make-overlay start-pos end-pos)))
                (overlay-put overlay 'face '(:background "#282828"))
                (overlay-put overlay 'ee-code-block t)  ; Mark it as our overlay
                (overlay-put overlay 'evaporate t))     ; Auto-remove when text is deleted
              ;; Make sure code within the block uses enhanced foreground colors
              (save-excursion
                (goto-char (1+ start-pos))  ; Skip the opening ``` line
                (while (< (point) (1- end-pos))  ; Stop before the closing ``` line
                  (when (not (looking-at "^```$"))
                    (let ((line-start (line-beginning-position))
                          (line-end (line-end-position)))
                      (add-face-text-property line-start line-end '(:foreground "#d4d4d4") 'append)))
                  (forward-line 1))))))))))

(defun ee-clear-code-block-highlights ()
  "Clear all code block highlighting overlays."
  (interactive)
  (remove-overlays (point-min) (point-max) 'ee-code-block t))

(defun ee-highlight-code-content-only ()
  "Highlight only the content inside code blocks, not the ``` markers."
  (interactive)
  (when (eq major-mode 'markdown-mode)
    (save-excursion
      (goto-char (point-min))
      ;; Clear existing overlays first
      (remove-overlays (point-min) (point-max) 'ee-code-block t)
      (while (re-search-forward "^```.*$" nil t)
        (let ((fence-start (match-beginning 0))
              (fence-end (match-end 0)))
          ;; Look for the matching closing ```
          (when (re-search-forward "^```$" nil t)
            (let ((content-start (1+ fence-end))  ; Start after opening fence
                  (content-end (match-beginning 0)))  ; End before closing fence
              ;; Add subtle background overlay to just the content
              (let ((overlay (make-overlay content-start content-end)))
                (overlay-put overlay 'face '(:background "#1e1e1e"))  ; Darker, more subtle
                (overlay-put overlay 'ee-code-block t)
                (overlay-put overlay 'evaporate t))
              ;; Enhance foreground colors for code content (NON-INTRUSIVE - use overlays)
              (save-excursion
                (goto-char content-start)
                (while (< (point) content-end)
                  (let ((line-start (line-beginning-position))
                        (line-end (line-end-position)))
                    ;; Use overlays instead of add-face-text-property to avoid modifying buffer
                    (let ((line-overlay (make-overlay line-start line-end)))
                      (overlay-put line-overlay 'face '(:foreground "#d4d4d4"))
                      (overlay-put line-overlay 'ee-code-content t)
                      (overlay-put line-overlay 'evaporate t)))
                  (forward-line 1))))))))))

;; Solaire mode - DISABLED by default to avoid theme conflicts
(use-package solaire-mode
  :straight t
  :defer t
  :config
  ;; Keep original functions but don't auto-enable
  (defun ee-enable-solaire ()
    "Enable solaire mode with theme-friendly settings."
    (interactive)
    (when (eq (frame-parameter nil 'background-mode) 'dark)
      (setq solaire-mode-exclude-modes '(dashboard-mode term-mode vterm-mode))
      ;; Use very subtle variations that respect your theme
      (face-remap-add-relative 'solaire-default-face :background "#2a2a2a")
      (face-remap-add-relative 'solaire-minibuffer-face :background "#2d2d2d")
      (face-remap-add-relative 'solaire-hl-line-face :background "#282828")
      (solaire-global-mode +1)
      (message "Solaire mode enabled with dark theme support")))

  (defun ee-disable-solaire ()
    "Disable solaire mode and restore original theme."
    (interactive)
    (solaire-global-mode -1)
    ;; Clear any solaire face remappings
    (mapatoms (lambda (sym)
                (when (and (facep sym)
                           (string-match "^solaire-" (symbol-name sym)))
                  (face-remap-reset-base sym))))
    (message "Solaire mode disabled - theme restored")))

;; Visual enhancements
(use-package diff-hl
  :straight t
  :config
  (setq diff-hl-flydiff-mode-threshold 1000
        diff-hl-flydiff-update-delay 0.5
        diff-hl-show-ignore-whitespace t)
  (global-diff-hl-mode 1))

(use-package rainbow-delimiters
  :straight t
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package pulsar
  :straight t
  :config
  (setq pulsar-delay 0.05
        pulsar-face 'pulsar-green
        pulsar-highlight-duration 0.1
        pulsar-pulse-only-if-changed nil
        pulsar-display-source-at-end t)
  (pulsar-global-mode 1)
  (setq pulsar-pulse-functions '(projectile-find-file embark-act ace-jump-char-mode avy-action-goto))
  (add-hook 'evil-yank-hook #'pulsar-pulse-line)
  (add-hook 'isearch-mode-hook #'pulsar-highlight-line)
  (add-hook 'isearch-mode-end-hook
            (lambda () (pulsar-highlight-lines isearch-matches))))

(defun ee-toggle-code-block-highlighting ()
  "Toggle code block background highlighting on/off."
  (interactive)
  (setq ee-enable-code-block-highlighting (not ee-enable-code-block-highlighting))
  (if ee-enable-code-block-highlighting
      (progn
        (ee-highlight-code-content-only)
        (message "Code block highlighting enabled"))
    (progn
      (ee-clear-code-block-highlights)
      (message "Code block highlighting disabled"))))

(defun ee-setup-code-block-auto-highlight ()
  "Set up automatic highlighting for markdown code blocks."
  (add-hook 'markdown-mode-hook
            (lambda ()
              (when ee-enable-code-block-highlighting
                (ee-highlight-code-content-only))))
  ;; Disable after-save-hook to avoid buffer modification issues
  ;; (add-hook 'after-save-hook
  ;;           (lambda ()
  ;;             (when (and ee-enable-code-block-highlighting
  ;;                        (eq major-mode 'markdown-mode))
  ;;               (ee-highlight-code-content-only)))))
  )

;; Enable auto-highlighting by default (DISABLED to avoid unwanted modifications)
(setq ee-enable-code-block-highlighting nil)
;; Auto-setup disabled to prevent unwanted behavior
;; (ee-setup-code-block-auto-highlight)

;; Function to preview and adjust code colors for better visibility
(defun ee-adjust-code-colors ()
  "Interactive function to adjust code colors for better visibility on dark themes."
  (interactive)
  (let* ((current-bg (face-attribute 'default :background))
         (suggestions '("#d4d4d4" "#e1e1e1" "#f0f0f0" "#a0a0a0" "#b0b0b0"))
         (choice (completing-read "Choose code color (or enter custom): " suggestions nil nil "#d4d4d4"))
         (inline-suggestions '("#ffa657" "#ff8c00" "#ffb86c" "#ff9f40" "#fd971f"))
         (inline-choice (completing-read "Choose inline code color: " inline-suggestions nil nil "#ffa657")))

    ;; Update markdown faces
    (when (boundp 'markdown-code-face)
      (face-remap-add-relative 'markdown-code-face :foreground choice)
      (face-remap-add-relative 'markdown-pre-face :foreground choice)
      (face-remap-add-relative 'markdown-inline-code-face :foreground inline-choice))

    ;; Update org faces
    (when (boundp 'org-block)
      (face-remap-add-relative 'org-block :foreground choice)
      (face-remap-add-relative 'org-block-begin-line :foreground "#8b949e")
      (face-remap-add-relative 'org-block-end-line :foreground "#8b949e"))

    (message "Code colors updated. Current: %s, Inline: %s" choice inline-choice)))

;; Debug function to check what's modifying buffers
(defun ee-debug-buffer-modifications ()
  "Debug function to help identify what's causing buffer modifications."
  (interactive)
  (when (and (buffer-modified-p) (not buffer-file-name))
    (message "Buffer is modified but has no file - this might be normal for new buffers"))
  (when (and buffer-file-name (buffer-modified-p))
    (message "Buffer %s is marked as modified" (buffer-name))
    (message "Buffer-undo-list length: %d" (length buffer-undo-list))
    (when (> (length buffer-undo-list) 0)
      (message "First undo entry: %s" (car buffer-undo-list)))
    (let ((mod-tick (buffer-modified-tick)))
      (message "Modification tick: %d" mod-tick))))

;; Add this to markdown mode for debugging
(add-hook 'markdown-mode-hook
          (lambda ()
            (when (buffer-modified-p)
              (message "WARNING: Buffer %s is modified on entry!" (buffer-name))
              (ee-debug-buffer-modifications))))

;; Function to reset all faces to theme defaults
(defun ee-reset-theme-faces ()
  "Reset all faces to theme defaults, useful if solaire or other packages mess up colors."
  (interactive)
  ;; Disable solaire if it's enabled
  (when (bound-and-true-p solaire-global-mode)
    (solaire-global-mode -1))
  ;; Clear any custom face remappings
  (mapatoms (lambda (sym)
              (when (facep sym)
                (face-remap-reset-base sym))))
  ;; Re-enable current theme
  (when custom-enabled-themes
    (let ((current-theme (car custom-enabled-themes)))
      (disable-theme current-theme)
      (load-theme current-theme t)))
  (message "Faces reset to theme defaults"))

;; Dired
(use-package dired
  :straight (:type built-in)
  :config
  (setq dired-listing-switches "-lah --group-directories-first"
        dired-dwim-target t
        dired-kill-when-opening-new-dired-buffer t))

;; Custom mode line styling
(defun +custom-mode-line-style ()
  "Apply custom mode line styling."
  (condition-case err
      (progn
        (set-face-attribute 'mode-line nil
                            :box `(:line-width 4 :color ,(face-attribute 'default :background) :style nil)
                            :overline (face-attribute 'default :foreground)
                            :background (face-attribute 'default :background))

        (set-face-attribute 'mode-line-inactive nil
                            :box `(:line-width 4 :color ,(face-attribute 'mode-line-inactive :background) :style nil)
                            :overline (face-attribute 'mode-line-inactive :foreground)))
    (error
     ;; Silently fail if faces aren't ready yet
     nil)))

(add-hook 'after-init-hook #'+custom-mode-line-style)

(defun +reapply-mode-line-style (_theme &optional _no-confirm)
  "Reapply mode line style after theme change."
  ;; Add a small delay to ensure theme is fully loaded
  (run-with-timer 0.1 nil #'+custom-mode-line-style))

(advice-add 'load-theme :after #'+reapply-mode-line-style)

(provide 'ee-ui)
