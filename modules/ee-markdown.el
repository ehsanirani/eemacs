;;; ee-markdown.el --- Markdown configuration for eemacs -*- lexical-binding: t; -*-

;; Markdown mode following Centaur's professional approach
(use-package markdown-mode
  :straight t
  :mode (("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init
  ;; Professional markdown settings (Centaur approach)
  (setq markdown-command "markdown"
        markdown-fontify-code-blocks-natively t
        markdown-enable-highlighting-syntax t
        markdown-enable-math t
        markdown-hide-urls nil
        markdown-asymmetric-header t
        markdown-make-gfm-checkboxes-buttons t
        markdown-gfm-uppercase-checkbox t
        markdown-enable-pandoc-style-yaml-metadata t
        markdown-open-command "xdg-open"
        ;; Use external CSS for web rendering while keeping clean display
        markdown-content-type "text/html"
        markdown-css-paths '("https://cdn.jsdelivr.net/npm/github-markdown-css/github-markdown.min.css")
        markdown-xhtml-header-content "<meta name='viewport' content='width=device-width, initial-scale=1, shrink-to-fit=no'>"
        markdown-xhtml-standalone-regexp ""
        markdown-xhtml-preamble ""
        markdown-xhtml-postamble "")

  ;; Clean, minimal styling - NO BACKGROUNDS! Theme-aware colors
  (custom-set-faces
   ;; Code blocks - lighter colors for dark themes
   '(markdown-code-face ((t (:inherit fixed-pitch :foreground "#d4d4d4"))))
   '(markdown-pre-face ((t (:inherit fixed-pitch :foreground "#d4d4d4"))))
   ;; Inline code - brighter accent color for visibility
   '(markdown-inline-code-face ((t (:inherit fixed-pitch :foreground "#ffa657" :weight semi-bold)))))

  ;; Apply additional styling after markdown-mode loads - NO BORDERS!
  (with-eval-after-load 'markdown-mode
    ;; Just ensure proper typography, no decorations
    (face-remap-add-relative 'markdown-code-face
                             :inherit 'fixed-pitch
                             :foreground "#d4d4d4")
    (face-remap-add-relative 'markdown-pre-face
                             :inherit 'fixed-pitch
                             :foreground "#d4d4d4"))

  ;; Simple language indicator - just highlight the language name (NON-INTRUSIVE)
  (defun ee-markdown-highlight-languages ()
    "Add subtle highlighting to language identifiers using overlays (doesn't modify buffer)."
    (when (eq major-mode 'markdown-mode)
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward "^```\\([a-zA-Z0-9+-]+\\)$" nil t)
          (when (match-string 1)
            ;; Use overlays instead of put-text-property to avoid modifying buffer content
            (let ((overlay (make-overlay (match-beginning 1) (match-end 1))))
              (overlay-put overlay 'face '(:foreground "#8b949e" :weight normal))
              (overlay-put overlay 'ee-language-overlay t)
              (overlay-put overlay 'evaporate t)))))))

  ;; Apply language highlighting
  (add-hook 'markdown-mode-hook 'ee-markdown-highlight-languages)

  ;; Ensure proper fontification
  (add-hook 'markdown-mode-hook
            (lambda ()
              (when font-lock-mode
                (font-lock-flush)
                (font-lock-ensure))))

  ;; Professional markdown hooks (Centaur approach)
  :hook (markdown-mode . (lambda ()
                           ;; Enable visual-line-mode for better text wrapping
                           (visual-line-mode 1)
                           ;; Auto-fill mode for comfortable writing (disabled by default to avoid unwanted changes)
                           ;; (auto-fill-mode 1)  ; Uncomment if you want auto-fill
                           ;; Flyspell for spell checking
                           (flyspell-mode 1)
                           ;; Highlight current line for better focus
                           (hl-line-mode 1)
                           ;; Rainbow delimiters for better code readability in code blocks
                           (when (fboundp 'rainbow-delimiters-mode)
                             (rainbow-delimiters-mode 1))
                           ;; Optional: Add code block background highlighting
                           ;; (ee-highlight-code-content-only)  ; Uncomment to enable by default
                           ))
  :config
  ;; Mermaid diagram support (Centaur-style) - conditional loading
  (when (executable-find "mmdc")
    (use-package mermaid-mode
      :straight t
      :mode "\\.mmd\\'"
      :hook (mermaid-mode . (lambda ()
                              (setq mermaid-output-format 'svg)
                              (setq mermaid-flags "-c {\"theme\":\"neutral\"}"))))))

;; Debug function to check if highlighting is working
(defun ee-markdown-debug-faces ()
  "Debug function to check markdown faces."
  (interactive)
  (message "markdown-code-face: %s" (face-attribute 'markdown-code-face :background))
  (message "markdown-pre-face: %s" (face-attribute 'markdown-pre-face :background))
  (message "markdown-inline-code-face: %s" (face-attribute 'markdown-inline-code-face :background))
  (message "Font lock mode: %s" font-lock-mode)
  (message "Rainbow delimiters: %s" (if (bound-and-true-p rainbow-delimiters-mode) "Enabled" "Disabled")))

;; Add debug command after markdown-mode is loaded
(with-eval-after-load 'markdown-mode
  (define-key markdown-mode-map (kbd "C-c C-d") 'ee-markdown-debug-faces))

;; Markdown-toc for table of contents
(use-package markdown-toc
  :straight t
  :after markdown-mode
  :bind (:map markdown-mode-map
              ("C-c C-t" . markdown-toc-generate-toc)))

;; Additional professional features (Centaur approach) - conditional loading
(when (executable-find "grip")
  (use-package grip-mode
    :straight t
    :commands grip-mode
    :init
    (setq grip-update-after-change nil)))

;; Provide feature
(provide 'ee-markdown)
;;; ee-markdown.el ends here