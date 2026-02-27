;;; ee-claude.el --- Claude Code IDE integration for eemacs -*- lexical-binding: t; -*-

;; Claude Code IDE - native integration with Claude Code CLI via MCP
;; Requires: Claude Code CLI installed and available in PATH
;; See: https://github.com/manzaltu/claude-code-ide.el

(use-package claude-code-ide
  :straight (:type git :host github :repo "manzaltu/claude-code-ide.el")
  :bind ("C-c C-'" . claude-code-ide-menu)
  :custom
  ;; Terminal backend (vterm is already configured in ee-terminals)
  (claude-code-ide-terminal-backend 'vterm)
  ;; Window placement
  (claude-code-ide-use-side-window t)
  (claude-code-ide-window-side 'right)
  (claude-code-ide-window-width 90)
  ;; Ediff integration for reviewing code suggestions
  (claude-code-ide-use-ide-diff t)
  ;; vterm rendering optimization
  (claude-code-ide-vterm-anti-flicker t)
  (claude-code-ide-vterm-render-delay 0.005)
  (claude-code-ide-prevent-reflow-glitch t)
  ;; MCP server for exposing Emacs tools to Claude
  (claude-code-ide-enable-mcp-server t)
  :config
  ;; Register Emacs MCP tools (LSP/xref, tree-sitter, imenu, project, diagnostics)
  (claude-code-ide-emacs-tools-setup))

(provide 'ee-claude)
;;; ee-claude.el ends here
