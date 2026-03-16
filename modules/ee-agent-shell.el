;;; ee-agent-shell.el --- Agent Shell integration for eemacs -*- lexical-binding: t; -*-

;; Agent Shell - multi-agent ACP interface for Emacs
;; Requires: claude-agent-acp on PATH (bun install -g @zed-industries/claude-agent-acp)
;; Authentication: uses Claude Code login session by default (run `claude login` first)
;; See: https://github.com/xenodium/agent-shell

;; Dependencies — not in straight's recipe repos yet, declare GitHub sources
(use-package shell-maker
  :straight (:type git :host github :repo "xenodium/shell-maker"))

(use-package acp
  :straight (:type git :host github :repo "xenodium/acp.el"))

(use-package agent-shell
  :straight (:type git :host github :repo "xenodium/agent-shell")
  :bind ("C-c A s" . agent-shell)
  :custom
  ;; Skip the agent picker — always use Claude Code
  (agent-shell-preferred-agent-config 'claude-code)
  ;; Use login-based auth (requires `claude login` to have been run)
  (agent-shell-anthropic-authentication
   (agent-shell-anthropic-make-authentication :login t)))

(provide 'ee-agent-shell)
;;; ee-agent-shell.el ends here
