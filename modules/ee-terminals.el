;;; ee-terminals.el --- Terminal configuration for eemacs -*- lexical-binding: t; -*-

;; Vterm terminal emulator
;; On NixOS, vterm (with its native module) is provided by the Nix Emacs
;; package, so we skip straight.el to avoid recompiling from source.
;; :straight's value is not evaluated at runtime, so we branch the
;; whole form rather than passing a conditional expression.

(require 'cl-lib)

(defun ee-vterm-locate-module ()
  "Put the directory holding a prebuilt `vterm-module' on `load-path'.

Under the Nix Emacs wrapper the module is already reachable and this is a
no-op.  Other Emacsen -- neomacs, in particular -- do not inherit that
wrapper's EMACSLOADPATH, so ask the `emacs' binary where the module lives
(about 70ms) instead of hardcoding a store path that changes on every
nixpkgs update."
  (or (locate-library "vterm-module")
      (let ((lib (ignore-errors
                   (car (process-lines
                         "emacs" "-Q" "--batch" "--eval"
                         "(princ (or (locate-library \"vterm-module\") \"\"))")))))
        (when (and (stringp lib)
                   (not (string-empty-p lib))
                   (file-exists-p lib))
          (add-to-list 'load-path (file-name-directory lib))
          lib))))

(defun ee-vterm-available-p ()
  "Load vterm, returning non-nil on success.

`vterm.el' runs a blocking `y-or-n-p' at load time when `vterm-module'
cannot be found, and on some Emacsen that branch ends the session outright
-- which aborts the rest of init, since this module is required from the
middle of it.  Stubbing `y-or-n-p' turns that branch into an ordinary
error, which we catch."
  (ee-vterm-locate-module)
  (or (featurep 'vterm)
      (condition-case err
          (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) nil)))
            (require 'vterm)
            t)
        (error
         (message "ee-terminals: vterm unavailable (%s)"
                  (error-message-string err))
         nil))))

;; Prefer a prebuilt module wherever one exists; only fall back to letting
;; straight.el build vterm when none can be found.
;;
;; This deliberately does NOT test for /etc/NIXOS.  That marker is invisible
;; inside sandboxed Emacsen (neomacs runs in a bubblewrap FHS environment
;; that binds only a fixed list of /etc entries), so the test reports
;; "not NixOS" on a NixOS machine and sends us down the compile-from-source
;; path -- where vterm.el's load-time `y-or-n-p' hangs the editor at startup.
;; Asking whether the module is actually loadable answers the real question.
(if (ee-vterm-available-p)
    (setq vterm-max-scrollback 10000
          vterm-kill-buffer-on-exit t
          vterm-shell (getenv "SHELL"))
  (use-package vterm
    :straight t
    :custom
    (vterm-max-scrollback 10000)
    (vterm-kill-buffer-on-exit t)
    :config
    (setq vterm-shell (getenv "SHELL"))))

;; Vterm toggle - VS Code style bottom terminal
(use-package vterm-toggle
  :straight t
  :if (featurep 'vterm)
  :after vterm
  :custom
  (vterm-toggle-fullscreen-p nil)
  (vterm-toggle-scope 'project)
  :bind
  (("C-`" . vterm-toggle)
   ("C-c T" . vterm-toggle-cd)
   :map vterm-mode-map
   ("C-`" . vterm-toggle))
  :config
  ;; Show vterm at bottom with 30% height
  (add-to-list 'display-buffer-alist
               '((lambda (buffer-or-name _)
                   (let ((buffer (get-buffer buffer-or-name)))
                     (with-current-buffer buffer
                       (or (equal major-mode 'vterm-mode)
                           (string-prefix-p vterm-buffer-name (buffer-name buffer))))))
                 (display-buffer-reuse-window display-buffer-at-bottom)
                 (reusable-frames . visible)
                 (window-height . 0.3))))

(provide 'ee-terminals)