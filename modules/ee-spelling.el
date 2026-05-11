;;; ee-spelling.el --- Spell-checking for eemacs -*- lexical-binding: t; -*-

;; On NixOS, jinx is provided via programs.emacs.extraPackages (epkgs.jinx).
;; On other systems, straight.el fetches and compiles it (requires enchant2
;; dev headers and a C compiler to be present at install time).
(defvar ee-nixos-p (file-exists-p "/etc/NIXOS")
  "Non-nil when running on NixOS.")

(unless ee-nixos-p
  (straight-use-package 'jinx))

(use-package jinx
  :straight nil
  :hook (emacs-startup . global-jinx-mode)
  :custom
  (jinx-languages "en_US de_DE")
  :bind
  ([remap ispell-word] . jinx-correct)
  ("C-M-$"            . jinx-languages))

(provide 'ee-spelling)
;;; ee-spelling.el ends here
