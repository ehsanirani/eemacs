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

;; --- Hunspell / ispell backend ---
;; On NixOS, add to your system configuration:
;;   environment.systemPackages = with pkgs; [
;;     hunspell
;;     hunspellDicts.en_US
;;     hunspellDicts.de_DE
;;     hunspellDicts.fa_IR
;;   ];
;; On other systems, install hunspell and dictionaries via your package manager
;; (e.g. apt install hunspell hunspell-en-us hunspell-de-de hunspell-fa).
(when (executable-find "hunspell")
  (require 'ispell)
  (setq ispell-program-name "hunspell"
        ispell-dictionary    "en_US")

  (dolist (entry '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" t ("-d" "en_US") nil utf-8)
                   ("de_DE" "[[:alpha:]]" "[^[:alpha:]]" "[']" t ("-d" "de_DE") nil utf-8)
                   ("fa_IR" "[[:alpha:]؀-ۿ‌]" "[^[:alpha:]؀-ۿ‌]" "[']" t ("-d" "fa-IR") nil utf-8)
                   ("multi" "[[:alpha:]]" "[^[:alpha:]]" "[']" t ("-d" "en_US,de_DE,fa-IR") nil utf-8)))
    (add-to-list 'ispell-local-dictionary-alist entry)))

;; Defined unconditionally so the symbol always exists; errors clearly if hunspell is absent.
(defun ee/ispell-switch-language (arg)
  "Switch the active ispell dictionary.
Without a prefix argument, switch globally (all buffers).
With a prefix argument (\\[universal-argument]), switch only the current buffer."
  (interactive "P")
  (unless (executable-find "hunspell")
    (user-error "hunspell not found; install it and its dictionaries first"))
  (let ((lang (completing-read "Ispell language: "
                               (mapcar #'car
                                       (seq-filter (lambda (e)
                                                     (member (car e) '("en_US" "de_DE" "fa_IR" "multi")))
                                                   ispell-local-dictionary-alist))
                               nil t)))
    (ispell-change-dictionary lang (not arg))))

(global-set-key (kbd "C-c S") #'ee/ispell-switch-language)

(provide 'ee-spelling)
;;; ee-spelling.el ends here
