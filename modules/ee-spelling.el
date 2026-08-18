;;; ee-spelling.el --- Spell-checking for eemacs -*- lexical-binding: t; -*-

;; jinx needs a native module.  On NixOS it is prebuilt and shipped via
;; programs.emacs.extraPackages (epkgs.jinx); elsewhere straight.el fetches
;; jinx and jinx.el compiles jinx-mod.c on first use, which needs enchant-2
;; dev headers and a C compiler.
;;
;; This deliberately does NOT test for /etc/NIXOS.  That marker is invisible
;; inside sandboxed Emacsen (neomacs runs in a bubblewrap FHS environment
;; that binds only a fixed list of /etc entries), so the test reports "not
;; NixOS" on a NixOS machine, straight builds a jinx without the module, and
;; every single `find-file' then reports
;;
;;   File mode specification error: (error "Jinx: Compilation of jinx-mod.so failed")
;;
;; because `global-jinx-mode' runs from the major-mode hook, inside
;; `set-auto-mode''s `with-demoted-errors'.  Ask where the module actually
;; is instead.

(require 'cl-lib)

(defun ee-jinx-locate-module ()
  "Return a prebuilt `jinx-mod' module file, putting its directory first on
`load-path'.

Under the Nix Emacs wrapper the module ships next to jinx.el and is already
reachable.  Other Emacsen -- neomacs, in particular -- do not inherit that
wrapper's EMACSLOADPATH, so ask the `emacs' binary where the module lives
rather than hardcoding a store path that changes on every nixpkgs update.
The directory is prepended so it wins over a straight.el build that carries
jinx.el but no compiled module."
  (let ((mod (concat "jinx-mod" (or module-file-suffix ".so"))))
    (or (locate-library mod t)
        (let ((lib (ignore-errors
                     (car (process-lines
                           "emacs" "-Q" "--batch" "--eval"
                           (format "(princ (or (locate-library \"%s\" t) \"\"))" mod))))))
          (when (and (stringp lib)
                     (not (string-empty-p lib))
                     (file-exists-p lib))
            (cl-pushnew (file-name-directory lib) load-path :test #'string=)
            lib)))))

(defvar ee-jinx-module (ee-jinx-locate-module)
  "Path to a prebuilt `jinx-mod' module, or nil if none was found.")

;; No prebuilt module anywhere: let straight fetch jinx and let jinx.el build
;; the module itself on first use.
(unless ee-jinx-module
  (straight-use-package 'jinx))

(defun ee-jinx-enable ()
  "Turn on `global-jinx-mode', tolerating an unusable native module.
Loading the module here surfaces a failure once, at startup, instead of
once per visited file through `set-auto-mode'."
  (condition-case err
      (progn
        (require 'jinx)
        (jinx--load-module)
        (global-jinx-mode 1))
    (error
     (message "ee-spelling: jinx unavailable (%s); spell checking is off"
              (error-message-string err)))))

(use-package jinx
  :straight nil
  :hook (emacs-startup . ee-jinx-enable)
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
