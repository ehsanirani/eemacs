# Hunspell ispell backend Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add hunspell as the `ispell-mode` backend in `ee-spelling.el`, with four named dictionaries (en_US, de_DE, fa_IR, multi) and an interactive language-switching command, while leaving jinx untouched.

**Architecture:** All changes land in `modules/ee-spelling.el`, inside a `(when (executable-find "hunspell") ...)` guard. `ispell-program-name` is set to `"hunspell"`, four entries are added to `ispell-local-dictionary-alist`, and `ee/ispell-switch-language` wraps `ispell-change-dictionary` with completing-read. No new files are created.

**Tech Stack:** Emacs Lisp, `ispell` (built-in), `hunspell` (system binary)

---

## File Map

| Action | Path | Responsibility |
|--------|------|----------------|
| Modify | `modules/ee-spelling.el` | Add hunspell ispell backend alongside existing jinx config |

---

### Task 1: Add hunspell configuration block to ee-spelling.el

**Files:**
- Modify: `modules/ee-spelling.el`

**Background on `ispell-change-dictionary`:**
Its second argument controls scope. When ARG is non-nil it sets `ispell-dictionary` (global default); when ARG is nil it sets `ispell-local-dictionary` (buffer-local). Our command passes `(not arg)` so that no prefix = global, prefix arg = buffer-local — the opposite of the raw ispell convention.

**Background on `ispell-local-dictionary-alist` entry format:**
```
(NAME CHARS NOT-CHARS OTHERCHARS MANY-OTHERCHARS-P ISPELL-ARGS EXTENDED-CHAR-MODE CODING)
```
- `CHARS` — regexp matching word characters
- `NOT-CHARS` — regexp matching non-word characters
- `OTHERCHARS` — regexp matching characters allowed inside words (apostrophes etc.)
- `MANY-OTHERCHARS-P` — t allows multiple otherchars in a word
- `ISPELL-ARGS` — list of extra args passed to hunspell, e.g. `("-d" "en_US")`
- `EXTENDED-CHAR-MODE` — nil for standard
- `CODING` — `utf-8`

- [ ] **Step 1: Open `modules/ee-spelling.el` and read it**

  Confirm it currently ends with:
  ```elisp
  (provide 'ee-spelling)
  ;;; ee-spelling.el ends here
  ```

- [ ] **Step 2: Replace the entire file with the updated version**

  The hunspell block is inserted after the jinx `use-package` form, before `(provide 'ee-spelling)`.

  Full file content:

  ```elisp
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
    (setq ispell-program-name "hunspell"
          ispell-dictionary    "en_US")

    (dolist (entry '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" t ("-d" "en_US") nil utf-8)
                     ("de_DE" "[[:alpha:]]" "[^[:alpha:]]" "[']" t ("-d" "de_DE") nil utf-8)
                     ("fa_IR" "[[:alpha:]]" "[^[:alpha:]]" "[']" t ("-d" "fa_IR") nil utf-8)
                     ("multi" "[[:alpha:]]" "[^[:alpha:]]" "[']" t ("-d" "en_US,de_DE,fa_IR") nil utf-8)))
      (add-to-list 'ispell-local-dictionary-alist entry))

    (defun ee/ispell-switch-language (arg)
      "Switch the active ispell dictionary.
  Without a prefix argument, switch globally (all buffers).
  With a prefix argument (\\[universal-argument]), switch only the current buffer."
      (interactive "P")
      (let ((lang (completing-read "Ispell language: "
                                   '("en_US" "de_DE" "fa_IR" "multi")
                                   nil t)))
        (ispell-change-dictionary lang (not arg))))

    (global-set-key (kbd "C-c S") #'ee/ispell-switch-language))

  (provide 'ee-spelling)
  ;;; ee-spelling.el ends here
  ```

- [ ] **Step 3: Byte-compile the file to catch syntax errors**

  Run in a terminal:
  ```bash
  emacs --batch --eval "(byte-compile-file \"modules/ee-spelling.el\")"
  ```

  Expected output: no errors or warnings (a `.elc` file is created). If you see `Symbol's function definition is void` for `jinx` or `straight-use-package`, that is expected in batch mode — they require a running Emacs with straight bootstrapped. Any other error is real and must be fixed.

  Clean up the compiled file:
  ```bash
  rm -f modules/ee-spelling.elc
  ```

- [ ] **Step 4: Verify in a running Emacs**

  If you have a running Emacs with this config loaded, evaluate:
  ```
  M-: (executable-find "hunspell") RET
  ```
  Expected: a path string like `/run/current-system/sw/bin/hunspell` (NixOS) or `/usr/bin/hunspell`.

  If nil, hunspell is not on PATH — install it first (see the NixOS comment in the file, or your distro's package manager). The rest of the block will be silently skipped until hunspell is available, which is the correct behavior.

  If hunspell is found, verify:
  ```
  M-: ispell-program-name RET        ; => "hunspell"
  M-: ispell-dictionary RET          ; => "en_US"
  M-: (assoc "multi" ispell-local-dictionary-alist) RET
  ; => ("multi" "[[:alpha:]]" "[^[:alpha:]]" "[']" t ("-d" "en_US,de_DE,fa_IR") nil utf-8)
  ```

- [ ] **Step 5: Verify the switching command**

  In Emacs, press `C-c S`. A completing-read prompt should appear with four options:
  ```
  en_US  de_DE  fa_IR  multi
  ```
  Select `de_DE`. Then verify:
  ```
  M-: ispell-dictionary RET   ; => "de_DE"  (switched globally)
  ```
  Press `C-u C-c S`, select `fa_IR`. Then verify:
  ```
  M-: ispell-local-dictionary RET   ; => "fa_IR"  (switched buffer-locally)
  M-: ispell-dictionary RET         ; => "de_DE"  (global unchanged)
  ```

- [ ] **Step 6: Verify ispell-buffer uses hunspell**

  Open a buffer with a deliberate misspelling, e.g.:
  ```
  This is a tset of speling.
  ```
  Run `M-x ispell-buffer`. The ispell correction UI should appear (not jinx). Accept or skip corrections. Confirm the session completes without errors.

- [ ] **Step 7: Commit**

  ```bash
  git add modules/ee-spelling.el
  git commit -m "spelling: add hunspell as ispell-mode backend with multi-language support

  Configure hunspell as the ispell-program-name with four named dictionaries
  (en_US, de_DE, fa_IR, multi) in ispell-local-dictionary-alist. Add
  ee/ispell-switch-language bound to C-c S for interactive language switching
  (global by default, buffer-local with prefix arg). Guard the entire block
  on executable-find so the config is safe on systems without hunspell.
  Jinx on-the-fly checking is unchanged."
  ```

---

## Self-Review

**Spec coverage:**
- [x] `ispell-program-name` set to "hunspell" — Task 1 Step 2
- [x] Four dictionary entries (en_US, de_DE, fa_IR, multi) — Task 1 Step 2
- [x] `ispell-dictionary` default "en_US" — Task 1 Step 2
- [x] `ee/ispell-switch-language` with completing-read — Task 1 Step 2
- [x] `C-c S` keybinding — Task 1 Step 2
- [x] Global by default, buffer-local with prefix arg — Task 1 Step 2 + Step 5
- [x] `(executable-find "hunspell")` guard — Task 1 Step 2
- [x] NixOS package comment — Task 1 Step 2
- [x] Non-NixOS comment — Task 1 Step 2
- [x] Jinx untouched — verified by full file shown in Step 2

**Placeholder scan:** No TBD/TODO. All steps contain actual code or commands.

**Type consistency:** Single file, single task — no cross-task name drift possible.
