# Org-Mode Visual Enhancements Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add `mixed-pitch` always-on for org buffers, `olivetti` for centered layout, and a `ee-org-writing-mode` minor mode that bundles centered layout + heading size/weight hierarchy behind a single toggle (`C-c o w`).

**Architecture:** All three changes go in `modules/ee-org.el`. `mixed-pitch` hooks into `org-mode-hook` unconditionally. `olivetti` is loaded but only activated by the minor mode. `ee-org-writing-mode` stores `face-remap-add-relative` cookies in a buffer-local variable and removes them on disable — no side effects on toggle off.

**Tech Stack:** Emacs Lisp, `mixed-pitch` package, `olivetti` package, `face-remap-add-relative` / `face-remap-remove-relative` built-ins, `define-minor-mode`.

---

## File Map

| File | Action | What changes |
|------|--------|--------------|
| `modules/ee-org.el` | Modify | Add `mixed-pitch` block after line 454 (after `org-modern`), add `olivetti` block, add `ee-org-writing-mode` definition, add keybinding at end of keybindings section |

No other files need to change.

---

## Testing Context

eemacs is deployed via home-manager. **To verify changes, run:**
```bash
cd ~/dotfiles && home-manager switch
```
Then start a fresh Emacs session. Do **not** test by restarting Emacs without running `home-manager switch` first — the deployed config won't reflect your edits.

For rapid iteration during development, you can eval individual `use-package` blocks with `M-x eval-region` after selecting the block, but final verification must use `home-manager switch`.

---

## Task 1: Add `mixed-pitch` — variable-pitch body, monospace for code/tables

**Files:**
- Modify: `modules/ee-org.el` after line 454 (end of `org-modern` block)

### What to insert

Insert the following block immediately after the closing paren of the `org-modern` `use-package` block (after line 454):

```elisp
;; Variable-pitch body text with monospace preserved for structured elements
(use-package mixed-pitch
  :straight t
  :hook (org-mode . mixed-pitch-mode)
  :custom
  (mixed-pitch-fixed-pitch-faces
   '(org-block
     org-block-begin-line
     org-block-end-line
     org-code
     org-date
     org-formula
     org-meta-line
     org-special-keyword
     org-table
     org-tag
     org-verbatim
     line-number
     line-number-current-line)))
```

- [ ] **Step 1: Insert the `mixed-pitch` block**

In `modules/ee-org.el`, locate the end of the `org-modern` use-package block (line 454, the closing `)`). Insert the block above immediately after it.

- [ ] **Step 2: Commit**

```bash
git add modules/ee-org.el
git commit -m "feat(org): add mixed-pitch for variable-pitch body text in org buffers"
```

---

## Task 2: Add `olivetti` — centered writing layout package

**Files:**
- Modify: `modules/ee-org.el` after the `mixed-pitch` block added in Task 1

### What to insert

Insert immediately after the `mixed-pitch` block:

```elisp
;; Centered writing layout — used by ee-org-writing-mode
(use-package olivetti
  :straight t
  :defer t
  :custom
  (olivetti-body-width 80))
```

- [ ] **Step 1: Insert the `olivetti` block**

In `modules/ee-org.el`, locate the end of the `mixed-pitch` block added in Task 1. Insert the block above immediately after it.

- [ ] **Step 2: Commit**

```bash
git add modules/ee-org.el
git commit -m "feat(org): add olivetti package for centered writing layout"
```

---

## Task 3: Define `ee-org-writing-mode` minor mode

**Files:**
- Modify: `modules/ee-org.el` after the `olivetti` block added in Task 2

### What to insert

Insert immediately after the `olivetti` block:

```elisp
;; Buffer-local storage for face remap cookies (used by ee-org-writing-mode)
(defvar-local ee-org--writing-mode-remaps nil)

(define-minor-mode ee-org-writing-mode
  "Toggle focused writing mode: centered layout + heading size/weight hierarchy.
When enabled, activates olivetti centering and scales org headings by level.
When disabled, restores the original flat org-modern appearance."
  :lighter " Write"
  (if ee-org-writing-mode
      (progn
        (olivetti-mode 1)
        (setq ee-org--writing-mode-remaps
              (list
               (face-remap-add-relative 'org-level-1 :height 1.25 :weight 'bold)
               (face-remap-add-relative 'org-level-2 :height 1.15 :weight 'semi-bold)
               (face-remap-add-relative 'org-level-3 :height 1.10 :weight 'semi-bold)
               (face-remap-add-relative 'org-level-4 :height 1.05 :weight 'normal)
               (face-remap-add-relative 'org-level-5 :height 1.0  :weight 'normal)
               (face-remap-add-relative 'org-level-6 :height 1.0  :weight 'normal)
               (face-remap-add-relative 'org-level-7 :height 1.0  :weight 'normal)
               (face-remap-add-relative 'org-level-8 :height 1.0  :weight 'normal))))
    (olivetti-mode -1)
    (mapc #'face-remap-remove-relative ee-org--writing-mode-remaps)
    (setq ee-org--writing-mode-remaps nil)))
```

- [ ] **Step 1: Insert `ee-org--writing-mode-remaps` and `ee-org-writing-mode`**

In `modules/ee-org.el`, insert the block above immediately after the `olivetti` block.

- [ ] **Step 2: Commit**

```bash
git add modules/ee-org.el
git commit -m "feat(org): add ee-org-writing-mode minor mode with olivetti and heading hierarchy"
```

---

## Task 4: Add keybinding for `ee-org-writing-mode`

**Files:**
- Modify: `modules/ee-org.el` — the `with-eval-after-load 'org` keybinding block (around line 561)

### What to change

Locate this block near the end of `ee-org.el`:

```elisp
(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c l") 'org-store-link)
  (define-key org-mode-map (kbd "C-c C-w") 'org-refile)
  (define-key org-mode-map (kbd "M-h") 'org-metaleft)
  (define-key org-mode-map (kbd "M-l") 'org-metaright)
  (define-key org-mode-map (kbd "M-j") 'org-metadown)
  (define-key org-mode-map (kbd "M-k") 'org-metaup))
```

Add one line before the closing `))`:

```elisp
(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c l") 'org-store-link)
  (define-key org-mode-map (kbd "C-c C-w") 'org-refile)
  (define-key org-mode-map (kbd "M-h") 'org-metaleft)
  (define-key org-mode-map (kbd "M-l") 'org-metaright)
  (define-key org-mode-map (kbd "M-j") 'org-metadown)
  (define-key org-mode-map (kbd "M-k") 'org-metaup)
  (define-key org-mode-map (kbd "C-c o w") #'ee-org-writing-mode))
```

- [ ] **Step 1: Add the keybinding**

Edit `modules/ee-org.el` to add `(define-key org-mode-map (kbd "C-c o w") #'ee-org-writing-mode)` as the last line inside the `with-eval-after-load 'org` keybinding block.

- [ ] **Step 2: Commit**

```bash
git add modules/ee-org.el
git commit -m "feat(org): bind C-c o w to ee-org-writing-mode"
```

---

## Task 5: Deploy and verify end-to-end

**Files:** None — verification only.

- [ ] **Step 1: Deploy via home-manager**

```bash
cd ~/dotfiles && home-manager switch
```

Expected: completes without errors.

- [ ] **Step 2: Start a fresh Emacs and open an org file**

Open any `.org` file. Verify `mixed-pitch-mode` is active:

```
M-x describe-mode
```

Expected: `mixed-pitch` listed among active minor modes. Body text should render in the variable-pitch font (typically a sans-serif). Code blocks and tables should remain in the monospace font.

- [ ] **Step 3: Verify heading sizes are flat (writing mode OFF)**

Look at headings at different levels (`* H1`, `** H2`, `*** H3`). They should all appear at the same size — the current org-modern flat appearance, unchanged.

- [ ] **Step 4: Enable writing mode and verify heading hierarchy**

```
C-c o w
```

Expected:
- `olivetti-mode` activates — text is centered in a ~80-char column with margins on both sides
- `H1` headings are visibly larger (1.25×) and bold
- `H2` headings are slightly smaller (1.15×) and semi-bold
- `H3` headings are slightly smaller still (1.10×) and semi-bold
- `H4+` headings appear at normal size

- [ ] **Step 5: Disable writing mode and verify restoration**

```
C-c o w
```

Expected:
- `olivetti-mode` deactivates — text returns to full-width layout
- All heading levels return to the same flat size as before enabling the mode
- org-modern TODO badges, tags, and table styling are untouched

- [ ] **Step 6: Verify toggle is buffer-local**

Open a second org buffer. Confirm writing mode is off in the new buffer even though it was toggled in the first. Enable it in the second buffer and confirm both buffers can have independent states.
