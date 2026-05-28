# UI / theme-portability fixes — implementation progress

Branch: `fix/theme-portability-ui`
Base: `main` @ `e8a2679` (fonts: propagate height through mixed-pitch and switch prose to float scale)

## Why this branch

The current UI config has several theme-blind hardcoded values and one
broken hook that together make theme-switching silently misbehave: fonts
do not re-apply after `M-x load-theme`, org tag/TODO badges keep their
hardcoded CSS colours under every theme, and the mode-line carries an
overline plus a duplicate definition. The goal is to make the appearance
layer fully theme-portable while preserving every user-facing setting in
`config.el` as a *default* that the user can swap.

Out of scope: typography opinions (italic keywords, semi-bold types),
pulsar colour, dashboard density, mode-line segment design. Those are
design discussions, not bugs.

## Testing protocol

After each phase: run `home-manager switch` at `~/dotfiles` (NOT a plain
Emacs restart — the config is loaded via Home Manager and direct restarts
won't pick up changes). Then restart Emacs and run the verification steps
listed under that phase. Each phase is a single commit and can be
reverted in isolation if a regression appears.

## Commit message convention

No mention of Claude / AI / assistants (per `~/.claude/CLAUDE.md`). Match
existing style from recent commits (e.g. `e8a2679 fonts: propagate height
through mixed-pitch and switch prose to float scale`).

---

## Phase 1 — Re-apply fonts on theme change

**Status:** [x] verified (commit `35efe1e`)

**Files:** `config-sample.el` (the tracked template; the same edits were
mirrored into the local untracked `config.el` so `home-manager switch`
exercises the new code path immediately). Line numbers below refer to
`config-sample.el`; the equivalent lines in the user's local `config.el`
are 71 / 105 / 108 / 98.

**Changes:**

1. `config-sample.el:64` — change `(defun ehsan/apply-font-after-theme ()` to
   `(defun ehsan/apply-font-after-theme (&rest _args)`. `enable-theme-functions`
   passes the theme symbol; the function must accept it.
2. `config-sample.el:98` — replace
   `(add-hook 'after-load-theme-hook #'ehsan/apply-font-after-theme)` with
   `(add-hook 'enable-theme-functions #'ehsan/apply-font-after-theme)`.
   `after-load-theme-hook` is not a real Emacs hook — nothing in standard
   Emacs or current `doom-themes` runs it. `enable-theme-functions` is the
   Emacs 29+ standard hook for "a theme was just enabled."
3. `config-sample.el:101` — delete the redundant
   `(add-hook 'after-init-hook #'ehsan/apply-font-after-theme)`. The direct
   call at `:89` already applies fonts at startup; the new
   `enable-theme-functions` hook handles re-application after any
   subsequent theme load (including the initial `load-theme` at
   `config-sample.el:30`).
4. Update the docstring of `ehsan/apply-font-after-theme` to mention that
   `&rest _args` exists to absorb the theme argument from
   `enable-theme-functions`.

**Commit title:** `fonts: re-apply on theme change via enable-theme-functions`

**Note on ordering:** the initial `load-theme` at `config-sample.el:30`
fires *before* the `add-hook` at `:98`, so the hook does **not** run on
the startup theme load. That's fine — the direct call to
`ehsan/apply-font-after-theme` at `config-sample.el:89` already applies
fonts at startup. The hook's job is to catch every *subsequent*
`load-theme`. Verification must therefore explicitly exercise a
post-init theme switch, not just inspect post-startup state.

**Verification:**

1. `home-manager switch`, restart Emacs.
2. Programmatic baseline check — `M-:` and evaluate
   `(face-attribute 'default :family)`. Should return your
   `ehsan/mono-font-family` value (default `JetBrainsMono Nerd Font`).
3. `M-x load-theme RET modus-vivendi RET` (or any theme other than
   `doom-material-dark`). Re-evaluate
   `(face-attribute 'default :family)` and
   `(face-attribute 'variable-pitch :family)`. Both must still match
   `ehsan/mono-font-family` and `ehsan/prose-font-family` — proving the
   hook fired.
4. `M-x load-theme RET ef-summer RET` (or any light theme). Repeat the
   `M-:` checks.
5. Sanity test that the hook isn't double-firing: `M-:` evaluate
   `(length (cl-remove-duplicates enable-theme-functions))` and verify
   no duplicate registration of `ehsan/apply-font-after-theme`.
6. Repeat #3 with `doom-material-dark` — fonts still survive.

**Regression risk:** very low. The change is a hook-name fix and a
signature widening; no visual behaviour changes unless the user actively
switches themes.

**Rollback:** `git revert <commit>`.

---

## Phase 2 — Subtle mode-line: theme-aware overline, remove duplicate

**Status:** [x] verified (commits `0e742ce` + follow-up)

**Files:** `ee-modeline.el`, `ee-fonts.el`

**Design intent (revised after testing):** The mode-line `:background`
is intentionally merged with the buffer background (the "floating" look).
That merge leaves the mode-line indistinguishable from the buffer unless
*something* separates them. The original overline used
`(face-attribute 'default :foreground)` — the buffer text colour — which
was too loud. After iterating through `vertical-border`, `shadow`, and
`font-lock-comment-face` (all too quiet against the merged background),
we settled on `(face-attribute 'mode-line :foreground)`: the modeline
face's own theme-provided foreground, which every theme calibrates to be
clearly visible against its own modeline background and therefore also
gives a clear hairline against the buffer background it now sits on.

**Changes:**

1. `ee-modeline.el:311-326` — in `+subtle-mode-line`, change the
   `:overline` colour source from `default :foreground` to
   `mode-line :foreground` on both `mode-line-active` and
   `mode-line-inactive`. Keep the 4px `:box` (padding) and the merged
   `:background`. The function continues to be installed via
   `ee-modeline-mode`.
2. `ee-fonts.el:75-88` — delete the duplicate `ee-subtle-mode-line`
   function entirely. Reason: it is functionally a copy of
   `+subtle-mode-line` but is unconditionally hooked, while
   `+subtle-mode-line` is gated by the `ee-modeline-mode` minor mode
   (`ee-ui.el:86` activates the mode unconditionally) and removes its
   hooks cleanly when the mode is toggled off. Keeping both means the
   mode-line face is set twice on every theme change.
3. `ee-fonts.el:116-117` — delete the two hook registrations for
   `ee-subtle-mode-line`.

**Commit titles:**
- `modeline: drop overline from subtle mode-line, remove duplicate` (initial — overline removal)
- Follow-up: restore overline using `mode-line` face for theme-aware separation

**Verification:**

1. `home-manager switch`, restart Emacs.
2. Look at any code buffer's mode-line. There should be a 1px hairline
   above the mode-line bar in the mode-line text colour — visible but
   not as heavy as a full-contrast buffer-foreground line. A few pixels
   of breathing room (from the `:box`) should sit below it.
3. Programmatic check — `M-:` evaluate
   `(face-attribute 'mode-line-active :overline)`. Should return a
   colour string (the active theme's `mode-line :foreground` value),
   not `unspecified`.
4. `M-x ee-modeline-mode` (toggle off), then again (toggle on). No
   errors in `*Messages*` about a missing `ee-subtle-mode-line`.
5. `M-x load-theme RET modus-operandi RET` (a light theme) — the
   hairline should still be visible-but-quiet against the light bg.
   Switch through a couple more themes to confirm the separator stays
   well-calibrated.

**Regression risk:** very low. The visible separator is back, but now
theme-aware and lower-contrast than the original.

**Rollback:** `git revert <commit>` for the follow-up reverts to no
overline; revert both to restore the original heavy overline + both
function definitions.

---

## Phase 3 — Theme-portable org tag and TODO faces

**Status:** [x] implemented (awaiting verification)

**Files:** `ee-org.el`

**Decision recorded:** Use **flat colored text** (no pill background).
A comment will note that the pill look can be restored by setting a
derived `:background` from `(face-attribute 'mode-line :background)`.

**Changes:**

1. `ee-org.el:424` — replace the `org-modern-tag` `:custom-face` spec
   ```elisp
   (org-modern-tag ((t (:inherit org-verbatim :weight regular
                        :foreground "black" :background "LightGray"
                        :box "black"))))
   ```
   with
   ```elisp
   ;; Flat tag rendering — inherits theme colors. For a filled-pill
   ;; look, add e.g. `:background ,(face-attribute 'mode-line :background)`
   ;; inside an `enable-theme-functions' hook so it tracks theme changes.
   (org-modern-tag ((t (:inherit (shadow org-verbatim)
                        :weight regular :box nil))))
   ```
2. `ee-org.el:430-434` — replace the `org-modern-todo-faces` spec
   ```elisp
   (org-modern-todo-faces
    '(("STRT" . (:inherit org-verbatim :weight semi-bold :foreground "white" :background "OrangeRed"))
      ("WAIT" . (:inherit org-verbatim :weight semi-bold :foreground "white" :background "coral"))
      ("KILL" . (:inherit org-verbatim :weight semi-bold :foreground "white" :background "DarkGreen"))
      ("DONE" . (:inherit org-verbatim :weight semi-bold :foreground "black" :background "LightGray"))))
   ```
   with theme-relative inheritance:
   ```elisp
   ;; Flat TODO rendering — colors come from the active theme via
   ;; inherited semantic faces. To restore filled badges, give each
   ;; entry a `:background' derived from a theme face inside an
   ;; `enable-theme-functions' hook (see commit message for example).
   (org-modern-todo-faces
    '(("STRT" . (:inherit (bold font-lock-constant-face)))
      ("WAIT" . (:inherit (bold warning)))
      ("KILL" . (:inherit (bold error)))
      ("DONE" . (:inherit (bold shadow)))))
   ```
3. `ee-org.el:249-254` — leave the `+org-todo-active` /
   `+org-todo-onhold` / `+org-todo-cancel` `custom-declare-face` calls
   alone. These remain the fallback faces used by
   `org-todo-keyword-faces` when `org-modern-mode` is disabled in a
   buffer.

**Commit title:** `org: use theme-relative faces for org-modern tag and TODO`

**Verification:**

1. `home-manager switch`, restart Emacs.
2. Open any `.org` file containing TODO entries and tags. Test fixture:
   ```org
   * TODO write report :work:
   * STRT debug issue :urgent:work:
   * WAIT vendor reply :urgent:
   * KILL stale task :archive:
   * DONE morning routine :daily:
   ```
3. **Under `doom-material-dark` (default):**
   - Tags render as low-contrast italic-ish text using the theme's
     `shadow` face. No background pill, no border.
   - `STRT` text colour matches `font-lock-constant-face` (typically a
     blue or teal accent in dark themes).
   - `WAIT` text matches `warning` (typically amber/yellow).
   - `KILL` text matches `error` (typically red).
   - `DONE` text matches `shadow` (typically dim grey).
4. `M-x load-theme RET modus-operandi RET` (a *light* theme). All five
   tags/TODO labels should automatically pick up the light theme's
   semantic colours. No leftover `OrangeRed` / `coral` / `DarkGreen` /
   `LightGray` patches. No white-on-coral contrast failures.
5. `M-x load-theme RET ef-summer RET`, `M-x load-theme RET modus-vivendi RET`,
   etc. Each theme switch should yield internally-consistent colours.
6. Fallback face check — `M-x org-modern-mode` to disable org-modern in
   the test buffer. STRT/WAIT/KILL should now render using the
   `+org-todo-active` / `+org-todo-onhold` / `+org-todo-cancel` faces
   from `ee-org.el:249-254` (which inherit from
   `font-lock-constant-face` / `warning` / `error`). Visual style
   differs from org-modern (no pill geometry, just coloured keyword
   text), but colours should be consistent with the theme. Re-enable
   `org-modern-mode` when done.

**Regression risk:** medium-low. Visual change is intentional and
significant — the user will see flat text instead of filled pills. If
they prefer pills, the file comment documents the one-line addition.

**Rollback:** `git revert <commit>`.

### Phase 3 follow-up — restore theme-aware pills and tune label size

After testing `adef603`, the flat-text design read as a visual regression
(the foreground-only specs didn't render as pills because they had no
`:background`; tags also picked up org-modern's display-property
padding spaces, producing a "ghost pill" with too-wide horizontal gap).
Iterated to a design that keeps pills but makes them theme-aware:

1. **Dropped** the `:custom-face` override for `org-modern-tag`. Tags
   now fall through to org-modern's default `org-modern-tag` face
   (`org-modern.el:313-317`) which inherits `secondary-selection` —
   a theme-provided face that gives a subtle pill background that
   tracks every theme.
2. **Added** `:inverse-video t` to every `org-modern-todo-faces` entry.
   Same trick the default `org-modern-todo` face uses
   (`org-modern.el:333-337`): the inherited semantic foreground becomes
   the pill background, the buffer background becomes the text inside.
   So `STRT` → `font-lock-constant-face`-coloured pill, `WAIT` →
   `warning`-coloured pill, etc. TODO not listed — it falls through to
   `org-modern-todo` which already applies inverse-video on `org-todo`.
3. **Added** a `:custom-face` override for `org-modern-label` setting
   `:height 0.9`. Default `:height 0.8` made every label noticeably
   smaller than surrounding text; 0.9 keeps labels distinct as badges
   without the aggressive 20% shrink. `:width condensed` from the
   default spec dropped — most coding fonts don't ship a condensed
   variant, so it silently fell back to normal width anyway. The
   `org-modern-label` defface docstring explicitly invites this
   override.

**Files:** `ee-org.el` only.

**Commit title:** `org: theme-aware org-modern pills with tuned label size`

**Verification (after testing on doom-material-dark):**

- Tags render as a subtle `secondary-selection`-coloured pill.
- TODO, STRT, WAIT, KILL, DONE render as saturated theme-aware pills
  (`org-todo`, `font-lock-constant-face`, `warning`, `error`, `shadow`
  respectively).
- All labels at 0.9× of surrounding text — visibly badge-like but no
  longer dwarfed.
- Theme switch (`M-x load-theme RET modus-operandi RET`, etc.) recolors
  every pill automatically; no hardcoded CSS colors anywhere.

---

## Phase 4 — Wire up emoji and Arabic font fallbacks

**Status:** [x] verified (commits `8ace452` + follow-ups `7f4f12b`, `c0cbe82`)

**Follow-ups after initial verification:**
- `7f4f12b` — added an explicit `(require 'cl-lib)` because the helper
  functions used `cl-return` to short-circuit the candidate `dolist`.
  Later superseded by switching to `catch`/`throw`.
- `c0cbe82` — three combined fixes:
  - Detect installed fonts via `find-font (font-spec :family ...)` instead
    of `(member ... (font-family-list))`. The previous check silently
    no-op'd for emoji on Linux because color emoji fonts are often
    resolvable yet absent from `font-family-list` at hook time.
  - Replace `cl-return` with `catch`/`throw`; remove the cl-lib require.
  - Reorder Arabic candidates (Vazirmatn first for the Persian-primary
    user; add Noto Sans/Naskh Arabic). Expand emoji candidates (Twitter
    Color Emoji, Twemoji, JoyPixels, Emoji One, Symbola).

**Files:** `ee-fonts.el`

**Changes:**

1. After `ee-fonts.el:117` (the last existing `add-hook`), add two new
   hook registrations:
   ```elisp
   (add-hook 'emacs-startup-hook #'ee-setup-emoji-font)
   (add-hook 'emacs-startup-hook #'ee-setup-arabic-font)
   (add-hook 'server-after-make-frame-hook #'ee-setup-emoji-font)
   (add-hook 'server-after-make-frame-hook #'ee-setup-arabic-font)
   ```
   Reason: `ee-setup-emoji-font` and `ee-setup-arabic-font` are defined
   in `ee-fonts.el:92-110` but are never called from any hook. The
   project loads `ee-rtl` (`init.el:50`) which expects Arabic rendering
   to work. Both functions guard on `(display-graphic-p)` and silently
   no-op under a headless daemon — so `emacs-startup-hook` is *not*
   sufficient when running `emacs --daemon`. The
   `server-after-make-frame-hook` registrations ensure the fontset
   modifications run the first time a graphical client connects.
   For non-daemon use, `set-fontset-font t ...` modifies the default
   fontset which is shared across all frames, so no extra hook is
   needed for additional GUI frames created via `(make-frame)`.

**Commit title:** `fonts: install emoji and arabic fallbacks on startup`

**Verification:**

1. `home-manager switch`, restart Emacs (and stop any running daemon
   first if applicable: `emacsclient -e '(kill-emacs)'`).
2. Open `*scratch*`. Insert an emoji with `C-x 8 RET 1F389 RET` (🎉) or
   simply paste 🎉. It should render as a colour glyph, not as a
   `[1F389]` tofu box.
3. Insert an Arabic string, e.g. `السلام عليكم`. It should render with
   proper right-to-left shaping and joined letterforms, not as
   disconnected boxes.
4. `M-x describe-fontset RET emoji RET` — the top of the fallback chain
   should show `Noto Color Emoji` (Linux/NixOS), `Apple Color Emoji`
   (macOS), or `Segoe UI Emoji` (Windows), whichever is first available.
5. Daemon test (optional): start Emacs as daemon
   (`emacs --daemon`), then `emacsclient -c` to open a new frame. Repeat
   #2 and #3 in that frame.

**Regression risk:** very low. The hooks attach new fallback fonts to
the `emoji` and `arabic` fontsets. If the system has none of the listed
fonts installed (`Noto Color Emoji`, `Amiri`, etc.), the functions
silently no-op (the `(member font (font-family-list))` guard).

**Rollback:** `git revert <commit>`.

---

## Phase 5 — Delete dead disabled code in `ee-ui.el`

**Status:** [x] verified (commit `b4ce198`)

**Decision recorded:** Option **5b** (delete) — the cleaner choice. The
`ee-highlight-*` family has been disabled by default since it was added,
the `solaire-mode` block is unused, and the `ee-debug-buffer-modifications`
function is leftover scaffolding.

**Files:** `ee-ui.el`, `ee-markdown.el`

**Changes:**

1. `ee-ui.el:126-128` — delete the `defvar ee-enable-code-block-highlighting`.
2. `ee-ui.el:130-157` — delete `ee-highlight-code-blocks`.
3. `ee-ui.el:159-162` — delete `ee-clear-code-block-highlights`.
4. `ee-ui.el:164-195` — delete `ee-highlight-code-content-only`.
5. `ee-ui.el:197-224` — delete the `solaire-mode` `use-package` block
   and the `ee-enable-solaire` / `ee-disable-solaire` helpers (the mode
   has never been activated in this config).
6. `ee-ui.el:255-265` — delete `ee-toggle-code-block-highlighting`.
7. `ee-ui.el:267-279` — delete `ee-setup-code-block-auto-highlight`.
8. `ee-ui.el:281-284` — delete the `setq` + commented call that
   guards the disabled feature.
9. `ee-ui.el:287-308` — delete `ee-adjust-code-colors` (the
   `completing-read`-based hex picker).
10. `ee-ui.el:311-322` — delete `ee-debug-buffer-modifications`.
11. `ee-ui.el:325-329` — delete the `markdown-mode-hook` debug warning
    (`message "WARNING: Buffer is modified on entry!"`).
12. `ee-ui.el:331-347` — delete `ee-reset-theme-faces`. The function's
    docstring and body both reference `solaire-global-mode` (lines
    333-337); with solaire-mode removed in change #5, this helper has
    no remaining reason to exist. The `face-remap-reset-base` loop and
    theme reload trick are general-purpose, but no key is bound to this
    command and nothing calls it.
13. `ee-markdown.el:104` — delete the stale commented-out call
    `;; (ee-highlight-code-content-only)  ; Uncomment to enable by default`.

Net deletion: ~135 lines. No remaining references to any deleted
symbol — verified by grep before commit (see verification step 1).

**Commit title:** `ui: drop disabled code-block highlight and solaire scaffolding`

**Verification:**

1. Before commit, run from the repo root:
   ```
   grep -rn --exclude-dir=straight --exclude-dir=eln-cache \
     -e 'ee-highlight-code-blocks' \
     -e 'ee-highlight-code-content-only' \
     -e 'ee-clear-code-block-highlights' \
     -e 'ee-toggle-code-block-highlighting' \
     -e 'ee-setup-code-block-auto-highlight' \
     -e 'ee-adjust-code-colors' \
     -e 'ee-debug-buffer-modifications' \
     -e 'ee-reset-theme-faces' \
     -e 'ee-enable-solaire' \
     -e 'ee-disable-solaire' \
     -e 'ee-enable-code-block-highlighting' \
     -e 'solaire' \
     .
   ```
   Expected output: empty (zero matches). If any line returns,
   investigate before committing.
2. `home-manager switch`, restart Emacs. Emacs should start without
   errors in `*Messages*` (no `void-function` warnings).
3. Open a `.md` file with fenced code blocks (` ```bash ... ``` `). It
   should render exactly as it does today (the disabled feature
   contributed nothing to the default visual).
4. Open a `.org` file with `#+begin_src` blocks. Same — unchanged.

**Regression risk:** low, provided the grep in step 1 returns no
unexpected references. Anyone who had bound a key to
`ee-toggle-code-block-highlighting` (none found in this repo) would
lose that binding.

**Rollback:** `git revert <commit>`.

---

## Out of scope for this branch

These came up in the three-agent review but are explicitly deferred:

- `ee-tweak-faces` italic-keyword / semi-bold-types overrides
  (`ee-fonts.el:55-67`) — design opinion, not a theme-portability bug.
- Pulsar green / `pulse-only-if-changed nil` (`ee-ui.el:243-248`) —
  design opinion.
- Project-name `inverse-video` and `error italic` for dirty buffers
  (`ee-modeline.el:156, 181`) — design opinion.
- `vim-tab-bar`, dashboard widget counts, internal-border tuning,
  line-spacing globals — design opinions.
- Dropping `doom-themes` / `catppuccin-theme` / `ef-themes` — user
  wants theme flexibility, so all three remain available.

## Status summary

| Phase | Title                                                            | Status                  |
|-------|------------------------------------------------------------------|-------------------------|
| 1     | Re-apply fonts on theme change                                   | [x] verified (`35efe1e`) |
| 2     | Subtle mode-line: theme-aware overline, remove duplicate         | [x] verified (`0e742ce` + follow-up) |
| 3     | Theme-portable org tag and TODO faces                            | [x] verified (`adef603` + follow-up) |
| 4     | Wire up emoji and Arabic font fallbacks                          | [x] verified (`8ace452` + follow-ups) |
| 5     | Delete dead disabled code in `ee-ui.el`                          | [x] verified (`b4ce198`) |

Update the `Status:` field at the top of each phase section and tick
the table row after the corresponding commit lands.
