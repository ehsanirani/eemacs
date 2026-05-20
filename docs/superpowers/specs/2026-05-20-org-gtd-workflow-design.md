# Org GTD & Note-taking Workflow Simplification — Design

**Status**: Draft — awaiting user review
**Date**: 2026-05-20
**Topic**: Trim the org-mode configuration to a workflow that matches `docs/org-workflow-guide.org` and supports a hybrid "lightweight capture + small daily ritual" workflow without over-engineering.

---

## Background

The current configuration in `modules/ee-org.el` carries the residue of three overlapping influences (MinEmacs GTD taxonomy, Centaur agenda polish, multi-project file scaffolding) plus an over-rich set of TODO states, capture templates, and TODO sequences. The result:

- The user has 9 active TODO states, 4 checkbox-style heading states, and 3 decision states — 16 in total.
- 9 capture templates, several broken or unused (`i` targets a non-existent headline; `b` targets a non-existent file; `r` is a duplicate of `n`; `pn`/`pi` write into `TODO.org` despite the broader story being "notes go in `notes/`").
- The daily/weekly ritual described in `docs/org-workflow-guide.org` is never wired up — there are no `org-agenda-custom-commands`, no `org-stuck-projects` config, and the stock agenda is left noisy by `org-agenda-start-with-log-mode t`.
- Every quick capture auto-clocks-in, producing 1-minute `:LOGBOOK:` drawers as visual noise.
- Two entries in `~/org/inbox.org` triggered the investigation: one was correctly written as `* TODO`, the other was written as `* [ ]` because the secondary checkbox-style sequence (`[ ](T)`) makes a capital-`T` misclick at the `C-c C-t` fast-selection prompt produce a heading-level state that should never exist (`[ ]` is meant for *list-item checkboxes inside a heading*, not for headings themselves).

The user's stated workflow (option C from brainstorming): lightweight day-to-day capture into `inbox.org`, one small morning ritual that surfaces today's scheduled items + work-in-progress + waiting-on + inbox-triage backlog, org-roam for research notes, per-project `TODO.org` for project tasks.

The user's own `docs/org-workflow-guide.org` already describes this layout. The implementation has drifted from it. This spec brings the implementation back to the doc and fills the daily-ritual gap, while trimming the existing over-rich taxonomy that was making the system harder to use than it needs to be.

---

## Goals

1. Match the implementation to the workflow described in `docs/org-workflow-guide.org`.
2. Support the daily ritual the user wants (one custom agenda view).
3. Reduce surface area where the existing config is richer than the workflow requires.
4. Update the workflow guide so the doc and implementation stay in sync.
5. Fix existing data in `~/org/inbox.org` and `~/org/projects.org` that is broken or stale under the new model.

## Non-goals

- Adopting the `org-notes` skill's hub-and-spoke pillar pattern as the default per-project layout. The skill remains available for explicit per-project invocation, but is not pre-scaffolded.
- Adding a `NEXT` keyword or any other GTD-strict discipline beyond what is already in use.
- Rewriting `ee-org-register-project` to scaffold more than the current minimal `TODO.org` + `notes/`.
- Building `org-stuck-projects` infrastructure.
- Touching org-roam, citar, pomodoro, or LaTeX export configuration.
- Modifying the central `~/org/roam/` knowledge graph.

---

## Design

### 1. TODO keywords

Replace the current three sequences:

```elisp
(org-todo-keywords
 '((sequence "TODO(t)" "PROJ(p)" "LOOP(r)" "STRT(s)" "WAIT(w)" "HOLD(h)" "IDEA(i)"
             "|" "DONE(d)" "KILL(k)")
   (sequence "[ ](T)" "[-](S)" "[?](W)" "|" "[X](D)")
   (sequence "|" "OKAY(o)" "YES(y)" "NO(n)")))
```

…with a single sequence:

```elisp
(org-todo-keywords
 '((sequence
    "TODO(t)"   ; needs doing
    "STRT(s)"   ; in progress
    "WAIT(w)"   ; blocked on someone or something external
    "|"
    "DONE(d)"   ; completed
    "KILL(k)")))  ; cancelled / no longer relevant
```

#### Rationale for each removal

| Dropped | Reason / replacement |
|---|---|
| `PROJ` | A project container is not something to *do* — it holds tasks. Use plain headings inside `projects.org` (the file's `#+filetags: :index:` already declares the whole file as projects). |
| `LOOP` | Replaced by a `TODO` with a date repeater (`SCHEDULED: <2026-05-22 Fri +1w>`). Native Org and clearer. |
| `HOLD` | Folded into `WAIT`. The external/self-imposed distinction adds a per-task decision without changing what the agenda needs to surface. |
| `IDEA` | Treat as plain inbox content; promote to `TODO` when the user decides. The keyword adds nothing the inbox doesn't already convey. |
| `[ ] [-] [?] [X]` heading sequence | Removed at heading level. `[ ]`/`[X]` remain available as **list-item checkboxes inside a heading**, which is the standard Org meaning and what `docs/org-workflow-guide.org` actually describes. Removing the heading-level form prevents the capital-`T` misclick that caused the original bug report. |
| `OKAY/YES/NO` decision sequence | Unused; removed. |

#### Knock-on face updates

`org-todo-keyword-faces` and `org-modern-todo-faces` in `ee-org.el` are trimmed to keep only the entries for `STRT`, `WAIT`, `KILL`, `DONE` (and remove entries for `[-]`, `[?]`, `PROJ`, `NO`, `HOLD`, `LOOP`, `IDEA`).

### 2. Capture templates

Replace nine templates with three:

```elisp
(setq org-capture-templates
      `(("t" "Todo" entry
         (file ,(expand-file-name "inbox.org" org-directory))
         "* TODO %?\n%U\n")

        ("j" "Journal" entry
         (file+olp+datetree ,(expand-file-name "journal.org" org-directory))
         "* %^{Title}\n%U\n\n%?"
         :clock-in t :clock-resume t)

        ("p" "Project Todo" entry
         (file ee-org--current-project-todo)
         "* TODO %?\n%U\n%a\n")))
```

#### Notes on the change

- **No auto-clock-in** on `t` or `p`. Clock manually with `C-c C-x C-i` when timing matters. Eliminates 1-minute LOGBOOK noise on every quick capture.
- **Journal keeps auto-clock-in.** Journaling is intrinsically time-tracked, so the clock is meaningful there.
- **`%a` (back-link to capture origin)** is kept only on `p` (Project Todo), where the back-link is genuinely useful ("fix bug I saw in `fft.c`"). Dropped from `t` so casual captures don't tie themselves to whatever buffer happened to be open.
- **Removed**: `n` (Note — redundant with `t`), `i` (Idea — keyword removed, headline didn't exist), `r` (Research note — duplicated `n`), `b` (Book/Paper — file didn't exist), `pn` (Project note — contradicted the "notes go in `notes/`" rule), `pi` (Project idea — keyword removed).
- **Workflow note added to the doc**: research notes that aren't tasks go through `C-c n c` (org-roam-capture), not `C-c x`.

### 3. Daily ritual: one custom agenda command

Add a single agenda dispatcher entry: `C-c a d`, the "Today" view.

```elisp
(setq org-agenda-custom-commands
      '(("d" "Today"
         ((agenda "" ((org-agenda-span 'day)
                      (org-agenda-overriding-header "Today")))
          (todo "STRT"
                ((org-agenda-overriding-header "In progress")))
          (todo "WAIT"
                ((org-agenda-overriding-header "Waiting on")))
          (tags-todo "+CATEGORY=\"inbox\""
                     ((org-agenda-overriding-header "Inbox to triage")))))))
```

Blocks shown by `C-c a d`:

| Block | Contents |
|---|---|
| Today | Items scheduled or deadlined for today (stock daily agenda). |
| In progress | Every `STRT` task across all agenda files. |
| Waiting on | Every `WAIT` task. |
| Inbox to triage | Every `TODO` whose category is `inbox` (Org auto-derives category from filename). |

Also flip two existing agenda settings that contribute noise:

```elisp
(org-agenda-include-diary nil)        ; was t — no diary file exists
(org-agenda-start-with-log-mode nil)  ; was t — past clock entries clutter the daily view
```

### 4. Dashboard integration

Two small edits in `modules/ee-dashboard.el`:

1. `dashboard-week-agenda` → `nil` so the dashboard's agenda widget shows today only instead of the next 7 days.
2. The navigator "Agenda" button and the `?` transient's "Open agenda" action both call `(org-agenda nil "d")` instead of `(org-agenda)` — launching the Today view directly.

The dashboard's agenda widget itself remains a stock dashboard widget (it still lists today's scheduled/deadlined items sorted by time). Only the navigator button and help action are repointed.

### 5. File migrations (one-off)

#### `~/org/inbox.org`

- Change `* [ ] Email to Dali` to `* TODO Email to Dali`.
- Strip both `:LOGBOOK:` drawers (1-minute clocks from the original captures — meaningless data).

#### `~/org/projects.org`

- `* PROJ TCell Reseach :research:` → `* TCell research :research:` (drop `PROJ` keyword, fix the "Reseach" typo).
- `* PROJ Chemotactic Bacteria in Porous Media` → `* Chemotactic Bacteria in Porous Media`.
- `* PROJ Active Polymers: BIPS vs Molecular Motors` → `* Active Polymers: BIPS vs Molecular Motors`.
- The child `** TODO Meeting with Dali` is left unchanged — it remains a real TODO and surfaces in the agenda.

### 6. Workflow guide updates (`docs/org-workflow-guide.org`)

| Section | Edit |
|---|---|
| Directory Structure → Central Hub | Remove the `agenda.org` line — it was never used, and the user confirmed dropping it. |
| Capturing → Central Captures | Delete the `n`, `i`, `r`, `b` subsections. Keep `t` and `j`. Add a one-line callout: *"For research notes that aren't tasks, use `C-c n c` (org-roam-capture), not `C-c x`."* |
| Capturing → Project-Local Captures | Collapse the three-row table to one row: `p` → `<project>/TODO.org`. |
| TODO States — What Each One Means | Replace the 9-state list with the new 5-state list (`TODO STRT WAIT DONE KILL`). |
| TODO States → Checkbox Sequence | Reframe as "list-item checkboxes inside a heading" (`[ ]`/`[X]` as list items, never as heading states). Remove `[-]` and `[?]`. |
| TODO States → Decision Sequence | Delete the section entirely. |
| TODO States → Example Project Structure | Replace `* PROJ Signal processing library …` with the new container model (plain heading, child TODOs). |
| Processing the Inbox | Unchanged — keyword shortcuts (`d` for DONE, `k` for KILL) still work. |
| Agenda → Navigating Your Day | Add a `d` row: "Today — daily ritual view (scheduled + STRT + WAIT + inbox triage)" with a short paragraph explaining the four blocks. |
| Daily Routine Summary | Replace the "*Morning* Check agenda (all projects) `C-c a a`" row with "*Morning* Daily ritual view `C-c a d`". |
| Quick Reference | No structural changes; `C-c a` still opens agenda dispatcher, `C-c a d` is the new common key. |

---

## Files touched

| File | Change kind |
|---|---|
| `modules/ee-org.el` | Trim TODO keywords; trim faces; rewrite capture templates; add `org-agenda-custom-commands`; flip `org-agenda-include-diary`, `org-agenda-start-with-log-mode`. |
| `modules/ee-dashboard.el` | Set `dashboard-week-agenda nil`; repoint navigator + transient "Open agenda" action to `(org-agenda nil "d")`. |
| `docs/org-workflow-guide.org` | Edits per section table above. |
| `~/org/inbox.org` | Fix `* [ ]` heading; strip `:LOGBOOK:` noise. |
| `~/org/projects.org` | Drop `PROJ` keyword from 3 headings; fix "Reseach" → "research". |

`config.el`, `init.el`, and other modules are untouched.

---

## Risks and considerations

- **Existing data with `PROJ`/`HOLD`/`IDEA`/`LOOP` states**: any subtree using a dropped keyword would render with the literal keyword as part of the heading text. Migration covers the only known occurrences (`~/org/projects.org`). No projects are currently registered via `ee-org-register-project` (the registry file `~/.emacs.d/org-projects.el` does not exist), but `~/projects/TCell-research/TODO.org` exists on disk and is referenced from `projects.org`; the implementation plan should grep that file (and any other project `TODO.org` it finds under `~/projects/`) for dropped keywords and surface anything that needs touching before applying changes.
- **Org-modern face refresh**: trimming `org-modern-todo-faces` requires the user to re-load `ee-org.el` (via `home-manager switch` per project memory, since `~/.emacs.d/modules` is a Nix-store symlink). Documented as part of implementation.
- **Behavioral change in dashboard**: users who liked seeing next-7-day agenda in the dashboard widget will find it narrowed to today. Acceptable since the explicit ask was to integrate the dashboard with the daily ritual.
- **No backup of `inbox.org` / `projects.org` before in-place edits**: implementation plan should `cp` to `.bak` siblings before editing, in case migration goes wrong.
- **Doc and implementation drift in the future**: this spec re-aligns them today, but future config edits should propagate to `docs/org-workflow-guide.org`. Not enforced; a soft convention only.

## Open follow-ups (not in scope)

- `org-stuck-projects` configuration — useful but the user explicitly deferred.
- `NEXT` keyword for stricter GTD — explicitly deferred.
- Hub-and-spoke pillar scaffold per `org-notes` skill — invoke per-project if/when a project's notes grow past ~15 files.
- Capture template for appointments (`agenda.org` replacement) — dropped for now; revisit if calendar-style scheduling becomes a real need.
