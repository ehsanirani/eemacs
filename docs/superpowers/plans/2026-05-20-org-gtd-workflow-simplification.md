# Org GTD & Note-taking Workflow Simplification — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Trim the org-mode configuration in `modules/ee-org.el` and `modules/ee-dashboard.el`, migrate the two affected user-data files (`~/org/inbox.org`, `~/org/projects.org`), and update `docs/org-workflow-guide.org` so the implementation matches the user's documented "lightweight capture + small daily ritual" workflow.

**Architecture:** Pure configuration edits — no new modules, no new dependencies. Changes flow into the running Emacs via the user's existing home-manager pipeline (eemacs repo → github → flake input → `home-manager switch` → Emacs restart). User-data files (`~/org/`) are edited in place after backup. Each repo task is its own commit. Syntax-check after every elisp edit via `emacs --batch`.

**Tech Stack:** GNU Emacs, Org-mode, straight.el, use-package, home-manager (NixOS), dashboard.el.

**Pre-flight context already verified:**
- `~/projects/TCell-research/TODO.org` declares its own `#+seq_todo: TODO IN-PROGRESS WAITING | DONE CANCELLED` and is unaffected by the global keyword trim.
- `~/.emacs.d/org-projects.el` does not exist → no projects are currently registered via `ee-org-register-project`.
- `~/org/projects.org` is the only file using dropped keywords at the heading level.

**Spec reference:** `docs/superpowers/specs/2026-05-20-org-gtd-workflow-design.md` (commit `dfe2503`).

---

### Task 1: Pre-flight backup of user data

User-data files in `~/org/` are outside the repo and not version-controlled by this plan. Create timestamped backups before any migration.

**Files:**
- Create: `~/org/inbox.org.bak-2026-05-20`
- Create: `~/org/projects.org.bak-2026-05-20`

- [ ] **Step 1: Back up `~/org/inbox.org`**

Run:
```bash
cp ~/org/inbox.org ~/org/inbox.org.bak-2026-05-20
```

Expected: exit 0, no output.

- [ ] **Step 2: Back up `~/org/projects.org`**

Run:
```bash
cp ~/org/projects.org ~/org/projects.org.bak-2026-05-20
```

Expected: exit 0, no output.

- [ ] **Step 3: Verify backups exist and match originals**

Run:
```bash
diff ~/org/inbox.org ~/org/inbox.org.bak-2026-05-20 && diff ~/org/projects.org ~/org/projects.org.bak-2026-05-20 && echo "OK"
```

Expected: prints `OK`. No diff output.

No commit (files outside the repo).

---

### Task 2: Trim `org-todo-keywords` in `ee-org.el`

**Files:**
- Modify: `/home/ehsan/projects/eemacs/modules/ee-org.el:220-233`

- [ ] **Step 1: Replace the `org-todo-keywords` block**

Find the block at `modules/ee-org.el:220-233`:

```elisp
  ;; TODO keywords — MinEmacs rich GTD taxonomy (3 sequences)
  (org-todo-keywords
   '((sequence
      "TODO(t)"  ; A task that needs to be done
      "PROJ(p)"  ; A project, which usually contains other tasks
      "LOOP(r)"  ; A recurring task
      "STRT(s)"  ; A task that is in progress
      "WAIT(w)"  ; Something external is holding up this task
      "HOLD(h)"  ; This task is paused/on hold because of me
      "IDEA(i)"  ; An unconfirmed and unapproved task or notion
      "|"
      "DONE(d)"  ; Task successfully completed
      "KILL(k)") ; Task was cancelled, aborted or is no longer applicable
     (sequence "[ ](T)" "[-](S)" "[?](W)" "|" "[X](D)")
     (sequence "|" "OKAY(o)" "YES(y)" "NO(n)")))
```

Replace with:

```elisp
  ;; TODO keywords — trimmed GTD set (single sequence)
  (org-todo-keywords
   '((sequence
      "TODO(t)"  ; needs doing
      "STRT(s)"  ; in progress
      "WAIT(w)"  ; blocked on someone or something external
      "|"
      "DONE(d)"  ; completed
      "KILL(k)"))) ; cancelled / no longer relevant
```

- [ ] **Step 2: Syntax-check the file**

Run:
```bash
emacs --batch --eval "(progn (find-file \"/home/ehsan/projects/eemacs/modules/ee-org.el\") (check-parens))"
```

Expected: exit 0, no parenthesis-mismatch errors.

- [ ] **Step 3: Commit**

Run:
```bash
cd /home/ehsan/projects/eemacs
git add modules/ee-org.el
git commit -m "org: trim TODO keywords to TODO STRT WAIT DONE KILL"
```

Expected: clean commit, no hook failures.

---

### Task 3: Trim `org-todo-keyword-faces` in `ee-org.el`

**Files:**
- Modify: `/home/ehsan/projects/eemacs/modules/ee-org.el:235-243`

- [ ] **Step 1: Replace the `org-todo-keyword-faces` block**

Find the block at `modules/ee-org.el:235-243`:

```elisp
  (org-todo-keyword-faces
   '(("[-]"  . +org-todo-active)
     ("STRT" . +org-todo-active)
     ("[?]"  . +org-todo-onhold)
     ("WAIT" . +org-todo-onhold)
     ("HOLD" . +org-todo-onhold)
     ("PROJ" . +org-todo-project)
     ("NO"   . +org-todo-cancel)
     ("KILL" . +org-todo-cancel)))
```

Replace with:

```elisp
  (org-todo-keyword-faces
   '(("STRT" . +org-todo-active)
     ("WAIT" . +org-todo-onhold)
     ("KILL" . +org-todo-cancel)))
```

- [ ] **Step 2: Syntax-check**

Run:
```bash
emacs --batch --eval "(progn (find-file \"/home/ehsan/projects/eemacs/modules/ee-org.el\") (check-parens))"
```

Expected: exit 0.

- [ ] **Step 3: Commit**

Run:
```bash
cd /home/ehsan/projects/eemacs
git add modules/ee-org.el
git commit -m "org: trim TODO keyword faces to match new keyword set"
```

---

### Task 4: Rewrite `org-capture-templates` in `ee-org.el`

**Files:**
- Modify: `/home/ehsan/projects/eemacs/modules/ee-org.el:271-298`

- [ ] **Step 1: Replace the capture-templates block**

Find the block at `modules/ee-org.el:271-298`:

```elisp
  ;; Capture templates — central + project-aware
  (setq org-capture-templates
        `(("t" "Todo" entry (file ,(expand-file-name "inbox.org" org-directory))
           "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
          ("n" "Note" entry (file ,(expand-file-name "inbox.org" org-directory))
           "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
          ("j" "Journal" entry (file+olp+datetree
                                ,(expand-file-name "journal.org" org-directory))
           "* %^{Title} %?\n%U\n%a\n" :clock-in t :clock-resume t)
          ("i" "Idea" entry (file+headline
                             ,(expand-file-name "inbox.org" org-directory) "Ideas")
           "* IDEA %?\n%U\n")
          ("r" "Research note" entry (file ,(expand-file-name "inbox.org" org-directory))
           "* %^{Title} :research:\n%U\nSource: %a\n\n%?" :clock-in t :clock-resume t)
          ("b" "Book/Paper" entry (file+olp+datetree
                                   ,(expand-file-name "reading.org" org-directory))
           "* %^{Title} %^g\n%U\nAuthor: %^{Author}\n\n%?")

          ;; Project-local captures
          ;; NOTE: (file ee-org--current-project-todo) works because org-capture
          ;; calls functionp symbols via funcall in org-capture-target-buffer.
          ("p" "Project")
          ("pt" "Project todo" entry (file ee-org--current-project-todo)
           "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
          ("pn" "Project note" entry (file ee-org--current-project-todo)
           "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
          ("pi" "Project idea" entry (file ee-org--current-project-todo)
           "* IDEA %?\n%U\n")))
```

Replace with:

```elisp
  ;; Capture templates — central inbox + journal + project todo
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

- [ ] **Step 2: Syntax-check**

Run:
```bash
emacs --batch --eval "(progn (find-file \"/home/ehsan/projects/eemacs/modules/ee-org.el\") (check-parens))"
```

Expected: exit 0.

- [ ] **Step 3: Commit**

Run:
```bash
cd /home/ehsan/projects/eemacs
git add modules/ee-org.el
git commit -m "org: collapse capture templates to t/j/p, drop auto-clock-in on quick captures"
```

---

### Task 5: Flip agenda settings and add the daily-ritual custom command

**Files:**
- Modify: `/home/ehsan/projects/eemacs/modules/ee-org.el:341-355`

- [ ] **Step 1: Replace the `use-package org-agenda` `:custom` block**

Find the block at `modules/ee-org.el:341-355`:

```elisp
(use-package org-agenda
  :straight (:type built-in)
  :after org
  :custom
  (org-agenda-tags-column 0)
  (org-agenda-span 'day)
  (org-agenda-start-with-log-mode t)
  (org-agenda-include-diary t)
  (org-agenda-use-time-grid t)
  (org-agenda-block-separator ?─)
  (org-agenda-current-time-string
   "⭠ now ─────────────────────────────────────────────────")
  (org-agenda-time-grid '((daily today require-timed)
                          (800 1000 1200 1400 1600 1800 2000)
                          " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")))
```

Replace with:

```elisp
(use-package org-agenda
  :straight (:type built-in)
  :after org
  :custom
  (org-agenda-tags-column 0)
  (org-agenda-span 'day)
  (org-agenda-start-with-log-mode nil)
  (org-agenda-include-diary nil)
  (org-agenda-use-time-grid t)
  (org-agenda-block-separator ?─)
  (org-agenda-current-time-string
   "⭠ now ─────────────────────────────────────────────────")
  (org-agenda-time-grid '((daily today require-timed)
                          (800 1000 1200 1400 1600 1800 2000)
                          " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄"))
  (org-agenda-custom-commands
   '(("d" "Today"
      ((agenda "" ((org-agenda-span 'day)
                   (org-agenda-overriding-header "Today")))
       (todo "STRT"
             ((org-agenda-overriding-header "In progress")))
       (todo "WAIT"
             ((org-agenda-overriding-header "Waiting on")))
       (tags-todo "+CATEGORY=\"inbox\""
                  ((org-agenda-overriding-header "Inbox to triage"))))))))
```

- [ ] **Step 2: Syntax-check**

Run:
```bash
emacs --batch --eval "(progn (find-file \"/home/ehsan/projects/eemacs/modules/ee-org.el\") (check-parens))"
```

Expected: exit 0.

- [ ] **Step 3: Commit**

Run:
```bash
cd /home/ehsan/projects/eemacs
git add modules/ee-org.el
git commit -m "org: quiet agenda defaults and add 'd' Today custom command"
```

---

### Task 6: Trim `org-modern-todo-faces` in `ee-org.el`

**Files:**
- Modify: `/home/ehsan/projects/eemacs/modules/ee-org.el:444-452`

- [ ] **Step 1: Replace the `org-modern-todo-faces` block**

Find the block at `modules/ee-org.el:444-452`:

```elisp
  (org-modern-todo-faces
   '(("IDEA" . (:inherit org-verbatim :weight semi-bold :foreground "white" :background "goldenrod"))
     ("STRT" . (:inherit org-verbatim :weight semi-bold :foreground "white" :background "OrangeRed"))
     ("WAIT" . (:inherit org-verbatim :weight semi-bold :foreground "white" :background "coral"))
     ("KILL" . (:inherit org-verbatim :weight semi-bold :foreground "white" :background "DarkGreen"))
     ("PROJ" . (:inherit org-verbatim :weight semi-bold :foreground "white" :background "LimeGreen"))
     ("HOLD" . (:inherit org-verbatim :weight semi-bold :foreground "white" :background "orange"))
     ("LOOP" . (:inherit org-verbatim :weight semi-bold :foreground "white" :background "SteelBlue"))
     ("DONE" . (:inherit org-verbatim :weight semi-bold :foreground "black" :background "LightGray"))))
```

Replace with:

```elisp
  (org-modern-todo-faces
   '(("STRT" . (:inherit org-verbatim :weight semi-bold :foreground "white" :background "OrangeRed"))
     ("WAIT" . (:inherit org-verbatim :weight semi-bold :foreground "white" :background "coral"))
     ("KILL" . (:inherit org-verbatim :weight semi-bold :foreground "white" :background "DarkGreen"))
     ("DONE" . (:inherit org-verbatim :weight semi-bold :foreground "black" :background "LightGray"))))
```

- [ ] **Step 2: Syntax-check**

Run:
```bash
emacs --batch --eval "(progn (find-file \"/home/ehsan/projects/eemacs/modules/ee-org.el\") (check-parens))"
```

Expected: exit 0.

- [ ] **Step 3: Commit**

Run:
```bash
cd /home/ehsan/projects/eemacs
git add modules/ee-org.el
git commit -m "org: trim org-modern todo faces to match new keyword set"
```

---

### Task 7: Dashboard integration — narrow widget to today and launch the Today view

**Files:**
- Modify: `/home/ehsan/projects/eemacs/modules/ee-dashboard.el:62` (transient agenda action)
- Modify: `/home/ehsan/projects/eemacs/modules/ee-dashboard.el:96-97` (add `dashboard-week-agenda`)
- Modify: `/home/ehsan/projects/eemacs/modules/ee-dashboard.el:127` (navigator agenda button)

- [ ] **Step 1: Repoint the transient's "Open agenda" action**

Find at `modules/ee-dashboard.el:62`:

```elisp
    ("A" "Open agenda"  org-agenda)
```

Replace with:

```elisp
    ("A" "Open agenda"  (lambda () (interactive) (org-agenda nil "d")))
```

- [ ] **Step 2: Add `dashboard-week-agenda` to the `:custom` block**

Find at `modules/ee-dashboard.el:96-97`:

```elisp
  (dashboard-agenda-prefix-format " %i %-12:c %s ")
  (dashboard-agenda-tags-format 'ignore)
```

Replace with:

```elisp
  (dashboard-agenda-prefix-format " %i %-12:c %s ")
  (dashboard-agenda-tags-format 'ignore)
  (dashboard-week-agenda nil)
```

- [ ] **Step 3: Repoint the navigator's Agenda button**

Find at `modules/ee-dashboard.el:125-127`:

```elisp
           (,(nerd-icons-codicon "nf-cod-calendar" :height 1.0 :v-adjust -0.1)
            "Agenda" "Org agenda (C-c a)"
            (lambda (&rest _) (org-agenda)))
```

Replace with:

```elisp
           (,(nerd-icons-codicon "nf-cod-calendar" :height 1.0 :v-adjust -0.1)
            "Agenda" "Today view (C-c a d)"
            (lambda (&rest _) (org-agenda nil "d")))
```

- [ ] **Step 4: Syntax-check**

Run:
```bash
emacs --batch --eval "(progn (find-file \"/home/ehsan/projects/eemacs/modules/ee-dashboard.el\") (check-parens))"
```

Expected: exit 0.

- [ ] **Step 5: Commit**

Run:
```bash
cd /home/ehsan/projects/eemacs
git add modules/ee-dashboard.el
git commit -m "dashboard: narrow agenda widget to today and launch Today view from navigator"
```

---

### Task 8: Migrate `~/org/inbox.org`

This is user data outside the repo. Backup already exists from Task 1. Do not commit anything in this task.

**Files:**
- Modify: `~/org/inbox.org`

Current content (verified):

```
* [ ] Email to Dali 
SCHEDULED: <2026-05-20 Wed>
:LOGBOOK:
CLOCK: [2026-05-20 Wed 12:39]--[2026-05-20 Wed 12:40] =>  0:01
:END:
[2026-05-20 Wed 12:39]
* TODO Email to IT for HZI VPN and HPC Cluster 
SCHEDULED: <2026-05-20 Wed>
:LOGBOOK:
CLOCK: [2026-05-20 Wed 12:44]--[2026-05-20 Wed 12:45] =>  0:01
:END:
[2026-05-20 Wed 12:44]
I should ask them to add me to a proper group.
```

(Note: the exact body of the second item is preserved as-is from the original.)

- [ ] **Step 1: Replace the file contents**

Overwrite `~/org/inbox.org` with:

```
* TODO Email to Dali
SCHEDULED: <2026-05-20 Wed>
[2026-05-20 Wed 12:39]

* TODO Email to IT for HZI VPN and HPC Cluster
SCHEDULED: <2026-05-20 Wed>
[2026-05-20 Wed 12:44]
I should ask them to add me to a proper group.
```

Changes from the original:
- `* [ ] Email to Dali ` → `* TODO Email to Dali` (drop the checkbox heading keyword, drop trailing space).
- Strip both `:LOGBOOK: … :END:` drawers (1-minute clock noise from the original captures).
- Strip trailing space on the second heading.
- Keep `SCHEDULED:` lines and the inactive `%U` timestamps.
- Keep the body line under the second item.

- [ ] **Step 2: Verify the file parses cleanly**

Run:
```bash
emacs --batch ~/org/inbox.org --eval "(progn (org-mode) (message \"OK: %d headings\" (length (org-map-entries (lambda () t)))))"
```

Expected: prints `OK: 2 headings`.

- [ ] **Step 3: Verify both items are TODOs**

Run:
```bash
emacs --batch ~/org/inbox.org --eval "(progn (org-mode) (org-map-entries (lambda () (message \"%s: %s\" (org-get-todo-state) (nth 4 (org-heading-components))))))"
```

Expected: two lines, both starting with `TODO: …`.

No commit (file outside repo).

---

### Task 9: Migrate `~/org/projects.org`

User data, outside the repo. Backup exists from Task 1.

**Files:**
- Modify: `~/org/projects.org`

Current content (verified):

```
#+title: Projects Master List
#+filetags: :index:

* PROJ TCell Reseach :research:
:PROPERTIES:
:DIR: [[file:~/projects/TCell-research/]]
:END:

- Project Tasks: [[file:~/projects/TCell-research/TODO.org]]

** TODO Meeting with Dali
- [ ] Finalize the time
- 

  
* PROJ Chemotactic Bacteria in Porous Media

* PROJ Active Polymers: BIPS vs Molecular Motors
```

- [ ] **Step 1: Replace the file contents**

Overwrite `~/org/projects.org` with:

```
#+title: Projects Master List
#+filetags: :index:

* TCell research :research:
:PROPERTIES:
:DIR: [[file:~/projects/TCell-research/]]
:END:

- Project Tasks: [[file:~/projects/TCell-research/TODO.org]]

** TODO Meeting with Dali
- [ ] Finalize the time

* Chemotactic Bacteria in Porous Media

* Active Polymers: BIPS vs Molecular Motors
```

Changes from the original:
- Drop `PROJ` keyword from all three top-level headings.
- Fix "Reseach" → "research" on the first heading.
- Drop the stray trailing `- ` empty list item in the TCell subtree (cleans up the list).
- Keep the `:research:` tag on the first heading.
- Keep `** TODO Meeting with Dali` and its child checkbox.
- Keep `#+title:` and `#+filetags:`.

- [ ] **Step 2: Verify the file parses and headings have the expected TODO states**

Run:
```bash
emacs --batch ~/org/projects.org --eval "(progn (org-mode) (org-map-entries (lambda () (message \"L%d %s: %s\" (org-current-level) (or (org-get-todo-state) \"-\") (nth 4 (org-heading-components))))))"
```

Expected output (order may vary by heading position):
```
L1 -: TCell research
L2 TODO: Meeting with Dali
L1 -: Chemotactic Bacteria in Porous Media
L1 -: Active Polymers: BIPS vs Molecular Motors
```

The top-level headings should show `-` (no TODO state); only the child task should be `TODO`.

No commit (file outside repo).

---

### Task 10: Update `docs/org-workflow-guide.org`

**Files:**
- Modify: `/home/ehsan/projects/eemacs/docs/org-workflow-guide.org`

This task has multiple sub-edits. Make all of them, then commit once.

- [ ] **Step 1: Remove `agenda.org` from the Central Hub directory tree**

Find at `docs/org-workflow-guide.org:11-20`:

```
#+begin_example
~/org/
├── inbox.org          <- everything lands here first (GTD inbox)
├── agenda.org         <- scheduled/deadlined items, appointments
├── projects.org       <- master project list with links
├── journal.org        <- daily log (datetree)
├── reading.org        <- book/paper notes (datetree)
├── archive/           <- completed/killed items go here
└── roam/              <- org-roam knowledge graph (interlinked notes)
#+end_example
```

Replace with:

```
#+begin_example
~/org/
├── inbox.org          <- everything lands here first (GTD inbox)
├── projects.org       <- master project list with links
├── journal.org        <- daily log (datetree)
├── reading.org        <- book/paper notes (datetree)
├── archive/           <- completed/killed items go here
└── roam/              <- org-roam knowledge graph (interlinked notes)
#+end_example
```

- [ ] **Step 2: Remove the central-files paragraph reference to `agenda.org`**

Find at `docs/org-workflow-guide.org:60-64`:

```
The central files (=inbox.org=, =agenda.org=, =projects.org=,
=journal.org=, =reading.org=) are created automatically by =org-capture=
the first time their template fires --- you do not need to pre-create
them.  =org-roam/= is also auto-created by org-roam itself on load, but
creating it up front avoids a one-time startup warning.
```

Replace with:

```
The central files (=inbox.org=, =projects.org=, =journal.org=,
=reading.org=) are created automatically by =org-capture= the first
time their template fires --- you do not need to pre-create them.
=org-roam/= is also auto-created by org-roam itself on load, but
creating it up front avoids a one-time startup warning.
```

- [ ] **Step 3: Update the `projects.org` example to drop `PROJ` keyword**

Find at `docs/org-workflow-guide.org:72-91`:

```
#+begin_example
,#+title: Projects Master List
,#+filetags: :index:

,* Active

,** PROJ DSP signal-processing library                              :research:
   - Code:  [[file:~/projects/my-dsp-project/]]
   - Tasks: [[file:~/projects/my-dsp-project/TODO.org]]
   - Notes: [[file:~/projects/my-dsp-project/notes/]]

   Free-form status, decisions, and links to roam nodes go here.

,** PROJ TCell research                                             :research:
   - Tasks: [[file:~/projects/TCell-research/TODO.org]]
   - Notes: [[file:~/projects/TCell-research/notes/notes.org]]

,* Backburner
,* Done / archived
#+end_example
```

Replace with:

```
#+begin_example
,#+title: Projects Master List
,#+filetags: :index:

,* DSP signal-processing library                                    :research:
  - Code:  [[file:~/projects/my-dsp-project/]]
  - Tasks: [[file:~/projects/my-dsp-project/TODO.org]]
  - Notes: [[file:~/projects/my-dsp-project/notes/]]

  Free-form status, decisions, and links to roam nodes go here.

,* TCell research                                                   :research:
  - Tasks: [[file:~/projects/TCell-research/TODO.org]]
  - Notes: [[file:~/projects/TCell-research/notes/notes.org]]
#+end_example
```

- [ ] **Step 4: Update the surrounding explanation about `PROJ` keyword**

Find at `docs/org-workflow-guide.org:93-96`:

```
Using the =PROJ= keyword on each project heading lets =C-c a m +PROJ=
surface every project at a glance during your weekly review.  Giving the
file an =:ID:= (=M-x org-id-get-create=) also makes it a roam node, so
=C-c n f= finds it and project notes can link back to it.
```

Replace with:

```
Each top-level heading inside =projects.org= is a project --- the
=:index:= filetag on the file already marks the whole file as a project
list, so no per-heading keyword is needed.  Giving the file an =:ID:=
(=M-x org-id-get-create=) also makes it a roam node, so =C-c n f= finds
it and project notes can link back to it.  Archive finished projects
with =C-c C-x C-a=.
```

- [ ] **Step 5: Trim the Central Captures section**

Find at `docs/org-workflow-guide.org:102-167`. Delete the entire subsections `*** =n= --- Note`, `*** =i= --- Idea`, `*** =r= --- Research Note`, and `*** =b= --- Book/Paper`. Keep `*** =t= --- Todo` and `*** =j= --- Journal`. After the `=j=` section, before the Project-Local Captures section, insert this paragraph:

```
For research notes that aren't tasks, use =C-c n c= (=org-roam-capture=)
rather than =C-c x=.  Org-capture is for items that need an active state
or a date; org-roam-capture is for knowledge nodes.
```

Concretely, the section structure goes from:

```
** Central Captures

*** =t= --- Todo
... (keep)
*** =n= --- Note
... (DELETE)
*** =j= --- Journal
... (keep)
*** =i= --- Idea
... (DELETE)
*** =r= --- Research Note
... (DELETE)
*** =b= --- Book/Paper
... (DELETE)
```

…to:

```
** Central Captures

*** =t= --- Todo
... (kept)
*** =j= --- Journal
... (kept)

For research notes that aren't tasks, use =C-c n c= (=org-roam-capture=)
rather than =C-c x=.  Org-capture is for items that need an active state
or a date; org-roam-capture is for knowledge nodes.
```

- [ ] **Step 6: Collapse the Project-Local Captures table**

Find at `docs/org-workflow-guide.org:171-178`:

```
These file directly into the current project's =TODO.org=, so tasks stay with the code.

| Key   | Template     | Destination            |
|-------+--------------+------------------------|
| =p t= | Project todo | =<project>/TODO.org=   |
| =p n= | Project note | =<project>/TODO.org=   |
| =p i= | Project idea | =<project>/TODO.org=   |

The "current project" is detected automatically from the file you're editing (via =project.el=). If you're editing =~/projects/my-dsp-project/src/fft.c= and press =C-c x p t=, the TODO goes into =~/projects/my-dsp-project/TODO.org=.
```

Replace with:

```
This files directly into the current project's =TODO.org=, so tasks stay with the code.

| Key  | Template     | Destination          |
|------+--------------+----------------------|
| =p=  | Project todo | =<project>/TODO.org= |

The "current project" is detected automatically from the file you're editing (via =project.el=). If you're editing =~/projects/my-dsp-project/src/fft.c= and press =C-c x p=, the TODO goes into =~/projects/my-dsp-project/TODO.org=.
```

- [ ] **Step 7: Rewrite the "TODO States" section**

Find at `docs/org-workflow-guide.org:287-319`:

```
* TODO States --- What Each One Means

The states form a workflow. Cycle with =C-c C-t= then press the shortcut key:

#+begin_example
TODO  ->  you need to do this
PROJ  ->  this is a project heading (contains sub-tasks)
LOOP  ->  recurring task (weekly review, daily standup, etc.)
STRT  ->  you're actively working on this right now
WAIT  ->  blocked on someone/something external
HOLD  ->  you paused this yourself (not blocked, just deprioritized)
IDEA  ->  unconfirmed, might become a task or might not
---------
DONE  ->  completed
KILL  ->  cancelled / no longer relevant
#+end_example

** Checkbox Sequence (for checklists within a task)

#+begin_example
[ ]  ->  to do
[-]  ->  in progress
[?]  ->  uncertain / on hold
[X]  ->  done
#+end_example

** Decision Sequence (for tracking choices)

#+begin_example
OKAY  ->  neutral decision recorded
YES   ->  approved / accepted
NO    ->  rejected / declined
#+end_example
```

Replace with:

```
* TODO States --- What Each One Means

The states form a workflow. Cycle with =C-c C-t= then press the shortcut key:

#+begin_example
TODO  ->  you need to do this
STRT  ->  you're actively working on this right now
WAIT  ->  blocked on someone/something external
---------
DONE  ->  completed
KILL  ->  cancelled / no longer relevant
#+end_example

For recurring tasks, use a =TODO= with a date repeater
(=SCHEDULED: <2026-05-22 Fri +1w>=) instead of a separate keyword.
For long-running pauses you control yourself, leave the state as =TODO=
or use =WAIT= --- the distinction between "blocked externally" and
"paused by me" rarely matters in the agenda.

** List-item checkboxes (inside a heading body)

These are not heading-level TODO states --- they're checkbox items in a
list, used to break a task into sub-items:

#+begin_example
,* TODO Prepare grant submission
  - [ ] Draft abstract
  - [-] Methods section
  - [X] Budget table
#+end_example
```

- [ ] **Step 8: Update the Example Project Structure**

Find at `docs/org-workflow-guide.org:336-346`:

```
** Example Project Structure

#+begin_example
,* PROJ Signal processing library :research:programming:
,** DONE Set up repository
,** STRT Implement FFT module
,*** TODO Write unit tests for edge cases
,*** DONE Basic radix-2 implementation
,** WAIT Get benchmark data from lab
,** IDEA Maybe add GPU acceleration?
#+end_example
```

Replace with:

```
** Example Project Structure

#+begin_example
,* Signal processing library :research:programming:
,** DONE Set up repository
,** STRT Implement FFT module
,*** TODO Write unit tests for edge cases
,*** DONE Basic radix-2 implementation
,** WAIT Get benchmark data from lab
,** TODO Investigate GPU acceleration
#+end_example
```

- [ ] **Step 9: Update the Agenda navigation section**

Find at `docs/org-workflow-guide.org:350-357`:

```
Press =C-c a= then:

| Key | View                                                             |
|-----+------------------------------------------------------------------|
| =a= | Day agenda -- all scheduled items from central + project files   |
| =t= | All TODOs across ALL registered projects and central org         |
| =m= | Match by tag (e.g., =research+URGENT=)                          |
| =s= | Search full text across everything                               |
```

Replace with:

```
Press =C-c a= then:

| Key | View                                                                |
|-----+---------------------------------------------------------------------|
| =d= | *Today --- daily ritual* (scheduled + STRT + WAIT + inbox triage)   |
| =a= | Day agenda --- all scheduled items from central + project files     |
| =t= | All TODOs across ALL registered projects and central org            |
| =m= | Match by tag (e.g., =research+URGENT=)                              |
| =s= | Search full text across everything                                  |

=C-c a d= is the morning ritual view.  It shows four blocks: today's
scheduled and deadlined items, everything in =STRT= state, everything
in =WAIT= state, and every =TODO= still sitting in =inbox.org= (the
=Inbox to triage= block uses =CATEGORY="inbox"= which Org derives
automatically from the filename).  If a block grows long, that's the
signal: too many things in progress, follow-ups stalling, or inbox
backlog accumulating.
```

- [ ] **Step 10: Update the Daily Routine Summary table**

Find at `docs/org-workflow-guide.org:510-521` (the Daily Routine Summary table).

Locate the row:

```
| *Morning*           | Check agenda (all projects)       | =C-c a a=                   |
```

Replace it with:

```
| *Morning*           | Daily ritual view                 | =C-c a d=                   |
```

Also locate:

```
| *Working on project* | Capture to that project           | =C-c x p t= / =C-c x p n= / =C-c x p i= |
```

Replace with:

```
| *Working on project* | Capture to that project           | =C-c x p=                   |
```

- [ ] **Step 11: Verify the doc still parses as Org**

Run:
```bash
emacs --batch /home/ehsan/projects/eemacs/docs/org-workflow-guide.org --eval "(progn (org-mode) (message \"OK: %d top-level headings\" (length (org-map-entries (lambda () t) \"LEVEL=1\"))))"
```

Expected: prints `OK: <some number>`. No errors.

- [ ] **Step 12: Commit**

Run:
```bash
cd /home/ehsan/projects/eemacs
git add docs/org-workflow-guide.org
git commit -m "docs(org): align workflow guide with trimmed config"
```

---

### Task 11: Apply to running Emacs and verify

This is the manual integration test. Three sub-steps depending on the user's home-manager workflow.

- [ ] **Step 1: Push the eemacs branch (if home-manager flake input is github)**

Run:
```bash
cd /home/ehsan/projects/eemacs
git log --oneline -8
git push
```

Expected: clean push to `origin/main`. If the user uses a local-path flake override instead, this step is unnecessary — skip and note it.

- [ ] **Step 2: Update flake input and home-manager switch**

Run (in the user's dotfiles repo):
```bash
cd ~/dotfiles
nix flake update eemacs
home-manager switch --flake .
```

Expected: flake updates the `eemacs` input to the new commit; `home-manager switch` succeeds with no Emacs-related errors.

If the user prefers to test before pushing, the alternative is:
```bash
cd ~/dotfiles
home-manager switch --flake . --override-input eemacs path:/home/ehsan/projects/eemacs
```

- [ ] **Step 3: Restart Emacs**

Quit running Emacs and start a new one (or restart the daemon). Restart is required because:
- `~/.emacs.d/init.el` and `~/.emacs.d/modules/` are symlinks to the Nix store. After `home-manager switch` the symlinks point at a new store path, but the already-loaded Emacs functions are from the old path.

- [ ] **Step 4: Manual verification checklist**

Inside the fresh Emacs:

1. **Captures work** — `C-c x` shows three options: `t Todo`, `j Journal`, `p Project Todo`. Press `t`, type a test capture, finalize with `C-c C-c`. Verify it appears at the end of `~/org/inbox.org` as `* TODO …` with a `%U` timestamp and **no** `:LOGBOOK:` drawer.
2. **Daily view works** — `C-c a d`. Buffer shows four labeled sections: `Today`, `In progress`, `Waiting on`, `Inbox to triage`. The two migrated items in `inbox.org` appear under `Today` (today's scheduled) **and** under `Inbox to triage` (they're still `TODO`).
3. **TODO state cycling** — open `~/org/inbox.org`, place point on a heading, press `C-c C-t`. Fast-selection menu shows only `TODO STRT WAIT DONE KILL` — no `[ ]`, no `OKAY/YES/NO`, no `PROJ/HOLD/IDEA/LOOP`.
4. **Dashboard** — `C-c d`. Click the navigator's `Agenda` button (or `?` then `A`). It opens the Today view directly, not the dispatch buffer. The dashboard's own agenda widget lists at most today's items.
5. **No org-mode load errors** — `M-x list-messages` (or `*Messages*` buffer). Search for `org-`, confirm no error spam during load.
6. **Test capture cleanup** — delete the test capture you added in step 1 from `~/org/inbox.org`.

If any of the six checks fail, restore backups and roll back repo commits:

```bash
# Restore user data
cp ~/org/inbox.org.bak-2026-05-20 ~/org/inbox.org
cp ~/org/projects.org.bak-2026-05-20 ~/org/projects.org

# Revert repo commits — there are 7 task commits since the design spec
# (Tasks 2, 3, 4, 5, 6, 7, 10). The spec commit is the safe rollback point.
cd /home/ehsan/projects/eemacs
git log --oneline    # find the design spec commit SHA (subject "add GTD workflow simplification design spec")
git revert <spec-sha>..HEAD --no-edit
```

…and report which check failed.

- [ ] **Step 5: Delete the backups once the system is verified**

Only after every item in step 4 passes:

```bash
rm ~/org/inbox.org.bak-2026-05-20 ~/org/projects.org.bak-2026-05-20
```

No commit (no repo files involved).

---

## Self-review notes (already applied)

- **Spec coverage:** every section of `docs/superpowers/specs/2026-05-20-org-gtd-workflow-design.md` maps to a task here: TODO keywords → Task 2, faces → Tasks 3 & 6, capture templates → Task 4, agenda settings + custom command → Task 5, dashboard → Task 7, `inbox.org` migration → Task 8, `projects.org` migration → Task 9, doc → Task 10, integration test → Task 11. The spec's Risks section (pre-flight grep of project TODOs) was completed during plan authoring — confirmed `TCell-research/TODO.org` uses its own `#+seq_todo:` and is unaffected.
- **Placeholder scan:** none — every edit shows the exact before/after, every command is runnable as-written.
- **Type/name consistency:** keyword names (`TODO STRT WAIT DONE KILL`) and face names (`+org-todo-active`, `+org-todo-onhold`, `+org-todo-cancel`) match across Tasks 2, 3, 5, 6.
