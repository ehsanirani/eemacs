# Org-Mode Hybrid Workflow Guide

## Directory Structure

Your org system spans two layers: a central `~/org/` hub and per-project org files.

### Central Hub

```
~/org/
├── inbox.org          <- everything lands here first (GTD inbox)
├── agenda.org         <- scheduled/deadlined items, appointments
├── projects.org       <- master project list with links
├── journal.org        <- daily log (datetree)
├── reading.org        <- book/paper notes (datetree)
├── archive/           <- completed/killed items go here
└── roam/              <- org-roam knowledge graph (interlinked notes)
```

### Per-Project (distributed)

```
~/projects/my-dsp-project/
├── src/
├── TODO.org           <- project tasks (auto-added to agenda)
└── notes/             <- project notes (auto-indexed by org-roam)
    ├── architecture.org
    └── experiments.org

~/projects/ml-research/
├── TODO.org
└── notes/
    └── model-comparison.org
```

The agenda aggregates TODOs from both layers. Org-roam indexes notes from both layers into one unified knowledge graph.

---

## 1. Capturing (The Entry Point for Everything)

Press `C-c x` from anywhere in Emacs. Pick a template:

### Central Captures

#### `t` -- Todo

Something you need to do. Goes to central inbox, clock starts automatically.

```org
* TODO Buy new oscilloscope
  [2026-02-27 Thu 14:30]
  [[file:~/code/project/README.org]]
```

#### `n` -- Note

Quick thought, meeting note, snippet. Also goes to inbox with clock-in.

```org
* Found interesting approach for signal processing :NOTE:
  [2026-02-27 Thu 15:00]
  [[file:~/papers/dsp-review.pdf]]
```

#### `j` -- Journal

Daily log entry. Goes into `journal.org` under today's date tree. Use this for daily standups, research log, what you worked on.

```org
* 2026
** 2026-02 February
*** 2026-02-27 Thursday
**** Debugged the FFT implementation
     Turns out the window function was wrong...
```

#### `i` -- Idea

Quick, unprocessed thought. No clock, no link -- just dump it fast.

```org
* IDEA What if we used wavelets instead of FFT for this?
  [2026-02-27 Thu]
```

#### `r` -- Research Note

For when you're reading a paper/article/code and want to capture a finding. Auto-tagged `:research:`, records where you were when you captured.

```org
* Kalman filter convergence proof :research:
  [2026-02-27 Thu 16:00]
  Source: [[file:~/papers/kalman-2024.pdf]]

  The key insight is that...
```

#### `b` -- Book/Paper

Structured reading note with author field. Goes into `reading.org` datetree.

```org
* Deep Learning for Signal Processing :AI:DSP:
  [2026-02-27 Thu]
  Author: Smith et al.

  Chapter 3 argues that...
```

### Project-Local Captures (`p` prefix)

These file directly into the current project's `TODO.org`, so tasks stay with the code.

| Key | Template | Destination |
|-----|----------|-------------|
| `p t` | Project todo | `<project>/TODO.org` |
| `p n` | Project note | `<project>/TODO.org` |
| `p i` | Project idea | `<project>/TODO.org` |

The "current project" is detected automatically from the file you're editing (via `project.el`). If you're editing `~/projects/my-dsp-project/src/fft.c` and press `C-c x p t`, the TODO goes into `~/projects/my-dsp-project/TODO.org`.

---

## 2. Multi-Project Management

### Registering a Project

To make a project's TODOs appear in your agenda and its notes searchable in org-roam:

```
M-x ee-org-register-project RET ~/projects/my-dsp-project RET
```

This does three things:
1. Adds `TODO.org` to your agenda (creates it if missing)
2. Creates a `notes/` directory (if missing) for org-roam notes
3. Persists the registration across Emacs restarts

### Unregistering a Project

```
M-x ee-org-unregister-project RET
```

Pick from a list of registered projects. This removes it from the agenda and org-roam indexing. It does NOT delete any files.

### Listing Projects

```
M-x ee-org-list-projects RET
```

Shows all registered project directories.

### Per-Project .dir-locals.el (optional)

For project-specific org settings, create a `.dir-locals.el` in the project root:

```elisp
;;; .dir-locals.el
((org-mode . ((org-roam-directory . "~/projects/my-dsp-project/notes")
              (citar-bibliography . ("~/projects/my-dsp-project/references.bib")))))
```

This makes org-roam commands and citation lookups use project-local files when you're editing inside that project.

### Example: Setting Up a New Research Project

```
;; 1. Register the project
M-x ee-org-register-project RET ~/projects/new-research RET

;; 2. Open the auto-created TODO.org
C-x C-f ~/projects/new-research/TODO.org RET

;; 3. Set up your project structure
* PROJ New Research Project :research:
** TODO Literature review
** TODO Set up experiment framework
** IDEA Could we use method X?

;; 4. Create a roam note in the project
;;    (from any file in the project)
C-c n c  ->  creates a note in ~/projects/new-research/notes/
```

---

## 3. Processing the Inbox (Daily/Weekly Ritual)

Open `inbox.org` and go through each item. For each one, decide:

| Decision | Action | Keys |
|----------|--------|------|
| Do it now (< 2 min) | Do it, mark DONE | `C-c C-t d` |
| Schedule it | Add a date, refile to `agenda.org` | `C-c C-s` then `C-c C-w` |
| It's part of a project | Refile to project's TODO.org | `C-c C-w` -> pick project |
| It's reference material | Create org-roam node | `C-c n f` -> new node |
| Trash it | Kill it | `C-c C-t k` (KILL) |

Note: registered project `TODO.org` files appear as refile targets, so you can refile directly from inbox to any project.

---

## 4. TODO States -- What Each One Means

The states form a workflow. Cycle with `C-c C-t` then press the shortcut key:

```
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
```

### Checkbox Sequence (for checklists within a task)

```
[ ]  ->  to do
[-]  ->  in progress
[?]  ->  uncertain / on hold
[X]  ->  done
```

### Decision Sequence (for tracking choices)

```
OKAY  ->  neutral decision recorded
YES   ->  approved / accepted
NO    ->  rejected / declined
```

### Example Project Structure

```org
* PROJ Signal processing library :research:programming:
** DONE Set up repository
** STRT Implement FFT module
*** TODO Write unit tests for edge cases
*** DONE Basic radix-2 implementation
** WAIT Get benchmark data from lab
** IDEA Maybe add GPU acceleration?
```

---

## 5. Agenda -- Your Daily Dashboard

Press `C-c a` then:

| Key | View |
|-----|------|
| `a` | Day agenda -- all scheduled items from central + project files |
| `t` | All TODOs across ALL registered projects and central org |
| `m` | Match by tag (e.g., `research+URGENT`) |
| `s` | Search full text across everything |

The agenda shows your day with the Unicode time grid:

```
Day-agenda (W09):
Thursday  27 February 2026
  8:00 ........... ...............
 10:00 ........... ...............
  agenda:     10:00 Lab meeting
 12:00 ........... ...............
  <-- now -------------------------
 14:00 ........... ...............
  my-dsp-project: Deadline: Write results section
  ml-research:    TODO Review paper draft
```

Tasks from different projects are clearly labeled with their source file.

### In Agenda View

- `K` -- start a pomodoro on the task at point
- `t` -- cycle TODO state
- `s` -- schedule
- `d` -- set deadline
- `r` -- refresh

---

## 6. Org-Roam -- Unified Knowledge Graph

Org-roam indexes notes from **both** your central `~/org/roam/` and every registered project's `notes/` directory. All notes are part of one interconnected graph.

| Keys | Action |
|------|--------|
| `C-c n f` | Find or create a node (searches all indexed dirs) |
| `C-c n i` | Insert a link to another node while writing |
| `C-c n l` | Toggle backlinks buffer |
| `C-c n c` | Capture a new roam note |
| `C-c n j` | Quick daily note |
| `C-c n g` | View the graph |
| `C-c n u` | Interactive graph in browser (org-roam-ui) |

### Cross-Project Linking

The power of a unified graph: notes in one project can link to notes in another.

```org
# ~/projects/my-dsp-project/notes/fft-optimization.org
#+title: FFT Optimization Approaches

See [[id:abc123][CUDA Kernel Patterns]] from the GPU project
for parallel implementation ideas.

The [[id:def456][Cooley-Tukey Algorithm]] note in the central
roam has the mathematical foundation.
```

### Research Workflow Example

1. `C-c n f` -> "Kalman Filter" -> creates node (in central roam or project notes)
2. While writing, `C-c n i` -> "Bayesian" -> links to another node (could be in any project)
3. `C-c n l` -> see all backlinks across all projects
4. `C-c n u` -> visual graph showing connections between projects

---

## 7. Pomodoro -- Focused Work Sessions

On any TODO item (central or project-local):

- `C-c C-x m` -- start a 25-minute pomodoro
- Modeline shows countdown
- After 25 min, notification + 5-min break
- Every 4 pomodoros, longer break

In the agenda, press `K` on a task to start a pomodoro for it.

---

## 8. Bibliography -- For Papers and Citations

When writing a research document in org:

- `C-c C-x @` -- insert a citation (citar searches your `.bib` file)
- Citations render in the buffer with icons (PDF, note, link)
- On export to LaTeX, they become proper `\cite{}` commands with biblatex

Set your bibliography in a file header:

```org
#+bibliography: ~/org/references.bib
#+cite_export: biblatex

As shown by [cite:@smith2024], the algorithm converges...
```

For project-local bibliographies, use `.dir-locals.el`:

```elisp
((org-mode . ((citar-bibliography . ("./references.bib")))))
```

---

## 9. Exporting -- Getting Output

| Format | How |
|--------|-----|
| PDF (LaTeX) | `C-c C-e l p` -- academic quality with booktabs tables |
| Beamer slides | `C-c C-e l P` -- from org headings to presentation |
| HTML | `C-c C-e h h` |
| GitHub Markdown | `C-c C-e g g` (ox-gfm) |
| ODT (LibreOffice) | `C-c C-e o o` |

Exports run in the background (async) so Emacs stays responsive.

---

## 10. Daily Routine Summary

| When | What | Keys |
|------|------|------|
| **Morning** | Check agenda (all projects) | `C-c a a` |
| **Throughout day** | Capture anything | `C-c x` -> pick template |
| **Working on a project** | Capture to that project | `C-c x p t` / `p n` / `p i` |
| **Starting focused work** | Start pomodoro | `C-c C-x m` |
| **While researching** | Create/link roam notes | `C-c n f`, `C-c n i` |
| **End of day** | Journal entry | `C-c x j` |
| **Weekly** | Process inbox, refile to projects | `inbox.org`, `C-c C-w` |
| **Weekly** | Review all projects | `C-c a m` -> `+PROJ` |
| **New project** | Register it | `M-x ee-org-register-project` |

---

## 11. Quick Reference -- All Commands

### Global

| Keys | Command |
|------|---------|
| `C-c a` | Open agenda |
| `C-c x` | Capture |
| `C-c n f` | Find/create roam node |
| `C-c n i` | Insert roam link |
| `C-c n l` | Toggle backlinks |
| `C-c n u` | Roam graph UI |
| `C-c n j` | Daily roam note |

### In Org Buffers

| Keys | Command |
|------|---------|
| `C-c C-t` | Cycle TODO state |
| `C-c C-s` | Schedule |
| `C-c C-d` | Deadline |
| `C-c C-w` | Refile |
| `C-c l` | Store link |
| `C-c C-x m` | Start pomodoro |
| `C-c C-e` | Export dispatcher |
| `M-h/l/j/k` | Org meta movement |

### Project Management

| Command | Action |
|---------|--------|
| `M-x ee-org-register-project` | Add project to agenda + roam |
| `M-x ee-org-unregister-project` | Remove project |
| `M-x ee-org-list-projects` | Show registered projects |
