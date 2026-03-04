# Org-Mode Hybrid Workflow Guide

## Directory Structure

After using this config, your `~/org/` will look like:

```
~/org/
├── inbox.org          <- everything lands here first (GTD inbox)
├── agenda.org         <- scheduled/deadlined items, appointments
├── projects.org       <- active projects with sub-tasks
├── journal.org        <- daily log (datetree)
├── reading.org        <- book/paper notes (datetree)
├── archive/           <- completed/killed items go here
└── roam/              <- org-roam knowledge graph (interlinked notes)
```

---

## 1. Capturing (The Entry Point for Everything)

Press `C-c x` from anywhere in Emacs. Pick a template:

### `t` — Todo

Something you need to do. Goes to inbox, clock starts automatically so you know when you created it and how long you spent thinking about it.

```org
* TODO Buy new oscilloscope
  [2026-02-27 Thu 14:30]
  [[file:~/code/project/README.org]]
```

### `n` — Note

Quick thought, meeting note, snippet. Also goes to inbox with clock-in.

```org
* Found interesting approach for signal processing :NOTE:
  [2026-02-27 Thu 15:00]
  [[file:~/papers/dsp-review.pdf]]
```

### `j` — Journal

Daily log entry. Goes into `journal.org` under today's date tree. Use this for daily standups, research log, what you worked on.

```org
* 2026
** 2026-02 February
*** 2026-02-27 Thursday
**** Debugged the FFT implementation
     Turns out the window function was wrong...
```

### `i` — Idea

Quick, unprocessed thought. No clock, no link -- just dump it fast.

```org
* IDEA What if we used wavelets instead of FFT for this?
  [2026-02-27 Thu]
```

### `r` — Research Note

For when you're reading a paper/article/code and want to capture a finding. Auto-tagged `:research:`, records where you were when you captured.

```org
* Kalman filter convergence proof :research:
  [2026-02-27 Thu 16:00]
  Source: [[file:~/papers/kalman-2024.pdf]]

  The key insight is that...
```

### `b` — Book/Paper

Structured reading note with author field. Goes into `reading.org` datetree so you can see your reading history chronologically.

```org
* Deep Learning for Signal Processing :AI:DSP:
  [2026-02-27 Thu]
  Author: Smith et al.

  Chapter 3 argues that...
```

---

## 2. Processing the Inbox (Daily/Weekly Ritual)

Open `inbox.org` and go through each item. For each one, decide:

| Decision | Action | Keys |
|----------|--------|------|
| Do it now (< 2 min) | Do it, mark DONE | `C-c C-t d` |
| Schedule it | Add a date, refile to `agenda.org` | `C-c C-s` then `C-c C-w` |
| It's part of a project | Refile under a project heading | `C-c C-w` -> pick project |
| It's reference material | Move to org-roam | Create a roam node, refile |
| Trash it | Kill it | `C-c C-t k` (KILL) |

---

## 3. TODO States -- What Each One Means

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

## 4. Agenda -- Your Daily Dashboard

Press `C-c a` then:

| Key | View |
|-----|------|
| `a` | Day agenda -- scheduled items, deadlines, time grid |
| `t` | All TODOs across files |
| `m` | Match by tag (e.g., `research+URGENT`) |
| `s` | Search full text |

The agenda shows your day with the Unicode time grid:

```
Day-agenda (W09):
Thursday  27 February 2026
  8:00 ........... ...............
 10:00 ........... ...............
  agenda:   10:00 Lab meeting
 12:00 ........... ...............
  <-- now -------------------------
 14:00 ........... ...............
  projects: Deadline: Write results section
```

### In Agenda View

- `K` -- start a pomodoro on the task at point
- `t` -- cycle TODO state
- `s` -- schedule
- `d` -- set deadline
- `r` -- refresh

---

## 5. Org-Roam -- Building a Knowledge Graph

This is where your long-term research knowledge lives. Unlike the flat org files above, roam notes are **interlinked**.

| Keys | Action |
|------|--------|
| `C-c n f` | Find or create a node (your main entry point) |
| `C-c n i` | Insert a link to another node while writing |
| `C-c n l` | Toggle backlinks buffer (see what links to current note) |
| `C-c n c` | Capture a new roam note |
| `C-c n j` | Quick daily note |
| `C-c n g` | View the graph |
| `C-c n u` | Interactive graph in browser (org-roam-ui) |

### Research Workflow Example

1. You read a paper on Kalman filters. `C-c n f` -> type "Kalman Filter" -> creates `~/org/roam/kalman_filter.org`
2. While writing, you realize it connects to Bayesian inference. `C-c n i` -> type "Bayesian" -> links to or creates that node
3. Later, you open "Bayesian Inference" and toggle backlinks (`C-c n l`) -- you see Kalman Filter links here
4. Open `C-c n u` -- see a visual graph of how your research topics connect

```org
# ~/org/roam/kalman_filter.org
#+title: Kalman Filter

Optimal recursive estimator for linear systems.

Related to [[id:abc123][Bayesian Inference]] - the predict/update
cycle is essentially Bayes' theorem applied sequentially.

Used in our [[id:def456][GPS Signal Processing Project]] for
position estimation.
```

---

## 6. Pomodoro -- Focused Work Sessions

On any TODO item:

- `C-c C-x m` -- start a 25-minute pomodoro
- Modeline shows countdown
- After 25 min, it notifies you and starts a 5-min break
- Every 4 pomodoros, longer break

In the agenda, press `K` on a task to start a pomodoro for it.

---

## 7. Bibliography -- For Papers and Citations

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

---

## 8. Exporting -- Getting Output

| Format | How |
|--------|-----|
| PDF (LaTeX) | `C-c C-e l p` -- academic quality with booktabs tables |
| Beamer slides | `C-c C-e l P` -- from org headings to presentation |
| HTML | `C-c C-e h h` |
| GitHub Markdown | `C-c C-e g g` (ox-gfm) |
| ODT (LibreOffice) | `C-c C-e o o` |

Exports run in the background (async) so Emacs stays responsive.

---

## 9. Daily Routine Summary

| When | What | Keys |
|------|------|------|
| **Morning** | Check agenda | `C-c a a` |
| **Throughout day** | Capture anything immediately | `C-c x` -> pick template |
| **Starting focused work** | Start pomodoro on a task | `C-c C-x m` |
| **While researching** | Create/link roam notes | `C-c n f`, `C-c n i` |
| **End of day** | Journal entry | `C-c x j` |
| **Weekly** | Process inbox, refile items | Open `inbox.org`, `C-c C-w` |
| **Weekly** | Review projects | `C-c a m` -> match `+PROJ` |
