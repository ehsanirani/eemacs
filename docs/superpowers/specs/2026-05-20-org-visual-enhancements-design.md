# Org-Mode Visual Enhancements Design

**Date:** 2026-05-20
**Status:** Approved

## Summary

Three visual enhancements to `ee-org.el` that improve org-mode's reading and writing experience while preserving the current GTD/task-management appearance by default.

1. **`mixed-pitch` always-on** — variable-pitch body text with monospace preserved for code, tables, and markup
2. **`ee-org-writing-mode`** — a single toggle minor mode that activates centered layout (olivetti) and enhanced heading hierarchy together
3. **Enhanced heading faces** — subtle size and weight scaling applied only when writing mode is active

---

## Section 1 — `mixed-pitch` (always-on)

### What

Add `mixed-pitch` via straight and hook it into `org-mode-hook`. Switches body prose to the variable-pitch face while keeping the following faces monospace:

- `org-code`
- `org-verbatim`
- `org-block`
- `org-block-begin-line`
- `org-block-end-line`
- `org-table`
- `org-formula`
- `org-date`
- `org-tag`

### Why

The current config is entirely monospace. Variable-pitch improves readability of long-form prose and notes without affecting structured elements like tables or code blocks.

### Behavior

- Active for every org buffer automatically
- No toggle — applies unconditionally via `org-mode-hook`
- Respects whatever variable-pitch font is configured in `ee-fonts.el`

---

## Section 2 — `ee-org-writing-mode` minor mode

### What

A `define-minor-mode` in `ee-org.el` named `ee-org-writing-mode`.

**When enabled:**
- Activates `olivetti-mode` with body width ~80 characters
- Applies enhanced heading face remaps (Section 3) via `face-remap-add-relative` (buffer-local)

**When disabled:**
- Deactivates `olivetti-mode`
- Removes all heading face remaps, restoring the current org-modern flat appearance exactly

### Keybinding

`C-c o w` — mnemonic: **o**rg **w**riting

### Packages

- `olivetti` via straight
- No autostart — purely opt-in per buffer, never added to any hook

### Why

Both prose writing and GTD task management are primary use cases. A single command maps cleanly to "I want to write" vs. "I want to manage tasks", avoiding the need to remember two separate toggles.

---

## Section 3 — Enhanced heading faces (writing mode only)

### What

Height and weight overlaid on `org-level-1` through `org-level-5+` via `face-remap-add-relative`. Foreground color is **not** overridden — the theme's existing `org-level-*` colors are preserved, keeping the enhancement theme-neutral.

| Face | Height | Weight |
|------|--------|--------|
| `org-level-1` | 1.25 | bold |
| `org-level-2` | 1.15 | semi-bold |
| `org-level-3` | 1.10 | semi-bold |
| `org-level-4` | 1.05 | normal |
| `org-level-5+` | 1.00 | normal |

### Why

Subdued scaling (max 1.25×) provides visual hierarchy without GitHub-CSS-style oversized headers. No overlines, underlines, or box decorations. Weight differentiation reinforces hierarchy without relying solely on size.

### Reversibility

All remaps stored in a buffer-local variable. Toggling the mode off calls `face-remap-remove-relative` on each remap cookie, restoring the original flat appearance with no side effects.

---

## Implementation Locations

All changes go in `modules/ee-org.el`:

- `mixed-pitch` `use-package` block near the top of the visual section (after `org-modern`)
- `olivetti` `use-package` block
- `ee-org-writing-mode` `define-minor-mode` with its face remap logic
- Keybinding in the existing `org-mode-map` binding section

No changes needed in `ee-ui.el`, `ee-fonts.el`, or any other module.

---

## What Does Not Change

- `org-modern` configuration remains untouched
- `org-appear`, `org-fragtog` remain untouched
- All existing TODO badge colors, tag styling, table spacing unchanged
- Agenda appearance unchanged
- Heading colors from the active theme are preserved
