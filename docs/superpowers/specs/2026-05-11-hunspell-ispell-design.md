# Hunspell ispell backend for eemacs

**Date:** 2026-05-11
**Scope:** `modules/ee-spelling.el`

## Goal

Add hunspell as the backend for `ispell-mode`, providing deliberate multi-language spell sessions (English, German, Persian) alongside the existing jinx on-the-fly highlighting. The two coexist: jinx is unchanged; hunspell serves `M-x ispell-buffer`, `M-x ispell-region`, and similar ispell-mode commands.

## Architecture

`ispell-program-name` is set to `"hunspell"`. Four named dictionaries are registered in `ispell-local-dictionary-alist`:

| Name      | Hunspell flag              | Purpose                        |
|-----------|----------------------------|--------------------------------|
| `en_US`   | `-d en_US`                 | Default at startup             |
| `de_DE`   | `-d de_DE`                 | German                         |
| `fa_IR`   | `-d fa_IR`                 | Persian                        |
| `multi`   | `-d en_US,de_DE,fa_IR`     | All three simultaneously       |

`ispell-dictionary` is set to `"en_US"` globally. A single interactive command `ee/ispell-switch-language` uses `completing-read` over the four names and calls `ispell-change-dictionary`, switching the active dictionary for the current buffer.

The existing `[remap ispell-word] → jinx-correct` binding is left untouched: the per-word correction key still goes through jinx. `C-c S` invokes the language switcher. Switching is global by default (affects all buffers); with a prefix argument (`C-u C-c S`) it switches only the current buffer, which is useful when working in multiple languages across open buffers.

## NixOS handling

The hunspell setup is gated on `(executable-find "hunspell")` so it is silently skipped if hunspell is absent. No auto-install is attempted.

**Required NixOS packages** (to document in a comment in `ee-spelling.el`):

```nix
environment.systemPackages = [
  pkgs.hunspell
  pkgs.hunspellDicts.en_US
  pkgs.hunspellDicts.de_DE
  pkgs.hunspellDicts.fa_IR
];
```

On non-NixOS systems (apt, brew, pacman, etc.), the user installs hunspell and the relevant dictionary packages manually; the Emacs config picks it up automatically via `executable-find`.

## Keybindings

| Key     | Command                    | Notes                              |
|---------|----------------------------|------------------------------------|
| `C-c S`       | `ee/ispell-switch-language` | Prompts for dict, switches globally      |
| `C-u C-c S`   | `ee/ispell-switch-language` | Prompts for dict, switches current buffer only |

## What does not change

- `global-jinx-mode` and all jinx configuration remain untouched.
- The `[remap ispell-word] → jinx-correct` binding remains.
- The `C-M-$` → `jinx-languages` binding remains.
- The `ee-nixos-p` guard for jinx installation remains.

## Out of scope

- Per-project default language via `dir-locals`.
- Auto-detecting the correct dictionary from buffer language.
- Installing hunspell dictionaries from within Emacs.
