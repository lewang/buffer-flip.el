# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

buffer-flip cycles through buffers and tab-bar tabs like Alt-Tab in Windows. It uses Emacs's built-in buffer stack
(no separate data structure) and transient keymaps for cycling sessions. Available on MELPA.

## Development

Pure Emacs Lisp package. Dependency: `cl-lib` (built-in). To test changes, eval files in order in Emacs and exercise
the interactive commands.

### Eval order

1. `buffer-flip-common.el`
2. `buffer-flip-buffers.el` and/or `buffer-flip-tabs.el`
3. `buffer-flip-test.el` (for tests)

### Running tests

```
M-x ert RET ^buffer-flip-test RET
```

Or batch: `(ert-run-tests-batch "^buffer-flip-test")`

## Architecture

### File structure

- **`buffer-flip.el`** — backward-compat wrapper; `(require 'buffer-flip)` loads `buffer-flip-buffers` (which loads
  `buffer-flip-common`). Existing user configs work unchanged.
- **`buffer-flip-common.el`** (`buffer-flip-common`) — shared display engine: faces, `buffer-flip--format-item`,
  `buffer-flip--format-items` (centering, fence, truncation), `buffer-flip-check-map-configuration`. No commands.
- **`buffer-flip-buffers.el`** (`buffer-flip-buffers`) — buffer cycling. Autoloaded entry points: `buffer-flip`,
  `buffer-flip-other-window`.
- **`buffer-flip-tabs.el`** (`buffer-flip-tabs`) — tab-bar tab cycling. Autoloaded entry point: `buffer-flip-tab`.
  Lazy-requires `tab-bar` at runtime.
- **`buffer-flip-test.el`** — ERT tests for common engine and tab time fixup.

### Cycling mechanism (buffers)

1. `buffer-flip` — validates keymap, records window configuration, calls `buffer-flip-cycle`, activates
   `buffer-flip-map` as transient map via `set-transient-map`.
2. `buffer-flip-cycle` — walks frame-local `(buffer-list)` forward/backward with modular arithmetic, skipping
   buffers per `buffer-flip-skip-buffer`.
3. Transient map exits when a non-mapped key is pressed; exit callback finalizes buffer choice.
4. `buffer-flip-abort` — restores saved `window-configuration`.

### Cycling mechanism (tabs)

1. `buffer-flip-tab` — snapshots tabs sorted by LRU (`time` property, current tab first) into
   `buffer-flip-tab--tabs`, cycles forward, activates `buffer-flip-tab-map`.
2. `buffer-flip-tab-cycle` — finds current tab in cached list by name, advances by index.
3. On confirm (transient map exit): `buffer-flip-tab--on-exit` calls `buffer-flip-tab--fixup-times` to assign
   synthetic decreasing times — original tab gets highest time (promoted to MRU).
4. On abort: fixup restores original order (`cdr` of cache), then switches back to original tab.
5. Cache is nil'd before calling exit function to prevent double-fixup.

### Display engine (`buffer-flip-common`)

`buffer-flip--format-items` takes a list of name strings and the current name. It rotates the list to center the
current item, inserts a `┃┃` fence at the wrap boundary, applies faces, and truncates to fit the minibuffer window
width. Both buffer and tab cycling call this with extracted name lists.

`buffer-flip-check-map-configuration` is generic — takes a map and variadic command symbols, signals `user-error` for
any unbound command.
