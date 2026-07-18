# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

buffer-flip cycles through buffers and tab-bar tabs like Alt-Tab in Windows. It uses Emacs's built-in buffer stack
(no separate data structure) and transient keymaps for cycling sessions. Available on MELPA.

## Development

Pure Emacs Lisp package. Dependency: `cl-lib` (built-in). To test changes, eval files in order in Emacs and exercise
the interactive commands.

### Eval order

1. `buffer-flip.el`
2. `buffer-flip-buffers.el` and/or `buffer-flip-tabs.el`
3. `buffer-flip-test.el` (for tests)

### Running tests

```
M-x ert RET ^buffer-flip-test RET
```

Or batch: `(ert-run-tests-batch "^buffer-flip-test")`

## Architecture

### File structure

- **`buffer-flip.el`** — shared display engine and base feature: `defgroup`, faces, `buffer-flip--format-item`,
  `buffer-flip--format-items` (centering, fence, truncation), `buffer-flip-check-map-configuration`.
  Provides `buffer-flip` feature; sub-modules require it.
- **`buffer-flip-buffers.el`** (`buffer-flip-buffers`) — buffer cycling. Requires `buffer-flip`. Autoloaded entry
  points: `buffer-flip-forward`, `buffer-flip-backward`.
- **`buffer-flip-tabs.el`** (`buffer-flip-tabs`) — tab-bar tab cycling. Requires `buffer-flip`. Autoloaded entry
  points: `buffer-flip-tab-forward`, `buffer-flip-tab-backward`. Lazy-requires `tab-bar` at runtime.
- **`buffer-flip-test.el`** — ERT tests for common engine and tab time fixup.

### Cycling mechanism (buffers)

All per-session state lives in one `cl-defstruct`, `buffer-flip--session` (slots: `target-window`, `exit-function`,
`window-configuration`, `skip-patterns`). The variable `buffer-flip--session` is nil when idle and holds the struct
while cycling.

1. `buffer-flip-forward` / `buffer-flip-backward` — dual entry points. On cold start (detected via
   `buffer-flip--in-session-p`, i.e. no `buffer-flip--session` yet), call `buffer-flip--start-session` to validate the
   keymap, normalise the buffer stack, save the window configuration, capture `buffer-flip-skip-patterns` into the
   session, and activate `buffer-flip-map` as transient map. Then cycle. The session struct (not `last-command`) is
   what carries a session across flip-key presses, so an arbitrarily-named entrance command can start one. With `C-u`,
   cycling operates in another window (the session's `target-window`) while focus stays in the original;
   `buffer-flip-cycle` uses `with-selected-window` to run in that window context.
2. `buffer-flip-cycle` — walks frame-local `(buffer-list)` forward/backward with modular arithmetic, skipping buffers
   per `buffer-flip-skip-buffer`.
3. Transient map exits when a non-mapped key is pressed; the exit callback commits the choice AND clears
   `buffer-flip--session` — the single teardown point for every deactivation path (out-of-map key, confirm, abort).
4. `buffer-flip-confirm` — explicitly confirms selection by calling the session's `exit-function`, consuming the
   keypress so nothing leaks onto the event loop. Optional — users who don't bind it keep the old behavior (unmapped
   keys exit + replay).
5. `buffer-flip-abort` — clears the session's `target-window` (so the exit commit doesn't re-switch it), restores the
   saved `window-configuration`, then calls the `exit-function`.

### Filtering (`buffer-flip-skip-patterns`)

`buffer-flip-skip-patterns` is a list whose elements are each either a buffer-name regexp or a predicate function of a
buffer (non-nil ⇒ skip); `buffer-flip-skip-buffer` also always skips already-visible and internal buffers. Because it
is an ordinary variable, a command can `let`-bind it — e.g. consing an extra skip predicate onto the front — to narrow
a single session. `buffer-flip--start-session` captures the value at start into the session struct, so the filter
survives the transient map even after the caller's `let` unwinds. There is deliberately no separate filtering knob.

### Cycling mechanism (tabs)

1. `buffer-flip-tab-forward` / `buffer-flip-tab-backward` — dual entry points. On cold start (detected via
   `last-command`), call `buffer-flip-tab--start-session` to validate keymap, snapshot tabs sorted by LRU into
   `buffer-flip-tab--tabs`, and activate `buffer-flip-tab-map`. Then cycle in the requested direction.
2. `buffer-flip-tab-cycle` — finds current tab in cached list by name, advances by index.
3. On confirm (transient map exit): `buffer-flip-tab--on-exit` calls `buffer-flip-tab--fixup-times` to assign
   synthetic decreasing times — original tab gets highest time (promoted to MRU).
4. On abort: fixup restores original order (`cdr` of cache), then switches back to original tab.
5. `buffer-flip-tab-confirm` — same pattern as buffer confirm; deactivates transient map cleanly.
6. Cache is nil'd before calling exit function to prevent double-fixup.

### Display engine (`buffer-flip.el`)

`buffer-flip--format-items` takes a list of name strings and the current name. It rotates the list to center the
current item, inserts a `┃┃` fence at the wrap boundary, applies faces, and truncates to fit the minibuffer window
width. Both buffer and tab cycling call this with extracted name lists.

`buffer-flip-check-map-configuration` is generic — takes a map and variadic command symbols, signals `user-error` for
any unbound command.
