buffer-flip
===========

This package streamlines the operation of switching between recent buffers and tab-bar tabs, with an emphasis on
minimizing keystrokes.  Inspired by the Alt-Tab convention in Windows, it keeps the most recently used items on the
top of the stack.

Installation
------------

buffer-flip is available on Melpa.

[![MELPA](https://melpa.org/packages/buffer-flip-badge.svg)](https://melpa.org/#/buffer-flip)

### Installing with package-install

    M-x package-install RET buffer-flip RET

Then add the following to your config, adapting to your preferences.

### Buffer cycling

```lisp
;; keys to begin cycling buffers.  Global keys.
(global-set-key (kbd "M-<tab>")   'buffer-flip-forward)
(global-set-key (kbd "M-S-<tab>") 'buffer-flip-backward)

;; transient keymap used once cycling starts
(setq buffer-flip-map
      (let ((map (make-sparse-keymap)))
        (define-key map (kbd "M-<tab>")   'buffer-flip-forward)
        (define-key map (kbd "M-S-<tab>") 'buffer-flip-backward)
        (define-key map (kbd "M-ESC")     'buffer-flip-abort)
        map))

;; buffers matching these patterns will be skipped
(setq buffer-flip-skip-patterns
      '("^\\*helm\\b"
        "^\\*swiper\\*$"))
```

### Tab-bar tab cycling

Requires Emacs 27+ with `tab-bar-mode`.  Only loaded when used — no overhead if you don't configure it.

```lisp
(global-set-key (kbd "M-S-<iso-lefttab>") 'buffer-flip-tab-forward)
(global-set-key (kbd "M-<iso-lefttab>")   'buffer-flip-tab-backward)

(setq buffer-flip-tab-map
      (let ((map (make-sparse-keymap)))
        (define-key map (kbd "M-S-<iso-lefttab>") 'buffer-flip-tab-forward)
        (define-key map (kbd "M-S-<tab>")         'buffer-flip-tab-backward)
        (define-key map (kbd "M-ESC")             'buffer-flip-tab-abort)
        map))
```

### use-package

```lisp
(use-package buffer-flip
  :ensure t
  :bind  (("M-<tab>"   . buffer-flip-forward)
          ("M-S-<tab>" . buffer-flip-backward)
          :map buffer-flip-map
          ( "M-<tab>" .   buffer-flip-forward)
          ( "M-S-<tab>" . buffer-flip-backward)
          ( "M-ESC" .     buffer-flip-abort))
  :config
  (setq buffer-flip-skip-patterns
        '("^\\*helm\\b"
          "^\\*swiper\\*$")))

;; Tab cycling (separate use-package block since it has its own autoload)
(use-package buffer-flip-tabs
  :ensure nil  ; comes with buffer-flip
  :bind (("M-S-<iso-lefttab>" . buffer-flip-tab-forward)
         ("M-<iso-lefttab>"   . buffer-flip-tab-backward)
         :map buffer-flip-tab-map
         ("M-S-<iso-lefttab>" . buffer-flip-tab-forward)
         ("M-S-<tab>"         . buffer-flip-tab-backward)
         ("M-ESC"             . buffer-flip-tab-abort)))
```

Usage example
-------------

The following assumes the above key bindings. 

To begin cycling through the buffers, press <kbd>Alt-Tab</kbd> to go
forward or <kbd>Alt-Shift-Tab</kbd> to go backward.  Either key starts
a cycling session and activates the transient buffer-flip-map.
Pressing <kbd>Alt-Tab</kbd> continues to cycle forward through the
buffer stack (more recent buffers first), and
<kbd>Alt-Shift-Tab</kbd> cycles in the opposite direction.  Just begin
working in the currently-displayed buffer to stop cycling.  Doing so
places that buffer on top of the stack.  <kbd>Alt-Esc</kbd> cancels
cycling and switches to the buffer you started in.

| Pressing                                                                    | Does this                                                                         |
|-----------------------------------------------------------------------------|-----------------------------------------------------------------------------------|
| <kbd>Alt-Tab</kbd>                                                          | Flips to second most recent buffer                                                |
| <kbd>Alt-Tab</kbd> <kbd>Alt-Tab</kbd>                                       | Flip to third most recent buffer                                                  |
| <kbd>Alt-Tab</kbd> <kbd>Alt-Tab</kbd> <kbd>Alt-Tab</kbd> <kbd>Alt-Esc</kbd> | Start flipping through buffers and then cancel, returning to the original buffer. |
| <kbd>Alt-Tab</kbd> <kbd>Alt-Tab</kbd> <kbd>Alt-Shift-Tab</kbd>              | Flips forward through the two most recent buffers, then flips backward one buffer.|

The buffer list is treated as a circle during cycling.  Filtered
buffers (internal, already-visible, or matching
`buffer-flip-skip-patterns`) are skipped but keep their positions.
The list is not reshuffled until cycling ends — only then is the
chosen buffer promoted to the front.

| Step                                   | Buffer list         | Notes                                 |
|----------------------------------------|---------------------|---------------------------------------|
| Start                                  | [**A**, 1, 2, B, C] | `A` is current; `1`, `2` are filtered |
| <kbd>Alt-Tab</kbd>                     | [A, 1, 2, **B**, C] | `1`, `2` skipped; list unchanged      |
| <kbd>Alt-Tab</kbd>                     | [A, 1, 2, B, **C**] | List still unchanged (NORECORD)       |
| <kbd>Alt-Shift-Tab</kbd>               | [A, 1, 2, **B**, C] | Back to `B`                           |
| <kbd>Alt-Shift-Tab</kbd>               | [**A**, 1, 2, B, C] | Back to `A`                           |
| <kbd>Alt-Shift-Tab</kbd>               | [A, 1, 2, B, **C**] | Wraps to end                          |
| Accept `C`                             | [**C**, A, 1, 2, B] | `C` promoted to front                 |


Another good key binding
------------------------

Personally I use the following key bindings which rely on key-chord.
I like it because I can reach the keys easily in home position.

```lisp
(use-package buffer-flip
  :ensure t
  :chords (("u8" . buffer-flip-forward))
  :bind  (:map buffer-flip-map
               ( "8" .   buffer-flip-forward)
               ( "*" .   buffer-flip-backward)
               ( "C-g" . buffer-flip-abort)))
```

With these bindings,

| Pressing                                               | Does this                                                                         |
|--------------------------------------------------------|-----------------------------------------------------------------------------------|
| <kbd>u8</kbd>                                          | Alternates between the two most recent buffers                                    |
| <kbd>u8</kbd> <kbd>8</kbd>                             | Flip to third most recent buffer                                                  |
| <kbd>u8</kbd> <kbd>8</kbd> <kbd>8</kbd> <kbd>C-g</kbd> | Start flipping through buffers and then cancel, returning to the original buffer. |
| <kbd>u8</kbd> <kbd>8</kbd> <kbd>*</kbd>                | Flips forward through the two most recent buffers, then flips backward one buffer.|

This is incidentally the default key binding for previous versions of
this package.  The current version has no default key binding.

With these chord bindings, pressing <kbd>u8</kbd> repeatedly
alternates between the two most recent buffers because the chord
restarts the cycling session each time.  In contrast, pressing
<kbd>Alt-Tab</kbd> repeatedly cycles through deeper and deeper buffers
on the stack because the transient map keeps the session alive.

Tab cycling
-----------

`buffer-flip-tab-forward` and `buffer-flip-tab-backward` work the same way but cycle through `tab-bar-mode` tabs in
LRU (most-recently-used) order.  Tabs are sorted by their `time` property — the tab you used most recently appears
first.  Either command starts a session from cold.

When cycling ends normally (you start working in the chosen tab), the tab you started from is promoted to the top of
the LRU stack so that a quick flip returns you there.  Aborting with `buffer-flip-tab-abort` restores the original tab
order exactly.

Tab cycling is loaded lazily — `buffer-flip-tabs.el` and `tab-bar` are only required when a tab cycling command is
first invoked.

The (Non) UI
-------------

Or, "Why don't you have a screenshot?"  This package streamlines the
operation of switching to recent buffers, a common operation in my
workflow.  Many buffer management systems display a list of buffer
names for you to select from.  Extra ui elements like that often come
at the cost of additional keystrokes.  This package doesn't have any
ui elements, it simply changes the current buffer as you cycle.  Once
you are looking at the buffer you want, just start editing and the
cycling automatically exits.  Pressing the key bound to
`buffer-flip-abort` during cycling will take you back to where you
started.

This package is not efficient for switching to a deeply-buried buffer.
There are
[other](http://tuhdo.github.io/helm-intro.html#ID-0386c827-7f5d-4056-bf4d-8d0fc01fc1ab)
[tools](http://www.gnu.org/software/emacs/manual/html_mono/ido.html)
for that.

Motivation
-----------

The [Alt-Tab](https://en.wikipedia.org/wiki/Alt-Tab) convention for
switching windows was one thing Microsoft got right.  Because it keeps
the most recently-used things on the top of the stack, it is often
very fast to switch to the thing you want.  There are [similar Emacs
packages](http://www.emacswiki.org/emacs/ControlTABbufferCycling) out
there, but many are too heavyweight for my needs, or are not
stack-based.

A note on the buffer stack
--------------------------

There is no additional buffer stack maintained by this package.  Emacs
already keeps its buffers in a stack, and this package leverages that
fact.  You can see the Emacs buffer stack by running `M-x
list-buffers` or by evaluating `(buffer-list)`.

File structure
--------------

| File                   | Provides              | Description                                              |
|------------------------|-----------------------|----------------------------------------------------------|
| `buffer-flip.el`       | `buffer-flip`         | Shared display engine (faces, formatting, map validation) |
| `buffer-flip-buffers.el`| `buffer-flip-buffers`| Buffer cycling commands                                  |
| `buffer-flip-tabs.el`  | `buffer-flip-tabs`    | Tab-bar tab cycling commands                             |
| `buffer-flip-test.el`  | `buffer-flip-test`    | ERT test suite                                           |