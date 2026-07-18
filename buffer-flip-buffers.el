;;; buffer-flip-buffers.el --- Cycle through buffers like Alt-Tab -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Le Wang

;; Author: Le Wang <lewang.dev.26@gmail.com>
;; Keywords: convenience
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Buffer cycling commands for the buffer-flip package.  Entry points
;; are `buffer-flip-forward' and `buffer-flip-backward'.
;;
;; With a `C-u' prefix, cycling operates in another window while focus
;; stays in the original window.  If no suitable window exists, the
;; current window is split.

;;; Code:

(require 'cl-lib)
(require 'buffer-flip)

(cl-defstruct (buffer-flip--session (:constructor buffer-flip--session-make)
                                    (:copier nil))
  "State for one buffer cycling session; the whole struct is nil when idle."
  ;; Window to cycle in; nil means the originally selected window.
  target-window
  ;; Deactivation function returned by `set-transient-map'; ends the session
  ;; (via `buffer-flip-confirm'/`buffer-flip-abort' or an out-of-map key).
  exit-function
  ;; Window configuration saved at start, restored by `buffer-flip-abort'.
  window-configuration
  ;; Copy of `buffer-flip-skip-patterns' captured at session start, so a
  ;; caller's `let'-binding keeps filtering after the transient map outlives
  ;; the frame the `let' wrapped.
  skip-patterns)

(defvar buffer-flip--session nil
  "The active `buffer-flip--session', or nil when not cycling.")

(defcustom buffer-flip-skip-patterns nil
  "Filters for buffers to skip while flipping.
Each element is either a regexp matched against the buffer name, or a
function of one argument (the buffer) returning non-nil to skip it.  A
buffer is skipped when any element matches.

This is an ordinary variable, so a command can `let'-bind it -- e.g.
consing an extra skip predicate onto the front -- to narrow a single
cycling session.  The value in effect when the session starts is captured
and used for the whole session."
  :type '(repeat (choice (regexp   :tag "Buffer-name regexp")
                         (function :tag "Skip predicate (buffer)")))
  :group 'buffer-flip)

(defun buffer-flip-skip-buffer (buf)
  "Return non-nil if BUF should be skipped.
During a session the patterns captured at its start are consulted (see
`buffer-flip--session'); otherwise the live `buffer-flip-skip-patterns'.
Each pattern is a buffer-name regexp or a predicate function of the buffer."
  (or (get-buffer-window buf)              ; already visible?
      (= ?\s (elt (buffer-name buf) 0))    ; internal?
      (let ((name (buffer-name buf))
            (patterns (if buffer-flip--session
                          (buffer-flip--session-skip-patterns buffer-flip--session)
                        buffer-flip-skip-patterns)))
        (cl-some (lambda (pat)
                   (if (functionp pat)
                       (funcall pat buf)
                     (string-match-p pat name)))
                 patterns))))

(defun buffer-flip-show-buffers ()
  "Display the eligible buffer list in the echo area.
Current buffer is shown in [brackets] and highlighted."
  (let* ((cur (current-buffer))
         (bufs (cl-remove-if (lambda (buf)
                               (and (not (eq buf cur))
                                    (buffer-flip-skip-buffer buf)))
                             (buffer-list (selected-frame))))
         (names (mapcar #'buffer-name bufs))
         (message-log-max nil))
    (message "%s" (buffer-flip--format-items names (buffer-name cur)))))

(defun buffer-flip--start-session (&optional other-window-p)
  "Set up a buffer cycling session in `buffer-flip--session'.
Validates the transient map, normalises the buffer stack, saves the window
configuration, captures `buffer-flip-skip-patterns', and activates the
transient map.

When OTHER-WINDOW-P is non-nil, cycling operates on another window while
focus stays in the original.  If no suitable window exists, the current
window is split horizontally.

The skip patterns are captured now rather than read live, so a caller that
`let'-binds `buffer-flip-skip-patterns' around the entrance command keeps
its filter for the whole session even though the transient map outlives
that `let'."
  (buffer-flip-check-map-configuration
   buffer-flip-map
   'buffer-flip-forward 'buffer-flip-backward 'buffer-flip-abort)
  (let ((target-window nil))
    (when other-window-p
      (walk-windows (lambda (w)
                      (unless (or target-window
                                  (eq w (selected-window))
                                  (window-dedicated-p w))
                        (setq target-window w))))
      (unless target-window
        (split-window-horizontally)
        (setq target-window (next-window))))
    (switch-to-buffer (current-buffer))
    (setq buffer-flip--session
          (buffer-flip--session-make
           :target-window target-window
           :window-configuration (current-window-configuration)
           :skip-patterns buffer-flip-skip-patterns))
    ;; The commit closure re-reads the target window from the session at exit
    ;; time; `buffer-flip-abort' clears that slot first to suppress the
    ;; commit.  `set-transient-map's on-exit runs on every deactivation path,
    ;; so it is the single teardown point that clears the session.
    (let* ((commit
            (if target-window
                (lambda ()
                  (when-let* ((win (and buffer-flip--session
                                        (buffer-flip--session-target-window
                                         buffer-flip--session))))
                    (with-selected-window win
                      (switch-to-buffer (current-buffer)))))
              (lambda () (switch-to-buffer (current-buffer)))))
           (exit-cb (lambda ()
                      (funcall commit)
                      (setq buffer-flip--session nil))))
      (setf (buffer-flip--session-exit-function buffer-flip--session)
            (set-transient-map buffer-flip-map t exit-cb)))))

(defun buffer-flip--in-session-p ()
  "Return non-nil if a buffer cycling session is active.
Reads the session flag rather than `last-command', so a session started by
any command (whatever its name) is carried across flip-key presses."
  (and buffer-flip--session t))

;;;###autoload
(defun buffer-flip-forward (&optional other-window)
  "Cycle to the next buffer.
Starts a new session if not already cycling.  With prefix
argument OTHER-WINDOW, cycle in another window while keeping
focus in the current one."
  (interactive "P")
  (unless (buffer-flip--in-session-p)
    (buffer-flip--start-session other-window))
  (buffer-flip-cycle 'forward))

;;;###autoload
(defun buffer-flip-backward (&optional other-window)
  "Cycle to the previous buffer.
Starts a new session if not already cycling.  With prefix
argument OTHER-WINDOW, cycle in another window while keeping
focus in the current one."
  (interactive "P")
  (unless (buffer-flip--in-session-p)
    (buffer-flip--start-session other-window))
  (buffer-flip-cycle 'backward))


(defun buffer-flip-cycle (&optional direction)
  "Cycle in the direction indicated by DIRECTION.
DIRECTION can be `forward' or `backward'."
  (with-selected-window (or (and buffer-flip--session
                                 (buffer-flip--session-target-window buffer-flip--session))
                            (selected-window))
    (let ((l (buffer-list (selected-frame))))
      (switch-to-buffer            ; Switch to next/prev buffer in stack
       (cl-do ((buf (current-buffer)     ; Using the current buffer as a
                    (nth (mod (+ (cl-position buf l) ; reference point to cycle
                                 (if (eq direction 'backward) -1 1)) ; fwd or back
                              (length l)) l)) ; Mod length to wrap
               (count (length l) (1- count))) ; count the number of iterations
           ((or (= 0 count) ;; don't cycle through list more than once.
                (not (buffer-flip-skip-buffer buf))) buf)) t))
    (buffer-flip-show-buffers)))

(defun buffer-flip-confirm ()
  "Confirm the current buffer selection and exit cycling."
  (interactive)
  (funcall (buffer-flip--session-exit-function buffer-flip--session)))

(defun buffer-flip-abort ()
  "Abort buffer cycling process and return to original buffer.
This command should be bound to a key inside of
`buffer-flip-map'."
  (interactive)
  (let ((session buffer-flip--session))
    ;; Clear the target window so the exit commit does not re-switch it.
    (setf (buffer-flip--session-target-window session) nil)
    (set-window-configuration (buffer-flip--session-window-configuration session))
    (funcall (buffer-flip--session-exit-function session))))

(provide 'buffer-flip-buffers)
;;; buffer-flip-buffers.el ends here
