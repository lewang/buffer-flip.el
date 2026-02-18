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
;; are `buffer-flip-forward', `buffer-flip-backward', and
;; `buffer-flip-other-window'.

;;; Code:

(require 'cl-lib)
(require 'buffer-flip)

(defvar buffer-flip-exit-function nil
  "Called by `buffer-flip-abort' to exit the transient map.")

(defvar buffer-flip-original-window-configuration nil
  "Saves the current window configuration when flipping begins.
Used by `buffer-flip-abort' to restore the original buffer.")

(defcustom buffer-flip-skip-patterns nil
  "A list of regular expressions.
Buffers with names matching these patterns will be skipped when
flipping through buffers."
  :type '(repeat string) :group 'buffer-flip)

(defun buffer-flip-skip-buffer (buf)
  "Return non-nil if BUF should be skipped."
  (or (get-buffer-window buf)         ; already visible?
      (= ? (elt (buffer-name buf) 0)) ; internal?
      (let ((name (buffer-name buf))) ; matches regex?
        (cl-find-if (lambda (rex) (string-match-p rex name))
                    buffer-flip-skip-patterns))))

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

(defun buffer-flip--start-session (&optional original-configuration)
  "Set up a buffer cycling session.
Validates the transient map, normalises the buffer stack, saves
the window configuration (or uses ORIGINAL-CONFIGURATION), and
activates the transient map."
  (buffer-flip-check-map-configuration
   buffer-flip-map
   'buffer-flip-forward 'buffer-flip-backward 'buffer-flip-abort)
  (switch-to-buffer (current-buffer))
  (setq buffer-flip-original-window-configuration
        (or original-configuration (current-window-configuration)))
  (setq buffer-flip-exit-function
        (set-transient-map buffer-flip-map t
                           (lambda () (switch-to-buffer (current-buffer))))))

(defun buffer-flip--in-session-p ()
  "Return non-nil if a buffer cycling session is active."
  (memq last-command '(buffer-flip-forward buffer-flip-backward)))

;;;###autoload
(defun buffer-flip-forward ()
  "Cycle to the next buffer.
Starts a new session if not already cycling."
  (interactive)
  (unless (buffer-flip--in-session-p)
    (buffer-flip--start-session))
  (buffer-flip-cycle 'forward))

;;;###autoload
(defun buffer-flip-backward ()
  "Cycle to the previous buffer.
Starts a new session if not already cycling."
  (interactive)
  (unless (buffer-flip--in-session-p)
    (buffer-flip--start-session))
  (buffer-flip-cycle 'backward))

;;;###autoload
(defun buffer-flip-other-window ()
  "Switch to another window and begin cycling through buffers in that window.
Skips dedicated windows.  If no suitable window exists, one is
created first."
  (interactive)
  (let ((original-window-configuration (current-window-configuration))
        (start-window (selected-window))
        (target nil))
    (walk-windows (lambda (w)
                    (when (and (not target)
                               (not (eq w start-window))
                               (not (window-dedicated-p w)))
                      (setq target w))))
    (if target
        (select-window target)
      (split-window-horizontally)
      (other-window 1))
    (buffer-flip--start-session original-window-configuration)
    (buffer-flip-cycle 'forward)))

(defun buffer-flip-cycle (&optional direction)
  "Cycle in the direction indicated by DIRECTION.
DIRECTION can be `forward' or `backward'."
  (let ((l (buffer-list (selected-frame))))
    (switch-to-buffer            ; Switch to next/prev buffer in stack
     (cl-do ((buf (current-buffer)     ; Using the current buffer as a
                  (nth (mod (+ (cl-position buf l) ; reference point to cycle
                               (if (eq direction 'backward) -1 1)) ; fwd or back
                            (length l)) l)) ; Mod length to wrap
             (count (length l) (1- count))) ; count the number of iterations
         ((or (= 0 count) ;; don't cycle through list more than once.
              (not (buffer-flip-skip-buffer buf))) buf)) t))
  (buffer-flip-show-buffers))

(defun buffer-flip-abort ()
  "Abort buffer cycling process and return to original buffer.
This command should be bound to a key inside of
`buffer-flip-map'."
  (interactive)
  (set-window-configuration buffer-flip-original-window-configuration)
  (funcall buffer-flip-exit-function))

(provide 'buffer-flip-buffers)
;;; buffer-flip-buffers.el ends here
