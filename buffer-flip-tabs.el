;;; buffer-flip-tabs.el --- Cycle through tab-bar tabs like Alt-Tab -*- lexical-binding: t; -*-

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

;; Tab-bar tab cycling commands for the buffer-flip package.  Entry
;; points are `buffer-flip-tab-forward' and `buffer-flip-tab-backward'.

;;; Code:

(require 'cl-lib)
(require 'buffer-flip)

(defvar buffer-flip-tab--tabs nil
  "Cached LRU-sorted tab list during cycling.
The `car' is the original tab at session start.
Each entry is a snapshot alist captured at session start.")

(defun buffer-flip-tab--key (tab)
  "Return a stable identity key for TAB.
Uses `w-workspace' if present (stable across name mutations),
otherwise falls back to a tagged cons of the tab display name."
  (or (alist-get 'w-workspace (cdr tab))
      (cons 'name (alist-get 'name (cdr tab)))))

(defvar buffer-flip-tab--exit-function nil
  "Called by `buffer-flip-tab-abort' to exit the transient map.")

(defun buffer-flip-tab--sorted-tabs ()
  "Return tabs sorted by descending `time', current tab first."
  (require 'tab-bar)
  (let* ((tabs (funcall tab-bar-tabs-function))
         (current (cl-find-if (lambda (tab) (eq 'current-tab (car tab))) tabs))
         (others (cl-remove current tabs)))
    (cons current
          (sort others (lambda (a b)
                         (> (or (alist-get 'time a) 0)
                            (or (alist-get 'time b) 0)))))))

(defun buffer-flip-tab--live-current ()
  "Return the live current-tab alist from the tab bar."
  (cl-find-if (lambda (tab) (eq 'current-tab (car tab)))
              (funcall tab-bar-tabs-function)))

(defun buffer-flip-tab--find-in-cache (live-tab)
  "Find the cached entry matching LIVE-TAB by stable key."
  (let ((key (buffer-flip-tab--key live-tab)))
    (cl-find-if (lambda (cached) (equal key (buffer-flip-tab--key cached)))
                buffer-flip-tab--tabs)))

(defun buffer-flip-tab-show ()
  "Display the tab list in the echo area.
Current tab is shown in [brackets] and highlighted."
  (require 'tab-bar)
  (let* ((live-current (buffer-flip-tab--live-current))
         (cached-current (buffer-flip-tab--find-in-cache live-current))
         (current-name (and cached-current (alist-get 'name cached-current)))
         (names (mapcar (lambda (tab) (alist-get 'name tab))
                        buffer-flip-tab--tabs))
         (message-log-max nil))
    (message "%s" (buffer-flip--format-items names current-name))))

(defun buffer-flip-tab--fixup-times (&optional aborting)
  "Assign synthetic decreasing times to reorder tab history.
When ABORTING is non-nil, restore the original order from the
cached list (sans current tab).  Otherwise promote the original
tab (the `car' of the cache) to most recent."
  (require 'tab-bar)
  (let* ((now (float-time))
         (original (car buffer-flip-tab--tabs))
         (desired-order (if aborting
                            (cdr buffer-flip-tab--tabs)
                          (cons original
                                (cl-remove-if
                                 (lambda (tab)
                                   (equal (buffer-flip-tab--key tab)
                                          (buffer-flip-tab--key original)))
                                 (cdr buffer-flip-tab--tabs)))))
         (live-tabs (funcall tab-bar-tabs-function)))
    (cl-loop for i from 0
             for cached-tab in desired-order
             for key = (buffer-flip-tab--key cached-tab)
             for live-tab = (cl-find-if (lambda (tab) (equal (buffer-flip-tab--key tab) key))
                                        live-tabs)
             when (and live-tab (not (eq 'current-tab (car live-tab))))
             do (setf (alist-get 'time live-tab) (- now (* (1+ i) 0.001))))))

(defun buffer-flip-tab--on-exit ()
  "Transient map exit callback.  Fixup times if cache is live."
  (when buffer-flip-tab--tabs
    (buffer-flip-tab--fixup-times)
    (setq buffer-flip-tab--tabs nil)))

(defun buffer-flip-tab--start-session ()
  "Set up a tab cycling session.
Validates the transient map, snapshots tabs, and activates
the transient map."
  (require 'tab-bar)
  (buffer-flip-check-map-configuration
   buffer-flip-tab-map
   'buffer-flip-tab-forward 'buffer-flip-tab-backward 'buffer-flip-tab-abort)
  (setq buffer-flip-tab--tabs (buffer-flip-tab--sorted-tabs))
  (setq buffer-flip-tab--exit-function
        (set-transient-map buffer-flip-tab-map t #'buffer-flip-tab--on-exit)))

(defun buffer-flip-tab--in-session-p ()
  "Return non-nil if a tab cycling session is active."
  (memq last-command '(buffer-flip-tab-forward buffer-flip-tab-backward)))

(defun buffer-flip-tab-cycle (&optional direction)
  "Cycle tabs in DIRECTION (`forward' or `backward')."
  (require 'tab-bar)
  (let* ((tabs buffer-flip-tab--tabs)
         (len (length tabs))
         (live-current (buffer-flip-tab--live-current))
         (current-key (buffer-flip-tab--key live-current))
         (idx (or (cl-position current-key tabs
                              :test (lambda (key tab) (equal key (buffer-flip-tab--key tab))))
                  0))
         (next-idx (mod (+ idx (if (eq direction 'backward) -1 1)) len))
         (next-tab (nth next-idx tabs))
         (live-name (buffer-flip-tab--live-name-for-key
                     (buffer-flip-tab--key next-tab))))
    (when live-name
      (tab-bar-switch-to-tab live-name)))
  (buffer-flip-tab-show))

;;;###autoload
(defun buffer-flip-tab-forward ()
  "Cycle to the next tab.
Starts a new session if not already cycling."
  (interactive)
  (unless (buffer-flip-tab--in-session-p)
    (buffer-flip-tab--start-session))
  (buffer-flip-tab-cycle 'forward))

;;;###autoload
(defun buffer-flip-tab-backward ()
  "Cycle to the previous tab.
Starts a new session if not already cycling."
  (interactive)
  (unless (buffer-flip-tab--in-session-p)
    (buffer-flip-tab--start-session))
  (buffer-flip-tab-cycle 'backward))

(defun buffer-flip-tab-confirm ()
  "Confirm the current tab selection and exit cycling."
  (interactive)
  (funcall buffer-flip-tab--exit-function))

(defun buffer-flip-tab--live-name-for-key (key)
  "Find the current display name of the live tab matching KEY."
  (let ((live-tab (cl-find-if (lambda (tab) (equal key (buffer-flip-tab--key tab)))
                               (funcall tab-bar-tabs-function))))
    (when live-tab (alist-get 'name live-tab))))

(defun buffer-flip-tab-abort ()
  "Abort tab cycling and return to the original tab."
  (interactive)
  (require 'tab-bar)
  (let* ((original (car buffer-flip-tab--tabs))
         (key (buffer-flip-tab--key original))
         (live-name (buffer-flip-tab--live-name-for-key key)))
    (buffer-flip-tab--fixup-times 'aborting)
    (setq buffer-flip-tab--tabs nil)
    (when live-name
      (tab-bar-switch-to-tab live-name)))
  (funcall buffer-flip-tab--exit-function))

(provide 'buffer-flip-tabs)
;;; buffer-flip-tabs.el ends here
