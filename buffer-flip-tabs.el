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
;; point is `buffer-flip-tab'.

;;; Code:

(require 'cl-lib)
(require 'buffer-flip)

(defvar buffer-flip-tab-map '(keymap)
  "The transient map which is active during tab cycling.
This must be explicitly configured by the user with keys mapped
to the three tab flipping commands, as shown in the following
example.

;; key to begin cycling tabs.  Global key.
\(global-set-key (kbd \"M-<tab>\") \\='buffer-flip-tab)

;; transient keymap used once cycling starts
\(setq buffer-flip-tab-map
      (let ((map (make-sparse-keymap)))
        (define-key map (kbd \"M-<tab>\")   \\='buffer-flip-tab-forward)
        (define-key map (kbd \"M-S-<tab>\") \\='buffer-flip-tab-backward)
        (define-key map (kbd \"M-ESC\")     \\='buffer-flip-tab-abort)
        map))")

(defvar buffer-flip-tab--tabs nil
  "Cached LRU-sorted tab list during cycling.
The `car' is the original tab at session start.")

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

(defun buffer-flip-tab-show ()
  "Display the tab list in the echo area.
Current tab is shown in [brackets] and highlighted."
  (require 'tab-bar)
  (let* ((current-tab (cl-find-if (lambda (tab) (eq 'current-tab (car tab)))
                                  (funcall tab-bar-tabs-function)))
         (current-name (alist-get 'name current-tab))
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
         (desired-order (if aborting
                            (cdr buffer-flip-tab--tabs)
                          (let ((original (car buffer-flip-tab--tabs)))
                            (cons original
                                  (cl-remove-if
                                   (lambda (tab)
                                     (equal (alist-get 'name tab)
                                            (alist-get 'name original)))
                                   (cdr buffer-flip-tab--tabs)))))))
    (cl-loop for i from 0
             for cached-tab in desired-order
             for name = (alist-get 'name cached-tab)
             for live-tab = (cl-find-if (lambda (tab) (equal (alist-get 'name tab) name))
                                        (funcall tab-bar-tabs-function))
             when (and live-tab (not (eq 'current-tab (car live-tab))))
             do (setf (alist-get 'time live-tab) (- now (* (1+ i) 0.001))))))

(defun buffer-flip-tab--on-exit ()
  "Transient map exit callback.  Fixup times if cache is live."
  (when buffer-flip-tab--tabs
    (buffer-flip-tab--fixup-times)
    (setq buffer-flip-tab--tabs nil)))

(defun buffer-flip-tab-cycle (&optional direction)
  "Cycle tabs in DIRECTION (`forward' or `backward')."
  (require 'tab-bar)
  (unless buffer-flip-tab--tabs
    (user-error "No active tab cycling session"))
  (let* ((tabs buffer-flip-tab--tabs)
         (len (length tabs))
         (current-tab (cl-find-if (lambda (tab) (eq 'current-tab (car tab)))
                                  (funcall tab-bar-tabs-function)))
         (current-name (alist-get 'name current-tab))
         (idx (cl-position current-name tabs
                           :test (lambda (name tab) (equal name (alist-get 'name tab)))))
         (next-idx (mod (+ idx (if (eq direction 'backward) -1 1)) len))
         (next-tab (nth next-idx tabs)))
    (tab-bar-switch-to-tab (alist-get 'name next-tab)))
  (buffer-flip-tab-show))

;;;###autoload
(defun buffer-flip-tab ()
  "Begin cycling through tab-bar tabs in LRU order."
  (interactive)
  (require 'tab-bar)
  (buffer-flip-check-map-configuration
   buffer-flip-tab-map
   'buffer-flip-tab-forward 'buffer-flip-tab-backward 'buffer-flip-tab-abort)
  (setq buffer-flip-tab--tabs (buffer-flip-tab--sorted-tabs))
  (buffer-flip-tab-cycle 'forward)
  (setq buffer-flip-tab--exit-function
        (set-transient-map buffer-flip-tab-map t #'buffer-flip-tab--on-exit)))

(defun buffer-flip-tab-forward ()
  "Switch to next tab during cycling."
  (interactive)
  (buffer-flip-tab-cycle 'forward))

(defun buffer-flip-tab-backward ()
  "Switch to previous tab during cycling."
  (interactive)
  (buffer-flip-tab-cycle 'backward))

(defun buffer-flip-tab-abort ()
  "Abort tab cycling and return to the original tab."
  (interactive)
  (require 'tab-bar)
  (let ((original (car buffer-flip-tab--tabs)))
    (buffer-flip-tab--fixup-times 'aborting)
    (setq buffer-flip-tab--tabs nil)
    (tab-bar-switch-to-tab (alist-get 'name original)))
  (funcall buffer-flip-tab--exit-function))

(provide 'buffer-flip-tabs)
;;; buffer-flip-tabs.el ends here
