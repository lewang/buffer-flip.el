;;; buffer-flip-common.el --- Shared display engine for buffer-flip -*- lexical-binding: t; -*-

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

;; Common display engine and faces shared by buffer-flip-buffers and
;; buffer-flip-tabs.  Not intended for direct use.

;;; Code:

(require 'cl-lib)

(defgroup buffer-flip nil
  "Cycle through buffers and tabs like Alt-Tab."
  :group 'convenience)

(defface buffer-flip-current-buffer-face
  '((t :inherit minibuffer-prompt))
  "Face for the current item in the buffer-flip display."
  :group 'buffer-flip)

(defface buffer-flip-other-buffer-face
  '((t :inherit default))
  "Face for non-current items in the buffer-flip display."
  :group 'buffer-flip)

(defface buffer-flip-fence-face
  '((t :inherit shadow))
  "Face for the boundary fence in the buffer-flip display."
  :group 'buffer-flip)

(defun buffer-flip--format-item (name current-p)
  "Format NAME string for display.
When CURRENT-P is non-nil, wrap in brackets and use
`buffer-flip-current-buffer-face'; otherwise use
`buffer-flip-other-buffer-face'."
  (propertize (if current-p (format "[%s]" name) name)
              'face (if current-p
                        'buffer-flip-current-buffer-face
                      'buffer-flip-other-buffer-face)))

(defun buffer-flip--format-items (names current-name)
  "Format NAMES for echo-area display with CURRENT-NAME highlighted.
Rotates the list to center the current item and inserts a fence
marker at the list boundary to show where the cycle wraps."
  (if (null names)
      ""
    (let* ((len (length names))
           (idx (cl-position current-name names :test #'equal))
           (start (mod (- idx (/ len 2)) len))
           (items (cl-loop for i below len
                           for real-idx = (mod (+ start i) len)
                           append (when (and (> i 0) (= real-idx 0))
                                    (list (propertize "┃┃" 'face 'buffer-flip-fence-face)))
                           collect (buffer-flip--format-item (nth real-idx names)
                                                             (equal (nth real-idx names) current-name))))
           (current-item-idx (if (>= idx start)
                                 (- idx start)
                               (+ idx (- len start) 1)))
           (current-start (cl-loop for i below current-item-idx
                                   sum (1+ (length (nth i items)))))
           (current-end (+ current-start (length (nth current-item-idx items))))
           (text (mapconcat #'identity items " "))
           (width (max 1 (1- (window-width (minibuffer-window)))))
           (text-len (length text))
           (start-col (max 0 (min (- (/ (+ current-start current-end) 2) (/ width 2))
                                  (max 0 (- text-len width))))))
      (truncate-string-to-width text (+ start-col width) start-col))))

(defun buffer-flip-check-map-configuration (map &rest commands)
  "Ensure MAP has bindings for each command in COMMANDS.
Signals `user-error' for any command not bound in MAP."
  (dolist (func commands)
    (unless (where-is-internal func (list (or map '(keymap))))
      (user-error "%s not bound to a key in the transient map.  See documentation" func))))

(provide 'buffer-flip-common)
;;; buffer-flip-common.el ends here
