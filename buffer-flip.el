;;; buffer-flip.el --- Cycle through buffers like Alt-Tab in Windows -*- lexical-binding: t; -*-

;; Author: Russell Black (killdash9@github)
;; Keywords: convenience
;; URL: https://github.com/killdash9/buffer-flip.el
;; Created: 10th November 2015
;; Version: 3.0
;; Package-Requires: ((cl-lib "0.5"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; Inspired by Alt-Tab.  Quickly flip through recently-used buffers.
;;
;; Shared display engine (faces, formatting, map validation) used by
;; buffer-flip-buffers and buffer-flip-tabs.

;;; Code:

(require 'cl-lib)

(defgroup buffer-flip nil
  "Cycle through buffers and tabs like Alt-Tab."
  :group 'convenience)

(defface buffer-flip-current-item-face
  '((t :inherit minibuffer-prompt))
  "Face for the currently selected item in the buffer-flip display."
  :group 'buffer-flip)

(defface buffer-flip-item-face
  '((t :inherit default))
  "Face for items in the buffer-flip display."
  :group 'buffer-flip)

(defface buffer-flip-fence-face
  '((t :inherit shadow))
  "Face for the boundary fence in the buffer-flip display."
  :group 'buffer-flip)

(defun buffer-flip--format-item (name current-p)
  "Format NAME string for display.
When CURRENT-P is non-nil, wrap in brackets and use
`buffer-flip-current-item-face'; otherwise use
`buffer-flip-item-face'."
  (propertize (if current-p (format "[%s]" name) name)
              'face (if current-p
                        'buffer-flip-current-item-face
                      'buffer-flip-item-face)))

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

(defvar buffer-flip-map '(keymap)
  "The transient map which is active during buffer cycling.
This must be explicitly configured by the user with keys mapped
to the three buffer flipping commands, as shown in the following
example.

;; keys to begin cycling buffers.  Global keys.
\(global-set-key (kbd \"M-<tab>\")   \\='buffer-flip-forward)
\(global-set-key (kbd \"M-S-<tab>\") \\='buffer-flip-backward)

;; transient keymap used once cycling starts
\(setq buffer-flip-map
      (let ((map (make-sparse-keymap)))
        (define-key map (kbd \"M-<tab>\")   \\='buffer-flip-forward)
        (define-key map (kbd \"M-S-<tab>\") \\='buffer-flip-backward)
        (define-key map (kbd \"M-ESC\")     \\='buffer-flip-abort)
        map))")

(defvar buffer-flip-tab-map '(keymap)
  "The transient map which is active during tab cycling.
This must be explicitly configured by the user with keys mapped
to the three tab flipping commands, as shown in the following
example.

;; keys to begin cycling tabs.  Global keys.
\(global-set-key (kbd \"M-<tab>\")   \\='buffer-flip-tab-forward)
\(global-set-key (kbd \"M-S-<tab>\") \\='buffer-flip-tab-backward)

;; transient keymap used once cycling starts
\(setq buffer-flip-tab-map
      (let ((map (make-sparse-keymap)))
        (define-key map (kbd \"M-<tab>\")   \\='buffer-flip-tab-forward)
        (define-key map (kbd \"M-S-<tab>\") \\='buffer-flip-tab-backward)
        (define-key map (kbd \"M-ESC\")     \\='buffer-flip-tab-abort)
        map))")

(defun buffer-flip-check-map-configuration (map &rest commands)
  "Ensure MAP has bindings for each command in COMMANDS.
Signals `user-error' for any command not bound in MAP."
  (dolist (func commands)
    (unless (where-is-internal func (list (or map '(keymap))))
      (user-error "%s not bound to a key in the transient map.  See documentation" func))))

(provide 'buffer-flip)
;;; buffer-flip.el ends here
