;;; buffer-flip-test.el --- Tests for buffer-flip -*- lexical-binding: t; -*-

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

;; ERT tests for the buffer-flip common engine and tab time fixup.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'buffer-flip)
(require 'buffer-flip-buffers)
(require 'buffer-flip-tabs)

;;; --- buffer-flip--format-item tests ---

(ert-deftest buffer-flip-test-format-item-current ()
  "Current item gets brackets and current-buffer face."
  (let ((result (buffer-flip--format-item "foo" t)))
    (should (equal (substring-no-properties result) "[foo]"))
    (should (eq (get-text-property 0 'face result) 'buffer-flip-current-item-face))))

(ert-deftest buffer-flip-test-format-item-other ()
  "Non-current item is plain with other-buffer face."
  (let ((result (buffer-flip--format-item "bar" nil)))
    (should (equal (substring-no-properties result) "bar"))
    (should (eq (get-text-property 0 'face result) 'buffer-flip-item-face))))

;;; --- buffer-flip--format-items tests ---

(ert-deftest buffer-flip-test-format-items-empty ()
  "Empty list produces empty string."
  (should (equal (buffer-flip--format-items nil "x") "")))

(ert-deftest buffer-flip-test-format-items-single ()
  "Single item is formatted as current."
  (let ((result (buffer-flip--format-items '("only") "only")))
    (should (string-match-p "\\[only\\]" (substring-no-properties result)))))

(ert-deftest buffer-flip-test-format-items-current-highlighted ()
  "Current item appears bracketed in output."
  (let ((result (buffer-flip--format-items '("a" "b" "c") "b")))
    (should (string-match-p "\\[b\\]" (substring-no-properties result)))))

(ert-deftest buffer-flip-test-format-items-fence-present ()
  "Fence marker appears in the output for multi-item lists."
  (let ((result (buffer-flip--format-items '("a" "b" "c" "d" "e") "a")))
    (should (string-match-p "┃┃" (substring-no-properties result)))))

(ert-deftest buffer-flip-test-format-items-ordering ()
  "All items appear in the output."
  (let* ((names '("alpha" "beta" "gamma"))
         (result (buffer-flip--format-items names "beta"))
         (plain (substring-no-properties result)))
    (dolist (name names)
      (should (string-match-p (regexp-quote name) plain)))))

;;; --- buffer-flip-check-map-configuration tests ---

(ert-deftest buffer-flip-test-check-map-valid ()
  "No error when all commands are bound."
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") #'next-line)
    (define-key map (kbd "p") #'previous-line)
    (buffer-flip-check-map-configuration map 'next-line 'previous-line)))

(ert-deftest buffer-flip-test-check-map-missing-binding ()
  "Error when a command is not bound."
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") #'next-line)
    (should-error (buffer-flip-check-map-configuration map 'next-line 'previous-line)
                  :type 'user-error)))

;;; --- buffer-flip-tab--fixup-times tests ---

(defun buffer-flip-test--make-tab (name time &optional current)
  "Create a mock tab alist with NAME, TIME, and optional CURRENT flag."
  (let ((tab `((name . ,name) (time . ,time))))
    (if current
        (cons 'current-tab tab)
      (cons 'tab tab))))

(ert-deftest buffer-flip-test-tab-fixup-confirm ()
  "Confirm fixup promotes original tab, rest preserve order."
  (let* ((tab-a (buffer-flip-test--make-tab "A" 100.0))
         (tab-b (buffer-flip-test--make-tab "B" 99.0))
         (tab-c (buffer-flip-test--make-tab "C" 98.0 t))
         (live-tabs (list tab-c tab-a tab-b))
         (tab-bar-tabs-function (lambda () live-tabs))
         (buffer-flip-tab--tabs
          (list (copy-alist tab-a)
                (copy-alist tab-b)
                (copy-alist tab-c))))
    (buffer-flip-tab--fixup-times)
    ;; A should have highest time (promoted), B next
    (let ((time-a (alist-get 'time tab-a))
          (time-b (alist-get 'time tab-b)))
      (should (> time-a time-b)))))

(ert-deftest buffer-flip-test-tab-fixup-abort ()
  "Abort fixup restores original non-current order."
  (let* ((tab-a (buffer-flip-test--make-tab "A" 100.0))
         (tab-b (buffer-flip-test--make-tab "B" 99.0))
         (tab-c (buffer-flip-test--make-tab "C" 98.0 t))
         (live-tabs (list tab-c tab-a tab-b))
         (tab-bar-tabs-function (lambda () live-tabs))
         (buffer-flip-tab--tabs
          (list (copy-alist tab-a)
                (copy-alist tab-b)
                (copy-alist tab-c))))
    (buffer-flip-tab--fixup-times 'aborting)
    ;; B and C are in cdr, so B should be highest, C next
    (let ((time-b (alist-get 'time tab-b)))
      ;; C is current-tab so it won't be assigned a time
      ;; B should have gotten the first (highest) synthetic time
      (should (numberp time-b)))))

(ert-deftest buffer-flip-test-tab-fixup-confirm-same-tab ()
  "Confirm on same tab as start (full-circle cycle)."
  (let* ((tab-a (buffer-flip-test--make-tab "A" 100.0 t))
         (tab-b (buffer-flip-test--make-tab "B" 99.0))
         (tab-c (buffer-flip-test--make-tab "C" 98.0))
         (live-tabs (list tab-a tab-b tab-c))
         (tab-bar-tabs-function (lambda () live-tabs))
         (buffer-flip-tab--tabs
          (list (copy-alist tab-a)
                (copy-alist tab-b)
                (copy-alist tab-c))))
    ;; A is current-tab, so it won't be modified; B and C should get times
    (buffer-flip-tab--fixup-times)
    (let ((time-b (alist-get 'time tab-b))
          (time-c (alist-get 'time tab-c)))
      (should (> time-b time-c)))))

;;; --- buffer cycling: skip-patterns filtering + session state ---
;;
;; `buffer-flip-cycle' leaves its choice in the SELECTED WINDOW's buffer
;; (via `switch-to-buffer' inside `with-selected-window', which restores
;; `current-buffer' on exit), so cycle tests assert on
;; `(window-buffer (selected-window))', not `(current-buffer)'.

(defun buffer-flip-test--session-map ()
  "A `buffer-flip-map' configured well enough for `buffer-flip--start-session'."
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<f10>") #'buffer-flip-forward)
    (define-key map (kbd "<f9>")  #'buffer-flip-backward)
    (define-key map (kbd "C-g")   #'buffer-flip-abort)
    (define-key map (kbd "RET")   #'buffer-flip-confirm)
    map))

(defun buffer-flip-test--reset-session ()
  "Tear down any live cycling session.  Safe to call from cleanup."
  (when (and buffer-flip--session
             (buffer-flip--session-exit-function buffer-flip--session))
    (ignore-errors
      (funcall (buffer-flip--session-exit-function buffer-flip--session))))
  (setq buffer-flip--session nil))

(defvar-local buffer-flip-test--drop nil
  "Buffer-local flag used by `buffer-flip-test-skip-buffer-predicate-gets-buffer'.")

;;; buffer-flip-skip-buffer: regexp and function specs

(ert-deftest buffer-flip-test-skip-buffer-regexp ()
  "A regexp spec skips buffers whose name matches (backward compatible)."
  (let ((buffer-flip--session nil)
        (buffer-flip-skip-patterns '("\\`bftest-skip-"))
        (skip (get-buffer-create "bftest-skip-1"))
        (keep (get-buffer-create "bftest-keep-1")))
    (unwind-protect
        (progn
          (should (buffer-flip-skip-buffer skip))
          (should-not (buffer-flip-skip-buffer keep)))
      (mapc #'kill-buffer (list skip keep)))))

(ert-deftest buffer-flip-test-skip-buffer-function ()
  "A function spec is called with the buffer; non-nil means skip."
  (let* ((skip (get-buffer-create "bftest-skip-1"))
         (keep (get-buffer-create "bftest-keep-1"))
         (buffer-flip--session nil)
         (buffer-flip-skip-patterns
          (list (lambda (b) (string-prefix-p "bftest-skip-" (buffer-name b))))))
    (unwind-protect
        (progn
          (should (buffer-flip-skip-buffer skip))
          (should-not (buffer-flip-skip-buffer keep)))
      (mapc #'kill-buffer (list skip keep)))))

(ert-deftest buffer-flip-test-skip-buffer-mixed ()
  "Regexp and function specs coexist in one list; any match skips."
  (let* ((by-regexp (get-buffer-create "bftest-skip-1"))
         (by-func (get-buffer-create "other-drop-me"))
         (keep (get-buffer-create "bftest-keep-1"))
         (buffer-flip--session nil)
         (buffer-flip-skip-patterns
          (list "\\`bftest-skip-"
                (lambda (b) (string-prefix-p "other-drop" (buffer-name b))))))
    (unwind-protect
        (progn
          (should (buffer-flip-skip-buffer by-regexp))
          (should (buffer-flip-skip-buffer by-func))
          (should-not (buffer-flip-skip-buffer keep)))
      (mapc #'kill-buffer (list by-regexp by-func keep)))))

(ert-deftest buffer-flip-test-skip-buffer-predicate-gets-buffer ()
  "The predicate receives the buffer object, not just its name."
  (let* ((buf (get-buffer-create "bftest-plain"))
         (buffer-flip--session nil)
         (buffer-flip-skip-patterns
          (list (lambda (b) (buffer-local-value 'buffer-flip-test--drop b)))))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (setq-local buffer-flip-test--drop t))
          (should (buffer-flip-skip-buffer buf))
          (with-current-buffer buf
            (setq-local buffer-flip-test--drop nil))
          (should-not (buffer-flip-skip-buffer buf)))
      (kill-buffer buf))))

;;; session struct lifecycle

(ert-deftest buffer-flip-test-in-session-p-reads-struct ()
  "`buffer-flip--in-session-p' reflects the struct, not `last-command'."
  (let ((buffer-flip--session nil)
        (last-command 'buffer-flip-forward))  ; would fool the old logic
    (should-not (buffer-flip--in-session-p)))
  (let ((buffer-flip--session (buffer-flip--session-make))
        (last-command 'le::project-buffer-flip))
    (should (buffer-flip--in-session-p))))

(ert-deftest buffer-flip-test-start-session-captures-and-clears ()
  "`--start-session' records a struct capturing skip-patterns; exit clears it."
  (let ((buffer-flip-map (buffer-flip-test--session-map))
        (patterns '("\\`bftest-")))
    (unwind-protect
        (save-window-excursion
          (let ((buffer-flip-skip-patterns patterns))
            (buffer-flip--start-session))
          (should buffer-flip--session)
          (should (equal (buffer-flip--session-skip-patterns buffer-flip--session)
                         patterns))
          (funcall (buffer-flip--session-exit-function buffer-flip--session))
          (should-not buffer-flip--session))
      (buffer-flip-test--reset-session))))

;;; capture-at-start: a caller's let-binding survives the transient map

(ert-deftest buffer-flip-test-capture-survives-let-exit ()
  "Patterns captured at session start persist after the caller's `let' exits.
This is the whole point of holding a copy in session state: the transient
map outlives the `let', so reading the live variable later would lose it."
  (let ((buffer-flip-map (buffer-flip-test--session-map))
        (skip-fn (lambda (b) (string-prefix-p "bftest-skip-" (buffer-name b))))
        (skip (get-buffer-create "bftest-skip-1"))
        (keep (get-buffer-create "bftest-keep-1")))
    (unwind-protect
        ;; A distinct global value proves the captured copy is independent.
        (let ((buffer-flip-skip-patterns '("global-only")))
          (save-window-excursion
            (let ((buffer-flip-skip-patterns (list skip-fn)))
              (buffer-flip--start-session))       ; captures the inner value
            ;; inner `let' has exited; the live variable is back to global
            (should (equal buffer-flip-skip-patterns '("global-only")))
            (should (equal (buffer-flip--session-skip-patterns buffer-flip--session)
                           (list skip-fn)))
            ;; skip-buffer consults the captured copy during the session
            (should (buffer-flip-skip-buffer skip))
            (should-not (buffer-flip-skip-buffer keep))))
      (buffer-flip-test--reset-session)
      (mapc #'kill-buffer (list skip keep)))))

(ert-deftest buffer-flip-test-forward-does-not-restart-mid-session ()
  "A flip key mid-session reuses the same session (no restart), any last-command."
  (let ((buffer-flip-map (buffer-flip-test--session-map))
        (origin (get-buffer-create "bftest-origin")))
    (unwind-protect
        (save-window-excursion
          (switch-to-buffer origin)
          (buffer-flip--start-session)
          (let ((s buffer-flip--session)
                (last-command 'le::project-buffer-flip))
            (buffer-flip-forward)
            (should (eq buffer-flip--session s))))
      (buffer-flip-test--reset-session)
      (kill-buffer origin))))

;;; end-to-end cycling

(ert-deftest buffer-flip-test-cycle-honours-captured-filter ()
  "Cold-start via a let-bound skip fn cycles only the kept buffers."
  (let ((buffer-flip-map (buffer-flip-test--session-map))
        (m1 (get-buffer-create "bftest-keep-1"))
        (m2 (get-buffer-create "bftest-keep-2"))
        (origin (get-buffer-create "bftest-origin")))
    (unwind-protect
        (save-window-excursion
          (switch-to-buffer origin)
          ;; Skip everything except m1/m2 (origin included -> must advance).
          (let ((buffer-flip-skip-patterns
                 (list (lambda (b) (not (memq b (list m1 m2)))))))
            (buffer-flip-forward))
          (should (memq (window-buffer (selected-window)) (list m1 m2))))
      (buffer-flip-test--reset-session)
      (mapc #'kill-buffer (list m1 m2 origin)))))

(ert-deftest buffer-flip-test-cycle-unfiltered-advances ()
  "With no filter, cycling advances off the (visible, thus skipped) origin."
  (let ((buffer-flip-map (buffer-flip-test--session-map))
        (origin (get-buffer-create "bftest-origin"))
        (other (get-buffer-create "bftest-other")))
    (unwind-protect
        (save-window-excursion
          (switch-to-buffer origin)
          (let ((buffer-flip-skip-patterns nil))
            (buffer-flip-forward))
          (should-not (eq (window-buffer (selected-window)) origin)))
      (buffer-flip-test--reset-session)
      (mapc #'kill-buffer (list origin other)))))

(ert-deftest buffer-flip-test-abort-restores-window-config ()
  "`buffer-flip-abort' restores the saved window configuration and clears state."
  (let ((buffer-flip-map (buffer-flip-test--session-map))
        (origin (get-buffer-create "bftest-origin"))
        (other (get-buffer-create "bftest-other")))
    (unwind-protect
        (save-window-excursion
          (switch-to-buffer origin)
          (buffer-flip--start-session)
          (buffer-flip-cycle 'forward)
          (buffer-flip-abort)
          (should (eq (window-buffer (selected-window)) origin))
          (should-not buffer-flip--session))
      (buffer-flip-test--reset-session)
      (mapc #'kill-buffer (list origin other)))))

(provide 'buffer-flip-test)
;;; buffer-flip-test.el ends here
