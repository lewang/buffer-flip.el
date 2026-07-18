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

;;; --- buffer cycling: session-scoped keep-list filter ---
;;
;; `buffer-flip-cycle' leaves its choice in the SELECTED WINDOW's buffer
;; (via `switch-to-buffer' inside `with-selected-window', which restores
;; `current-buffer' on exit), so these tests assert on
;; `(window-buffer (selected-window))', not `(current-buffer)'.  Where
;; cycling behaviour matters we stub `buffer-flip-skip-buffer' so the outcome
;; does not depend on real window visibility.

(defun buffer-flip-test--marked-p (buf)
  "Return non-nil if BUF's name carries the test marker prefix."
  (string-prefix-p "bftest-mark-" (buffer-name buf)))

(defun buffer-flip-test--keep-marked (bufs)
  "Keep only the marked buffers in BUFS (a `buffer-flip' keep-list filter)."
  (cl-remove-if-not #'buffer-flip-test--marked-p bufs))

(defun buffer-flip-test--session-map ()
  "A `buffer-flip-map' configured well enough for `buffer-flip--start-session'."
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<f10>") #'buffer-flip-forward)
    (define-key map (kbd "<f9>")  #'buffer-flip-backward)
    (define-key map (kbd "C-g")   #'buffer-flip-abort)
    (define-key map (kbd "RET")   #'buffer-flip-confirm)
    map))

(defun buffer-flip-test--reset-session ()
  "Tear down any live cycling session and clear session state.
Safe to call unconditionally from test cleanup."
  (when (functionp buffer-flip-exit-function)
    (ignore-errors (funcall buffer-flip-exit-function)))
  (setq buffer-flip--session-active nil
        buffer-flip--session-filter nil
        buffer-flip-exit-function nil
        buffer-flip--target-window nil))

;;; buffer-flip--candidates

(ert-deftest buffer-flip-test-candidates-no-filter-identity ()
  "With no session filter, candidates equal the frame buffer list."
  (let ((buffer-flip--session-filter nil))
    (should (equal (buffer-flip--candidates)
                   (buffer-list (selected-frame))))))

(ert-deftest buffer-flip-test-candidates-filter-narrows ()
  "A session filter narrows candidates to the kept buffers."
  (let ((m1 (get-buffer-create "bftest-mark-1"))
        (m2 (get-buffer-create "bftest-mark-2"))
        (plain (get-buffer-create "bftest-plain-1")))
    (unwind-protect
        (with-current-buffer m1              ; anchor is itself marked
          (let* ((buffer-flip--session-filter #'buffer-flip-test--keep-marked)
                 (c (buffer-flip--candidates)))
            (should (memq m1 c))
            (should (memq m2 c))
            (should-not (memq plain c))
            (should (cl-every #'buffer-flip-test--marked-p c))))
      (mapc #'kill-buffer (list m1 m2 plain)))))

(ert-deftest buffer-flip-test-candidates-anchor-retained-when-dropped ()
  "The current buffer stays as anchor even when the filter drops it."
  (let ((m1 (get-buffer-create "bftest-mark-1"))
        (plain (get-buffer-create "bftest-plain-1")))
    (unwind-protect
        (with-current-buffer plain           ; anchor is NOT marked
          (let* ((buffer-flip--session-filter #'buffer-flip-test--keep-marked)
                 (c (buffer-flip--candidates)))
            (should (memq plain c))
            (should (eq (car c) plain))       ; consed onto the front
            (should (memq m1 c))
            (should (cl-every (lambda (b)
                                (or (eq b plain)
                                    (buffer-flip-test--marked-p b)))
                              c))))
      (mapc #'kill-buffer (list m1 plain)))))

(ert-deftest buffer-flip-test-candidates-anchor-not-duplicated ()
  "When the filter keeps the current buffer, it is not added twice."
  (let ((m1 (get-buffer-create "bftest-mark-1")))
    (unwind-protect
        (with-current-buffer m1
          (let ((buffer-flip--session-filter #'buffer-flip-test--keep-marked))
            (should (= 1 (cl-count m1 (buffer-flip--candidates))))))
      (kill-buffer m1))))

;;; buffer-flip--in-session-p / session flag

(ert-deftest buffer-flip-test-in-session-p-reads-flag ()
  "`buffer-flip--in-session-p' reflects the flag, not `last-command'.
This is the continuity fix: a differently-named entrance leaves
`last-command' unrelated, yet the session must still be recognised."
  (let ((buffer-flip--session-active nil)
        (last-command 'buffer-flip-forward))  ; would fool the old logic
    (should-not (buffer-flip--in-session-p)))
  (let ((buffer-flip--session-active t)
        (last-command 'le::project-buffer-flip))
    (should (buffer-flip--in-session-p))))

;;; buffer-flip--start-session: stash filter, set/clear flag

(ert-deftest buffer-flip-test-start-session-stashes-and-clears ()
  "`--start-session' stashes the filter and raises the flag; exit clears both."
  (let ((buffer-flip-map (buffer-flip-test--session-map))
        (filter #'buffer-flip-test--keep-marked))
    (unwind-protect
        (save-window-excursion
          (buffer-flip--start-session nil filter)
          (should (eq buffer-flip--session-active t))
          (should (eq buffer-flip--session-filter filter))
          (funcall buffer-flip-exit-function)
          (should-not buffer-flip--session-active)
          (should-not buffer-flip--session-filter))
      (buffer-flip-test--reset-session))))

;;; buffer-flip-cycle: honours the filter, skips composed on top

(ert-deftest buffer-flip-test-cycle-respects-filter ()
  "A filtered cycle lands on a kept buffer, advancing past the dropped anchor."
  (let ((buffer-flip-map (buffer-flip-test--session-map))
        (m1 (get-buffer-create "bftest-mark-1"))
        (m2 (get-buffer-create "bftest-mark-2"))
        (plain (get-buffer-create "bftest-plain-1")))
    (unwind-protect
        (save-window-excursion
          (switch-to-buffer plain)
          ;; Skip exactly the dropped anchor so advancement is deterministic,
          ;; independent of real window visibility.
          (cl-letf (((symbol-function 'buffer-flip-skip-buffer)
                     (lambda (buf) (eq buf plain))))
            (buffer-flip--start-session nil #'buffer-flip-test--keep-marked)
            (buffer-flip-cycle 'forward)
            (let ((landed (window-buffer (selected-window))))
              (should (memq landed (list m1 m2)))
              (should-not (eq landed plain)))))
      (buffer-flip-test--reset-session)
      (mapc #'kill-buffer (list m1 m2 plain)))))

(ert-deftest buffer-flip-test-cycle-unfiltered-advances ()
  "An unfiltered cycle advances off the (skipped) start buffer, filter stays nil."
  (let ((buffer-flip-map (buffer-flip-test--session-map))
        (start (get-buffer-create "bftest-plain-start"))
        (other (get-buffer-create "bftest-plain-other")))
    (unwind-protect
        (save-window-excursion
          (switch-to-buffer start)
          (cl-letf (((symbol-function 'buffer-flip-skip-buffer)
                     (lambda (buf) (eq buf start))))
            (buffer-flip--start-session)       ; no filter
            (should-not buffer-flip--session-filter)
            (buffer-flip-cycle 'forward)
            (should-not (eq (window-buffer (selected-window)) start))))
      (buffer-flip-test--reset-session)
      (mapc #'kill-buffer (list start other)))))

;;; Session continuity across flip-key presses

(ert-deftest buffer-flip-test-forward-preserves-filter-mid-session ()
  "A flip key mid-session neither restarts nor drops the filter.
`buffer-flip-forward' sees the flag (not `last-command'), so it skips
`--start-session' and keeps cycling the still-filtered candidate set."
  (let ((buffer-flip-map (buffer-flip-test--session-map))
        (filter #'buffer-flip-test--keep-marked)
        (m1 (get-buffer-create "bftest-mark-1"))
        (m2 (get-buffer-create "bftest-mark-2"))
        (plain (get-buffer-create "bftest-plain-1")))
    (unwind-protect
        (save-window-excursion
          (switch-to-buffer plain)
          (cl-letf (((symbol-function 'buffer-flip-skip-buffer)
                     (lambda (buf) (eq buf plain))))
            (buffer-flip--start-session nil filter)
            ;; Second keypress: last-command is the entrance, not a flip
            ;; command.  The old `last-command' logic would restart unfiltered.
            (let ((last-command 'le::project-buffer-flip))
              (buffer-flip-forward))
            (should (eq buffer-flip--session-active t))
            (should (eq buffer-flip--session-filter filter))
            (should (memq (window-buffer (selected-window)) (list m1 m2)))))
      (buffer-flip-test--reset-session)
      (mapc #'kill-buffer (list m1 m2 plain)))))

(ert-deftest buffer-flip-test-forward-cold-start-unfiltered ()
  "`buffer-flip-forward' with no active session starts an unfiltered one."
  (let ((buffer-flip-map (buffer-flip-test--session-map))
        (start (get-buffer-create "bftest-plain-start"))
        (other (get-buffer-create "bftest-plain-other")))
    (unwind-protect
        (save-window-excursion
          (switch-to-buffer start)
          (cl-letf (((symbol-function 'buffer-flip-skip-buffer)
                     (lambda (buf) (eq buf start))))
            (setq buffer-flip--session-active nil
                  buffer-flip--session-filter nil)
            (buffer-flip-forward)
            (should buffer-flip--session-active)
            (should-not buffer-flip--session-filter)))
      (buffer-flip-test--reset-session)
      (mapc #'kill-buffer (list start other)))))

(provide 'buffer-flip-test)
;;; buffer-flip-test.el ends here
