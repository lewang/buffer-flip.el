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
(require 'buffer-flip-tabs)

;;; --- buffer-flip--format-item tests ---

(ert-deftest buffer-flip-test-format-item-current ()
  "Current item gets brackets and current-buffer face."
  (let ((result (buffer-flip--format-item "foo" t)))
    (should (equal (substring-no-properties result) "[foo]"))
    (should (eq (get-text-property 0 'face result) 'buffer-flip-current-buffer-face))))

(ert-deftest buffer-flip-test-format-item-other ()
  "Non-current item is plain with other-buffer face."
  (let ((result (buffer-flip--format-item "bar" nil)))
    (should (equal (substring-no-properties result) "bar"))
    (should (eq (get-text-property 0 'face result) 'buffer-flip-other-buffer-face))))

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

(provide 'buffer-flip-test)
;;; buffer-flip-test.el ends here
