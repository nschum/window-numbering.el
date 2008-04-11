;;; window-numbering --- Emacs window shortcuts

;; Copyright (C) 2006 Nikolaj Schumacher <bugs * nschum , de>

;;; License

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Usage

;; Use `deftest' to define a test and `run-elk-test' to run it.
;; Create test bundles with `defsuite' or `build-suite'.
;; Verify your code with  `assert-equal', `assert-eq', `assert-eql',
;; `assert-nonnil', `assert-t', `assert-nil' and `assert-error'
;; to verify your code.

;;; Version 0.9

(defvar window-numbering-table nil
  "table -> (window vector . number table)")

(defun select-window-by-number (i)
  (interactive)
  (let ((windows (car (gethash (selected-frame) window-numbering-table)))
        window)
    (if (and (>= i 0) (< i 10)
             (setq window (aref windows i)))
        (select-window window)
    (error "No window numbered %s" i))))

(defvar window-numbering-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map "\M-0" 'select-window-0)
    (define-key map "\M-1" 'select-window-1)
    (define-key map "\M-2" 'select-window-2)
    (define-key map "\M-3" 'select-window-3)
    (define-key map "\M-4" 'select-window-4)
    (define-key map "\M-5" 'select-window-5)
    (define-key map "\M-6" 'select-window-6)
    (define-key map "\M-7" 'select-window-7)
    (define-key map "\M-8" 'select-window-8)
    (define-key map "\M-9" 'select-window-9)
    map)
  "Keymap used in by `window-numbering-mode'.")

(defvar window-numbering-auto-assign-0-to-minibuffer t
  "If non-nil, `window-numbering-mode' assigns 0 to the minibuffer if active.")

(defun window-numbering-enumerate ()
;;   (let ((table (gethash (selected-frame) window-numbering-table)))
  ;;     (unless table

  (let ((table (cons (make-vector 10 nil) (make-hash-table :size 10))))
    (puthash (selected-frame) table window-numbering-table)
    (let ((window-numbering-windows (car table))
          (window-numbering-numbers (cdr table))
          (minibuffer (when window-numbering-auto-assign-0-to-minibuffer
                        (active-minibuffer-window))))
      (window-numbering-walk (car (window-tree)) 1 (if minibuffer 9 10))
      (when minibuffer
        (set (aref window-numbering-windows 0) minibuffer)
        (puthash minibuffer 0 window-numbering-numbers)))
    window-numbering-windows))

;;   (setq window-numbering-windows (make-vector 10 nil))
;;   (setq window-numbering-numbers (make-hash-table :size 10))
;;   (let* ((minibuffer (when window-numbering-auto-assign-0-to-minibuffer
;;                        (active-minibuffer-window))))
;;     (window-numbering-walk (car (window-tree)) 1 (if minibuffer 9 10))
;;     (when minibuffer
;;       (setf (aref window-numbering-windows 0) minibuffer)
;;       (puthash minibuffer 0 window-numbering-numbers)))
;;   window-numbering-windows)

(defun select-window-0 ()
  (interactive)
  (select-window-by-number 0))
(defun select-window-1 ()
  (interactive)
  (select-window-by-number 1))
(defun select-window-2 ()
  (interactive)
  (select-window-by-number 2))
(defun select-window-3 ()
  (interactive)
  (select-window-by-number 3))
(defun select-window-4 ()
  (interactive)
  (select-window-by-number 4))
(defun select-window-5 ()
  (interactive)
  (select-window-by-number 5))
(defun select-window-6 ()
  (interactive)
  (select-window-by-number 6))
(defun select-window-7 ()
  (interactive)
  (select-window-by-number 7))
(defun select-window-8 ()
  (interactive)
  (select-window-by-number 8))
(defun select-window-9 ()
  (interactive)
  (select-window-by-number 9))

(defun window-numbering-walk (tree i max-window)
  (if (windowp tree)
      (when (<= i max-window)
        (let ((pos (if (= i 10) 0 i)))
          (set (aref window-numbering-windows pos) tree)
          (puthash tree pos window-numbering-numbers))
          (incf i))
    (let ((windows (if (windowp tree) `(,tree) (cddr tree))))
      (dolist (window windows)
        (setq i (window-numbering-walk window i max-window)))))
    i)

(defconst window-numbering-mode-line-position 1
  "The position in the mode-line `window-numbering-mode' displays the number.")
;; (window-numbering-get-number)
(defun window-numbering-get-number (&optional window)
  (int-to-string
   (gethash (or window (selected-window)) window-numbering-numbers)))

(define-minor-mode window-numbering-mode
  "A minor mode that assigns a number to each window."
  nil nil window-numbering-keymap :global t
  (if window-numbering-mode
      (save-excursion
        (setq window-numbering-table (make-hash-table :size 16))
        (window-numbering-install-mode-line)
        (add-hook 'window-configuration-change-hook
                  'window-numbering-enumerate)
        (dolist (frame (frame-list))
          (select-frame frame)
          (window-numbering-enumerate)))
    (window-numbering-clear-mode-line)
    (remove-hook 'window-configuration-change-hook
                 'window-numbering-enumerate)
    (setq window-numbering-table nil)))

(defun window-numbering-install-mode-line (&optional position)
  (let ((mode-line (default-value 'mode-line-format))
        (res))
    (dotimes (i (min (or position window-numbering-mode-line-position)
                     (length mode-line)))
      (push (car mode-line) res)
      (pop mode-line))
    (push '(:eval (window-numbering-get-number)) res)
    (while mode-line
      (push (car mode-line) res)
      (pop mode-line))
    (setq-default mode-line-format (nreverse res)))
  (force-mode-line-update t))

(defun window-numbering-clear-mode-line ()
  "Remove the dot installed by `test-runner-install-dot' from the mode-line."
  (let ((mode-line (default-value 'mode-line-format))
        (res))
    (while mode-line
      (let ((item (car mode-line)))
        (unless (equal item '(:eval (window-numbering-get-number)))
          (push item res)))
      (pop mode-line))
    (setq-default mode-line-format (nreverse res)))
  (force-mode-line-update t))

(provide 'window-numbering)
