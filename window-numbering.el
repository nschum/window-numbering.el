;;; window-numbering --- Emacs window shortcuts

;; Copyright (C) 2006-2007 Nikolaj Schumacher <bugs * nschum , de>

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

;; Enable window-numbering-mode and use M-1 through M-0 to navigate.
;;
;; If you want to affect the numbers, use window-numbering-before-hook or
;; window-numbering-assign-func.
;; For instance, to always assign the calculator window the number 9, add the
;; following to your .emacs:
;;
;; (setq window-numbering-assign-func
;;       (lambda () (when (equal (buffer-name) "*Calculator*") 9)))


;;; Changes

;; 2007-02-18 (1.1)
;;    added window-numbering-before-hook, window-numbering-assign-func

;;; Configuration

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

(defconst window-numbering-mode-line-position 1
  "The position in the mode-line `window-numbering-mode' displays the number.")

(defvar window-numbering-before-hook nil
  "Hook called before `window-numbering-mode' starts assigning numbers.
The number of windows that will be numbered is passed as a parameter.
Use `window-numbering-assign' to manually assign some of them a number.
If you want to assign a number to just one buffer, use
`window-numbering-assign-func' instead.")

(defvar window-numbering-assign-func nil
  "Function called for each window by `window-numbering-mode'.
This is called before automatic assignment begins.
The function should return a number to have it assigned to the current-window,
nil otherwise.")

;;; Code

(require 'cl)

(defvar window-numbering-table nil
  "table -> (window vector . number table)")

(defun window-numbering-calculate-left (windows)
  (let ((i 9) left)
    (while (>= i 0)
      (let ((window (aref windows i)))
        (unless window
          (push (% (1+ i) 10) left)))
      (decf i))
    left))


(defun window-numbering-assign (window &optional number)
  (assert (boundp 'window-numbering-windows))
  (assert (boundp 'window-numbering-numbers))
  (assert (boundp 'window-numbering-left))
  (if number
      (if (aref window-numbering-windows number)
          (progn (message "Number %s assigned to two buffers (%s and %s)"
                          number window (aref window-numbering-windows number))
                 nil)
        (setf (aref window-numbering-windows number) window)
        (puthash window number window-numbering-numbers)
        (setq window-numbering-left (delq number window-numbering-left))
        t)
    ;; else default adding
    (when window-numbering-left
      (unless (gethash window window-numbering-numbers)
        (let ((number (car window-numbering-left)))
          (window-numbering-assign window number)
          number)))))

(defun select-window-by-number (i)
  "Select window given number I by `window-numbering-mode'."
  (interactive)
  (let ((windows (car (gethash (selected-frame) window-numbering-table)))
        window)
    (if (and (>= i 0) (< i 10)
             (setq window (aref windows i)))
        (select-window window)
    (error "No window numbered %s" i))))

(defun window-numbering-update ()
  "Update the window numbering for the current frame.
Optional parameter PREASSIGNED-WINDOWS is a hashmap already mapping some
windows to numbers."
  (let* ((window-numbering-windows (make-vector 10 nil))
         (window-numbering-numbers (make-hash-table :size 10))
         (window-numbering-left
          (window-numbering-calculate-left window-numbering-windows)))
    (puthash (selected-frame)
             (cons window-numbering-windows window-numbering-numbers)
             window-numbering-table)
    (when (and window-numbering-auto-assign-0-to-minibuffer
               (active-minibuffer-window))
      (window-numbering-assign (active-minibuffer-window) 0))
    (let ((windows (window-list nil 0 (window-at 0 0))))
      (run-hook-with-args 'window-numbering-before-hook windows)
      (when window-numbering-assign-func
        (mapc `(lambda (window)
                 (with-selected-window window
                   (with-current-buffer (window-buffer window)
                     (let ((num (funcall ,window-numbering-assign-func)))
                       (when num
                         (window-numbering-assign window num))))))
              windows))
      (dolist (window windows)
        (window-numbering-assign window))
      )))

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

(defun window-numbering-get-number-string (&optional window)
  (int-to-string (window-numbering-get-number window)))

(defun window-numbering-get-number (&optional window)
  (gethash (or window (selected-window))
           (cdr (gethash (selected-frame) window-numbering-table))))

(define-minor-mode window-numbering-mode
  "A minor mode that assigns a number to each window."
  nil nil window-numbering-keymap :global t
  (if window-numbering-mode
      (unless window-numbering-table
        (save-excursion
          (setq window-numbering-table (make-hash-table :size 16))
          (window-numbering-install-mode-line)
          (add-hook 'window-configuration-change-hook
                    'window-numbering-update)
          (dolist (frame (frame-list))
            (select-frame frame)
            (window-numbering-update))))
    (window-numbering-clear-mode-line)
    (remove-hook 'window-configuration-change-hook
                 'window-numbering-update)
    (setq window-numbering-table nil)))

(defun window-numbering-install-mode-line (&optional position)
  "Install the window number from `window-numbering-mode' to the mode-line."
  (let ((mode-line (default-value 'mode-line-format))
        (res))
    (dotimes (i (min (or position window-numbering-mode-line-position)
                     (length mode-line)))
      (push (car mode-line) res)
      (pop mode-line))
    (push '(:eval (window-numbering-get-number-string)) res)
    (while mode-line
      (push (car mode-line) res)
      (pop mode-line))
    (setq-default mode-line-format (nreverse res)))
  (force-mode-line-update t))

(defun window-numbering-clear-mode-line ()
  "Remove the window number of `window-numbering-mode' from the mode-line."
  (let ((mode-line (default-value 'mode-line-format))
        (res))
    (while mode-line
      (let ((item (car mode-line)))
        (unless (equal item '(:eval (window-numbering-get-number-string)))
          (push item res)))
      (pop mode-line))
    (setq-default mode-line-format (nreverse res)))
  (force-mode-line-update t))

(provide 'window-numbering)
