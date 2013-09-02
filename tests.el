(require 'ert)
(require 'window-numbering)

(ert-deftest window-numbering-assign ()
  (let ((window-numbering-windows (make-vector 10 nil))
        (window-numbering-numbers (make-hash-table :size 10))
        (window-numbering-left '(1 2 3)))
    (should (not (null (window-numbering-assign 'xx 7))))
    (should (null (window-numbering-assign 'yy 7)))
    (should (not (null (window-numbering-assign 'zz 8))))
    (should (equal 8 (gethash 'zz window-numbering-numbers)))
    (should (equal 7 (gethash 'xx window-numbering-numbers)))
    (should (equal 'zz (aref window-numbering-windows 8)))
    (should (equal 'xx (aref window-numbering-windows 7)))
    ))


(ert-deftest window-numbering-assign-auto ()
  (let ((window-numbering-windows (make-vector 10 nil))
        (window-numbering-numbers (make-hash-table :size 10))
        (window-numbering-left '(1 2 3 4)))
    (should (eq 1 (window-numbering-assign 'xx)))
    (should (not (null (window-numbering-assign 'yy 3))))
    (should (eq 2 (window-numbering-assign 'zz)))
    (should (eq 4 (window-numbering-assign 'aa)))
    ))


(ert-deftest window-numbering-calculate-left ()
  (should (equal '(6) (window-numbering-calculate-left
                       [t t t t t nil t t t t])))
  (should (equal '(1 2 3) (window-numbering-calculate-left
                           [nil nil nil t t t t t t t])))
  (should (equal '(1 2 3 4 5 6 7 8 9 0)
                 (window-numbering-calculate-left
                  [nil nil nil nil nil nil nil nil nil nil])))
  )
