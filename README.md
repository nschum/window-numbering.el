window-numbering
================

Numbered window shortcuts for Emacs

[![Build Status](https://travis-ci.org/nschum/window-numbering.el.png?branch=master)](https://travis-ci.org/nschum/window-numbering.el)

Enable `window-numbering-mode` and use M-1 through M-0 to navigate.

If you want to affect the numbers, use window-numbering-before-hook or
window-numbering-assign-func.
For instance, to always assign the calculator window the number 9, add the
following to your .emacs:

    (setq window-numbering-assign-func
          (lambda () (when (equal (buffer-name) "*Calculator*") 9)))
