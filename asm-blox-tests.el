;;; asm-blox-tests.el --- Tests for the application -*- lexical-binding: t -*-

;; This file is not part of GNU Emacs

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


;;; Commentary:

;; This package contains unit tests

;;; Code:

(require 'ert)
(require 'asm-blox)


(defconst asm-blox-test--gameboard-simple-move
  `((,(asm-blox--code-cell-init "((mov 1 right))" 0 0) ,(asm-blox--code-cell-init "((mov left down))" 1 0))
    (,(asm-blox--code-cell-init "((mov right up))" 0 1) ,(asm-blox--code-cell-init "((mov up left))" 1 1))))

(defconst asm-blox-test--gameboard-nil-move
  `((,(asm-blox--code-cell-init "((mov 10 right) (mov 1 right))" 0 0) ,(asm-blox--code-cell-init "((mov left nil) (mov left down))" 1 0))
    (,(asm-blox--code-cell-init "((mov right up))" 0 1) ,(asm-blox--code-cell-init "((mov up left))" 1 1))))

(defun asm-blox-test--probe (x y port)
  "Returns the value on the output PORT of cell at coords (X, Y)."
  (let ((cell (asm-blox--fetch-cell (list x y))))
    (asm-blox-code-cell-get-port cell port)))

(defun asm-blox-test--cell-string (cell)
  "Returns the value on the output PORT of cell at coords (X, Y)."
  (format "%s@%s: %s %s %s %s"
          (asm-blox-code-cell-coord cell)
          (asm-blox-code-cell-pc cell)
          (asm-blox-code-cell-up cell)
          (asm-blox-code-cell-right cell)
          (asm-blox-code-cell-down cell)
          (asm-blox-code-cell-left cell)))

(defun asm-blox-test--debug-board ()
  "Returns the value on the output PORT of cell at coords (X, Y)."
  (seq-do (lambda (row)
            (print "")
            (seq-do (lambda (cell)
                      (prin1 (asm-blox-test--cell-string cell)))
                    row))
          asm-blox--gameboard)
  (print ""))

(ert-deftest asm-blox-test-mov ()
  "Test conversion of !!str to JSON scalar"
  (should (= (progn
               (setq asm-blox--gameboard asm-blox-test--gameboard-simple-move)
               (dotimes (x 4)
                 (asm-blox--board-step))
               (asm-blox-test--probe 0 1 'up))
             1))
  (should (= (progn
               (setq asm-blox--gameboard asm-blox-test--gameboard-nil-move)
               (dotimes (x 6)
                 (asm-blox--board-step))
               (asm-blox-test--probe 0 1 'up))
              1)))

(asm-blox-test--debug-board)

(provide 'asm-blox-tests)

;;; asm-blox-tests.el ends here
