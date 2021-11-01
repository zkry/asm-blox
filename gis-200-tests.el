;;; gis-200-tests.el --- Tests for the application -*- lexical-binding: t -*-

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
(require 'gis-200)


(defconst gis-200-test--gameboard-simple-move
  `((,(gis-200--code-cell-init "((mov 1 right))" 0 0) ,(gis-200--code-cell-init "((mov left down))" 1 0))
    (,(gis-200--code-cell-init "((mov right up))" 0 1) ,(gis-200--code-cell-init "((mov up left))" 1 1))))

(defconst gis-200-test--gameboard-nil-move
  `((,(gis-200--code-cell-init "((mov 10 right) (mov 1 right))" 0 0) ,(gis-200--code-cell-init "((mov left nil) (mov left down))" 1 0))
    (,(gis-200--code-cell-init "((mov right up))" 0 1) ,(gis-200--code-cell-init "((mov up left))" 1 1))))

(defun gis-200-test--probe (x y port)
  "Returns the value on the output PORT of cell at coords (X, Y)."
  (let ((cell (gis-200--fetch-cell (list x y))))
    (gis-200-code-cell-get-port cell port)))

(defun gis-200-test--cell-string (cell)
  "Returns the value on the output PORT of cell at coords (X, Y)."
  (format "%s@%s: %s %s %s %s"
          (gis-200-code-cell-coord cell)
          (gis-200-code-cell-pc cell)
          (gis-200-code-cell-up cell)
          (gis-200-code-cell-right cell)
          (gis-200-code-cell-down cell)
          (gis-200-code-cell-left cell)))

(defun gis-200-test--debug-board ()
  "Returns the value on the output PORT of cell at coords (X, Y)."
  (seq-do (lambda (row)
            (print "")
            (seq-do (lambda (cell)
                      (prin1 (gis-200-test--cell-string cell)))
                    row))
          gis-200--gameboard)
  (print ""))

(ert-deftest gis-200-test-mov ()
  "Test conversion of !!str to JSON scalar"
  (should (= (progn
               (setq gis-200--gameboard gis-200-test--gameboard-simple-move)
               (dotimes (x 4)
                 (gis-200--board-step))
               (gis-200-test--probe 0 1 'up))
             1))
  (should (= (progn
               (setq gis-200--gameboard gis-200-test--gameboard-nil-move)
               (dotimes (x 6)
                 (gis-200--board-step))
               (gis-200-test--probe 0 1 'up))
              1)))

(gis-200-test--debug-board)

(provide 'gis-200-tests)

;;; gis-200-tests.el ends here
