;;; asm-blox-test.el --- Tests for asm-blox game -*- lexical-binding: t -*-

;; Author: Zachary Romero

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

;; Tests for various asm-blox interactions.

;;; Code:

(require 'ert)
(require 'asm-blox)

(defun asm-blox-test--to-idx (row col)
  "Convert ROW and COL to a gameboard index."
  (+ (* row asm-blox--gameboard-col-ct) col))

(defun asm-blox-test--step ()
  (asm-blox--gameboard-step) (asm-blox--resolve-port-values))

(defun asm-blox-test--create-gameboard (cells)
  (let ((gameboard (make-vector (* asm-blox--gameboard-col-ct asm-blox--gameboard-row-ct)
                       nil)))
    (dolist (cell cells)
      (let ((row (nth 0 cell))
            (col (nth 1 cell))
            (text (nth 2 cell)))
        (let ((parse-result (asm-blox--parse-cell (list row col) text)))
          (if (asm-blox--cell-runtime-p parse-result)
              (setf (aref gameboard (asm-blox-test--to-idx row col)) parse-result)
            (let* ((asm (asm-blox--parse-tree-to-asm parse-result))
                   (asm (if (not (listp asm)) (list asm) asm))
                   (runtime (asm-blox--cell-runtime-create
                             :instructions asm
                             :pc 0
                             :stack '()
                             :row row
                             :col col)))
              (setf (aref gameboard (asm-blox-test--to-idx row col)) runtime))))))
    (dotimes (row asm-blox--gameboard-row-ct)
      (dotimes (col asm-blox--gameboard-col-ct)
        (let ((idx (+ (* row asm-blox--gameboard-col-ct) col)))
          (when (not (aref gameboard idx))
            (let* ((runtime (asm-blox--cell-runtime-create
                             :instructions nil
                             :pc 0
                             :stack '()
                             :row row
                             :col col)))
              (setf (aref gameboard idx) runtime))))))
    gameboard))

(ert-deftest asm-blox-wat-cells ()
  (let* ((gameboard (asm-blox-test--create-gameboard
                     '((0 0 "(const 1) (const 2) (const 3) (const 4)"))))
         (asm-blox--gameboard gameboard))
    (dotimes (_ 4)
      (asm-blox-test--step))
    (should
     (equal '(4 3 2 1) (asm-blox--cell-runtime-stack (aref asm-blox--gameboard 0)))))
  
  (let* ((gameboard (asm-blox-test--create-gameboard
                     '((0 0 "(const 1) (send down) (const 3) (send right)"))))
         (asm-blox--gameboard gameboard))
    (dotimes (_ 4)
      (asm-blox-test--step))
    (should
     (equal '() (asm-blox--cell-runtime-stack (asm-blox--cell-at-row-col 0 0))))
    (should
     (equal 1 (asm-blox--cell-runtime-down (asm-blox--cell-at-row-col 0 0))))
    (should
     (equal 3 (asm-blox--cell-runtime-right (asm-blox--cell-at-row-col 0 0)))))
  
  (let* ((gameboard (asm-blox-test--create-gameboard
                     '((0 0 "(const 1) (send down)")
                       (1 0 "(get up)"))))
         (asm-blox--gameboard gameboard))
    (dotimes (_ 4)
      (asm-blox--gameboard-step) (asm-blox--resolve-port-values))
    (should
     (equal '() (asm-blox--cell-runtime-stack (asm-blox--cell-at-row-col 0 0))))
    (should
     (equal '(1) (asm-blox--cell-runtime-stack (asm-blox--cell-at-row-col 1 0))))))



(provide 'asm-blox-test)

;;; asm-blox-test.el ends here
