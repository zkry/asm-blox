;;; asm-blox-puzzles.el --- Puzzle Definitions for asm-blox -*- lexical-binding: t -*-

;; Author: Zachary Romero
;; Maintainer: Zachary Romero
;; Version: 0.0.1
;; Homepage: https://github.com/zkry/asm-blox
;; Keywords: games

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

;; This file contains the definitions of the various game puzzles.

;;; Code:

(require 'cl-lib)

(defvar asm-blox-puzzles)
(declare-function asm-blox--problem-spec-create "asm-blox")
(declare-function asm-blox--cell-source-create "asm-blox")
(declare-function asm-blox--cell-sink-create "asm-blox")
(declare-function asm-blox--problem-spec-name "asm-blox")
(declare-function asm-blox--flatten-list "asm-blox")

(defun asm-blox-puzzles-list-of-lists-to-lisp (lists)
  "Return a list of LISTS from 0-terminated list of number lists."
  (reverse (car (seq-reduce (lambda (acc x)
                              (let ((lol (car acc))
                                    (curr-list (cadr acc)))
                                (if (= x 0)
                                    (list (cons (reverse curr-list) lol) '())
                                  (list lol (cons  x curr-list)))))
                            lists
                            (list '() '())))))

(defun asm-blox-puzzles-random-list-of-lists (&optional limit)
  "Generate list of 0-terminated lists as helper.
If LIMIT is non-nil, the number generated will be less-than it."
  (let* ((nums (seq-map (lambda (_) (if limit
                                        (1+ (random limit))
                                      (1+ (random 998))))
                        (make-list 40 nil)))
         (breaks (seq-map (lambda (_)
                            (random 5))
                          (make-list 40 nil)))
         (switched nil)
         (i 0))
    (seq-mapn (lambda (a b)
                     (setq i (1+ i))
                     (if (or (= i 40)
                             (and (not (= i 39))
                                  (not (= i 1))
                                  (and (not switched) (= b 0))))
                         (progn
                           (setq switched t)
                           0)
                       (setq switched nil)
                       a))
              nums breaks)))

(defun asm-blox-puzzles--stack-machine-solver (args ops)
  "Simulate the stack-machine with ARGS and OPS to generate solution."
  (let ((args (seq-reverse args))
        (result))
    (dolist (op ops)
      (pcase op
        (0 (setq result (append result (list (car args))))
           (setq args (cdr args)))
        (1 (setq args (cons (+ (car args) (cadr args)) (cddr args))))
        (2 (setq args (cons (- (car args)) (cdr args))))))
    result))

(defun asm-blox-puzzles--stack-machine ()
  "Generate a problem for simulating a simple stack machine."
  (let* ((args (seq-map (lambda (_) (random 10)) (make-list 40 nil)))
         (ops (append (seq-map (lambda (_) (random 3)) (make-list 10 nil)) (list 0)))
         (result (asm-blox-puzzles--stack-machine-solver args ops)))
    (asm-blox--problem-spec-create
     :name "Stack Machine"
     :difficulty 'hard
     :sources (list (asm-blox--cell-source-create :row 0
                                                  :col -1
                                                  :data ops
                                                  :name "O")
                    (asm-blox--cell-source-create :row 0
                                                  :col 4
                                                  :data args
                                                  :name "A"))
     :sinks
     (list (asm-blox--cell-sink-create
            :row 3
            :col 1
            :expected-data result
            :name "T"))
     :description "Read all 40 values from A, pushing them on a
stack so that the last number in A is at the top of the stack.
One by one, read an operation from O and do the following based on it's value:
0 -> send the top value of the stack to T.
1 -> pop the top two values of the stack, add them, and push onto the stack
2 -> pop the top value of the stack, multiply it by -1, and push onto the stack.")))

(defun asm-blox-puzzles--triangle-area ()
  "Generate a problem of determining area of triangle."
  (let* ((bases (seq-map (lambda (_) (* (1+ (random 10)) 2)) (make-list 40 nil)))
         (heights (seq-map (lambda (_) (1+ (random 20))) (make-list 40 nil)))
         (areas (seq-mapn (lambda (b h) (/ (* b h) 2)) bases heights)))
    (asm-blox--problem-spec-create
     :name "Triangle Area"
     :difficulty 'easy
     :sources (list (asm-blox--cell-source-create :row 3
                                                  :col 2
                                                  :data bases
                                                  :name "B")
                    (asm-blox--cell-source-create :row 1
                                                  :col 4
                                                  :data heights
                                                  :name "H"))
     :sinks
     (list (asm-blox--cell-sink-create
            :row -1
            :col 1
            :expected-data areas
            :name "A"))
     :description "Read base and height of a right-triangle
from B and H respectively.  Send the area of right-triangle to A")))

(defun asm-blox-puzzles--delete-word ()
  "Generate a problem of deleting a word."
  (let* ((words '("chair" "pen" "book" "camera" "note" "printer" "cable"
                  "square" "thought" "mouse" "alarm" "case" "lamp" "bed"))
         (word-ct (length words))
         (l1 (string-join (list (nth (random word-ct) words)
                                           (nth (random word-ct) words)
                                           (nth (random word-ct) words)) " "))
         (l2 (string-join (list (nth (random word-ct) words)
                                           (nth (random word-ct) words)
                                           (nth (random word-ct) words)) " "))
         (l3 (string-join (list (nth (random word-ct) words)
                                           (nth (random word-ct) words)
                                           (nth (random word-ct) words)) " "))
         (text (string-join (list l1 l2 l3) "\n"))
         (del-idx (random 9))
         (new-text (with-temp-buffer
                     (insert text)
                     (goto-char (point-min))
                     (dotimes (_ del-idx)
                       (forward-word 1)
                       (forward-char 1))
                     (kill-word 1)
                     (buffer-string))))
    (asm-blox--problem-spec-create
     :name "Delete Word"
     :difficulty 'hard
     :sources (list (asm-blox--cell-source-create :row -1
                                                  :col 3
                                                  :data (list del-idx)
                                                  :name "I"))
     :sinks
     (list (asm-blox--cell-sink-create
            :row 1
            :col 5
            :expected-data nil
            :name "O"
            :default-editor-text text
            :editor-point 0
            :expected-text
            new-text))
     :description "<editor> Read 0-based index from I. Delete
that number word in the text.")))

;; TODO: give different inputs
(defun asm-blox-puzzles--indentation ()
  "Generate a problem of indenting a code sequence properly."
  (asm-blox--problem-spec-create
   :name "Indentation I"
   :difficulty 'medium
   :sinks
   (list (asm-blox--cell-sink-create
          :row 1
          :col 5
          :expected-data nil
          :name "O"
          :default-editor-text
          "func main () {\nfmt.Println(\"hello world\")\nreturn\n}"
          :editor-point 1
          :expected-text
          "func main () {\n  fmt.Println(\"hello world\")\n  return\n}"))
   :description "<editor> Edit text to match the target."))

(defun asm-blox-puzzles--number-sum ()
  "Generate a problem of calculating y=x(x+1)/2."
  (let* ((input (seq-map (lambda (_) (+ 1 (random 10))) (make-list 40 nil)))
         (expected-output (seq-map (lambda (x) (/ (* x (+ 1 x)) 2))input)))
    (asm-blox--problem-spec-create
     :name "Number Sum"
     :difficulty 'easy
     :sources (list (asm-blox--cell-source-create :row -1
                                                 :col 3
                                                 :data input
                                                 :name "I"))
     :sinks
     (list (asm-blox--cell-sink-create :row 2
                                      :col 4
                                      :expected-data expected-output
                                      :name "O"
                                      :editor-text nil
                                      :editor-point nil
                                      :expected-text nil))
     :description "Read a number from I, send to O the sum of numbers
from 0 to the read number. (ex. 3->6, 4->10, 5->15)")))

(defun asm-blox-puzzles--meeting-point ()
  "Generate a problem of finding the point that minimizes movement."
  (let* ((input (seq-map (lambda (_) (+ 1 (random 10))) (make-list 10 nil)))
         (expected-output
          (list (cl-loop for i from 1 to 1000
                         minimize (cl-loop for d in input
                                           sum (abs (- i d)))))))
    (asm-blox--problem-spec-create
     :name "Meeting point"
     :difficulty 'hard
     :sources (list (asm-blox--cell-source-create :row 1
                                                 :col -1
                                                 :data input
                                                 :name "N"))
     :sinks
     (list (asm-blox--cell-sink-create :row 2
                                      :col 4
                                      :expected-data expected-output
                                      :name "O"
                                      :editor-text nil
                                      :editor-point nil
                                      :expected-text nil))
     :description "Read the 10 numbers from N (n1, n2, ..., n40).
Send a number x which minimizes the equation
(cl-loop for n in N
         sum (abs (- n x)))")))

(defun asm-blox-puzzles--simple-graph ()
  "Generate a problem for the user to draw a simple graph."
  (let* ((input (seq-map (lambda (_) (+ 1 (random 10))) (make-list 10 nil)))
         (expected-text (string-join
                         (seq-map (lambda (x) (make-string x ?#)) input) "\n")))
    (asm-blox--problem-spec-create
     :name "Simple Graph"
     :difficulty 'medium
     :sources (list (asm-blox--cell-source-create :row 1
                                                 :col -1
                                                 :data input
                                                 :name "A"))
     :sinks
     (list (asm-blox--cell-sink-create :row 1
                                      :col 5
                                      :expected-data nil
                                      :name "O"
                                      :editor-text ""
                                      :editor-point 1
                                      :expected-text expected-text))
     :description
     "<editor> Read a number from A,
 draw a line with that many '#' characters.")))

(defun asm-blox-puzzles--hello-world ()
  "Generate a problem involving writing Hello World to the screen."
  (asm-blox--problem-spec-create
   :name "Editor Basics"
   :difficulty 'easy
   :sources (list )
   :sinks
   (list (asm-blox--cell-sink-create :row 1
                                    :col 5
                                    :expected-data nil
                                    :name "O"
                                    :editor-text "01"
                                    :editor-point 3
                                    :expected-text "Hello World"))
   :description "<editor> Write the string \"Hello World\" to the editor."))

(defun asm-blox-puzzles--upcase ()
  "Generate a problem involving upcasing characters."
  (let* ((input-1 (seq-map (lambda (_)
                             (+ (random 95) 32))
                           (make-list 40 nil)))
         (expected (string-to-list (upcase (apply #'string input-1)))))
    (asm-blox--problem-spec-create
     :name "Upcase"
     :difficulty 'easy
     :sources (list (asm-blox--cell-source-create :row 1
                                                 :col -1
                                                 :data input-1
                                                 :name "C"))
     :sinks
     (list (asm-blox--cell-sink-create :row 1
                                      :col 4
                                      :expected-data expected
                                      :name "O"))
     :description "Read a character from C and send it to O,
upcasing it if it is a lowercase letter.")))

(defun asm-blox-puzzles--inc-ct ()
  "Generate a simple addition problem."
  (let* ((input-1 (append (seq-map (lambda (_)
                                     (random 999))
                                   (make-list 39 nil))
                          (list 0)))
         (expected (list (seq-reduce #'+
                          (seq-mapn (lambda (a b)
                                      (if (> b a) 1 0))
                                    input-1
                                    (cdr input-1))
                          0))))
    (asm-blox--problem-spec-create
     :name "Increment Cout"
     :difficulty 'medium
     :sources (list (asm-blox--cell-source-create :row 1
                                                 :col -1
                                                 :data input-1
                                                 :name "I"))
     :sinks
     (list (asm-blox--cell-sink-create :row 1
                                      :col 4
                                      :expected-data expected
                                      :name "O"))
     :description
     "Return the number of times subsequent values of I increase.
ex. 1 2 0 5 6 4
     + - + + -     3 increses")))

(defun asm-blox-puzzles--list-reverse ()
  "Generate a simple list reverse problem."
  (let* ((input-1 (asm-blox-puzzles-random-list-of-lists))
         (lists (asm-blox-puzzles-list-of-lists-to-lisp input-1))
         (expected (asm-blox--flatten-list (seq-map (lambda (l)
                                       (append (reverse l) (list 0)))
                                     lists))))
    (asm-blox--problem-spec-create
     :name "List Reverse"
     :difficulty 'medium
     :sources (list (asm-blox--cell-source-create :row -1
                                                 :col 2
                                                 :data input-1
                                                 :name "L"))
     :sinks
     (list (asm-blox--cell-sink-create :row 3
                                      :col 1
                                      :expected-data expected
                                      :name "R"))
     :description
     "Lists are 0 terminated.
Read a list from L, reverse it, and send it to R (terminating it with 0).")))

(defun asm-blox-puzzles--list-length ()
  "Generate a simple addition problem."
  (let* ((nums)
         (lengths))
    (while (< (length nums) 30)
      (let ((len (1+ (random 10))))
        (setq lengths (cons len lengths))
        (setq nums (append nums
                           (seq-map (lambda (_)
                                      (1+ (random 10)))
                                    (make-list len nil))
                           (list 0)))))
    (setq lengths (reverse lengths))
    lengths
    (asm-blox--problem-spec-create
     :name "List Length"
     :difficulty 'medium
     :sources (list (asm-blox--cell-source-create :row 1
                                                 :col -1
                                                 :data nums
                                                 :name "I"))
     :sinks
     (list (asm-blox--cell-sink-create :row 1
                                      :col 4
                                      :expected-data lengths
                                      :name "O"))
     :description "Lists are 0 terminated. Read a list from I,
calculate its length and send it to O.")))

(defun asm-blox-puzzles--turing ()
  "Generate a simple Brain****-like puzzle."
  (let* ((input-1 (list ?> ?> ?> ?+ ?+ ?. ?. ?< ?+ ?. ?> ?. ?+ ?. ?> ?> ?.))
         (expected (list 2 2 1 2 3 0)))
    (asm-blox--problem-spec-create
     :name "Turing"
     :difficulty 'hard
     :sources (list (asm-blox--cell-source-create :row 0
                                                 :col -1
                                                 :data input-1
                                                 :name "X"))
     :sinks
     (list (asm-blox--cell-sink-create :row 1
                                      :col 4
                                      :expected-data expected
                                      :name "O"))
     :description
     "Read a number from X. Implement a machine that moves a head
on a tape with values of all zero.
 - If X is the ASCII character '<' move the head one position to the left
 - If X is the ASCII character '>' move the head one position to the right
 - If X is the ASCII character '+' increment the value of the cell
 - If X is the ASCII character '.' send the current value at the tape to O.

NOTE The head will go no more than +-10 spaces
     from where the head starts off.")))


(defun asm-blox-puzzles--merge-step ()
  "Generate a simple addition problem."
  (let* ((input-1 (seq-sort #'< (seq-map (lambda (_) (random 100))
                                         (make-list 20 nil))))
         (input-2 (seq-sort #'< (seq-map (lambda (_) (random 100))
                                         (make-list 20 nil))))
         (expected (seq-sort #'< (append input-1 input-2))))
    (asm-blox--problem-spec-create
     :name "Merge Step"
     :difficulty 'hard
     :sources (list (asm-blox--cell-source-create :row 0
                                                 :col -1
                                                 :data input-1
                                                 :name "A")
                    (asm-blox--cell-source-create :row 2
                                                 :col -1
                                                 :data input-2
                                                 :name "B"))
     :sinks
     (list (asm-blox--cell-sink-create :row 1
                                      :col 4
                                      :expected-data expected
                                      :name "C"))
     :description "Numbers in A and B are sorted. Read numbers from A and B,
combine them sorted and send it them to C.")))

(defun asm-blox-puzzles--filter ()
  "Generate a simple addition problem."
  (let* ((input-1 (seq-map (lambda (_) (random 100)) (make-list 40 nil)))
         (expected (seq-map (lambda (x)
                              (if (= (mod x 2) 0)
                                  0
                                x))
                               input-1)))
    (asm-blox--problem-spec-create
     :name "Number Filter"
     :difficulty 'easy
     :sources (list (asm-blox--cell-source-create :row 1
                                                 :col -1
                                                 :data input-1
                                                 :name "I"))
     :sinks
     (list (asm-blox--cell-sink-create :row 1
                                      :col 4
                                      :expected-data expected
                                      :name "O"))
     :description
     "Read a value from I. If it is even send 0 to O, else send the value.")))

(defun asm-blox-puzzles--clock ()
  "Generate a simple addition problem."
  (let* ((input-1 (seq-map (lambda (_) (random 24)) (make-list 40 nil)))
         (expected (cdr (seq-reverse
                         (seq-reduce (lambda (acc x)
                                       (let ((top (car acc)))
                                         (cons (mod (+ top x) 24)
                                               acc)))
                                     input-1
                                     '(0))))))
    (asm-blox--problem-spec-create
     :name "Clock Hours"
     :difficulty 'easy
     :sources (list (asm-blox--cell-source-create :row 1
                                                 :col -1
                                                 :data input-1
                                                 :name "H"))
     :sinks
     (list (asm-blox--cell-sink-create :row 3
                                      :col 1
                                      :expected-data expected
                                      :name "T"))
     :description "On a clock with hours 0 to 23, read a value from H and add
that value to the current time which starts at 0.

Write the current time to T for every time you move the current time.")))

(defun asm-blox-puzzles--add ()
  "Generate a simple addition problem."
  (let* ((input-1 (seq-map (lambda (_) (random 10)) (make-list 40 nil)))
         (input-2 (seq-map (lambda (_) (random 10)) (make-list 40 nil)))
         (input-3 (seq-map (lambda (_) (random 10)) (make-list 40 nil)))
         (expected (seq-mapn #'+ input-1 input-2 input-3)))
    (asm-blox--problem-spec-create
     :name "Number Addition"
     :difficulty 'easy
     :sources (list (asm-blox--cell-source-create :row -1
                                                 :col 0
                                                 :data input-1
                                                 :name "A")
                    (asm-blox--cell-source-create :row -1
                                                 :col 1
                                                 :data input-2
                                                 :name "B")
                    (asm-blox--cell-source-create :row -1
                                                 :col 2
                                                 :data input-3
                                                 :name "C"))
     :sinks
     (list (asm-blox--cell-sink-create :row 3
                                      :col 1
                                      :expected-data expected
                                      :name "S"))
     :description
     "Take input from A, B, and C, add the three together, and send it to S.")))

(defun asm-blox-puzzles--number-sorter ()
  "Generate problem of comparing two numbers, send them in different places."
  (let* ((input-1 (seq-map (lambda (_) (random 10))
                           (make-list 40 nil)))
         (input-2 (seq-map (lambda (_) (random 10))
                           (make-list 40 nil)))
         (expected-1 (seq-mapn (lambda (a b) (if (> a b) a 0))
                               input-1 input-2))
         (expected-2 (seq-mapn (lambda (a b) (if (> b a) b 0))
                               input-1 input-2)))
    (asm-blox--problem-spec-create
     :name "Number Chooser"
     :difficulty 'easy
     :sources (list (asm-blox--cell-source-create :row -1
                                                 :col 0
                                                 :data input-1
                                                 :name "A")
                    (asm-blox--cell-source-create :row -1
                                                 :col 1
                                                 :data input-2
                                                 :name "B"))
     :sinks
     (list (asm-blox--cell-sink-create :row 0
                                      :col 4
                                      :expected-data expected-1
                                      :name "L")
           (asm-blox--cell-sink-create :row 2
                                      :col 4
                                      :expected-data expected-2
                                      :name "R"))
     :description "Take an input from A and B. If A>B then send A to L, 0 to R;
If B>A then send B to R, 0 to L. If A=B send 0 to L and R.")))

(defun asm-blox-puzzles--constant ()
  "Generate a simple addition problem."
  (let* ((expected (make-list 40 1)))
    (asm-blox--problem-spec-create
     :name "Constant Generator"
     :difficulty 'tutorial
     :sources (list )
     :sinks
     (list (asm-blox--cell-sink-create :row 0
                                      :col 4
                                      :expected-data expected
                                      :name "N"))
     :description "Repeatedly send the number 1 to N. There are no inputs.")))

(defun asm-blox-puzzles--identity ()
  "Generate a simple addition problem."
  (let* ((input-1 (seq-map (lambda (_) (random 10)) (make-list 40 nil)))
         (expected input-1))
    (asm-blox--problem-spec-create
     :name "Identity"
     :difficulty 'tutorial
     :sources (list (asm-blox--cell-source-create :row -1
                                                 :col 0
                                                 :data input-1
                                                 :name "X"))
     :sinks
     (list (asm-blox--cell-sink-create :row 3
                                      :col 3
                                      :expected-data expected
                                      :name "X"))
     :description
     "Take an input from the input X and send it to the output X.")))

(defun asm-blox-puzzles--diagnostic-test ()
  "Generate a problem."
  (let* ((input-1 (seq-map (lambda (_) (random 10)) (make-list 40 nil)))
         (input-2 (seq-map (lambda (_) (random 10)) (make-list 40 nil))))
    (asm-blox--problem-spec-create
     :name "Diagnostic Test"
     :difficulty 'tutorial
     :sources (list (asm-blox--cell-source-create :row 0
                                                 :col -1
                                                 :data input-1
                                                 :name "A")
                    (asm-blox--cell-source-create :row 2
                                                 :col -1
                                                 :data input-2
                                                 :name "B"))
     :sinks
     (list (asm-blox--cell-sink-create :row 0
                                      :col 4
                                      :expected-data input-1
                                      :name "X")
           (asm-blox--cell-sink-create :row 2
                                      :col 4
                                      :expected-data input-2
                                      :name "Y"))
     :description
     "Send data from A to X. Send data from B to Y.")))

(defun asm-blox-puzzles--signal-amplifier ()
  "Generate a problem."
  (let* ((input (seq-map (lambda (_) (random 10)) (make-list 40 nil)))
         (output (seq-map (lambda (x) (* 2 x)) input)))
    (asm-blox--problem-spec-create
     :name "Signal Amplifier"
     :difficulty 'tutorial
     :sources (list (asm-blox--cell-source-create :row -1
                                                  :col 2
                                                  :data input
                                                  :name "I"))
     :sinks
     (list (asm-blox--cell-sink-create :row 3
                                       :col 1
                                       :expected-data output
                                       :name "O"))
     :description
     "Read a value from I. Double it. Send that to O.")))

(defun asm-blox-puzzles--differential-converter ()
  "Generate a problem."
  (let* ((input-a (seq-map (lambda (_) (random 10)) (make-list 40 nil)))
         (input-b (seq-map (lambda (_) (random 10)) (make-list 40 nil)))
         (output-p (seq-mapn (lambda (a b) (- a b)) input-a input-b))
         (output-n (seq-mapn (lambda (a b) (- b a)) input-a input-b)))
    (asm-blox--problem-spec-create
     :name "Differential Converter"
     :difficulty 'tutorial
     :sources (list (asm-blox--cell-source-create :row -1
                                                  :col 1
                                                  :data input-a
                                                  :name "A")
                    (asm-blox--cell-source-create :row -1
                                                  :col 2
                                                  :data input-b
                                                  :name "B"))
     :sinks
     (list (asm-blox--cell-sink-create :row 3
                                       :col 1
                                       :expected-data output-p
                                       :name "P")
           (asm-blox--cell-sink-create :row 3
                                       :col 2
                                       :expected-data output-n
                                       :name "N"))
     :description
     "Read a value from A and B. Send A - B to P. Send B - A to N.")))

(defun asm-blox-puzzles--signal-comparator ()
  "Generate a problem."
  (let* ((input (seq-map (lambda (_) (- (random 10) 5)) (make-list 40 nil)))
         (output-g (seq-mapn (lambda (x) (if (> x 0) 1 0)) input))
         (output-e (seq-mapn (lambda (x) (if (= x 0) 1 0)) input))
         (output-l (seq-mapn (lambda (x) (if (< x 0) 1 0)) input)))
    (asm-blox--problem-spec-create
     :name "Signal Comparator"
     :difficulty 'tutorial
     :sources (list (asm-blox--cell-source-create :row -1
                                                  :col 0
                                                  :data input
                                                  :name "I"))
     :sinks
     (list (asm-blox--cell-sink-create :row 3
                                       :col 1
                                       :expected-data output-g
                                       :name "G")
           (asm-blox--cell-sink-create :row 3
                                       :col 2
                                       :expected-data output-e
                                       :name "E")
           (asm-blox--cell-sink-create :row 3
                                       :col 3
                                       :expected-data output-l
                                       :name "L"))
     :description
     "Read a value from I.
If I > 0 send 1 to G.
If I < 0 send 1 to L.
If I = 0 send 1 to E.
When sending 1 to output, send 0 to the other two output ports.")))

(defun asm-blox-puzzles--sequence-generator ()
  "Generate a problem."
  (let* ((input-1 (seq-map (lambda (_) (1+ (* (random 10) 2))) (make-list 8 nil)))
         (input-2 (seq-map (lambda (_) (* (random 10) 2)) (make-list 8 nil)))
         (output (asm-blox--flatten-list
                  (seq-mapn (lambda (a b)
                              (if (< a b)
                                  (list a b 0)
                                (list b a 0)))
                            input-1 input-2))))
    (asm-blox--problem-spec-create
     :name "Sequence Generator"
     :difficulty 'easy
     :sources (list (asm-blox--cell-source-create :row -1
                                                  :col 1
                                                  :data input-1
                                                  :name "A")
                    (asm-blox--cell-source-create :row -1
                                                  :col 2
                                                  :data input-2
                                                  :name "B"))
     :sinks
     (list (asm-blox--cell-sink-create :row 3
                                       :col 2
                                       :expected-data output
                                       :name "O"))
     :description
     "Read a value from A and B.
Send the lesser of the two to O.
Send the other value to O.
Send 0 to O.")))

(defun asm-blox-puzzles--sequence-counter ()
  "Generate a problem."
  (let* ((input (asm-blox-puzzles-random-list-of-lists 30))
         (lists (asm-blox-puzzles-list-of-lists-to-lisp input))
         (output-sum (seq-map
                      (lambda (list)
                        (apply #'+ list))
                      lists))
         (output-length (seq-map
                         (lambda (list)
                           (length list))
                         lists)))
    (asm-blox--problem-spec-create
     :name "Sequence Counter"
     :difficulty 'easy
     :sources (list (asm-blox--cell-source-create :row -1
                                                  :col 1
                                                  :data input
                                                  :name "I"))
     :sinks
     (list (asm-blox--cell-sink-create :row 3
                                       :col 2
                                       :expected-data output-sum
                                       :name "S")
           (asm-blox--cell-sink-create :row 3
                                       :col 3
                                       :expected-data output-length
                                       :name "L"))
     :description
     "Read a 0-terminated sequence from I.
Write the length of the sequence to L.
Write the sum of the sequence to S.")))

(defun asm-blox-puzzles--signal-edge-detector ()
  "Generate a problem."
  (let* ((input (asm-blox-puzzles-random-list-of-lists 100))
         (output (seq-mapn (lambda (at prev)
                             (if (>= at (+ prev 10))
                                 1
                               0))
                           input
                           (cons (car input) input))))
    (asm-blox--problem-spec-create
     :name "Signal Edge Detector"
     :difficulty 'easy
     :sources (list (asm-blox--cell-source-create :row -1
                                                  :col 1
                                                  :data input
                                                  :name "I"))
     :sinks
     (list (asm-blox--cell-sink-create :row 3
                                       :col 2
                                       :expected-data output
                                       :name "O"))
     :description
     "Read a value from I, comparing it with the previous value.
If the value increased by 10 or more, write 1 to O.
Otherwise write 0 to O.
Always write 0 for the first input.")))

(defun asm-blox-puzzles--make-interrupt-handler-seq ()
  "Generated an interrupt sequence."
  (let ((state 'zero)
        (res '()))
    (while (< (length res) 40)
      (let ((seq-length (1+ (random 9))))
        (when (> (+ seq-length (length res)) 40)
          (setq seq-length (- 40 (length res))))
        (setq res (append res (make-list seq-length (if (eql state 'zero) 0 1))))
        (setq state (if (eql state 'zero) 'one 'zero))))
    res))

(defun asm-blox-puzzles--make-interrupt-handler-solution (a b c d)
  "Return the solution to interrupt handler given sequences A, B, C and D."
  (catch 'result
   (cl-labels
       ((compare (x prev) (if (and (= prev 0) (= x 1)) 1 0)))
     (seq-mapn
      (lambda (a pa b pb c pc d pd)
        (let ((ress (list (compare a pa)
                          (compare b pb)
                          (compare c pc)
                          (compare d pd))))
          (when (> (apply #'+ ress) 1)
            (throw 'result nil))
          (if (= (apply #'+ ress) 0)
              0
            (seq-find #'identity
                      (seq-mapn (lambda (x idx)
                                  (if (= 1 x)
                                      idx
                                    nil))
                                ress '(1 2 3 4))))))
      (cdr a) a (cdr b) b (cdr c) c (cdr d) d))))

(defun asm-blox-puzzles--interrupt-handler ()
  "Generate a problem."
  (let* ((input-a)
         (input-b)
         (input-c)
         (input-d)
         (res))
    ;; kindof hacky....
    (while (not res)
      (setq input-a (asm-blox-puzzles--make-interrupt-handler-seq))
      (setq input-b (asm-blox-puzzles--make-interrupt-handler-seq))
      (setq input-c (asm-blox-puzzles--make-interrupt-handler-seq))
      (setq input-d (asm-blox-puzzles--make-interrupt-handler-seq))
      (setq res (asm-blox-puzzles--make-interrupt-handler-solution
                 input-a input-b input-c input-d)))
    (asm-blox--problem-spec-create
     :name "Interrupt Handler"
     :difficulty 'easy
     :sources (list (asm-blox--cell-source-create :row -1
                                                  :col 0
                                                  :data input-a
                                                  :name "1")
                    (asm-blox--cell-source-create :row -1
                                                  :col 1
                                                  :data input-b
                                                  :name "2")
                    (asm-blox--cell-source-create :row -1
                                                  :col 2
                                                  :data input-c
                                                  :name "3")
                    (asm-blox--cell-source-create :row -1
                                                  :col 3
                                                  :data input-d
                                                  :name "4"))
     :sinks
     (list (asm-blox--cell-sink-create :row 3
                                       :col 2
                                       :expected-data res
                                       :name "O"))
     :description
     "Read inputs 1, 2, 3, and 4.
Whenever an input sequence changes from 0 to 1
   write the input port number to O.

Example:
       /---- Input ports
1 2 3 4 | O <- Output
--------+--
0 0 0 0 | 0
0 1 0 0 | 2
0 1 0 0 | 0
0 1 0 1 | 4
1 1 0 0 | 1
1 1 0 0 | 0")))

(defun asm-blox-puzzles--signal-pattern-detector ()
  "Generate a problem."
  (let* ((input (seq-map (lambda (_)
                           (let ((res (random 5)))
                             (if (>= res 3)
                                 0
                               res)))
                         (make-list 40 nil)))
         (output (seq-mapn
                  (lambda (at p1 p2)
                    (if (= at p1 p2 0)
                        1
                      0))
                  input
                  (cons -1 input)
                  (cons -1 (cons -1 input)))))
    (if (< (apply #'+ output) 5)
        ;; hacky...
        (asm-blox-puzzles--signal-pattern-detector)
      (asm-blox--problem-spec-create
      :name "Signal Pattern Detector"
      :difficulty 'medium
      :sources (list (asm-blox--cell-source-create :row -1
                                                   :col 1
                                                   :data input
                                                   :name "I"))
      :sinks
      (list (asm-blox--cell-sink-create :row 3
                                        :col 2
                                        :expected-data output
                                        :name "O"))
      :description
      "Read a value from I.
Find the pattern 0, 0, 0:
- If the current value, and the two previous values are all 0,
   write 1.
- Otherwise write 0."))))

(defun asm-blox-puzzles--sequence-peak-detector ()
  "Generate a problem."
  (let* ((input (asm-blox-puzzles-random-list-of-lists 100))
         (input-list (asm-blox-puzzles-list-of-lists-to-lisp input))
         (output-n (seq-map (lambda (l)
                              (apply #'min l))
                            input-list))
         (output-x (seq-map (lambda (l)
                              (apply #'max l))
                            input-list)))
    (asm-blox--problem-spec-create
     :name "Sequence Peak Detector"
     :difficulty 'medium
     :sources (list (asm-blox--cell-source-create :row -1
                                                  :col 1
                                                  :data input
                                                  :name "I"))
     :sinks
     (list (asm-blox--cell-sink-create :row 3
                                       :col 2
                                       :expected-data output-n
                                       :name "N")
           (asm-blox--cell-sink-create :row 3
                                       :col 2
                                       :expected-data output-x
                                       :name "X"))
     :description
     "Read a 0-terminated sequence from I.
Write the minimum value of the sequence to N.
Write the maximum value of the sequence to X.")))

(defun asm-blox-puzzles--sequence-reverser ()
  "Generate a problem."
  (let* ((input (asm-blox-puzzles-random-list-of-lists 100))
         (input-list (asm-blox-puzzles-list-of-lists-to-lisp input))
         (reversed (asm-blox--flatten-list
                    (seq-map (lambda (l)
                               (append (reverse l) '(0)))
                             input-list))))
    (asm-blox--problem-spec-create
     :name "Sequence Reverser"
     :difficulty 'medium
     :sources (list (asm-blox--cell-source-create :row -1
                                                  :col 1
                                                  :data input
                                                  :name "I"))
     :sinks
     (list (asm-blox--cell-sink-create :row 3
                                       :col 2
                                       :expected-data reversed
                                       :name "R"))
     :description
     "Read a sequence from I. Reverse the sequence and write it to R (0-terminated).")))

(defun asm-blox-puzzles--signal-multiplier ()
  "Generate a problem."
  (let* ((input-a (seq-map (lambda (_) (random 10)) (make-list 40 nil)))
         (input-b (seq-map (lambda (_) (random 10)) (make-list 40 nil)))
         (output (seq-mapn (lambda (a b) (* a b)) input-a input-b)))
    (asm-blox--problem-spec-create
     :name "Signal Multiplier"
     :difficulty 'medium
     :sources (list (asm-blox--cell-source-create :row -1
                                                  :col 1
                                                  :data input-a
                                                  :name "A")
                    (asm-blox--cell-source-create :row -1
                                                  :col 2
                                                  :data input-b
                                                  :name "B"))
     :sinks
     (list (asm-blox--cell-sink-create :row 3
                                       :col 2
                                       :expected-data output
                                       :name "M"))
     :description
     "Read a value from A and B. Multiply the numbers. Send result to M."
     :banned-commands '(MUL))))

(defun asm-blox-puzzles--signal-window-filter ()
  "Generate a problem."
  (let* ((input (seq-map (lambda (_) (random 10)) (make-list 40 nil)))
         (output-3 (seq-mapn
                    (lambda (a b c)
                      (+ a b c))
                    input
                    (cons 0 input)
                    (cons 0 (cons 0 input))))
         (output-5 (seq-mapn
                    (lambda (a b c d e)
                      (+ a b c d e))
                    input
                    (cons 0 input)
                    (cons 0 (cons 0 input))
                    (append '(0 0 0) input)
                    (append '(0 0 0 0) input))))
    (asm-blox--problem-spec-create
     :name "Signal Window Filter"
     :difficulty 'hard
     :sources (list (asm-blox--cell-source-create :row -1
                                                  :col 1
                                                  :data input
                                                  :name "I"))
     :sinks
     (list (asm-blox--cell-sink-create :row 3
                                       :col 1
                                       :expected-data output-3
                                       :name "3")
           (asm-blox--cell-sink-create :row 3
                                       :col 2
                                       :expected-data output-5
                                       :name "5"))
     :description
     "Read a value from I.
Write the sum of the last 3 values to output port `3'.
Write the sum of the last 5 values to output port `5'.
Assume previous numbers are 0.")))

(defun asm-blox-puzzles--signal-divider ()
  "Generate a problem."
  (let* ((input-a (seq-map (lambda (_) (+ (random 10) 10)) (make-list 40 nil)))
         (input-b (seq-map (lambda (_) (+ (random 10) 2)) (make-list 40 nil)))
         (output-q (seq-mapn
                    (lambda (a b)
                      (/ a b))
                    input-a
                    input-b))
         (output-r (seq-mapn
                    (lambda (a b)
                      (% a b))
                    input-a
                    input-b)))
    (asm-blox--problem-spec-create
     :name "Signal Divider"
     :difficulty 'hard
     :sources (list (asm-blox--cell-source-create :row -1
                                                  :col 1
                                                  :data input-a
                                                  :name "A")
                    (asm-blox--cell-source-create :row -1
                                                  :col 2
                                                  :data input-b
                                                  :name "B"))
     :sinks
     (list (asm-blox--cell-sink-create :row 3
                                       :col 1
                                       :expected-data output-r
                                       :name "R")
           (asm-blox--cell-sink-create :row 3
                                       :col 2
                                       :expected-data output-q
                                       :name "Q"))
     :description
     "Read a value from A and B.
Send the quotient of A / B to Q.
Send the remainder of A / B to R."
     :banned-commands '(DIV REM))))

(defun asm-blox-puzzles--sequence-indexer ()
  "Generate a problem."
  (let* ((db (append (seq-map (lambda (_) (+ (random 500) 300)) (make-list 10 nil)) '(0)))
         (idxs (seq-map (lambda (_) (random 10)) (make-list 40 nil)))
         (output (seq-map
                  (lambda (idx)
                    (nth idx db))
                  idxs)))
    (asm-blox--problem-spec-create
     :name "Sequence Indexer"
     :difficulty 'hard
     :sources (list (asm-blox--cell-source-create :row -1
                                                  :col 1
                                                  :data db
                                                  :name "D")
                    (asm-blox--cell-source-create :row -1
                                                  :col 2
                                                  :data idxs
                                                  :name "X"))
     :sinks
     (list (asm-blox--cell-sink-create :row 3
                                       :col 1
                                       :expected-data output
                                       :name "V"))
     :description
     "Read a 0-terminated sequence from D,
  storing it to be accessable later on.
Read an index from X.
Send the Xth value read from D to output port V.

Example:
  D  X | V
-------+---
 100 0 | 100
 200 2 | 400
 400 1 | 200
   0 1 | 200
     0 | 100
")))

(defun asm-blox-puzzles--sequence-sorter ()
  "Generate a problem."
  (let* ((input (asm-blox-puzzles-random-list-of-lists 100))
         (input-list (asm-blox-puzzles-list-of-lists-to-lisp input))
         (output (asm-blox--flatten-list
                  (seq-map
                   (lambda (l)
                     (append (seq-sort #'< l) '(0)))
                   input-list))))
    (asm-blox--problem-spec-create
     :name "Sequence Sorter"
     :difficulty 'hard
     :sources (list (asm-blox--cell-source-create :row -1
                                                  :col 1
                                                  :data input
                                                  :name "I"))
     :sinks
     (list (asm-blox--cell-sink-create :row 3
                                       :col 2
                                       :expected-data output
                                       :name "O"))
     :description
     "Read a 0-terminated sequence from I.
Sort the sequence and write it to O (0-terminated).")))


(setq asm-blox-puzzles
      (list
       #'asm-blox-puzzles--indentation
       #'asm-blox-puzzles--constant
       #'asm-blox-puzzles--identity
       #'asm-blox-puzzles--add
       #'asm-blox-puzzles--filter
       #'asm-blox-puzzles--number-sum
       #'asm-blox-puzzles--number-sorter
       #'asm-blox-puzzles--clock
       #'asm-blox-puzzles--list-length
       #'asm-blox-puzzles--list-reverse
       #'asm-blox-puzzles--inc-ct
       #'asm-blox-puzzles--upcase
       #'asm-blox-puzzles--merge-step
       #'asm-blox-puzzles--hello-world
       #'asm-blox-puzzles--simple-graph
       #'asm-blox-puzzles--meeting-point
       #'asm-blox-puzzles--turing
       #'asm-blox-puzzles--stack-machine
       #'asm-blox-puzzles--delete-word
       #'asm-blox-puzzles--triangle-area

       #'asm-blox-puzzles--diagnostic-test
       #'asm-blox-puzzles--signal-amplifier
       #'asm-blox-puzzles--differential-converter
       #'asm-blox-puzzles--signal-comparator
       #'asm-blox-puzzles--sequence-generator
       #'asm-blox-puzzles--sequence-counter
       #'asm-blox-puzzles--signal-edge-detector
       #'asm-blox-puzzles--interrupt-handler
       #'asm-blox-puzzles--signal-pattern-detector
       #'asm-blox-puzzles--sequence-peak-detector
       #'asm-blox-puzzles--sequence-reverser
       #'asm-blox-puzzles--signal-multiplier
       #'asm-blox-puzzles--signal-window-filter
       #'asm-blox-puzzles--signal-divider
       #'asm-blox-puzzles--sequence-indexer
       #'asm-blox-puzzles--sequence-sorter))

(provide 'asm-blox-puzzles)

;;; asm-blox-puzzles.el ends here
