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

(defvar asm-blox-puzzles)
(declare-function asm-blox--problem-spec-create "asm-blox")
(declare-function asm-blox--cell-source-create "asm-blox")
(declare-function asm-blox--cell-sink-create "asm-blox")
(declare-function asm-blox--problem-spec-name "asm-blox")
(declare-function asm-blox--flatten-list "asm-blox")

;; (defun asm-blox-puzzles-list-of-lists-to-lisp (lists)
;;   "Return a list of LISTS from 0-terminated list of number lists."
;;   (reverse
;;    (car
;;     (seq-reduce (lambda (acc x)
;;                   (let ((lol (car acc))
;;                         (curr-list (cadr acc)))
;;                     (if (= x 0)
;;                         (list (cons (reverse curr-list) lol) '())
;;                       (list lol (cons  x curr-list)))))
;;                 (list '() '())
;;                 lists))))

(defun asm-blox-puzzles-random-list-of-lists ()
  "Generate list of 0-terminated lists as helper."
  (let* ((nums (seq-map (lambda (_) (random 999)) (make-list 40 nil)))
         (breaks (seq-map (lambda (_) (random 5)) (make-list 40 nil)))
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
                                                  :idx 0
                                                  :name "O")
                    (asm-blox--cell-source-create :row 0
                                                  :col 4
                                                  :data args
                                                  :idx 0
                                                  :name "A"))
     :sinks
     (list (asm-blox--cell-sink-create
            :row 3
            :col 1
            :expected-data result
            :idx 0
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
                                                  :idx 0
                                                  :name "B")
                    (asm-blox--cell-source-create :row 1
                                                  :col 4
                                                  :data heights
                                                  :idx 0
                                                  :name "H"))
     :sinks
     (list (asm-blox--cell-sink-create
            :row -1
            :col 1
            :expected-data areas
            :idx 0
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
                                                  :idx 0
                                                  :name "I"))
     :sinks
     (list (asm-blox--cell-sink-create
            :row 1
            :col 5
            :expected-data nil
            :idx 0
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
          :idx 0
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
                                                 :idx 0
                                                 :name "I"))
     :sinks
     (list (asm-blox--cell-sink-create :row 2
                                      :col 4
                                      :expected-data expected-output
                                      :idx 0
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
                                                 :idx 0
                                                 :name "N"))
     :sinks
     (list (asm-blox--cell-sink-create :row 2
                                      :col 4
                                      :expected-data expected-output
                                      :idx 0
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
                                                 :idx 0
                                                 :name "A"))
     :sinks
     (list (asm-blox--cell-sink-create :row 1
                                      :col 5
                                      :expected-data nil
                                      :idx 0
                                      :name "O"
                                      :editor-text ""
                                      :editor-point 1
                                      :expected-text expected-text))
     :description
     "<editor> Read a number from A,
 draw a line with that many '#' characters.")))

(defun asm-blox-puzzles--hello-world ()
  "Generate a problem involving writing Hello World to the srceen."
  (asm-blox--problem-spec-create
   :name "Editor Basics"
   :difficulty 'tutorial
   :sources (list )
   :sinks
   (list (asm-blox--cell-sink-create :row 1
                                    :col 5
                                    :expected-data nil
                                    :idx 0
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
                                                 :idx 0
                                                 :name "C"))
     :sinks
     (list (asm-blox--cell-sink-create :row 1
                                      :col 4
                                      :expected-data expected
                                      :idx 0
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
                                                 :idx 0
                                                 :name "I"))
     :sinks
     (list (asm-blox--cell-sink-create :row 1
                                      :col 4
                                      :expected-data expected
                                      :idx 0
                                      :name "O"))
     :description
     "Return the number of times subsequent values of I increase.
ex. 1 2 0 5 6 4
     + - + + -     3 increses")))

(defun asm-blox-puzzles--tax ()
  "Generate a simple tax problem."
  (let* ((high-start-ct (random 20))
         (start-seq (seq-map (lambda (_) (random 999))
                             (make-list high-start-ct nil)))
         (high-seq (seq-map (lambda (_) (+ 500 (random 499)))
                            (make-list 12 nil)))
         (rest-seq (seq-map (lambda (_) (random 999))
                            (make-list (- 40 high-start-ct 12) nil)))
         (input-1 (append start-seq high-seq rest-seq))
         (expected (cdr (seq-reduce (lambda (acc x)
                                      (if (listp acc)
                                          acc
                                        (let ((at-ct (if (>= x 500)
                                                         (1+ acc)
                                                       0)))
                                          (if (= at-ct 12)
                                              (list 'done (/ x 40))
                                            at-ct))))
                                    input-1
                                    0))))
    (asm-blox--problem-spec-create
     :name "Tax"
     :difficulty 'hard
     :sources (list (asm-blox--cell-source-create :row 1
                                                 :col -1
                                                 :data input-1
                                                 :idx 0
                                                 :name "I"))
     :sinks
     (list (asm-blox--cell-sink-create :row 1
                                      :col 4
                                      :expected-data expected
                                      :idx 0
                                      :name "O"))
     :description
     "Read values from I. After the 12th consecutive value is greater than
or equal to 500, return that 12th value divided by 40.")))

;; (defun asm-blox-puzzles--list-reverse ()
;;   "Generate a simple addition problem."
;;   (let* ((input-1 (asm-blox-puzzles-random-list-of-lists))
;;          (lists (asm-blox-puzzles-list-of-lists-to-lisp input-1))
;;          (expected (asm-blox--flatten-list (seq-map (lambda (l)
;;                                        (append (reverse l) (list 0)))
;;                                      lists))))
;;     (asm-blox--problem-spec-create
;;      :name "List Reverse"
;;      :difficulty 'medium
;;      :sources (list (asm-blox--cell-source-create :row -1
;;                                                  :col 2
;;                                                  :data input-1
;;                                                  :idx 0
;;                                                  :name "L"))
;;      :sinks
;;      (list (asm-blox--cell-sink-create :row 3
;;                                       :col 1
;;                                       :expected-data expected
;;                                       :idx 0
;;                                       :name "R"))
;;      :description
;;      "Lists are 0 terminated.
;; Read a list from L, reverse it, and send it to R (terminating it with 0).")))

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
                                                 :idx 0
                                                 :name "I"))
     :sinks
     (list (asm-blox--cell-sink-create :row 1
                                      :col 4
                                      :expected-data lengths
                                      :idx 0
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
                                                 :idx 0
                                                 :name "X"))
     :sinks
     (list (asm-blox--cell-sink-create :row 1
                                      :col 4
                                      :expected-data expected
                                      :idx 0
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
                                                 :idx 0
                                                 :name "A")
                    (asm-blox--cell-source-create :row 2
                                                 :col -1
                                                 :data input-2
                                                 :idx 0
                                                 :name "B"))
     :sinks
     (list (asm-blox--cell-sink-create :row 1
                                      :col 4
                                      :expected-data expected
                                      :idx 0
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
                                                 :idx 0
                                                 :name "I"))
     :sinks
     (list (asm-blox--cell-sink-create :row 1
                                      :col 4
                                      :expected-data expected
                                      :idx 0
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
                                                 :idx 0
                                                 :name "H"))
     :sinks
     (list (asm-blox--cell-sink-create :row 3
                                      :col 1
                                      :expected-data expected
                                      :idx 0
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
                                                 :idx 0
                                                 :name "A")
                    (asm-blox--cell-source-create :row -1
                                                 :col 1
                                                 :data input-2
                                                 :idx 0
                                                 :name "B")
                    (asm-blox--cell-source-create :row -1
                                                 :col 2
                                                 :data input-3
                                                 :idx 0
                                                 :name "C"))
     :sinks
     (list (asm-blox--cell-sink-create :row 3
                                      :col 1
                                      :expected-data expected
                                      :idx 0
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
                                                 :idx 0
                                                 :name "A")
                    (asm-blox--cell-source-create :row -1
                                                 :col 1
                                                 :data input-2
                                                 :idx 0
                                                 :name "B"))
     :sinks
     (list (asm-blox--cell-sink-create :row 0
                                      :col 4
                                      :expected-data expected-1
                                      :idx 0
                                      :name "L")
           (asm-blox--cell-sink-create :row 2
                                      :col 4
                                      :expected-data expected-2
                                      :idx 0
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
                                      :idx 0
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
                                                 :idx 0
                                                 :name "X"))
     :sinks
     (list (asm-blox--cell-sink-create :row 3
                                      :col 3
                                      :expected-data expected
                                      :idx 0
                                      :name "X"))
     :description
     "Take an input from the input X and send it to the output X.")))

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
       #'asm-blox-puzzles--tax
       #'asm-blox-puzzles--list-length
       ;;#'asm-blox-puzzles--list-reverse
       #'asm-blox-puzzles--inc-ct
       #'asm-blox-puzzles--upcase
       #'asm-blox-puzzles--merge-step
       #'asm-blox-puzzles--hello-world
       #'asm-blox-puzzles--simple-graph
       #'asm-blox-puzzles--meeting-point
       #'asm-blox-puzzles--turing
       #'asm-blox-puzzles--stack-machine
       #'asm-blox-puzzles--delete-word
       #'asm-blox-puzzles--triangle-area))

(provide 'asm-blox-puzzles)

;;; asm-blox-puzzles.el ends here
