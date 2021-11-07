;;; gis-200.el --- Grided Intelligence System -*- lexical-binding: t -*-

;; Author: Zachary Romero
;; Maintainer: Zachary Romero
;; Version: 0.0.1
;; Package-Requires: ()
;; Homepage:
;; Keywords: game


;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; commentary

;;; Code:

(require 'cl-lib)
(require 'seq)

;;;                  parse start location
;;; ASM Parse       / parse end location
;;                 / /
;; ( ((CONST 100) 0 10) )
;;           
;; (IF (THEN (CONST 1) (CALL $LOG))
;;     (ELSE (CONST 0) (CALL $LOG)))
;;
;; ((JEZ ELSE_CLAUSE) 0 64)
;; ((CONST 1) 10 18)
;; ((CALL $LOG) 20 30)
;; ((JMP END_CLAUSE) 0 64) ;; skip this
;; ((LABEL ELSE_CLAUSE))
;; ((CONST 0) 42 50)
;; ((CALL $LOG) 52 62)
;; ((LABEL END_CLAUSE))
;;
;;
;; (LOOP (CALL $LOG) (BR 0))
;;
;; ((LABEL LOOP_TOP))
;; ((CALL $LOG) 6 15)
;; ((JMP LOOP_TOP) 0 24)

(cl-defstruct (gis-200-code-node
               (:constructor gis-200--code-node-create)
               (:copier nil))
  children start-pos end-pos)

(defun gis-200--parse-error-p (err)
  "Return non-nil if ERR is a parse error."
  (eql 'error (car err)))

(defun gis-200--parse-assembly (code)
  "Parse ASM CODE returning a list of instructions."
  (with-temp-buffer
    (erase-buffer)
    (insert code)
    (goto-char (point-min))
    (let* ((top-body '()))
      (cl-labels
          ((whitespace-p (c)
                         (or (eql c ?\s)
                             (eql c ?\t)
                             (eql c ?\n)))
           (current-char ()
                         (char-after (point)))
           (consume-space ()
                          (while (and (whitespace-p (current-char))
                                      (not (eobp)))
                            (forward-char)))
           (symbol-char-p (c)
                          (or (<= ?a c ?z)
                              (<= ?A c ?Z)))
           (digit-char-p (c) (<= ?0 c ?9))
           (parse-element (&optional top-level)
                          (let ((elements '()))
                            (catch 'end
                              (while t
                                (consume-space)
                                (if (eobp)
                                    (throw 'end nil)
                                  (let ((at-char (current-char)))
                                    (cond
                                     ;; Start of children list
                                     ((eql at-char ?\()
                                      (let ((start (point)))
                                        (forward-char 1)
                                        (let* ((children (parse-element))
                                               (node (gis-200--code-node-create :children children
                                                                                :start-pos start
                                                                                :end-pos (point))))
                                          (push node elements))))
                                     ;; End of children list
                                     ((eql at-char ?\))
                                      (if top-level
                                          (throw 'error `(error ,(point) "SYNTAX ERROR"))
                                          (forward-char 1)
                                        (throw 'end nil)))
                                     ;; Symbol
                                     ((symbol-char-p at-char)
                                      (let ((start (point)))
                                        (forward-char 1)
                                        (while (and (not (eobp))
                                                    (symbol-char-p (current-char)))
                                          (forward-char 1))
                                        (let ((symbol (intern (buffer-substring-no-properties start (point)))))
                                          (push symbol elements))))

                                     ;; digit
                                     ((digit-char-p at-char)
                                      (let ((start (point)))
                                        (forward-char 1)
                                        (while (and (not (eobp))
                                                    (digit-char-p (current-char)))
                                          (forward-char 1))
                                        (let ((number (string-to-number (buffer-substring-no-properties start (point)))))
                                          (push number elements))))

                                     ;; Unknown character
                                     (t (throw 'error `(error ,(point) "unexpected character"))))))))
                            (reverse elements))))
        (catch 'error (parse-element t))))))

(defconst gis-200-base-operations
  '(GET SET TEE CONST NULL IS_NULL DROP
        NOP ADD SUB MUL DIV REM AND OR EQZ
        EQ NE LT GT GE LE SEND PUSH POP))

(defvar gis-200--parse-depth nil)
(defvar gis-200--branch-labels nil)

(defun gis-200--make-label ()
  (intern (concat "L_" (number-to-string (random 100000)) "_" (number-to-string gis-200--parse-depth))))

(defun gis-200--parse-tree-to-asm* (parse)
  "Convert PARSE into a list of ASM instructions recursively."
  (let ((gis-200--parse-depth (if gis-200--parse-depth
                                  (1+ gis-200--parse-depth)
                                0)))
    (cond
     ((listp parse)
      (let ((asm-stmts (mapcar #'gis-200--parse-tree-to-asm* parse)))
        (apply #'append asm-stmts)))
     ((gis-200-code-node-p parse)
      (let* ((children (gis-200-code-node-children parse))
             (first-child (car children))
             (rest-children (cdr children)))
        (cond

         ((memq first-child gis-200-base-operations)
          (list parse))

         ((eql first-child 'BLOCK)
          (let* ((label-symbol (gis-200--make-label))
                 (gis-200--branch-labels (cons (cons gis-200--parse-depth label-symbol) gis-200--branch-labels))
                 (rest-asm-stmts (mapcar #'gis-200--parse-tree-to-asm* rest-children)))
            (append rest-asm-stmts
                    (list (gis-200--code-node-create
                           :children (list 'LABEL label-symbol)
                           :start-pos nil
                           :end-pos nil)))))
         
         ((eql first-child 'LOOP)
          (let* ((label-symbol (gis-200--make-label))
                 (gis-200--branch-labels (cons (cons gis-200--parse-depth label-symbol) gis-200--branch-labels))
                 (rest-asm-stmts (mapcar #'gis-200--parse-tree-to-asm* rest-children)))
            (append (list (gis-200--code-node-create
                           :children (list 'LABEL label-symbol)
                           :start-pos nil
                           :end-pos nil))
                    rest-asm-stmts)))
         ((eql first-child 'BR)
          (let* ((br-num (car rest-children))
                 (lbl-ref-level (- gis-200--parse-depth br-num 1))
                 (label-symbol (or (cdr (assoc lbl-ref-level gis-200--branch-labels))
                                   (concat "NOT_FOUND_" (number-to-string br-num) "_" (number-to-string gis-200--parse-depth) "_" (assoc br-num gis-200--branch-labels)))))
            (gis-200--code-node-create
             :children (list 'JMP label-symbol)
             :start-pos nil
             :end-pos nil)))

         ((eql first-child 'BR_IF)
          (let* ((br-num (car rest-children))
                 (lbl-ref-level (- gis-200--parse-depth br-num 1))
                 (label-symbol (or (cdr (assoc lbl-ref-level gis-200--branch-labels))
                                   (concat "NOT_FOUND_" (number-to-string br-num) "_" (number-to-string gis-200--parse-depth) "_" (assoc br-num gis-200--branch-labels)))))
            (gis-200--code-node-create
             :children (list 'JMP_IF label-symbol)
             :start-pos nil
             :end-pos nil)))

         ((eql first-child 'IF)
          (let* ((then-case (car rest-children))
                 (else-case (cadr rest-children))
                 (then-label (gis-200--make-label))
                 (end-label (gis-200--make-label)))
            `(,(gis-200--code-node-create
               :children (list 'JMP_IF_NOT then-label)
               :start-pos nil
               :end-pos nil)
              ,@(if then-case
                    (seq-map #'gis-200--parse-tree-to-asm* (cdr (gis-200-code-node-children then-case)))
                  nil)
              ,(gis-200--code-node-create
               :children (list 'JMP end-label)
               :start-pos nil
               :end-pos nil)
              ,(gis-200--code-node-create
               :children (list 'LABEL then-label)
               :start-pos nil
               :end-pos nil)
              ,@(if else-case
                    (seq-map #'gis-200--parse-tree-to-asm* (cdr (gis-200-code-node-children else-case)))
                  nil)
              ,(gis-200--code-node-create
               :children (list 'LABEL end-label)
               :start-pos nil
               :end-pos nil))))))))))

(defun gis-200--resolve-labels (asm)
  "Change each label reference in ASM to index in program."
  (let ((idxs '())
        (idx 0))
    (dolist (code-node asm)
      (let* ((code-data (gis-200-code-node-children code-node))
             (cmd (car code-data)))
        (when (eql cmd 'LABEL)
          (let ((label-name (cadr code-data)))
            (setq idxs (cons (cons label-name idx) idxs)))))
      (setq idx (1+ idx)))
    (dolist (code-node asm)
      (let* ((code-data (gis-200-code-node-children code-node))
             (cmd (car code-data)))
        (when (or (eql cmd 'JMP)
                  (eql cmd 'JMP_IF)
                  (eql cmd 'JMP_IF_NOT))
          (let* ((label-name (cadr code-data))
                 (jmp-to-idx (cdr (assoc label-name idxs))))
            (setcdr code-data (list jmp-to-idx))))))))

(defun gis-200--parse-tree-to-asm (parse)
  "Generate game bytecode from tree of PARSE, resolving labels."
  (let ((asm (flatten-list (gis-200--parse-tree-to-asm* parse))))
    (gis-200--resolve-labels asm)
    asm))

(defun gis-200--pprint-asm (instructions)
  (message "%s" "======INSTRUCTIONS=======")
  (let ((i 1))
    (dolist (instr instructions nil)
      (let ((instr (gis-200-code-node-children instr)))
        (message "%d %s" i instr)
        (setq i (1+ i)))))
  (message "%s" "======INSTRUCTIONS_END=======")
  nil)

;;; RUNTIME ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl-defstruct (gis-200--cell-runtime
               (:constructor gis-200--cell-runtime-create)
               (:copier nil))
  instructions pc stack row col up down left right)

(defconst gis-200--gameboard-col-ct 4)
(defconst gis-200--gameboard-row-ct 3)

(defvar gis-200--gameboard nil)
(defvar gis-200--extra-gameboard-cells nil)

(defun gis-200--cell-runtime-current-instruction (cell-runtime)
  "Return the current instruction of CELL-RUNTIME based in pc."
  (let* ((pc (gis-200--cell-runtime-pc cell-runtime))
         (instrs (gis-200--cell-runtime-instructions cell-runtime))
         (instrs (if (not (listp instrs)) (list instrs) instrs))
         (_ (and (>= pc (length instrs)) (error "End of program error"))))
    (car (nthcdr pc instrs))))


;; TODO - consolidate this function with gis-200--cell-at-moved-row-col
(defun gis-200--cell-at-row-col (row col)
  "Return the cell at index ROW COL from the gameboard."
  (aref gis-200--gameboard
        (+ (* row gis-200--gameboard-col-ct)
           col)))

(defun gis-200--set-cell-at-row-col (row col asm)
  "Create a runtime from ASM at set board cell at ROW, COL to it."
  (when (not gis-200--gameboard)
    (setq gis-200--gameboard (make-vector (* gis-200--gameboard-col-ct gis-200--gameboard-row-ct) nil)))
  (let ((runtime (gis-200--cell-runtime-create
                  :instructions asm
                  :pc 0
                  :stack nil
                  :row row
                  :col col
                  :up 1
                  :down 2
                  :left 3
                  :right 4)))
    (setf (aref gis-200--gameboard (+ (* row gis-200--gameboard-col-ct) col)) runtime)))

(defun gis-200--cell-at-moved-row-col (row col dir)
  "Return the item at the cell in the gameboard at position DIR from ROW,COL."
  (let* ((d-row (cond ((eql dir 'UP) -1)
                      ((eql dir 'DOWN) 1)
                      (t 0)))
         (d-col (cond ((eql dir 'LEFT) -1)
                      ((eql dir 'RIGHT) 1)
                      (t 0)))
         (row* (+ row d-row))
         (col* (+ col d-col)))
    (gis-200--cell-at-row-col row* col*)))

(defun gis-200--mirror-direction (direction)
  "Retunr the opposite of DIRECTION."
  (pcase direction
    ('UP 'DOWN)
    ('DOWN 'UP)
    ('LEFT 'RIGHT)
    ('RIGHT 'LEFT)))

(defun gis-200--get-value-from-direction (cell-runtime direction)
  "Dynamically look up and return value at DIRECTION on CELL-RUNTIME."
  (pcase direction
    ('UP (gis-200--cell-runtime-up cell-runtime))
    ('RIGHT (gis-200--cell-runtime-right cell-runtime))
    ('DOWN (gis-200--cell-runtime-down cell-runtime))
    ('LEFT (gis-200--cell-runtime-left cell-runtime))))

(defun gis-200--gameboard-source-at-pos (row col &optional dir)
  "Return non-nil if a source exists at ROW, COL (at offset DIR)."
  (let* ((d-row (cond ((eql dir 'UP) -1)
                      ((eql dir 'DOWN) 1)
                      (t 0)))
         (d-col (cond ((eql dir 'LEFT) -1)
                      ((eql dir 'RIGHT) 1)
                      (t 0)))
         (row* (+ row d-row))
         (col* (+ col d-col))
         (sources (gis-200--problem-spec-sources gis-200--extra-gameboard-cells)))
    (seq-find (lambda (source)
                (and (= (gis-200--cell-source-row source) row*)
                     (= (gis-200--cell-source-col source) col*)))
              sources)))

(defun gis-200--valid-position (row col &optional dir)
  "Return non-nil if cell exists at ROW, COL (plus optional DIR)."
  (let* ((d-row (cond ((eql dir 'UP) -1)
                      ((eql dir 'DOWN) 1)
                      (t 0)))
         (d-col (cond ((eql dir 'LEFT) -1)
                      ((eql dir 'RIGHT) 1)
                      (t 0)))
         (row* (+ row d-row))
         (col* (+ col d-col)))
    (and (<= 0 row* (1- gis-200--gameboard-row-ct))
         (<= 0 col* (1- gis-200--gameboard-col-ct)))))

(defun gis-200--cell-is-blocked-on-get (cell-runtime)
  "Return non-nil if CELL-RUNTIME's current instruction is a GET when no value exists."
  (let* ((current-instruction (gis-200--cell-runtime-current-instruction cell-runtime))
         (code-data (gis-200-code-node-children current-instruction))
         (cmd (car code-data)))
    (if (not (eql cmd 'GET))
        nil
      (let* ((direction (cadr code-data))
             (opposite-direction (gis-200--mirror-direction direction))
             (row (gis-200--cell-runtime-row cell-runtime))
             (col (gis-200--cell-runtime-col cell-runtime)))
        (cond
         ((gis-200--gameboard-source-at-pos row col direction) nil)
         ((not (gis-200--valid-position row col direction)) t)
         (t (let* ((from-cell (gis-200--cell-at-moved-row-col row col direction))
                   (recieving-val (gis-200--get-value-from-direction from-cell opposite-direction)))
              (if recieving-val
                  nil
                t))))))))

(defun gis-200--gameboard-get-all-blocked-cells ()
  "Return a list of gameboard indexes of blocked cells."
  (let ((res))
    (dotimes (idx (length gis-200--gameboard))
      (let ((cell (aref gis-200--gameboard idx)))
        (when (gis-200--cell-is-blocked-on-get cell)
          (setq res (cons idx res)))))
    res))

(defun gis-200--remove-value-from-direction (cell-runtime direction)
  "Dynamically look up and return value at DIRECTION on CELL-RUNTIME."
  (pcase direction
    ('UP (setf (gis-200--cell-runtime-up cell-runtime) nil))
    ('RIGHT (setf (gis-200--cell-runtime-right cell-runtime) nil))
    ('DOWN (setf (gis-200--cell-runtime-down cell-runtime) nil))
    ('LEFT (setf (gis-200--cell-runtime-left cell-runtime) nil))))

(defun gis-200--cell-runtime-instructions-length (cell-runtime)
  "Return the length of CELL-RUNTIME."
  (length (gis-200--cell-runtime-instructions cell-runtime)))

(defun gis-200--cell-runtime-pc-inc (cell-runtime)
  "Return the length of CELL-RUNTIME."
  (let ((instr-ct (gis-200--cell-runtime-instructions-length cell-runtime))
        (pc (gis-200--cell-runtime-pc cell-runtime)))
    (if (= (1+ pc) instr-ct)
        (setf (gis-200--cell-runtime-pc cell-runtime) 0)
      (setf (gis-200--cell-runtime-pc cell-runtime) (1+ pc)))))

(defun gis-200--cell-runtime-push (cell-runtime value)
  "Add VALUE to the stack of CELL-RUNTIME."
  ;; TODO: Handle stack overflow error.
  (let ((stack (gis-200--cell-runtime-stack cell-runtime)))
    (setf (gis-200--cell-runtime-stack cell-runtime) (cons value stack))))

(defun gis-200--cell-runtime-pop (cell-runtime)
  "Pop and return a value from the stack of CELL-RUNTIME."
  (let* ((stack (gis-200--cell-runtime-stack cell-runtime))
         (val (car stack)))
    ;; TODO: Handle stack underflow error.
    (prog1 val
      (setf (gis-200--cell-runtime-stack cell-runtime) (cdr stack)))))

(defun gis-200--binary-operation (cell-runtime function)
  "Perform binary operation FUNCTION on the top two items of CELL-RUNTIME."
  (let* ((v1 (gis-200--cell-runtime-pop cell-runtime))
         (v2 (gis-200--cell-runtime-pop cell-runtime))
         (res (funcall function v1 v2)))
    (gis-200--cell-runtime-push cell-runtime res)))

(defun gis-200--unary-operation (cell-runtime function)
  "Perform binary operation FUNCTION on the top two items of CELL-RUNTIME."
  (let* ((v (gis-200--cell-runtime-pop cell-runtime))
         (res (funcall function v)))
    (gis-200--cell-runtime-push cell-runtime res)))

(defun gis-200--cell-runtime-send (cell-runtime direction)
  "Put the top value of CELL-RUNTIME's stack on the DIRECTION register."
  (let ((v (gis-200--cell-runtime-pop cell-runtime))
        (current-val))
    (pcase direction
      ('UP (setq current-val (gis-200--cell-runtime-up cell-runtime)))
      ('DOWN (setq current-val (gis-200--cell-runtime-down cell-runtime)))
      ('LEFT (setq current-val (gis-200--cell-runtime-left cell-runtime)))
      ('RIGHT (setq current-val (gis-200--cell-runtime-right cell-runtime))))
    (if current-val
        ;; item is blocked
        'blocked
      (pcase direction
        ('UP (setf (gis-200--cell-runtime-up cell-runtime) v))
        ('DOWN (setf (gis-200--cell-runtime-down cell-runtime) v))
        ('LEFT (setf (gis-200--cell-runtime-left cell-runtime) v))
        ('RIGHT (setf (gis-200--cell-runtime-right cell-runtime) v))))))

(defun gis-200--cell-runtime-get-extra (cell-runtime direction)
  "Perform the GET command on CELL-RUNTIME outside the gameboard at DIRECTION."
  (let* ((at-row (gis-200--cell-runtime-row cell-runtime))
         (at-col (gis-200--cell-runtime-col cell-runtime))
         (source (gis-200--gameboard-source-at-pos at-row at-col direction)))
    (if (not source)
        'blocked
      (let ((v (gis-200--cell-source-pop source)))
        (gis-200--cell-runtime-push cell-runtime v)))))

(defun gis-200--cell-runtime-get (cell-runtime direction)
  "Perform the GET command running from CELL-RUNTIME, recieving from DIRECTION."
  (let* ((at-row (gis-200--cell-runtime-row cell-runtime))
         (at-col (gis-200--cell-runtime-col cell-runtime)))
    (if (not (gis-200--valid-position at-row at-col direction))
        (gis-200--cell-runtime-get-extra cell-runtime direction)
      (let* ((opposite-direction (gis-200--mirror-direction direction))
             (from-cell (gis-200--cell-at-moved-row-col at-row at-col direction))
             (recieve-val (gis-200--get-value-from-direction from-cell opposite-direction)))
        (if recieve-val
            (progn
              (gis-200--cell-runtime-push cell-runtime recieve-val)
              (gis-200--remove-value-from-direction from-cell opposite-direction))
          'blocked)))))

(defun gis-200--true-p (v)
  "Return non-nil if V is truthy."
  (not (= 0 v)))

(defun gis-200--cell-runtime-step (cell-runtime)
  "Perform one step of CELL-RUNTIME."
  (let* ((current-instr (gis-200--cell-runtime-current-instruction cell-runtime))
         (code-data (gis-200-code-node-children current-instr))
         (cmd (car code-data))
         (status
          (pcase cmd
            ('CONST (let ((const (cadr code-data)))
                      (gis-200--cell-runtime-push cell-runtime const)))
            ('ADD (gis-200--binary-operation cell-runtime #'+))
            ('SUB (gis-200--binary-operation cell-runtime #'-))
            ('MUL (gis-200--binary-operation cell-runtime #'*))
            ('DIV (gis-200--binary-operation cell-runtime #'/))
            ('REM (gis-200--binary-operation cell-runtime #'%))
            ('AND (gis-200--binary-operation cell-runtime #'logand))
            ('OR (gis-200--binary-operation cell-runtime #'logior))
            ('EQZ (gis-200--unary-operation cell-runtime (lambda (x) (= 0 x))))
            ('EQ (gis-200--binary-operation cell-runtime  #'=))
            ('NE (gis-200--binary-operation cell-runtime (lambda (a b) (not (= a b)))))
            ('LT (gis-200--binary-operation cell-runtime #'<))
            ('LE (gis-200--binary-operation cell-runtime #'<=))
            ('GT (gis-200--binary-operation cell-runtime #'>))
            ('GE (gis-200--binary-operation cell-runtime #'>=))
            ('NOP (ignore))
            ('DROP (gis-200--cell-runtime-pop cell-runtime))
            ('SEND (gis-200--cell-runtime-send cell-runtime (cadr code-data)))
            ('GET (gis-200--cell-runtime-get cell-runtime (cadr code-data)))
            ('JMP (let ((position (cadr code-data)))
                    (setf (gis-200--cell-runtime-pc cell-runtime) position)))
            ('LABEL 'label)
            ('JMP_IF (let ((position (cadr code-data))
                           (top-value (gis-200--cell-runtime-pop cell-runtime)))
                       (when (gis-200--true-p top-value)
                         (setf (gis-200--cell-runtime-pc cell-runtime) position))))
            ('JMP_IF_NOT (let ((position (cadr code-data))
                               (top-value (gis-200--cell-runtime-pop cell-runtime)))
                           (when (not (gis-200--true-p top-value))
                             (setf (gis-200--cell-runtime-pc cell-runtime) position)))))))
    ;; handle PC movement
    (pcase status
      ('blocked nil)
      ('jump nil)
      ('label (progn
                (gis-200--cell-runtime-pc-inc cell-runtime)
                (gis-200--cell-runtime-step cell-runtime)))
      (_ (gis-200--cell-runtime-pc-inc cell-runtime)))))

(defun gis-200--extra-gameboard-step ()
  "Perform step on all things not on the gameboard."
  (let ((sinks (gis-200--problem-spec-sinks gis-200--extra-gameboard-cells)))
    (dolist (sink sinks)
      (gis-200--cell-sink-get sink))))

(defun gis-200--gameboard-step ()
  "Perform step on all cells on the gameboard."
  (let ((blocked-idxs (gis-200--gameboard-get-all-blocked-cells)))
    (dotimes (idx (length gis-200--gameboard))
      (when (not (memq idx blocked-idxs))
        (let ((cell (aref gis-200--gameboard idx)))
          (gis-200--cell-runtime-step cell))))))


;;; Gameboard Display Helpers ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Functions that map the domain of the gameboard to that of the
;; display.

(defun gis-200--get-direction-col-registers (row col direction)
  "Return the register value for the DIRECTION registers at ROW, COL.
ROW and COL here do not refer to the coordinates of a
cell-runtime but rather the in-between row/col."
  (assert (or (eql 'LEFT direction) (eql 'RIGHT direction)))
  (assert (<= 0 row (1- gis-200--gameboard-row-ct)))
  (assert (<= 0 col gis-200--gameboard-col-ct))
  (let ((cell-col (if (eql 'RIGHT direction) (1- col) col)))
    (cond
     ;; outputs are never displayed on the board
     ((or (and (= col 0) (eql direction 'LEFT))
          (and (= row gis-200--gameboard-col-ct) (eql direction 'RIGHT)))
      nil)
     ((or (= col 0)
          (= col gis-200--gameboard-col-ct))
      (let* ((source (gis-200--gameboard-source-at-pos row cell-col)))
        (if (not source)
            nil
          (car (gis-200--cell-source-data source)))))
     (t
      (let* ((cell-runtime (gis-200--cell-at-row-col row cell-col)))
        (gis-200--get-value-from-direction cell-runtime direction))))))

(defun gis-200--get-direction-row-registers (row col direction)
  "Return the register value for the DIRECTION registers at ROW, COL.
ROW and COL here do not refer to the coordinates of a
cell-runtime but rather the in-between row/col."
  (assert (or (eql 'UP direction) (eql 'DOWN direction)))
  (assert (<= 0 row gis-200--gameboard-row-ct))
  (assert (<= 0 col (1- gis-200--gameboard-col-ct)))
  (let ((cell-row (if (eql 'DOWN direction) (1- row) row)))
    (cond
     ((or (and (= row 0) (eql direction 'UP))
          (and (= row gis-200--gameboard-row-ct) (eql direction 'DOWN)))
      nil)
     ((or (= row 0)
          (= row gis-200--gameboard-row-ct))
      (let* ((source (gis-200--gameboard-source-at-pos cell-row col)))
        (if (not source)
            nil
          (car (gis-200--cell-source-data source)))))
     (t
      (let* ((cell-runtime (gis-200--cell-at-row-col cell-row col)))
        (gis-200--get-value-from-direction cell-runtime direction))))))

(defun gis-200--get-source-idx-at-position (row col)
  "Return name of source at position ROW, COL if exists."
  (let ((sources (gis-200--problem-spec-sources gis-200--extra-gameboard-cells))
        (idx 0)
        (found))
    (while sources
      (let ((source (car sources)))
        (if (and (= (gis-200--cell-source-row source) row)
                 (= (gis-200--cell-source-col source) col))
            (progn (setq sources nil)
                   (setq found idx))
          (setq sources (cdr sources))
          (setq idx (1+ idx)))))
    found))
(defun gis-200--get-sink-idx-at-position (row col)
  "Return name of source at position ROW, COL if exists."
  (let ((sinks (gis-200--problem-spec-sinks gis-200--extra-gameboard-cells))
        (idx 0)
        (found))
    (while sinks
      (let ((source (car sinks)))
        (if (and (= (gis-200--cell-sink-row source) row)
                 (= (gis-200--cell-sink-col source) col))
            (progn (setq sinks nil)
                   (setq found idx))
          (setq sinks (cdr sinks))
          (setq idx (1+ idx)))))
    found))

;;; Problem Infrastructure ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A problem generator is a function that when called, returns a
;; problem-spec, ie a list of source nodes and a list of sink nodes.
;; The source nodes indicate at which positions input should be feed
;; to the board while the sink node indicates at which position should
;; output be consumed.  The sink node should also have a list of the
;; expected output.

(cl-defstruct (gis-200--cell-source
               (:constructor gis-200--cell-source-create)
               (:copier nil))
  row col data)

(cl-defstruct (gis-200--cell-sink
               (:constructor gis-200--cell-sink-create)
               (:copier nil))
  row col expected-data)

(cl-defstruct (gis-200--problem-spec
               (:constructor gis-200--problem-spec-create)
               (:copier nil))
  sources sinks)

(defun gis-200--cell-sink-get (sink)
  "Grab a value and put it into SINK from the gameboard."
  (let* ((row (gis-200--cell-sink-row sink))
         (col (gis-200--cell-sink-col sink))
         (direction (cond
                     ((>= col gis-200--gameboard-col-ct) 'LEFT)
                     ((> 0 col) 'RIGHT)
                     ((> 0 row) 'DOWN)
                     ((> row gis-200--gameboard-row-ct) 'UP)))
         (opposite-direction (gis-200--mirror-direction direction))
         (cell-runtime (gis-200--cell-at-moved-row-col row col direction))
         (v (gis-200--get-value-from-direction cell-runtime opposite-direction)))
    (if v
        (let* ((old-stack (gis-200--cell-sink-expected-data sink))
               (expected-value (car old-stack)))
          (when (not (equal expected-value v))
            ;; TODO - do something here
            (message "Unexpected value"))
          (setf (gis-200--cell-sink-expected-data sink) (cdr old-stack))
          (gis-200--remove-value-from-direction cell-runtime opposite-direction))
        'blocked)))

(defun gis-200--cell-source-pop (source)
  "Pop a value from the data of SOURCE."
  (unless (gis-200--cell-source-p source)
    (error "Cell-source-pop type error"))
  (let* ((data (gis-200--cell-source-data source))
         (top (car data))
         (rest (cdr data)))
    (setf (gis-200--cell-source-data source) rest)
    top))

(defun gis-200--problem--add ()
  "Generate a simple addition problem."
  (let* ((input-1 (seq-map (lambda (_) (random 10)) (make-list 40 nil)))
         (input-2 (seq-map (lambda (_) (random 10)) (make-list 40 nil)))
         (expected (seq-mapn #'+ input-1 input-2)))
    (gis-200--problem-spec-create
     :sources (list (gis-200--cell-source-create :row -1 :col 0 :data input-1)
                   (gis-200--cell-source-create :row -1 :col 1 :data input-2))
     :sinks
     (list (gis-200--cell-sink-create :row 4 :col 1 :expected-data expected)))))


(defmacro comment (&rest x) nil)

(comment
 (gis-200--parse-tree-to-asm (gis-200--parse-assembly "(CONST 1)"))
 (gis-200--parse-assembly ")")
 (let* ((parsed (gis-200--parse-assembly "(CONST 1)"))
        (parsed2 (gis-200--parse-assembly "(GET LEFT) (POP)"))
        (asm (gis-200--parse-tree-to-asm parsed))
        (asm2 (gis-200--parse-tree-to-asm parsed2))
        (runtime (gis-200--cell-runtime-create
                  :instructions asm
                  :pc 0
                  :stack nil
                  :row 0
                  :col 0
                  :up nil
                  :down nil
                  :left nil
                  :right nil))
        (runtime-2 (gis-200--cell-runtime-create
                    :instructions asm2
                    :pc 0
                    :stack nil
                    :row 0
                    :col 1
                    :up nil
                    :down nil
                    :left 73
                    :right nil))
        (gis-200--gameboard (vector runtime runtime-2))
        (gis-200--extra-gameboard-cells
         (gis-200--problem-spec-create :sources (list (gis-200--cell-source-create :row -1 :col 0 :data '(44 55 66)))
                                       :sinks (list (gis-200--cell-sink-create :row 0 :col -1 :expected-data '(1 2)))))
        (gis-200--gameboard-col-ct 2))
   (gis-200--gameboard-step)
   (gis-200--gameboard-step)
   (gis-200--gameboard-step)
   (gis-200--extra-gameboard-step)
   (gis-200--cell-runtime-stack runtime)
   (car (gis-200--problem-spec-sinks gis-200--extra-gameboard-cells))))

(provide 'gis-200)

;;; gis-200.el ends here
