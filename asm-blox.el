;;; asm-blox.el --- Programming game involving WAT -*- lexical-binding: t -*-

;; Copyright © 2021-2022 Zachary Romero <zkry@posteo.net>

;; Author: Zachary Romero
;; Maintainer: Zachary Romero
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.1") (yaml "0.3.4"))
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

;; Asm-blox is a programming game where you are tasked with writing
;; code to complete puzzles.  Code is written on a 4x3 grid of cells
;; where your room to write code and memory is limited.  Complex
;; programs must be written by combining code cells, passing messages
;; between them.  The command `asm-blox' will open the puzzle listing
;; buffer where you can select a puzzle to work on.  For detains on
;; how to program the board please refer to the reference material
;; provided by this package.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'yaml)

(defgroup asm-blox nil
  "Programming game involving tiled WAT and YAML code cells."
  :prefix "asm-blox-"
  :group 'games)

(defcustom asm-blox-save-directory-name (expand-file-name ".asm-blox" user-emacs-directory)
  "The directory in which all puzzles will be saved and searched for."
  :group 'asm-blox
  :type 'directory)

(defconst asm-blox-box-height 12
  "Height of an individual code block.")
(defconst asm-blox-box-width 20
  "Width of an individual code block.")
(defconst asm-blox--gameboard-col-ct 4
  "Amount of columns on a Asm-blox gameboard.")
(defconst asm-blox--gameboard-row-ct 3
  "Amount of rows on a Asm-blox gameboard.")

(defvar-local asm-blox-box-contents nil
  "Hashtable containing the textual contents of the gameboard.
Keys are in the form '(ROW COL).")

(defvar asm-blox--gameboard nil
  "Vector of combiled code-cells.")
(defvar-local asm-blox--extra-gameboard-cells nil
  "List of sources (ie puzzle input) and sinks (ie win conditions) of a game.")
(defvar asm-blox--gameboard-state nil
  "Contains the state of the board whether it be victory or error.")

(defvar asm-blox--parse-depth nil
  "Dynamic variable used when parsing WAT to determine label references.")
(defvar asm-blox--branch-labels nil
  "Dynamic variable to store where lables which labels refer to which depths.")

(defvar asm-blox-runtime-error nil
  "If non-nil, contains the runtime error encountered.
The format of the error is (list message row column).")

(defvar-local asm-blox-execution-origin-buffer nil
  "The buffer where the execution buffer was created from.")

(cl-defstruct (asm-blox-code-node
               (:constructor asm-blox--code-node-create)
               (:copier nil))
  "Parsed sexp of code.  Used to keep track of start and end position in text."
  children start-pos end-pos)

(defun asm-blox--parse-error-p (err)
  "Return non-nil if ERR is a parse error."
  (and (listp err) (eql 'error (car err))))

(defun asm-blox--parse-cell (coords code)
  "Parse a the CODE of a text box at COORDS, returning a cell runtime."
  (let* ((first-char
          (and (not (string-empty-p (string-trim code)))
               (substring-no-properties (string-trim-left code) 0 1)))
         ;; There is currently no switch the user can use to indicate
         ;; filetype, thus the need of heuristic.
         (sexp-p (eql (ignore-errors (car (read code))) 'module))
         (wat-p (or (not first-char)
                    (string= first-char "(")
                    (string= first-char ")")
                    (string= first-char ";"))))
    (cond
     (sexp-p
      (asm-blox--create-sexp-code-node (car coords) (cadr coords) code))
     (wat-p
      (let ((parse-tree (asm-blox--parse-assembly code)))
        (if (asm-blox--parse-error-p parse-tree)
            parse-tree
          (let ((row (car coords))
                (col (cadr coords))
                (asm (asm-blox--parse-tree-to-asm parse-tree)))
            (if (asm-blox--parse-error-p asm)
                asm
              (asm-blox--cell-runtime-create
               :instructions asm
               :pc 0
               :stack '()
               :row row
               :col col))))))
     (t
      (asm-blox--create-yaml-code-node (car coords) (cadr coords) code)))))

(defun asm-blox--parse-assembly (code)
  "Parse ASM CODE returning a list of instructions."
  (with-temp-buffer
    (erase-buffer)
    (insert code)
    (goto-char (point-min))
    (cl-labels
        ((whitespace-p
          (c)
          (or (eql c ?\s)
              (eql c ?\t)
              (eql c ?\n)))
         (current-char
          ()
          (char-after (point)))
         (consume-space
          ()
          (while (and (whitespace-p (current-char))
                      (not (eobp)))
            (forward-char)))
         (symbol-char-p
          (c)
          (or (<= ?a c ?z)
              (<= ?A c ?Z)
              (= ?_ c)))
         (parse-element
          (&optional top-level)
          (let ((elements '()))
            (catch 'end
              (while t
                (consume-space)
                (if (eobp)
                    (if top-level
                        (throw 'end nil)
                      (throw 'error `(error ,(point) "SYNTAX ERROR")))
                  (let ((at-char (current-char)))
                    (cond
                     ;; Start of children list
                     ((eql at-char ?\()
                      (let ((start (point)))
                        (forward-char 1)
                        (let* ((children (parse-element))
                               (node (asm-blox--code-node-create
                                      :children children
                                      :start-pos start
                                      :end-pos (point))))
                          (push node elements))))
                     ;; End of children list
                     ((eql at-char ?\) )
                      (if top-level
                          (throw 'error `(error ,(point) "SYNTAX ERROR"))
                        (forward-char 1)
                        (throw 'end nil)))
                     ((eql at-char ?\?)
                      (forward-char 1)
                      (let ((c))
                        (if (looking-at "\\\\")
                            (progn
                              (forward-char 1)
                              (let ((escape-c (char-after (point))))
                                (pcase escape-c
                                  (?n (setq c ?\n))
                                  (?s (setq c ?\s))
                                  (?b (setq c ?\b))
                                  (_ (throw 'error `(error ,(point) "BAD ESCAPE CODE"))))))
                          (setq c (char-after (point)))
                          (when (or (= c ?\s) (= c ?\n))
                            (throw 'error `(error ,(point) "INVALID CHAR"))))
                        (forward-char 1)
                        (push c elements)))
                     ;; Symbol
                     ((eql at-char ?\;)
                      (while (and (not (looking-at "\n"))
                                  (not (eobp)))
                        (forward-char 1)))
                     ((symbol-char-p at-char)
                      (let ((start (point)))
                        (forward-char 1)
                        (while (and (not (eobp))
                                    (symbol-char-p (current-char)))
                          (forward-char 1))
                        (let ((symbol (intern (upcase
                                               (buffer-substring-no-properties
                                                start
                                                (point))))))
                          (push symbol elements))))

                     ;; digit
                     ((or (cl-digit-char-p at-char) (eql at-char ?-))
                      (let ((start (point)))
                        (forward-char 1)
                        (while (and (not (eobp))
                                    (cl-digit-char-p (current-char)))
                          (forward-char 1))
                        (let ((number (string-to-number
                                       (buffer-substring-no-properties
                                        start
                                        (point)))))
                          (when (< number -999)
                            (throw 'error `(error ,(point) "TOO LOW NUMBER")))
                          (when (> number 999)
                            (throw 'error `(error ,(point) "TOO HIGH NUMBER")))
                          (push number elements))))

                     ;; Unknown character
                     (t (throw 'error `(error ,(point) "unexpected character"))))))))
            (reverse elements))))
      (catch 'error (parse-element t)))))

(defconst asm-blox-base-operations
  '(GET SET TEE CONST NULL IS_NULL DROP
        NOP ADD INC DEC SUB MUL DIV REM AND OR EQZ GZ LZ
        EQ NE LT GT GE LE SEND PUSH POP
        CLR NOT DUP ABS))

(defconst asm-blox-command-specs
  '((SET integerp asm-blox--subexpressions)
    (CLR)
    (CONST integerp)
    (DUP asm-blox--subexpressions)
    (ABS asm-blox--subexpressions)
    (ADD asm-blox--subexpressions)
    (SUB asm-blox--subexpressions)
    (MUL asm-blox--subexpressions)
    (DIV asm-blox--subexpressions)
    (NEG asm-blox--subexpressions)
    (REM asm-blox--subexpressions)
    (AND asm-blox--subexpressions)
    (NOT asm-blox--subexpressions)
    (OR asm-blox--subexpressions)
    (EQ asm-blox--subexpressions)
    (NE asm-blox--subexpressions)
    (LT asm-blox--subexpressions)
    (LE asm-blox--subexpressions)
    (GT asm-blox--subexpressions)
    (GE asm-blox--subexpressions)
    (GZ asm-blox--subexpressions)
    (LZ asm-blox--subexpressions)
    (EQZ asm-blox--subexpressions)
    (BLOCK asm-blox--subexpressions)
    (LOOP asm-blox--subexpressions)
    (INC integerp)
    (DEC integerp)
    (BR_IF integerp)
    (BR integerp)
    (NOP)
    (DROP asm-blox--subexpressions)
    (SEND asm-blox--portp asm-blox--subexpressions)
    (GET (lambda (x) (or (asm-blox--portp x) (integerp x))))
    (LEFT)  ;; TODO: Should these be in final game?
    (RIGHT) ;; TODO
    (UP)    ;; TODO
    (DOWN)  ;; TODO
    (FN t)  ;; FN needs special verification code
    )
  "List of commands and specifications for command arguments.")

(defun asm-blox--portp (x)
  "Return non-nil if X is a port direction."
  (if (stringp x)
      (memq (intern x) '(UP DOWN LEFT RIGHT))
    (memq x '(UP DOWN LEFT RIGHT))))

(defun asm-blox--code-node-validate (code-node)
  "Determine if CODE-NODE adheres to the corresponding specification."
  (let* ((children (asm-blox-code-node-children code-node))
         (start-pos (asm-blox-code-node-start-pos code-node))
         ;; (end-pos (asm-blox-code-node-end-pos code-node))
         (first-child (car children))
         (cmd-spec (assoc first-child asm-blox-command-specs)))
    (cond
     ((not first-child)
      `(error ,start-pos "No command found"))
     ((not cmd-spec)
      `(error ,start-pos "Command not found"))
     (t
      (let* ((specs (cdr cmd-spec))
             (rest-children (cdr children))
             (at-spec (car specs)))
        (catch 'error
          (while (or specs rest-children)
            (cond
             ((eql at-spec 'asm-blox--subexpressions)
              (if (seq-every-p #'asm-blox-code-node-p rest-children)
                  (setq rest-children nil)
                (throw 'error `(error ,start-pos "bad end expressions"))))
             ((and rest-children (not specs))
              (throw 'error `(error ,start-pos "too many args")))
             ((and specs (not rest-children))
              (throw 'error `(error ,start-pos "not enough args")))
             (t
              (let* ((at-child (car rest-children))
                     (ok-p (funcall at-spec at-child)))
                (if ok-p
                    (setq rest-children (cdr rest-children))
                  (let ((msg (format "bad arg to '%s'" first-child)))
                    (throw 'error `(error ,start-pos ,msg)))))))
            (setq specs (cdr specs))
            (setq at-spec (car specs)))))))))

(defun asm-blox--make-label ()
  "Depending on the parse-depth create a label for the various goto statements."
  (intern (concat "L_"
                  (number-to-string (random 100000))
                  "_"
                  (number-to-string asm-blox--parse-depth))))

(defun asm-blox--parse-tree-to-asm* (parse)
  "Convert PARSE into a list of ASM instructions recursively."
  (let ((asm-blox--parse-depth (if asm-blox--parse-depth
                                  (1+ asm-blox--parse-depth)
                                0)))
    (cond
     ((listp parse)
      (let ((asm-stmts (mapcar #'asm-blox--parse-tree-to-asm* parse)))
        (apply #'append asm-stmts)))
     ((asm-blox-code-node-p parse)
      (let ((err (asm-blox--code-node-validate parse)))
        (if err
            (throw 'error err)
          (let* ((children (asm-blox-code-node-children parse))
                 (start-pos (asm-blox-code-node-start-pos parse))
                 (end-pos (asm-blox-code-node-end-pos parse))
                 (first-child (car children))
                 (rest-children (cdr children)))
            (cond
             ((not first-child)
              (throw 'error `(error ,start-pos "No cmd found")))

             ((eql first-child 'BLOCK)
              (let* ((label-symbol (asm-blox--make-label))
                     (asm-blox--branch-labels
                      (cons (cons asm-blox--parse-depth label-symbol)
                            asm-blox--branch-labels))
                     (rest-asm-stmts (mapcar #'asm-blox--parse-tree-to-asm*
                                             rest-children)))
                (append rest-asm-stmts
                        (list (asm-blox--code-node-create
                               :children (list 'LABEL label-symbol)
                               :start-pos nil
                               :end-pos nil)))))

             ((eql first-child 'LOOP)
              (let* ((label-symbol (asm-blox--make-label))
                     (asm-blox--branch-labels
                      (cons (cons asm-blox--parse-depth label-symbol)
                            asm-blox--branch-labels))
                     (rest-asm-stmts
                      (mapcar #'asm-blox--parse-tree-to-asm* rest-children)))
                (append (list (asm-blox--code-node-create
                               :children (list 'LABEL label-symbol)
                               :start-pos nil
                               :end-pos nil))
                        rest-asm-stmts)))
             ((eql first-child 'BR)
              (let* ((br-num (car rest-children))
                     (lbl-ref-level (- asm-blox--parse-depth br-num 1))
                     (label-symbol
                      (or (cdr (assoc lbl-ref-level
                                      asm-blox--branch-labels))
                          (throw 'error `(error ,start-pos "Label not found")))))
                (asm-blox--code-node-create
                 :children (list 'JMP label-symbol)
                 :start-pos start-pos
                 :end-pos end-pos)))

             ((eql first-child 'BR_IF)
              (let* ((br-num (car rest-children))
                     (lbl-ref-level (- asm-blox--parse-depth br-num 1))
                     (label-symbol
                      (or (cdr (assoc lbl-ref-level asm-blox--branch-labels))
                          (throw 'error `(error ,start-pos "Label not found")))))
                (asm-blox--code-node-create
                 :children (list 'JMP_IF label-symbol)
                 :start-pos start-pos
                 :end-pos end-pos)))

             ((eql first-child 'IF)
              (let* ((then-case (car rest-children))
                     (else-case (cadr rest-children))
                     (then-label (asm-blox--make-label))
                     (end-label (asm-blox--make-label)))
                `(,(asm-blox--code-node-create
                    :children (list 'JMP_IF_NOT then-label)
                    :start-pos nil
                    :end-pos nil)
                  ,@(if then-case
                        (seq-map #'asm-blox--parse-tree-to-asm*
                                 (cdr (asm-blox-code-node-children then-case)))
                      nil)
                  ,(asm-blox--code-node-create
                    :children (list 'JMP end-label)
                    :start-pos nil
                    :end-pos nil)
                  ,(asm-blox--code-node-create
                    :children (list 'LABEL then-label)
                    :start-pos nil
                    :end-pos nil)
                  ,@(if else-case
                        (seq-map #'asm-blox--parse-tree-to-asm*
                                 (cdr (asm-blox-code-node-children else-case)))
                      nil)
                  ,(asm-blox--code-node-create
                    :children (list 'LABEL end-label)
                    :start-pos nil
                    :end-pos nil))))

             ((assoc first-child asm-blox-command-specs)
              (let* ((cmd-spec (assoc first-child asm-blox-command-specs))
                     (spec (cdr cmd-spec))
                     (rest-children (cdr children))
                     (children-cmds '()))
                ;; Determine which children are commands
                ;; that run before the command we're at.
                (while (and spec rest-children)
                  (when (eql (car spec) 'asm-blox--subexpressions)
                    (setq children-cmds (seq-map #'asm-blox--parse-tree-to-asm*
                                                 rest-children)))
                  (setq spec (cdr spec))
                  (setq rest-children (cdr rest-children)))
                (append children-cmds (list parse))))


             (t `(error ,start-pos ,(format "Bad cmd: %s " first-child)))))))))))

(defun asm-blox--flatten-list (tree)
  "Return a \"flattened\" copy of TREE. Copied from Emacs 27.1."
  (let (elems)
    (while (consp tree)
      (let ((elem (pop tree)))
        (while (consp elem)
          (push (cdr elem) tree)
          (setq elem (car elem)))
        (if elem (push elem elems))))
    (if tree (push tree elems))
    (nreverse elems)))

(defun asm-blox--parse-tree-to-asm (parse)
  "Generate game bytecode from tree of PARSE, resolving labels."
  (catch 'error
    (let ((asm (asm-blox--flatten-list (asm-blox--parse-tree-to-asm* parse))))
      (asm-blox--resolve-labels asm)
      asm)))

(defun asm-blox--resolve-labels (asm)
  "Change each label reference in ASM to index in program."
  (let ((idxs '())
        (idx 0))
    (dolist (code-node asm)
      (let* ((code-data (asm-blox-code-node-children code-node))
             (cmd (car code-data)))
        (when (eql cmd 'LABEL)
          (let ((label-name (cadr code-data)))
            (setq idxs (cons (cons label-name idx) idxs)))))
      (setq idx (1+ idx)))
    (dolist (code-node asm)
      (let* ((code-data (asm-blox-code-node-children code-node))
             (cmd (car code-data)))
        (when (or (eql cmd 'JMP)
                  (eql cmd 'JMP_IF)
                  (eql cmd 'JMP_IF_NOT))
          (let* ((label-name (cadr code-data))
                 (jmp-to-idx (cdr (assoc label-name idxs))))
            (setcdr code-data (list jmp-to-idx))))))))

;;; RUNTIME ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl-defstruct (asm-blox--cell-runtime
               (:constructor asm-blox--cell-runtime-create)
               (:copier nil))
  "Structure that contains the runtime for a cell on the board.
A cell can be either for ASM or YAML.  An ASM cell will have
instructions, a PC, and a stack, while a YAML runtime will have
run-function, run-spec, and run-state.

Both have directional ports and staging ports.  The staging ports
are a way to let all of the cells run without the cells that run
previously change what values the later cells read.  After all
cells have moved, the staging port becomes the current port."
  instructions
  pc
  stack
  row col
  staging-up staging-down staging-left staging-right
  up down left right
  run-function message-function
  run-spec
  run-state)

(defun asm-blox--cell-message-at-pos (row col)
  "Return cell runtimes display message at ROW COL if exists."
  (let* ((cell-runtime (asm-blox--cell-at-row-col row col))
         (msg-fn (asm-blox--cell-runtime-message-function cell-runtime)))
    (when msg-fn
      (funcall msg-fn cell-runtime))))

(defun asm-blox--cell-runtime-current-instruction (cell-runtime)
  "Return the current instruction of CELL-RUNTIME based in pc."
  (let* ((pc (asm-blox--cell-runtime-pc cell-runtime))
         (instrs (asm-blox--cell-runtime-instructions cell-runtime))
         (instrs (if (not (listp instrs)) (list instrs) instrs)))
    (if (not instrs)
        (asm-blox--code-node-create :children '(_EMPTY))
      (and (>= pc (length instrs)) (error "End of program error"))
      (car (nthcdr pc instrs)))))

(defun asm-blox--gameboard-in-final-state-p ()
  "Return non-nil if the gameboard is in a finalized state."
  ;; If asm-blox--gameboard-state is not nil then it is in finalized state.
  asm-blox--gameboard-state)

;; TODO - consolidate this function with asm-blox--cell-at-moved-row-col
(defun asm-blox--cell-at-row-col (row col)
  "Return the cell at index ROW COL from the gameboard."
  (if (and (<= 0 row (1- asm-blox--gameboard-row-ct))
           (<= 0 col (1- asm-blox--gameboard-col-ct)))
      (aref asm-blox--gameboard
            (+ (* row asm-blox--gameboard-col-ct)
               col))
    ;; if reference to out of bounds, return empty cell runtime.
    (asm-blox--cell-runtime-create)))

(defun asm-blox--set-cell-at-row-col (row col cell-runtime)
  "Set board cell at ROW, COL to CELL-RUNTIME."
  (when (not asm-blox--gameboard)
    (setq asm-blox--gameboard
          (make-vector (* asm-blox--gameboard-col-ct asm-blox--gameboard-row-ct)
                       nil)))
  (setf (aref asm-blox--gameboard (+ (* row asm-blox--gameboard-col-ct) col))
        cell-runtime))

(defun asm-blox--cell-at-moved-row-col (row col dir)
  "Return the item at the cell in the gameboard at position DIR from ROW,COL."
  (let* ((d-row (cond ((eql dir 'UP) -1)
                      ((eql dir 'DOWN) 1)
                      (t 0)))
         (d-col (cond ((eql dir 'LEFT) -1)
                      ((eql dir 'RIGHT) 1)
                      (t 0)))
         (row* (+ row d-row))
         (col* (+ col d-col)))
    (asm-blox--cell-at-row-col row* col*)))

(defun asm-blox--mirror-direction (direction)
  "Retunr the opposite of DIRECTION."
  (pcase direction
    ('UP 'DOWN)
    ('DOWN 'UP)
    ('LEFT 'RIGHT)
    ('RIGHT 'LEFT)))

(defun asm-blox--get-value-from-direction (cell-runtime direction)
  "Dynamically look up and return value at DIRECTION on CELL-RUNTIME."
  (pcase direction
    ('UP (asm-blox--cell-runtime-up cell-runtime))
    ('RIGHT (asm-blox--cell-runtime-right cell-runtime))
    ('DOWN (asm-blox--cell-runtime-down cell-runtime))
    ('LEFT (asm-blox--cell-runtime-left cell-runtime))))

(defun asm-blox--get-value-from-staging-direction (cell-runtime direction)
  "Dynamically look up and return value at DIRECTION on CELL-RUNTIME."
  (pcase direction
    ('UP (asm-blox--cell-runtime-staging-up cell-runtime))
    ('RIGHT (asm-blox--cell-runtime-staging-right cell-runtime))
    ('DOWN (asm-blox--cell-runtime-staging-down cell-runtime))
    ('LEFT (asm-blox--cell-runtime-staging-left cell-runtime))))

(defun asm-blox--gameboard-source-at-pos (row col &optional dir)
  "Return non-nil if a source exists at ROW, COL (at offset DIR)."
  (let* ((d-row (cond ((eql dir 'UP) -1)
                      ((eql dir 'DOWN) 1)
                      (t 0)))
         (d-col (cond ((eql dir 'LEFT) -1)
                      ((eql dir 'RIGHT) 1)
                      (t 0)))
         (row* (+ row d-row))
         (col* (+ col d-col))
         (sources (asm-blox--problem-spec-sources asm-blox--extra-gameboard-cells)))
    (seq-find (lambda (source)
                (and (= (asm-blox--cell-source-row source) row*)
                     (= (asm-blox--cell-source-col source) col*)))
              sources)))

(defun asm-blox--valid-position (row col &optional dir)
  "Return non-nil if cell exists at ROW, COL (plus optional DIR)."
  (let* ((d-row (cond ((eql dir 'UP) -1)
                      ((eql dir 'DOWN) 1)
                      (t 0)))
         (d-col (cond ((eql dir 'LEFT) -1)
                      ((eql dir 'RIGHT) 1)
                      (t 0)))
         (row* (+ row d-row))
         (col* (+ col d-col)))
    (and (<= 0 row* (1- asm-blox--gameboard-row-ct))
         (<= 0 col* (1- asm-blox--gameboard-col-ct)))))

(defun asm-blox--cell-runtime-merge-ports-with-staging (cell-runtime)
  "For CELL-RUNTIME, if the staging region has a value, move it to port.
If the port does't have a value, set staging to nil."
  ;; This function is needed to prevent execution order from tampering with the
  ;; execution results.
  (dolist (direction '(UP DOWN LEFT RIGHT))
    (let ((staging-value
           (asm-blox--get-value-from-staging-direction cell-runtime direction))
          (value (asm-blox--get-value-from-direction cell-runtime direction)))
      (when (and (eql staging-value 'sent) (not value))
        (asm-blox--cell-runtime-set-staging-value-from-direction cell-runtime direction nil)
        (setq staging-value nil))
      (when (and staging-value (not value))
        (asm-blox--cell-runtime-set-value-from-direction cell-runtime direction staging-value)
        (asm-blox--cell-runtime-set-staging-value-from-direction cell-runtime direction 'sent)))))

(defun asm-blox--remove-value-from-staging-direction (cell-runtime direction)
  "Dynamically look up and return value at staging DIRECTION on CELL-RUNTIME."
  (pcase direction
    ('UP (setf (asm-blox--cell-runtime-staging-up cell-runtime) nil))
    ('RIGHT (setf (asm-blox--cell-runtime-staging-right cell-runtime) nil))
    ('DOWN (setf (asm-blox--cell-runtime-staging-down cell-runtime) nil))
    ('LEFT (setf (asm-blox--cell-runtime-staging-left cell-runtime) nil))))

(defun asm-blox--remove-value-from-direction (cell-runtime direction)
  "Dynamically look up and return value at DIRECTION on CELL-RUNTIME."
  (pcase direction
    ('UP (setf (asm-blox--cell-runtime-up cell-runtime) nil))
    ('RIGHT (setf (asm-blox--cell-runtime-right cell-runtime) nil))
    ('DOWN (setf (asm-blox--cell-runtime-down cell-runtime) nil))
    ('LEFT (setf (asm-blox--cell-runtime-left cell-runtime) nil))))

;; TODO don't repeat this logic elsewhere
(defun asm-blox--cell-runtime-set-value-from-direction (cell-runtime direction value)
  "Dynamically set the DIRECTION port of CELL-RUNTIME to VALUE."
  (pcase direction
    ('UP (setf (asm-blox--cell-runtime-up cell-runtime) value))
    ('RIGHT (setf (asm-blox--cell-runtime-right cell-runtime) value))
    ('DOWN (setf (asm-blox--cell-runtime-down cell-runtime) value))
    ('LEFT (setf (asm-blox--cell-runtime-left cell-runtime) value))))

(defun asm-blox--cell-runtime-set-staging-value-from-direction (cell-runtime direction value)
  "Dynamically set the staging DIRECTION port of CELL-RUNTIME to VALUE."
  (pcase direction
    ('UP (setf (asm-blox--cell-runtime-staging-up cell-runtime) value))
    ('RIGHT (setf (asm-blox--cell-runtime-staging-right cell-runtime) value))
    ('DOWN (setf (asm-blox--cell-runtime-staging-down cell-runtime) value))
    ('LEFT (setf (asm-blox--cell-runtime-staging-left cell-runtime) value))))

(defun asm-blox--cell-runtime-instructions-length (cell-runtime)
  "Return the length of CELL-RUNTIME."
  (length (asm-blox--cell-runtime-instructions cell-runtime)))

(defun asm-blox--cell-runtime-pc-inc (cell-runtime)
  "Return the length of CELL-RUNTIME."
  (let ((instr-ct (asm-blox--cell-runtime-instructions-length cell-runtime))
        (pc (asm-blox--cell-runtime-pc cell-runtime)))
    (if (= (1+ pc) instr-ct)
        (setf (asm-blox--cell-runtime-pc cell-runtime) 0)
      (setf (asm-blox--cell-runtime-pc cell-runtime) (1+ pc)))))

(defun asm-blox--cell-runtime-push (cell-runtime value)
  "Add VALUE to the stack of CELL-RUNTIME."
  ;; TODO: Handle stack overflow error.
  (let* ((stack (asm-blox--cell-runtime-stack cell-runtime)))
    (when (>= (length stack) 4)
      (let ((row (asm-blox--cell-runtime-row cell-runtime))
            (col (asm-blox--cell-runtime-col cell-runtime)))
        (throw 'runtime-error `(error "Stack overflow" ,row ,col))))
    (setf (asm-blox--cell-runtime-stack cell-runtime) (cons value stack))))

(defun asm-blox--cell-runtime-pop (cell-runtime)
  "Pop and return a value from the stack of CELL-RUNTIME."
  (let* ((stack (asm-blox--cell-runtime-stack cell-runtime))
         (val (car stack)))
    ;; TODO: Handle stack underflow error.
    (when (not val)
      (let ((row (asm-blox--cell-runtime-row cell-runtime))
            (col (asm-blox--cell-runtime-col cell-runtime)))
        (throw 'runtime-error `(error "Stack underflow" ,row ,col))))
    (prog1 val
      (setf (asm-blox--cell-runtime-stack cell-runtime) (cdr stack)))))

(defun asm-blox--binary-operation (cell-runtime function)
  "Perform binary operation FUNCTION on the top two items of CELL-RUNTIME."
  (let* ((v1 (asm-blox--cell-runtime-pop cell-runtime))
         (v2 (asm-blox--cell-runtime-pop cell-runtime))
         (res (funcall function v2 v1)))
    (when (> res 999)
      (setq res (- res (* 2 999))))
    (when (< res -999)
      (setq res (+ res (* 2 999))))
    (asm-blox--cell-runtime-push cell-runtime res)))

(defun asm-blox--cell-runtime-set-stack (cell-runtime offset &optional op)
  "Set the stack at a given OFFSET of CELL-RUNTIME to top stack value.

If OP is a symbol, perform special logic."
  (let* ((row (asm-blox--cell-runtime-row cell-runtime))
         (col (asm-blox--cell-runtime-col cell-runtime))
         (stack (asm-blox--cell-runtime-stack cell-runtime))
         (offset (if (< offset 0) (+ offset (length stack)) offset))
         (curr-val (nth (- (length stack) offset 1) stack))
         (v (cond
             ((eql op 'INC) (1+ curr-val))
             ((eql op 'DEC) (1- curr-val))
             (t (asm-blox--cell-runtime-pop cell-runtime)))))
    (when (or (< offset 0) (>= offset (length stack)))
      (setq asm-blox-runtime-error  ;; TODO: extract this logic
            (list "Idx out of bounds" row col))
      (setq asm-blox--gameboard-state 'error))
    (setcar (nthcdr (- (length stack) offset 1) stack) v)))

(defun asm-blox--unary-operation (cell-runtime function)
  "Perform binary operation FUNCTION on the top two items of CELL-RUNTIME."
  (let* ((v (asm-blox--cell-runtime-pop cell-runtime))
         (res (funcall function v)))
    (asm-blox--cell-runtime-push cell-runtime res)))

(defun asm-blox--cell-runtime-send (cell-runtime direction)
  "Put the top value of CELL-RUNTIME's stack on the DIRECTION register."
  (let ((v (asm-blox--cell-runtime-pop cell-runtime))
        (current-val))
    (pcase direction
      ('UP (setq current-val (asm-blox--cell-runtime-staging-up cell-runtime)))
      ('DOWN (setq current-val (asm-blox--cell-runtime-staging-down cell-runtime)))
      ('LEFT (setq current-val (asm-blox--cell-runtime-staging-left cell-runtime)))
      ('RIGHT (setq current-val (asm-blox--cell-runtime-staging-right cell-runtime))))
    (let ((result
           (if current-val
               ;; item is blocked
               'blocked
             (asm-blox--cell-runtime-set-staging-value-from-direction cell-runtime direction v))))
      (when (eql result 'blocked)
        (asm-blox--cell-runtime-push cell-runtime v))
      result)))

(defun asm-blox--cell-runtime-get-extra (cell-runtime direction)
  "Perform the GET command on CELL-RUNTIME outside the gameboard at DIRECTION."
  (let* ((at-row (asm-blox--cell-runtime-row cell-runtime))
         (at-col (asm-blox--cell-runtime-col cell-runtime))
         (source (asm-blox--gameboard-source-at-pos at-row at-col direction)))
    (if (or (not source) (not (asm-blox--cell-source-current-value source)))
        'blocked
      (let ((v (asm-blox--cell-source-pop source)))
        (asm-blox--cell-runtime-push cell-runtime v)))))

(defun asm-blox--cell-runtime-stack-get (cell-runtime loc)
  "Perform GET command variant, grabbing the LOC value from CELL-RUNTIME's stack."
  (let ((row (asm-blox--cell-runtime-row cell-runtime))
        (col (asm-blox--cell-runtime-col cell-runtime))
        (stack (seq-reverse (asm-blox--cell-runtime-stack cell-runtime)))
        (val))
    ;; Error checking
    (if (>= loc 0)
        (progn
          (when (>= loc (length stack))
            (setq asm-blox-runtime-error ;; TODO: extract the logic here to separate function
                  (list (format "Bad idx %d/%d" loc (length stack)) row col)))
          (setq val (nth loc stack)))
      (when (> (- loc) (length stack))
        (setq asm-blox-runtime-error ;; TODO: extract the logic here to separate function
                  (list (format "Bad idx %d/%d" loc (length stack)) row col)))
      (setq val (nth (+ (length stack) loc) stack)))
    (asm-blox--cell-runtime-push cell-runtime val)))

(defun asm-blox--cell-runtime-get (cell-runtime direction)
  "Perform the GET command running from CELL-RUNTIME, recieving from DIRECTION."
  (if (integerp direction)
      (asm-blox--cell-runtime-stack-get cell-runtime direction)
    (let* ((at-row (asm-blox--cell-runtime-row cell-runtime))
           (at-col (asm-blox--cell-runtime-col cell-runtime)))
      (if (not (asm-blox--valid-position at-row at-col direction))
          (asm-blox--cell-runtime-get-extra cell-runtime direction)
        (let* ((opposite-direction (asm-blox--mirror-direction direction))
               (from-cell (asm-blox--cell-at-moved-row-col at-row at-col direction))
               (recieve-val (asm-blox--get-value-from-direction from-cell opposite-direction)))
          (if recieve-val
              (progn
                (asm-blox--cell-runtime-push cell-runtime recieve-val)
                (asm-blox--remove-value-from-direction from-cell opposite-direction))
            'blocked))))))

(defun asm-blox--true-p (v)
  "Return non-nil if V is truthy."
  (not (= 0 v)))

(defun asm-blox--cell-runtime-skip-labels (cell-runtime)
  "Skip pc over any label instructions for CELL-RUNTIME.
This logic  is needed to display current command properly."
  (while (let* ((current-instr (asm-blox--cell-runtime-current-instruction cell-runtime))
                (code-data (asm-blox-code-node-children current-instr))
                (cmd (car code-data)))
           (eql cmd 'LABEL))
    ;; TODO: check case of all LABEL commands
    (asm-blox--cell-runtime-pc-inc cell-runtime)))

(defun asm-blox--cell-runtime-step (cell-runtime)
  "Perform one step of CELL-RUNTIME."
  (let* ((current-instr (asm-blox--cell-runtime-current-instruction cell-runtime))
         (code-data (asm-blox-code-node-children current-instr))
         (cmd (car code-data))
         (status
          (pcase cmd
            ('_EMPTY 'blocked)
            ('CONST (let ((const (cadr code-data)))
                      (asm-blox--cell-runtime-push cell-runtime const)))
            ('SET (let ((stack-offset (cadr code-data)))
                    (asm-blox--cell-runtime-set-stack cell-runtime stack-offset)))
            ('INC (let ((stack-offset (cadr code-data)))
                    (asm-blox--cell-runtime-set-stack cell-runtime stack-offset 'INC)))
            ('DEC (let ((stack-offset (cadr code-data)))
                    (asm-blox--cell-runtime-set-stack cell-runtime stack-offset 'DEC)))
            ('CLR (setf (asm-blox--cell-runtime-stack cell-runtime) nil))
            ('DUP (let ((stack (asm-blox--cell-runtime-stack cell-runtime)))
                     (setf (asm-blox--cell-runtime-stack cell-runtime) (append stack stack))))
            ('ADD (asm-blox--binary-operation cell-runtime #'+))
            ('SUB (asm-blox--binary-operation cell-runtime #'-))
            ('MUL (asm-blox--binary-operation cell-runtime #'*))
            ('DIV (asm-blox--binary-operation cell-runtime #'/))
            ('REM (asm-blox--binary-operation cell-runtime #'%))
            ('AND (asm-blox--binary-operation cell-runtime(lambda (a b) (if (and (asm-blox--true-p a)
                                                                                (asm-blox--true-p b))
                                                                           1 0))))
            ('NOT (asm-blox--unary-operation cell-runtime (lambda (x) (if (asm-blox--true-p x) 0 1))))
            ('NEG (asm-blox--unary-operation cell-runtime (lambda (x) (- x))))
            ('ABS (asm-blox--unary-operation cell-runtime (lambda (x) (abs x))))
            ('OR (asm-blox--binary-operation cell-runtime (lambda (a b) (if (or (asm-blox--true-p a)
                                                                               (asm-blox--true-p b))
                                                                           1 0))))
            ('EQZ (asm-blox--unary-operation cell-runtime (lambda (x) (if (= 0 x) 1 0))))

            ('LZ (asm-blox--unary-operation cell-runtime (lambda (x) (if (< x 0) 1 0))))
            ('GZ (asm-blox--unary-operation cell-runtime (lambda (x) (if (> x 0) 1 0))))

            ('EQ (asm-blox--binary-operation cell-runtime (lambda (a b) (if (= a b) 1 0))))
            ('NE (asm-blox--binary-operation cell-runtime (lambda (a b) (if (not (= a b)) 1 0))))
            ('LT (asm-blox--binary-operation cell-runtime (lambda (a b) (if (< a b) 1 0))))
            ('LE (asm-blox--binary-operation cell-runtime (lambda (a b) (if (<= a b) 1 0))))
            ('GT (asm-blox--binary-operation cell-runtime (lambda (a b) (if (> a b) 1 0))))
            ('GE (asm-blox--binary-operation cell-runtime (lambda (a b) (if (>= a b) 1 0))))
            ('NOP (ignore))
            ('DROP (asm-blox--cell-runtime-pop cell-runtime))
            ('SEND (asm-blox--cell-runtime-send cell-runtime (cadr code-data)))
            ('GET (asm-blox--cell-runtime-get cell-runtime (cadr code-data)))
            ('RIGHT (asm-blox--cell-runtime-get cell-runtime 'RIGHT))
            ('LEFT (asm-blox--cell-runtime-get cell-runtime 'LEFT))
            ('UP (asm-blox--cell-runtime-get cell-runtime 'UP))
            ('DOWN (asm-blox--cell-runtime-get cell-runtime 'DOWN))
            ('JMP (let ((position (cadr code-data)))
                    (setf (asm-blox--cell-runtime-pc cell-runtime) position)))
            ('LABEL 'label)
            ('JMP_IF (let ((position (cadr code-data))
                           (top-value (asm-blox--cell-runtime-pop cell-runtime)))
                       (when (asm-blox--true-p top-value)
                         (setf (asm-blox--cell-runtime-pc cell-runtime) position))))
            ('JMP_IF_NOT (let ((position (cadr code-data))
                               (top-value (asm-blox--cell-runtime-pop cell-runtime)))
                           (when (not (asm-blox--true-p top-value))
                             (setf (asm-blox--cell-runtime-pc cell-runtime) position)))))))
    ;; handle PC movement
    (pcase status
      ('blocked nil)
      ('jump nil)
      ('label (progn
                (asm-blox--cell-runtime-pc-inc cell-runtime)
                (asm-blox--cell-runtime-step cell-runtime)))
      (_ (asm-blox--cell-runtime-pc-inc cell-runtime)))
    (asm-blox--cell-runtime-skip-labels cell-runtime)))

(defun asm-blox--step ()
  "Perform the operations needed to progress the game one step."
  (asm-blox--gameboard-step)
  (asm-blox--resolve-port-values)
  (asm-blox--extra-gameboard-step))

(defun asm-blox--extra-gameboard-step ()
  "Perform step on all things not on the gameboard."
  (let ((sinks (asm-blox--problem-spec-sinks asm-blox--extra-gameboard-cells)))
    (dolist (sink sinks)
      (asm-blox--cell-sink-get sink))))

(defun asm-blox--gameboard-step ()
  "Perform step on all cells on the gameboard."
  (let ((res
         (catch 'runtime-error
           (let ((last-cell-fns '()))
             (dotimes (idx (length asm-blox--gameboard))
               (let ((cell (aref asm-blox--gameboard idx)))
                 (let ((fn (asm-blox--cell-runtime-run-function cell)))
                   (if (functionp fn)
                       (setq last-cell-fns (cons (cons fn cell) last-cell-fns))
                     (asm-blox--cell-runtime-step cell)))))
             ;; We need to run the non-code cells last because they directly
             ;; manipulate their ports.
             (dolist (fn+cell last-cell-fns)
               (funcall (car fn+cell) (cdr fn+cell)))))))
    (when (eql (car res) 'error)
      (setq asm-blox--gameboard-state 'error)
      (setq asm-blox-runtime-error (cdr res)))))

(defun asm-blox--resolve-port-values ()
  "Move staging port values to main, propogate nils up to staging."
  (dotimes (idx (length asm-blox--gameboard))
    (let ((cell (aref asm-blox--gameboard idx)))
      (asm-blox--cell-runtime-merge-ports-with-staging cell))))

(defun asm-blox-check-winning-conditions ()
  "Return non-nil if all sinks are full."
  (when (not (asm-blox--gameboard-in-final-state-p))
    (let ((sinks (asm-blox--problem-spec-sinks asm-blox--extra-gameboard-cells))
          (win-p t))
      (while (and sinks win-p)
        (let* ((sink (car sinks))
               (expected-data (asm-blox--cell-sink-expected-data sink))
               (idx (asm-blox--cell-sink-idx sink))
               (err-val (asm-blox--cell-sink-err-val sink)))
          (if (asm-blox--cell-sink-expected-text sink)
              ;; If expected text exists we are dealing with an editor sink
              (let* ((expected-text (string-trim-right (asm-blox--cell-sink-expected-text sink)))
                     (expected-lines (split-string expected-text "\n"))
                     (text (string-trim-right (asm-blox--cell-sink-editor-text sink)))
                     (text-lines (split-string text "\n")))
                (if (not (equal (length text-lines) (length expected-lines)))
                    (setq win-p nil)
                  (unless (cl-loop for expected-line in expected-lines
                                   for line in text-lines
                                   always (equal (string-trim-right expected-line) (string-trim-right line)))
                    (setq win-p nil))))
            (when (or (< idx (length expected-data))
                      err-val)
              (setq win-p nil)))
          (setq sinks (cdr sinks))))
      (when win-p
        (asm-blox--win-file-for-current-buffer)
        ;; TODO: Do something else for the victory.
        (setq asm-blox--gameboard-state 'win)
        (message "Congragulations, you won!")))))

;;; Gameboard Display Helpers ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Functions that map the domain of the gameboard to that of the
;; display.

(defun asm-blox--get-direction-col-registers (row col direction)
  "Return the register value for the DIRECTION registers at ROW, COL.
ROW and COL here do not refer to the coordinates of a
cell-runtime but rather the in-between row/col."
  (cl-assert (or (eql 'LEFT direction) (eql 'RIGHT direction)))
  (cl-assert (<= 0 row (1- asm-blox--gameboard-row-ct)))
  (cl-assert (<= 0 col asm-blox--gameboard-col-ct))
  (let ((cell-col (if (eql 'RIGHT direction) (1- col) col)))
    (cond
     ;; outputs are never displayed on the board
     ((or (and (= col 0) (eql direction 'LEFT))
          (and (= row asm-blox--gameboard-col-ct) (eql direction 'RIGHT)))
      nil)
     ((or (= col 0)
          (= col asm-blox--gameboard-col-ct))
      (let* ((source (asm-blox--gameboard-source-at-pos row cell-col)))
        (if (not source)
            nil
          (asm-blox--cell-source-current-value source))))
     (t
      (let* ((cell-runtime (asm-blox--cell-at-row-col row cell-col)))
        (asm-blox--get-value-from-direction cell-runtime direction))))))

(defun asm-blox--get-direction-row-registers (row col direction)
  "Return the register value for the DIRECTION registers at ROW, COL.
ROW and COL here do not refer to the coordinates of a
cell-runtime but rather the in-between row/col."
  (cl-assert (or (eql 'UP direction) (eql 'DOWN direction)))
  (cl-assert (<= 0 row asm-blox--gameboard-row-ct))
  (cl-assert (<= 0 col (1- asm-blox--gameboard-col-ct)))
  (let ((cell-row (if (eql 'DOWN direction) (1- row) row)))
    (cond
     ((or (and (= row 0) (eql direction 'UP))
          (and (= row asm-blox--gameboard-row-ct) (eql direction 'DOWN)))
      nil)
     ((or (= row 0)
          (= row asm-blox--gameboard-row-ct))
      (let* ((source (asm-blox--gameboard-source-at-pos cell-row col)))
        (if (not source)
            nil
          (asm-blox--cell-source-current-value source))))
     (t
      (let* ((cell-runtime (asm-blox--cell-at-row-col cell-row col)))
        (asm-blox--get-value-from-direction cell-runtime direction))))))

(defun asm-blox--get-source-idx-at-position (row col)
  "Return name of source at position ROW, COL if exists."
  (let ((sources (asm-blox--problem-spec-sources asm-blox--extra-gameboard-cells))
        (found))
    (while sources
      (let ((source (car sources)))
        (if (and (= (asm-blox--cell-source-row source) row)
                 (= (asm-blox--cell-source-col source) col))
            (progn (setq sources nil)
                   (setq found (asm-blox--cell-source-name source)))
          (setq sources (cdr sources)))))
    found))

(defun asm-blox--get-sink-name-at-position (row col)
  "Return name of sink at position ROW, COL if exists."
  (let ((sinks (asm-blox--problem-spec-sinks asm-blox--extra-gameboard-cells))
        (found))
    (while sinks
      (let ((sink (car sinks)))
        (if (and (= (asm-blox--cell-sink-row sink) row)
                 (= (asm-blox--cell-sink-col sink) col))
            (progn (setq sinks nil)
                   (setq found (asm-blox--cell-sink-name sink)))
          (setq sinks (cdr sinks)))))
    found))

;;; Problem Infrastructure ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A problem generator is a function that when called, returns a
;; problem-spec, ie a list of source nodes and a list of sink nodes.
;; The source nodes indicate at which positions input should be feed
;; to the board while the sink node indicates at which position should
;; output be consumed.  The sink node should also have a list of the
;; expected output.

(cl-defstruct (asm-blox--cell-source
               (:constructor asm-blox--cell-source-create)
               (:copier nil))
  "A provider of data to the gameboard.
ROW and COL are expected to be off the board (ie less than 0 or
greater than max).  DATA contains the numbers that will be feeded
to the neighboring cell and IDX contains which datum is to be
sent next.  NAME contains the letter that will be displayd on the board."
  row col data idx name)

(cl-defstruct (asm-blox--cell-sink
               (:constructor asm-blox--cell-sink-create)
               (:copier nil))
  "A consumber of data to detirmine winstate.
A normal sink contains expected-data while an editor sink contains
editor- values (and no data).  ERR-VAL is the incorrect value that was fed
into the sink.  EDITOR-TEXT and EXPECTED-TEXT is the current text and target
text respectively."
  row col expected-data idx name err-val
  default-editor-text
  editor-text editor-point expected-text)

(cl-defstruct (asm-blox--problem-spec
               (:constructor asm-blox--problem-spec-create)
               (:copier nil))
  "An entire definition of a gameboard.
SOURCES and SINKS are the input and output of the game respectively.  NAME,
DESCRIPTION, and DIFFICULTY are metadata about the puzzle."
  sources sinks name description difficulty)

(defun asm-blox--reset-extra-gameboard-cells-state ()
  "Reset the state of all cells not in the grid (sources and sinks)."
  (let ((sources (asm-blox--problem-spec-sources asm-blox--extra-gameboard-cells))
        (sinks (asm-blox--problem-spec-sinks asm-blox--extra-gameboard-cells)))
    (dolist (source sources)
      (setf (asm-blox--cell-source-idx source) 0))
    (dolist (sink sinks)
      (setf (asm-blox--cell-sink-idx sink) 0)
      (setf (asm-blox--cell-sink-err-val sink) nil)
      (when (asm-blox--cell-sink-expected-text sink)
        (if (asm-blox--cell-sink-default-editor-text sink)
            (setf (asm-blox--cell-sink-editor-text sink)
                  (asm-blox--cell-sink-default-editor-text sink))
          (setf (asm-blox--cell-sink-editor-text sink) ""))
        (setf (asm-blox--cell-sink-editor-point sink) 1)))))

(defun asm-blox--cell-sink-get (sink)
  "Grab a value and put it into SINK from the gameboard."
  (let* ((row (asm-blox--cell-sink-row sink))
         (col (asm-blox--cell-sink-col sink))
         (direction (cond
                     ((>= col asm-blox--gameboard-col-ct) 'LEFT)
                     ((> 0 col) 'RIGHT)
                     ((> 0 row) 'DOWN)
                     ((>= row asm-blox--gameboard-row-ct) 'UP)))
         (opposite-direction (asm-blox--mirror-direction direction))
         (cell-runtime (asm-blox--cell-at-moved-row-col row col direction))
         (v (asm-blox--get-value-from-direction cell-runtime opposite-direction)))
    (if v
        (let* ((data (asm-blox--cell-sink-expected-data sink))
               (idx (asm-blox--cell-sink-idx sink))
               (expected-value (nth idx data)))
          (when (not (equal expected-value v))
            ;; TODO - do something here
            (setq asm-blox--gameboard-state 'error)
            (setf (asm-blox--cell-sink-err-val sink) v)
            (message "Unexpected value"))
          (setf (asm-blox--cell-sink-idx sink) (1+ idx))
          (asm-blox--remove-value-from-direction cell-runtime opposite-direction))
      'blocked)))

(defun asm-blox--cell-sink-insert-character (sink char)
  "For a textual SINK, insert CHAR."
  (let ((text (asm-blox--cell-sink-editor-text sink))
        (point (asm-blox--cell-sink-editor-point sink)))
    (cond
     ((<= 32 char 126)
      (setf (asm-blox--cell-sink-editor-text sink)
            (concat (substring text 0 (1- point))
                    (char-to-string char)
                    (substring text (1- point))))
      (setf (asm-blox--cell-sink-editor-point sink)
            (1+ point)))
     ((= char ?\n)
      (setf (asm-blox--cell-sink-editor-text sink)
            (concat (substring text 0 (1- point))
                    (char-to-string char)
                    (substring text (1- point))))
      (setf (asm-blox--cell-sink-editor-point sink)
            (1+ point)))
     ((or (= char ?\b) (= char -1))
      (when (not (= 1 point))
        (setf (asm-blox--cell-sink-editor-text sink)
              (concat (substring text 0 (- point 2))
                      (substring text (- point 1))))
        (setf (asm-blox--cell-sink-editor-point sink)
              (max (1- point) 1))))
     ((or (= char -2))
      (when (not (= 1 point))
        (setf (asm-blox--cell-sink-editor-text sink)
              (concat (substring text 0 (- point 1))
                      (substring text point))))))))

(defun asm-blox--cell-sink-move-point (sink point)
  "For a textual SINK, move the point to POINT."
  (let* ((text (asm-blox--cell-sink-editor-text sink))
         (bounded-pt (max (min point (1+ (length text))) 1)))
    (setf (asm-blox--cell-sink-editor-point sink) bounded-pt)))

(defun asm-blox--cell-source-current-value (source)
  "Return the value of SOURCE that will be taken next."
  (unless (asm-blox--cell-source-p source)
    (error "Cell-source-pop type error"))
  (let* ((data (asm-blox--cell-source-data source))
         (idx (asm-blox--cell-source-idx source))
         (top (nth idx data)))
    top))

(defun asm-blox--cell-source-pop (source)
  "Pop a value from the data of SOURCE."
  (unless (asm-blox--cell-source-p source)
    (error "Cell-source-pop type error"))
  (let* ((data (asm-blox--cell-source-data source))
         (idx (asm-blox--cell-source-idx source))
         (top (nth idx data)))
    (setf (asm-blox--cell-source-idx source) (1+ idx))
    top))

;;; File Saving ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun asm-blox--generate-new-puzzle-filename (name)
  "For puzzle NAME, determine the name for a new puzzle."
  ;; TODO: This work when deleting puzzles.
  (ignore-errors
    (make-directory asm-blox-save-directory-name))
  (let* ((last-id (car (reverse (asm-blox--saved-puzzle-ct-ids name))))
         (new-idx (number-to-string (1+ (or last-id 0)))))
    (expand-file-name (concat name "-" new-idx ".asbx")
                      asm-blox-save-directory-name)))

(defun asm-blox--parse-saved-buffer ()
  "Extract the puzzle contents from the saved buffer setting up state."
  (save-excursion
    (goto-char (point-min))
    (setq asm-blox-box-contents (make-hash-table :test 'equal))
    ;; Get the literal contents of each box.
    (let ((match-regexp (format "│\\(.\\{%d\\}\\)│" asm-blox-box-width)))
     (dotimes (i (* asm-blox--gameboard-row-ct
                    asm-blox--gameboard-col-ct
                    asm-blox-box-height))
       (search-forward-regexp match-regexp)
       (let* ((col (mod i asm-blox--gameboard-col-ct))
              (row (/ (/ i asm-blox--gameboard-col-ct) asm-blox-box-height))
              (match (string-trim-right (match-string-no-properties 1)))
              (current-text (gethash (list row col) asm-blox-box-contents))
              (next-text (if current-text
                             (concat current-text "\n" match)
                           match)))
         (puthash (list row col) next-text asm-blox-box-contents))))
    ;; Trim the ends of the boxes, assume user doesn't want it.
    (dotimes (row asm-blox--gameboard-row-ct)
      (dotimes (col asm-blox--gameboard-col-ct)
        (let ((prev-val (gethash (list row col) asm-blox-box-contents)))
          (puthash (list row col)
                   (string-trim-right prev-val)
                   asm-blox-box-contents))))
    ;; Find the name of the puzzle.
    (search-forward-regexp "^\\([[:alnum:] -]+\\):")
    (let ((match (match-string 1)))
      (unless match
        (error "Bad file format, no puzzle name found"))
      (let ((puzzle (asm-blox--get-puzzle-by-id match)))
        (unless puzzle
          (error "No puzzle with name %s found" puzzle))
        (setq asm-blox--extra-gameboard-cells (funcall puzzle))))))

(defun asm-blox--saved-puzzle-ct-ids (id)
  "Return saved puzzle ids that start with the puzzle ID."
  (let ((ids))
    (dolist (file-name (directory-files asm-blox-save-directory-name))
      (when (string-match (concat "\\`" (regexp-quote id) "-\\([0-9]+\\)") file-name)
        (let ((id-val (string-to-number (match-string 1 file-name))))
          (setq ids (cons id-val ids)))))
    (sort ids #'<)))

(defun asm-blox--make-puzzle-idx-file-name (id idx)
  "Create a file name for puzzle with ID and IDX."
  (expand-file-name (concat id "-" (number-to-string idx) ".asbx")
                    asm-blox-save-directory-name))

(defun asm-blox--win-file-for-current-buffer ()
  "Return the name of the win-backup for the current execution buffer."
  (unless (equal (buffer-name) "*asm-blox-execution*")
    (error "Unable to win from non-execution buffer"))
  (unless (bufferp asm-blox-execution-origin-buffer)
    (error "Unable to find buffer with winning solution"))
  (let* ((buffer-contents (with-current-buffer asm-blox-execution-origin-buffer
                            (buffer-string)))
         (bfn (with-current-buffer asm-blox-execution-origin-buffer
                            (buffer-file-name)))
         (name (file-name-nondirectory bfn))
         (path (file-name-directory bfn))
         (new-name (concat path "." name ".win.txt")))
    (save-window-excursion
      (let ((inhibit-read-only t))
        (find-file new-name)
        (erase-buffer)
        (insert buffer-contents)
        (save-buffer)
        (kill-buffer)))))

(defun asm-blox--restore-backup ()
  "Create a backup file for the current buffer."
  (let* (;; (buffer-contents (buffer-string))
         (bfn (buffer-file-name))
         (name (file-name-nondirectory bfn))
         (path (file-name-directory bfn))
         (new-name (concat path "." name ".backup.txt")))
    (when (file-exists-p new-name)
      (let ((backup-contents))
       (save-window-excursion
         (find-file new-name)
         (setq backup-contents (buffer-string)))
       (let ((inhibit-read-only t))
         (erase-buffer)
         (insert backup-contents))
       (asm-blox--parse-saved-buffer)))))

(defun asm-blox--backup-file-for-current-buffer ()
  "Create a backup file for the current buffer."
  (let* ((buffer-contents (buffer-string))
         (bfn (buffer-file-name))
         (name (file-name-nondirectory bfn))
         (path (file-name-directory bfn))
         (new-name (concat path "." name ".backup.txt")))
    (save-window-excursion
      (let ((inhibit-read-only t))
        (find-file new-name)
        (erase-buffer)
        (insert buffer-contents)
        (save-buffer)
        (kill-buffer)))))


(defun asm-blox--puzzle-won-p (puzzle-name)
  "Return non-nil if a win file exists for puzzle with name PUZZLE-NAME."
  (ignore-errors
    (make-directory asm-blox-save-directory-name))
  (seq-find (lambda (n) (and (string-match (regexp-quote puzzle-name) n)
                             (string-match "\\.win" n)))
            (directory-files asm-blox-save-directory-name)))

;;; YAML Blocks

(defun asm-blox--create-yaml-code-node (row col code)
  "Create a runtime for the parsed CODE, located at ROW COL."
  (catch 'error
   (let-alist (yaml-parse-string code :object-type 'alist :sequence-type 'list)
     ;; .apiVersion .kind .metadata .spec
     (unless (equal "v1" .apiVersion)
       (throw 'error '(error 0 "bad api version")))
     (when (or (not .spec) (eql .spec :null))
       (throw 'error '(error 0 "must define spec")))
     (pcase .kind
       ("Stack" (asm-blox--yaml-create-stack row col .metadata .spec))
       ("Controller" (asm-blox--yaml-create-controller row col .metadata .spec))
       ("Container" (error "Container not implemented"))
       ("Network" (error "Network not implemented"))
       ("Heap" (asm-blox--yaml-create-heap row col .metadata .spec))
       (_ (throw 'error '(error 0 "unknown kind")))))))

(defun asm-blox--transform-sexp-data (plist)
  "Convert sexp spec to a legacy spec."
  (let ((res '()))
   (while plist
     (let* ((key (symbol-name (car plist)))
            (val (cadr plist))
            (_ (when (not (= (aref key 0) ?:))
                 (throw 'error '(error 0 "invalid spec key"))))
            (new-key (substring key 1 (length key)))
            (new-key (string-replace "-" "" (capitalize new-key)))
            (new-key (concat (downcase (substring new-key 0 1))
                             (substring new-key 1 (length new-key)))))
       (when (symbolp val)
         (setq val (symbol-name val)))
       (when (listp val)
         (setq val (seq-map #'symbol-name val)))
       (push (cons (intern new-key) val) res)
       (setq plist (cddr plist))))
   res))

(defun asm-blox--create-sexp-code-node (row col code)
  "Create a runtime for the parsed CODE, located at ROW COL."
  (catch 'error
    (let* ((data (read code))
           (kind (cadr data))
           (spec (asm-blox--transform-sexp-data (cddr data))))
      (when (or (not spec))
        (throw 'error '(error 0 "must define spec")))
      (pcase kind
        ('stack
         (asm-blox--yaml-create-stack row col nil spec))
        ('controller
         (asm-blox--yaml-create-controller row col nil spec))
        ('container
         (error "Container not implemented"))
        ('network
         (error "Network not implemented"))
        ('heap
         (asm-blox--yaml-create-heap row col nil spec))
        (_ (throw 'error '(error 0 "unknown kind")))))))

(defun asm-blox--yaml-message-stack (cell-runtime)
  "Return message to disblay for CELL-RUNTIME of YAML Stack."
  (let* ((state (asm-blox--cell-runtime-run-state cell-runtime))
         (spec (asm-blox--cell-runtime-run-spec cell-runtime))
         (size (cdr (assoc 'size spec)))
         (output-port (intern (upcase (cdr (assoc 'outputPort spec)))))
         (val (asm-blox--get-value-from-direction cell-runtime output-port)))
    (when val
      (setq state (cons val state)))
    (if (= 0 (length state))
        "empty stack"
      (format "top:%d size:%d/%d" (car state) (length state) (or size 20)))))

(defun asm-blox--yaml-step-stack (cell-runtime)
  "Perform the step operation for the CELL-RUNTIME of kind Stack."
  (let ((row (asm-blox--cell-runtime-row cell-runtime))
        (col (asm-blox--cell-runtime-col cell-runtime))
        (spec (asm-blox--cell-runtime-run-spec cell-runtime))
        (state (asm-blox--cell-runtime-run-state cell-runtime)))
    (let-alist spec
      ;; .inputPort .outputPort .sizePort .size .logLevel
      ;; .inputPorts
      ;; First put port back on the stack
      (let* ((port-sym (intern (upcase .outputPort)))
             (size-port-sym (and .sizePort (intern (upcase .sizePort))))
             (val (asm-blox--get-value-from-direction cell-runtime port-sym)))
        (when val
          (asm-blox--remove-value-from-direction cell-runtime port-sym)
          (setq state (cons val state)))
        (when .sizePort
          (asm-blox--remove-value-from-direction cell-runtime size-port-sym)))

      ;; Then add new values to stack
      (seq-do
       (lambda (port)
         (let* ((port-sym (intern (upcase port)))
                (from-cell (asm-blox--cell-at-moved-row-col row col port-sym))
                (opposite-port (asm-blox--mirror-direction port-sym))
                (recieve-val (asm-blox--get-value-from-direction from-cell opposite-port)))
           (when recieve-val
             (asm-blox--remove-value-from-direction from-cell opposite-port)
             (setq state (cons recieve-val state))
             (when (> (length state) (or .size 20)) ;; TODO: find a good way of setting defualts.
               (setf (asm-blox--cell-runtime-run-state cell-runtime) state)
               (throw 'runtime-error `(error ,(format "Stack overflow %d/%d" (length state) (or .size 20))
                                             ,row ,col))))))
       (if .inputPorts .inputPorts (list .inputPort)))

      ;; Add size to sizePort
      (when .sizePort
        (asm-blox--cell-runtime-set-staging-value-from-direction
         cell-runtime
         (intern (upcase .sizePort))
         (length state)))

      ;; Add top value of stack to port
      (when state
        (asm-blox--cell-runtime-set-staging-value-from-direction
         cell-runtime
         (intern (upcase .outputPort))
         (car state))
        (setq state (cdr state)))

      ;; Persist state for next rount
      (setf (asm-blox--cell-runtime-run-state cell-runtime) state))))

(defun asm-blox--yaml-get-editor-sink (_)
  "Return the sink corresponding to CELL-RUNTIME."
  ;; For now there will only be one editor. If more use parameter passed.
  (car (asm-blox--problem-spec-sinks asm-blox--extra-gameboard-cells)))

(defun asm-blox--yaml-step-controller (cell-runtime)
  "Perform runtime step for a CELL-RUNTIME of kind YAML Controller."
  (let ((row (asm-blox--cell-runtime-row cell-runtime))
        (col (asm-blox--cell-runtime-col cell-runtime))
        (spec (asm-blox--cell-runtime-run-spec cell-runtime))
        ;; (state (asm-blox--cell-runtime-run-state cell-runtime))
        (sink (asm-blox--yaml-get-editor-sink cell-runtime)))
    (let-alist spec
      ;; .inputPort  .setPointPort
      ;; .charAtPort .pointPort
      ;; First consume setPoint port
      (when .setPointPort
        (let* ((port-sym (intern (upcase .setPointPort)))
               (from-cell (asm-blox--cell-at-moved-row-col row col port-sym))
               (opposite-port (asm-blox--mirror-direction port-sym))
               (recieve-val (asm-blox--get-value-from-direction from-cell opposite-port)))
          (when recieve-val
            (asm-blox--remove-value-from-direction from-cell opposite-port)
            (asm-blox--cell-sink-move-point sink recieve-val))))

      ;; Next consume inputPort
      (when .inputPort
        (let* ((port-sym (intern (upcase .inputPort)))
               (from-cell (asm-blox--cell-at-moved-row-col row col port-sym))
               (opposite-port (asm-blox--mirror-direction port-sym))
               (recieve-val (asm-blox--get-value-from-direction from-cell opposite-port)))
          (when recieve-val
            (asm-blox--remove-value-from-direction from-cell opposite-port)
            (asm-blox--cell-sink-insert-character sink recieve-val))))

      ;; Get the point and atChar and send them to respective ports
      (when .charAtPort
        (let* ((port-sym (intern (upcase .charAtPort)))
               (val (asm-blox--get-value-from-direction cell-runtime port-sym))
               (point (asm-blox--cell-sink-editor-point sink))
               (text (asm-blox--cell-sink-editor-text sink)))
          (when val
            (asm-blox--remove-value-from-direction cell-runtime port-sym))
          (asm-blox--cell-runtime-set-staging-value-from-direction
           cell-runtime
           (intern (upcase .charAtPort))
           (aref text (1- point)))))

      (when .pointPort
        (let* ((port-sym (intern (upcase .pointPort)))
               (val (asm-blox--get-value-from-direction cell-runtime port-sym))
               (point (asm-blox--cell-sink-editor-point sink)))
          (when val
            (asm-blox--remove-value-from-direction cell-runtime port-sym))
          (asm-blox--cell-runtime-set-staging-value-from-direction
           cell-runtime
           (intern (upcase .pointPort))
           point))))))

(defun asm-blox--yaml-message-heap (cell-runtime)
  "Return message to display at bottom of CELL-RUNTIME of YAML Heap."
  (let* ((state (asm-blox--cell-runtime-run-state cell-runtime))
         (offset (car state))
         (data (cdr state)))
    (cond
     ((>= offset (length data)) "end of file")
     ((>= offset 0)
      (let ((offset-val (aref data offset)))
        (format "%d @%d/%d" offset-val offset (1- (length data)))))
     (t "~~~"))))

(defun asm-blox--yaml-step-heap (cell-runtime)
  "Perform runtime step for CELL-RUNTIME of type YAML HEAP."
  (let* ((row (asm-blox--cell-runtime-row cell-runtime))
         (col (asm-blox--cell-runtime-col cell-runtime))
         (spec (asm-blox--cell-runtime-run-spec cell-runtime))
         (state (asm-blox--cell-runtime-run-state cell-runtime))
         (offset (car state))
         (data (cdr state)))
    (let-alist spec
      ;; A heap contains as state two elements:
      ;; - current address
      ;; - offset, starting at 1
      ;; .readPort .writePort
      ;; .seekPort .offsetPort
      ;; .setPort  .peekPort
      (when .readPort
        (when (not (asm-blox--get-value-from-direction cell-runtime (intern (upcase .readPort))))
          (setq offset (1+ offset))))
      (when .seekPort
        (let* ((port-sym (intern (upcase .seekPort)))
               (from-cell (asm-blox--cell-at-moved-row-col row col port-sym))
               (opposite-port (asm-blox--mirror-direction port-sym))
               (recieve-val (asm-blox--get-value-from-direction from-cell opposite-port))
               (stage-recieve-val (asm-blox--get-value-from-staging-direction from-cell opposite-port)))
          (cond
           (recieve-val
            (asm-blox--remove-value-from-direction from-cell opposite-port)
            (setq offset recieve-val))
           (stage-recieve-val
            (asm-blox--remove-value-from-staging-direction from-cell opposite-port)
            (setq offset stage-recieve-val)))))
      (when .writePort
        (let* ((port-sym (intern (upcase .writePort)))
               (from-cell (asm-blox--cell-at-moved-row-col row col port-sym))
               (opposite-port (asm-blox--mirror-direction port-sym))
               (recieve-val (asm-blox--get-value-from-direction from-cell opposite-port)))
          (when (not (<= 0 offset (1- (length data))))
            (throw 'runtime-error `(error "Idx out of bounds" ,row ,col)))
          (when recieve-val
            (asm-blox--remove-value-from-direction from-cell opposite-port)
            (aset data offset recieve-val)
            (setq offset (1+ offset)))))
      (when .setPort
        (let* ((port-sym (intern (upcase .setPort)))
               (from-cell (asm-blox--cell-at-moved-row-col row col port-sym))
               (opposite-port (asm-blox--mirror-direction port-sym))
               (recieve-val (asm-blox--get-value-from-direction from-cell opposite-port)))
          (when recieve-val
            (asm-blox--remove-value-from-direction from-cell opposite-port)
            (aset data offset recieve-val))))
      (when .offsetPort
        (let* ((port-sym (intern (upcase .offsetPort)))
               (val (asm-blox--get-value-from-direction cell-runtime port-sym)))
          (when val
            (asm-blox--remove-value-from-direction cell-runtime port-sym))
          (asm-blox--cell-runtime-set-staging-value-from-direction
           cell-runtime
           (intern (upcase .offsetPort))
           offset)))
      (when .readPort
        (let* ((port-sym (intern (upcase .readPort)))
               (val (asm-blox--get-value-from-direction cell-runtime port-sym))
               (datum (if (>= offset (length data)) -999 (aref data offset))))
          (when val
            (asm-blox--remove-value-from-direction cell-runtime port-sym))
          (asm-blox--cell-runtime-set-staging-value-from-direction
           cell-runtime
           (intern (upcase .readPort))
           datum)))
      (when .peekPort
        (when (= -1 offset) (setq offset 0)) ;; another hack related to the way readPort works
        (let* ((port-sym (intern (upcase .peekPort)))
               (val (asm-blox--get-value-from-direction cell-runtime port-sym))
               (datum (if (>= offset (length data)) -999 (aref data offset))))
          (when val
            (asm-blox--remove-value-from-direction cell-runtime port-sym))
          (asm-blox--cell-runtime-set-staging-value-from-direction
           cell-runtime
           (intern (upcase .peekPort))
           datum)))
      (setf (asm-blox--cell-runtime-run-state cell-runtime) (cons offset data)))))

(defun asm-blox--yaml-create-stack (row col _ spec)
  "Return a Stack runtime according to SPEC with METADATA at ROW COL."
  ;; .inputPort .outputPort .sizePort .size .logLevel
  (asm-blox--verify-stack spec)
  (asm-blox--cell-runtime-create
   :instructions nil
   :pc nil
   :row row
   :col col
   :run-function #'asm-blox--yaml-step-stack
   :message-function #'asm-blox--yaml-message-stack
   :run-spec spec))

(defun asm-blox--verify-port (name port)
  "Throw error for PORT with NAME if not-empty and not a port."
  (when (and port (or (eql port ':null)
                      (not (asm-blox--portp (upcase port)))))
    (throw 'error `(error 0 ,(format "invalid %s" name)))))

(defun asm-blox--verify-stack (spec)
  "Throw error if YAML box's SPEC is not a valid Stack."
  (when (eql spec ':null)
    (throw 'error '(error 0 "spec can't be empty")))
  (let* ((input-port (cdr (assoc 'inputPort spec)))
         (input-ports (cdr (assoc 'inputPorts spec)))
         (input-ports (or input-ports (list input-port)))
         (output-port (cdr (assoc 'outputPort spec)))
         (size-port (cdr (assoc 'sizePort spec)))
         (size (cdr (assoc 'size spec))))
    (when (or (= (length input-ports) 0) (not (car input-ports)))
      (throw 'error `(error 0 "missing inputPort")))

    (when (not output-port)
      (throw 'error `(error 0 "missing outputPort")))
    (when (eql output-port ':null)
      (throw 'error `(error 0 "outputPort is empty"))) ;; TODO: test this

    (when (and size-port (or (eql size-port ':null)
                             (not (asm-blox--portp (upcase size-port)))))
      (throw 'error `(error 0 "invalid sizePort")))

    (dolist (input-port input-ports)
      (when (or (eql input-port ':null)
                (not (asm-blox--portp (upcase input-port))))
        (throw 'error `(error 0 "invalid inputPort"))))

    (when (and size (or (eql size ':null)
                        (not (numberp size))
                        (<= size 0)
                        (<= 999 size)))
      (throw 'error '(error 0 "invalid size")))
    (when (and size-port output-port (eql size-port output-port))
      (error 'error `(error 0 "same OUT ports")))))

(defun asm-blox--verify-controller (spec)
  "Throw error if YAML box's SPEC is not a valid Controller."
  (when (eql spec ':null)
    (throw 'error '(error 0 "spec can't be empty"))))

(defun asm-blox--yaml-create-controller (row col _ spec)
  "Return a Controller runtime according to SPEC with METADATA at ROW COL."
  (asm-blox--cell-runtime-create
   :instructions nil
   :pc nil
   :row row
   :col col
   :run-function #'asm-blox--yaml-step-controller
   :run-spec spec))

(defun asm-blox--verify-heap (spec)
  "Throw error if YAML box's SPEC is not a valid Heap."
  (when (eql spec ':null)
    (throw 'error '(error 0 "spec can't be empty")))
  (dolist (prop '(writePort seekPort setPort offsetPort peekPort readPort))
    (let ((port (cdr (assoc prop spec))))
      (asm-blox--verify-port prop port)))
  (dolist (props '(("IN" writePort seekPort setPort) ("OUT" offsetPort peekPort readPort)))
    (let ((ports (seq-map
                  (lambda (n) (intern n))
                  (seq-filter
                   #'stringp
                   (seq-map (lambda (prop-name) (cdr (assoc prop-name spec))) (cdr props))))))
      (while ports
        (let ((top (car ports))
              (rest (cdr ports)))
          (when (and top (memq top rest))
            (throw 'error `(error 0 ,(format "same port: %s" (symbol-name top))))))
        (setq ports (cdr ports)))))
  (let ((size (cdr (assoc 'size spec))))
    (when (and size (or (eql size ':null)
                        (not (numberp size))
                        (<= size 0)
                        (<= 999 size)))
      (throw 'error `(error 0 "invalid sizePort")))))

(defun asm-blox--yaml-create-heap (row col _ spec)
  "Return a Stack runtime according to SPEC with METADATA at ROW COL."
  ;; .size
  (asm-blox--verify-heap spec)
  (let-alist spec
    (when (and .size (>= 1 .size 999))
      (throw 'error '(error 0 "invalid heap size")))
    (let ((data (make-vector (or .size 20) 0)))
      (cl-loop for elt across .data
               for i from 0
               do (aset data i elt))
      (asm-blox--cell-runtime-create
       :instructions nil
       :pc nil
       :row row
       :col col
       :run-function #'asm-blox--yaml-step-heap
       :message-function #'asm-blox--yaml-message-heap
       ;; -1 because used hack to increment offset which will
       ;; run once at the start of the game.
       :run-state (cons -1 data)
       :run-spec spec))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Display Code ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defface asm-blox-show-paren-match-face
  '((t (:inherit show-paren-match)))
  "`asm-blox-mode' face used for a matching paren pair."
  :group 'asm-blox)

(defface asm-blox-region-face
  '((t (:inherit region)))
  "`asm-blox-mode' face used for a matching paren pair."
  :group 'asm-blox)

(defface asm-blox-highlight-face
  '((t (:inherit underline)))
  "`asm-blox-mode' face used for highlighting executing code."
  :group 'asm-blox)

(defface asm-blox-error-face
  '((t (:inherit error)))
  "`asm-blox-mode' face used for displaying errors."
  :group 'asm-blox)

(defvar asm-blox--widget-row-idx nil)
(defvar asm-blox--current-widgets nil
  "List of widgets to display on right of gameboard.")
(defvar asm-blox-parse-errors nil)
(defvar asm-blox--disable-redraw nil
  "If non-nil, commands should not opt-in to redrawing the gameboard.")
(defvar asm-blox--end-of-box-points nil
  "Contains a hashmap of the points where each box ends.
This was added for performance reasons.")

(defconst asm-blox--mirror-buffer-name "*asm-blox-temp*")

(defvar-local asm-blox--display-mode 'edit)

(defun asm-blox--get-error-at-cell (row col)
  "Return the error at position ROW COL."
  (if (and (eql asm-blox--display-mode 'execute)
           asm-blox-runtime-error
           (equal (list row col) (cdr asm-blox-runtime-error)))
      (truncate-string-to-width (car asm-blox-runtime-error) (1- asm-blox-box-width) )
    (let ((err (assoc (list row col) asm-blox-parse-errors)))
      (cdr err))))

;;; Widget display ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun asm-blox--make-source-widget (source)
  "Return a widget displaying SOURCE."
  (let ((source-widget-offset-ct 2))
    (lambda (msg)
      (pcase msg
        ('width 6)
        (`(display ,n)
         (let ((data (asm-blox--cell-source-data source))
               (idx (asm-blox--cell-source-idx source))
               (name (asm-blox--cell-source-name source)))
           (pcase n
             (0 (format "IN: %s " name))
             (1 "┌────┐")
             (47 "└────┘")
             (_
              (let* ((num (nth (- n source-widget-offset-ct) data))
                     (current-row-p (= (- n source-widget-offset-ct)
                                       idx))
                     (inner-str (if num (format "%4d" num) "    "))
                     (inner-str (if current-row-p
                                    (propertize inner-str
                                                'font-lock-face '(:background "#777"))
                                  inner-str)))
                (format "│%s│" inner-str))))))))))

(defun asm-blox--make-sink-widget (sink)
  "Return a widget displaying SINK."
  (let ((sink-widget-offset-ct 2))
    (lambda (msg)
      (pcase msg
        ('width 6)
        (`(display ,n)
         (let* ((data (asm-blox--cell-sink-expected-data sink))
                (err-val (asm-blox--cell-sink-err-val sink))
                (idx (asm-blox--cell-sink-idx sink))
                (last-idx-p (= (1- idx) (- n sink-widget-offset-ct)))
                (name (asm-blox--cell-sink-name sink)))
           (pcase n
             (0 (format "OUT: %s" name))
             (1 "┌────┐")
             (47 "└────┘")
             (_
              (let* ((num (nth (- n sink-widget-offset-ct) data))
                     (included-p (<= (- n sink-widget-offset-ct)
                                     (1- idx)))
                     (inner-str (if (and num included-p) (format "%4d" num) "    "))
                     (inner-str (if (and err-val last-idx-p)
                                    (propertize (format "%4d" err-val) 'font-lock-face '(:background "red"))
                                  inner-str)))
                (format "│%s│" inner-str))))))))))

(defun asm-blox--make-editor-widget (sink)
  "Return a widget displaying a SINK as an editor."
  (let ((width 32))
    (lambda (msg)
      (pcase msg
        ('width width)
        (`(display ,n)
         (let* ((box-top (concat "┌" (make-string 30 ?─) "┐"))
                (box-bottom (concat "└" (make-string 30 ?─) "┘"))
                (spacing (make-string 32 ?\s))
                ;; (data (asm-blox--cell-sink-expected-data sink))
                (text (asm-blox--cell-sink-editor-text sink))
                (expected-text (asm-blox--cell-sink-expected-text sink))
                (point (1- (asm-blox--cell-sink-editor-point sink))))
           (cl-labels ((text-line (n) (or (nth n (split-string text "\n")) ""))
                       (expected-text-line (n) (or (nth n (split-string expected-text "\n")) ""))
                       (line-pt-idx (line-no)
                                    (when (> 20 n 1)
                                      (let ((line-offset (seq-reduce
                                                          #'+
                                                          (seq-map-indexed
                                                           (lambda (l idx)
                                                             (if (< idx line-no)
                                                                 (+ (length l) 1)
                                                               0))
                                                           (split-string text "\n"))
                                                          0)))
                                        (- point line-offset)))))
             (pcase n
               (0 (format "%-32s" "BUFFER:"))
               (1 box-top)
               ((pred (lambda (x) (> 20 x 1)))
                (let ((line-pt-idx (line-pt-idx (- n 2)))
                      (line-text (text-line (- n 2))))
                  (when (<= 0 line-pt-idx (length line-text))
                    (cond
                     ((= 0 (length line-text))
                      (setq line-text (concat (propertize " "
                                                  'font-lock-face
                                                  '(:background "#777"))
                                              " ")))
                     ((= line-pt-idx (length line-text))
                      (setq line-text (concat line-text
                                              (propertize " " 'font-lock-face '(:background "#777"))
                                              " ")))
                     (t
                      (setq line-text (concat (substring line-text 0 line-pt-idx)
                                              (propertize
                                               (substring line-text line-pt-idx (1+ line-pt-idx))
                                               'font-lock-face
                                               '(:background "#777"))
                                              (substring line-text (1+ line-pt-idx))
                                              " ")))))
                  (format "│%-30s│" (truncate-string-to-width line-text 30))))
               (20 box-bottom)
               (21 spacing)
               (22 (format "%-32s" "TARGET:"))
               (23 box-top)
               (42 box-bottom)
               ((pred (lambda (x) (> x 42))) (format "%32s" " "))
               (_ (format "│%-30s│" (expected-text-line (- n 24))))))))))))

(defun asm-blox--display-widget ()
  "Display the current line of the active widget.
This should normally be called when the point is at the end of the display."
  (dolist (widget asm-blox--current-widgets)
    (let ((widget-text (funcall widget (list 'display asm-blox--widget-row-idx))))
      (insert widget-text)
      (insert "  ")))
  (setq asm-blox--widget-row-idx (1+ asm-blox--widget-row-idx)))


;;; Main grid display ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun asm-blox--initialize-box-contents ()
  "Set the contents of each cell to the empty string."
  (setq asm-blox-box-contents (make-hash-table :test 'equal))
  (dotimes (row 3)
    (dotimes (col asm-blox--gameboard-col-ct)
      (puthash (list row col) "" asm-blox-box-contents))))

(defun asm-blox--swap-box-contents (row-1 col-1 row-2 col-2)
  "Swap the contents of the cells at positions (ROW-1,COL-1) and (ROW-2,COL-2)."
  (let ((contents-1 (asm-blox--get-box-content row-1 col-1))
        (contents-2 (asm-blox--get-box-content row-2 col-2)))
    (asm-blox--set-box-content row-2 col-2 contents-1)
    (asm-blox--set-box-content row-1 col-1 contents-2)))

(defun asm-blox--get-box-content (row col)
  "Return the contens of the cell at position ROW COL."
  (gethash (list row col) asm-blox-box-contents))

(defun asm-blox--set-box-content (row col text)
  "Set the contents of the box at position ROW COL to TEXT."
  (puthash (list row col) text asm-blox-box-contents))

(defun asm-blox--get-box-line-content (row col line-no)
  "Get the contents of LINE-NO of cell located at ROW COL."
  (let ((text (asm-blox--get-box-content row col)))
    (or (nth line-no (split-string text "\n")) "")))

(defun asm-blox--row-register-display (row col direction)
  "Return the display string for the up/down DIRECTION port of cell at ROW COL."
  (if (eql 'execute asm-blox--display-mode)
      (let ((val (asm-blox--get-direction-row-registers row col direction)))
        (cond
         ((not val) "    ")
         ((numberp val)
          (propertize
           (if (eql 'UP direction)
               (format "%4d" val)
             (format "%-4d" val))
            'font-lock-face '(:weight bold)))))
    "    "))

;; TODO: Dry this display logic up.
(defun asm-blox--col-register-display (row col direction)
  "Return the display string for left/right DIRECTION port of cell at ROW COL."
  ;; Note: the space between the cells is 5 splaces.
  (if (eql 'execute asm-blox--display-mode)
      (let ((val (asm-blox--get-direction-col-registers row col direction)))
        (cond
         ((not val) "     ")
         ((numberp val)
          (propertize
           (format "%4d " val)
           'font-lock-face '(:weight bold)))
         ((symbolp val)
          (format "%4s " (symbol-name val)))))
    "     "))

(defun asm-blox--row-arrow-label-display (position type col)
  "Return the up/down port info of TYPE source/sink at board POSITION for COL.
POSITION will be either top or bottom, indicating the very top of
the board or very bottom.  TYPE will either be source or sink."
  (let* ((row (if (eql position 'top) -1 3))
         (name (if (eql type 'source)
                   (asm-blox--get-source-idx-at-position row col)
                 (asm-blox--get-sink-name-at-position row col))))
    (or name " ")))

(defun asm-blox--col-arrow-label-display (position type row)
  "Return the left/right port info of TYPE source/sink at POSITION for ROW.
POSITION will be either left or right, indicating the very left
of the board or very right.  TYPE will either be source or sink."
  (let* ((col (if (eql position 'left) -1 4))
         (name (if (eql type 'source)
                   (asm-blox--get-source-idx-at-position row col)
                 (asm-blox--get-sink-name-at-position row col))))
    (or name " ")))

;;; Display constants
(defconst asm-blox-display-chars
  (let ((hash (make-hash-table)))
    (prog1 hash
      (puthash :box-horizontal ?─ hash)
      (puthash :box-vertical ?│ hash)
      (puthash :box-top-left ?┌ hash)
      (puthash :box-top-right ?┐ hash)
      (puthash :box-bottom-right ?┘ hash)
      (puthash :box-bottom-left ?└ hash))))

(defconst asm-blox-display--arrow-up "↑")
(defconst asm-blox-display--arrow-down "↓")
(defconst asm-blox-display--arrow-right "→")
(defconst asm-blox-display--arrow-left "←")
(defconst asm-blox-display--space-start (make-string 6 ?\s))
(defconst asm-blox-display--space-between (make-string 5 ?\s))
(defconst asm-blox-display--box-line-top-bottom
  (make-string asm-blox-box-width
               (gethash :box-horizontal asm-blox-display-chars)))
(defconst asm-blox-display--box-inside (make-string asm-blox-box-width ?\s))


(defun asm-blox-display--insert-row-top (row)
  "Draw the  ┌───┐┌───┐┌───┐┌───┐ part of the board for ROW."
  (insert asm-blox-display--space-start)
  (insert asm-blox-display--space-between)
  (dotimes (col asm-blox--gameboard-col-ct)
    (let ((err (asm-blox--get-error-at-cell row col)))
      (if err
          (progn
            (insert
             (propertize (char-to-string
                          (gethash :box-top-left asm-blox-display-chars))
                         'font-lock-face
                         'asm-blox-error-face))
            (insert
             (propertize asm-blox-display--box-line-top-bottom
                         'font-lock-face
                         'asm-blox-error-face))
            (insert
             (propertize (char-to-string
                          (gethash :box-top-right asm-blox-display-chars))
                         'font-lock-face
                         'asm-blox-error-face)))
        (insert (gethash :box-top-left asm-blox-display-chars))
        (insert asm-blox-display--box-line-top-bottom)
        (insert (gethash :box-top-right asm-blox-display-chars)))
      (insert asm-blox-display--space-between)))
  (when (eql asm-blox--display-mode 'execute)
    (asm-blox--display-widget))
  (insert "\n"))

(defun asm-blox-display--insert-row-bottom (row)
  "Draw the  └───┘└───┘└───┘└───┘ part of the board for ROW."
  (insert asm-blox-display--space-start)
  (insert asm-blox-display--space-between)
  (dotimes (col asm-blox--gameboard-col-ct)
    (let ((err (asm-blox--get-error-at-cell row col)))
      (if err
          (progn (insert
                  (propertize (char-to-string (gethash :box-bottom-left asm-blox-display-chars))
                              'font-lock-face
                              'asm-blox-error-face))
                 (insert
                  (propertize asm-blox-display--box-line-top-bottom
                              'font-lock-face
                              'asm-blox-error-face))
                 (insert
                  (propertize (char-to-string (gethash :box-bottom-right asm-blox-display-chars))
                              'font-lock-face
                              'asm-blox-error-face)))
        (insert (gethash :box-bottom-left asm-blox-display-chars))
        (insert asm-blox-display--box-line-top-bottom)
        (insert (gethash :box-bottom-right asm-blox-display-chars)))
      (insert asm-blox-display--space-between)))
  (when (eql asm-blox--display-mode 'execute)
    (asm-blox--display-widget))
  (insert "\n"))

(defun asm-blox-display--insert-v-border (position)
  "Draw the very top line if POSITION equals `top', the bottom line otherwise."
  (let* ((left-of-arrows-len
          (1+ (- (/ (length asm-blox-display--box-line-top-bottom) 2) 1)))
         (right-of-arrows-len
          (1+ (- (length asm-blox-display--box-line-top-bottom)
                 2
                 left-of-arrows-len))))
    (insert asm-blox-display--space-start)
    (insert asm-blox-display--space-between)
    (dotimes (col asm-blox--gameboard-col-ct)
      (insert (make-string left-of-arrows-len ?\s))
      (let ((sink-char (asm-blox--row-arrow-label-display position
                                                          'sink
                                                          col))
            (source-char (asm-blox--row-arrow-label-display position
                                                            'source
                                                            col)))
        (if (eql position 'top)
            (insert (format "%s %s" sink-char source-char))
          (insert (format "%s %s" source-char sink-char))))
      (insert (make-string right-of-arrows-len ?\s))
      (insert asm-blox-display--space-between)))
  (when (eql asm-blox--display-mode 'execute)
    (asm-blox--display-widget))
  (insert "\n"))

(defvar asm-blox--beginning-of-box-points nil
  "Contains a hashmap of the points where each box begins.
This was added for performance reasons.")

(defun asm-blox-display--insert-row-middle (row box-row)
  "Draw the ⇋|   |⇋|   |⇋|   |⇋|   |⇋ part of the board.

ROW is the global board row.  BOX-ROW is the row number of the
individual box."
  (let ((box-vertical (gethash :box-vertical asm-blox-display-chars))
        (space-start asm-blox-display--space-start)
        (space-between asm-blox-display--space-between)
        (box-inside asm-blox-display--box-inside)
        (arrow-right asm-blox-display--arrow-right)
        (arrow-left asm-blox-display--arrow-left))
    (insert space-start)
             (cond
              ((= 4 box-row)
               ;; NOTE: capital LEFT indicates arrow direction
               ;;       while lower-case represents board side.
               (let ((display (asm-blox--col-register-display row 0 'RIGHT)))
                 (insert display)))
              ((= 5 box-row)
               (let ((label (asm-blox--col-arrow-label-display 'left 'source row)))
                 (if (equal label " ")
                     (insert space-between)
                   (progn (insert "  ")
                          (insert label)
                          (insert arrow-right)
                          (insert ?\s)))))
              ((= 7 box-row)
               (let ((label (asm-blox--col-arrow-label-display 'left 'sink row)))
                 (if (equal label " ")
                     (insert space-between)
                   (progn (insert "  ")
                          (insert label)
                          (insert arrow-left)
                          (insert ?\s)))))
              ((= 8 box-row)
               (let ((display (asm-blox--col-register-display row 0 'LEFT)))
                 (insert display)))
              (t (insert space-between)))
             ;; Draw each box column
             (dotimes (col asm-blox--gameboard-col-ct)
               (let ((err (asm-blox--get-error-at-cell row col)))
                 (if err
                     (insert (propertize (char-to-string box-vertical)
                                         'font-lock-face
                                         'asm-blox-error-face))
                   (insert box-vertical))
                 (when (= box-row 0)
                   (puthash (list row col)
                            (point)
                            asm-blox--beginning-of-box-points))
                 ;; Draw the inner contents of the box
                 (let* ((text (asm-blox--get-box-line-content row col box-row))
                        (spacing (make-string (- (length box-inside)
                                                 (length text))
                                              ?\s)))
                   (cond
                    ((and (= 11 box-row) err (eql asm-blox--display-mode 'edit))
                     (let ((err-text (nth 2 (asm-blox--get-error-at-cell row col))))
                       (let ((err-ov-beg (1- (point))))
                         (let ((ov (make-overlay err-ov-beg (1+ err-ov-beg))))
                           (overlay-put ov 'after-string (concat err-text (make-string (- asm-blox-box-width (length err-text)) ?\s)))
                           (overlay-put ov 'evaporate t))
                         (insert (propertize "                    " 'invisible t )))))  ;; DRY these two cases
                    ((and (= 11 box-row) (eql asm-blox--display-mode 'execute) (asm-blox--cell-message-at-pos row col))
                     (let ((text (asm-blox--cell-message-at-pos row col)))
                       (let ((ov-beg (1- (point))))
                         (let ((ov (make-overlay ov-beg (1+ ov-beg))))
                           (overlay-put ov 'after-string (concat text (make-string (- asm-blox-box-width (length text)) ?\s)))
                           (overlay-put ov 'evaporate t))
                         (insert (propertize "                    " 'invisible t )))))
                    (t
                     (insert (propertize text 'asm-blox-box-id (list row col box-row)))
                     (insert (propertize spacing
                                         'asm-blox-box-id (list row col box-row)
                                         'asm-blox-text-type 'spacing)))))
                 (when (= box-row (1- asm-blox-box-height))
                   (puthash (list row col) (point) asm-blox--end-of-box-points))
                 (let ((pipe-str (cond
                                  ((= box-row (1- asm-blox-box-height))
                                   (if err
                                       (propertize (char-to-string box-vertical)
                                                   'asm-blox-text-type
                                                   `(box-end ,row ,col)
                                                   'font-lock-face
                                                   'asm-blox-error-face)
                                     (propertize (char-to-string box-vertical)
                                                 'asm-blox-text-type
                                                 `(box-end ,row ,col))))
                                  (err
                                   (propertize (char-to-string box-vertical)
                                               'font-lock-face
                                               'asm-blox-error-face))
                                  (t
                                   (char-to-string box-vertical)))))
                   (insert pipe-str)))
               (when (< col (1- asm-blox--gameboard-col-ct))
                 (cond
                  ((= 4 box-row)
                   (let ((display (asm-blox--col-register-display row
                                                                 (1+ col)
                                                                 'RIGHT)))
                     (insert display)))
                  ((= 5 box-row)
                   (progn (insert "  ") (insert arrow-right) (insert "  ")))
                  ((= 7 box-row)
                   (progn (insert "  ") (insert arrow-left) (insert "  ")))
                  ((= 8 box-row)
                   (let ((display (asm-blox--col-register-display row
                                                                 (1+ col)
                                                                 'LEFT)))
                     (insert display)))
                  (t
                   (insert space-between)))))
             (cond
              ((= 4 box-row)
               (let ((display (asm-blox--col-register-display row
                                                             asm-blox--gameboard-col-ct
                                                             'RIGHT)))
                 (insert display)))
              ((= 5 box-row)
               (let ((label (asm-blox--col-arrow-label-display 'right 'sink row)))
                 (if (equal label " ")
                     (insert space-between)
                   (progn
                     (insert ?\s)
                     (insert arrow-right)
                     (insert label)
                     (insert "  ")))))
              ((= 7 box-row)
               (let ((label (asm-blox--col-arrow-label-display 'right 'source row)))
                 (if (equal label " ")
                     (insert space-between)
                   (progn
                     (insert ?\s)
                     (insert arrow-left)
                     (insert label)
                     (insert "  ")))))
              ((= 8 box-row)
               (let ((display (asm-blox--col-register-display row
                                                             asm-blox--gameboard-col-ct
                                                             'LEFT)))
                 (insert display)))
              (t
               (insert space-between)))
             (when (eql asm-blox--display-mode 'execute)
               (asm-blox--display-widget))
             (insert "\n")))

(defun asm-blox-display--insert-middle-row-space (row)
  "Draw the  ↑↓    ↑↓    ↑↓    ↑↓ part of the board for ROW."
  (let ((box-line-top-bottom asm-blox-display--box-line-top-bottom)
        (arrow-up asm-blox-display--arrow-up)
        (arrow-down asm-blox-display--arrow-down)
        (space-start asm-blox-display--space-start)
        (space-between asm-blox-display--space-between))
    (let* ((left-of-arrows-len (1+ (- (/ (length box-line-top-bottom)
                                         2)
                                      1)))
           (right-of-arrows-len (1+ (- (length box-line-top-bottom)
                                       2
                                       left-of-arrows-len)))
           (padding-space-left (make-string (- left-of-arrows-len 5)
                                            ?\s))
           (padding-space-right (make-string (- right-of-arrows-len 5)
                                             ?\s)))
      (insert space-start)
      (insert space-between)
      (dotimes (col asm-blox--gameboard-col-ct)
        (insert padding-space-left)
        ;; logic to display arrow and contnents
        (let* ((up-arrow-display
                (asm-blox--row-register-display row col 'UP))
               (down-arrow-display
                (asm-blox--row-register-display row col 'DOWN))
               (label-position (cond ((= 0 row) 'top)
                                     ((= 3 row) 'bottom)
                                     (t nil)))
               (source-label
                (and label-position
                     (asm-blox--row-arrow-label-display
                      label-position
                      (if (eql label-position 'top)
                          'source 'sink)
                      col)))
               (sink-label
                (and label-position
                     (asm-blox--row-arrow-label-display
                      label-position
                      (if (eql label-position 'top)
                          'sink 'source)
                      col))))
          (insert up-arrow-display)
          (insert " ")
          (if (and label-position
                   (or (not sink-label) (equal sink-label " ")))
              (insert " ")
            (insert arrow-up))
          (insert ?\s)
          (if (and label-position
                   (or (not source-label) (equal source-label " ")))
              (insert " ")
            (insert arrow-down))
          (insert " ")
          (insert down-arrow-display))
        (insert padding-space-right)
        (insert space-between))
      (when (eql asm-blox--display-mode 'execute)
        (asm-blox--display-widget))
      (insert "\n"))))

(defun asm-blox-display-game-board ()
  "Insert the characters of the board to the buffer."
  (setq asm-blox--widget-row-idx 0)
  (when (not (hash-table-p asm-blox--end-of-box-points))
    (setq asm-blox--end-of-box-points (make-hash-table :test 'equal)))
  (when (not (hash-table-p asm-blox--beginning-of-box-points))
    (setq asm-blox--beginning-of-box-points (make-hash-table :test 'equal)))
  (asm-blox-display--insert-v-border 'top)
  (asm-blox-display--insert-middle-row-space 0)
  (dotimes (row 3)
    (asm-blox-display--insert-row-top row)
    (dotimes (box-row asm-blox-box-height)
      (asm-blox-display--insert-row-middle row box-row))
    (asm-blox-display--insert-row-bottom row)
    (when (not (= 2 row))
      (asm-blox-display--insert-middle-row-space (1+ row))))
  (asm-blox-display--insert-middle-row-space 3)
  (asm-blox-display--insert-v-border 'bottom)
  (insert "\n\n")
  (let ((name
         (asm-blox--problem-spec-name asm-blox--extra-gameboard-cells))
        (description
         (asm-blox--problem-spec-description asm-blox--extra-gameboard-cells)))
    (insert name ":\n")
    (insert description "\n"))
  (when (and (eql asm-blox--display-mode 'execute) asm-blox-runtime-error)
    (insert (format "\nERROR: %s at cell (%d, %d)\n"
                    (car asm-blox-runtime-error)
                    (cadr asm-blox-runtime-error)
                    (caddr asm-blox-runtime-error))))
  (asm-blox--propertize-errors))

(defun asm-blox--draw-win-message ()
  "Display a message indicating that the user won the level on the gameboard."
  (when (and (eql asm-blox--display-mode 'execute)
             (eql 'win asm-blox--gameboard-state))
    (let ((inhibit-read-only t))
      (goto-char (point-min))
      (dolist (l '("+=========================+"
                   "|Congragulations, you won!|"
                   "+=========================+"))
        (end-of-line)
        (insert "  " l)
        (forward-line)))))

(defun asm-blox--box-point-forward (ct)
  "With the point in a text box, move forward a point in box-buffer by CT."
  (while (> ct 0)
    (cond
     ((eql (get-text-property (point) 'asm-blox-text-type) 'spacing)
      (forward-char))
     ((not (asm-blox-in-box-p))
      (let ((col (- (current-column) asm-blox-box-width)))
        (forward-line 1)
        (move-to-column col)
        (setq ct (1- ct))))
     (t (forward-char)
        (setq ct (1- ct))))))

(defun asm-blox--propertize-errors ()
  "Add text properties to errors."
  (let ((errs asm-blox-parse-errors))
    (dolist (err errs)
      (let ((row (caar err))
            (col (cadar err))
            (pt (1- (caddr err)))
            (asm-blox--disable-redraw t))
        (asm-blox--move-to-box row col)
        ;; move to the point where the error occured.
        ;; TODO: create box-point-forward to save this logic
        (while (> pt 0)
          (cond
           ((eql (get-text-property (point) 'asm-blox-text-type) 'spacing)
            (forward-char))
           ((not (asm-blox-in-box-p))
            (let ((col (- (current-column) asm-blox-box-width)))
              (forward-line 1)
              (move-to-column col)
              (setq pt (1- pt))))
           (t (forward-char)
              (setq pt (1- pt)))))
        (let ((inhibit-read-only t))
          (put-text-property (point)
                             (1+ (point))
                             'font-lock-face
                             '(:underline (:color "red" :style wave))))))))

(defun asm-blox-redraw-game-board ()
  "Erase the buffer and redraw it."
  (let ((at-row (line-number-at-pos nil))
        (at-col (current-column))
        (prev-text (buffer-string)))
    (erase-buffer)
    (condition-case err
        (asm-blox-display-game-board)
      (error
       (erase-buffer)
       (insert prev-text)
       (asm-blox--parse-saved-buffer)
       (signal (car err) (cdr err))))
    (goto-char (point-min))
    (forward-line (1- at-row))
    (forward-char at-col)))

(defun asm-blox-in-box-p ()
  "Return non-nil if the point is in an edit box."
  (get-text-property (point) 'asm-blox-box-id))

(defun asm-blox-get-line-col-num (&optional point)
  "Return the line column number in the current box at POINT if provided."
  (unless (asm-blox-in-box-p)
    (error "Not in text box"))
  (let ((i 0))
    (save-excursion
      (when point (goto-char point))
      (while (get-text-property (point) 'asm-blox-box-id)
        (forward-char -1)
        (setq i (1+ i))))
    (1- i)))

(defun asm-blox--beginning-of-box ()
  "Move the point to the beginning of the box."
  (unless (asm-blox-in-box-p)
    (error "Not in box 1"))
  (let ((col (current-column)))
    (while (asm-blox-in-box-p)
      (forward-line -1)
      (move-to-column col)) ;; TODO - is this command buggy?
    (forward-line 1)     ;; TODO - use forward line
    (move-to-column col)
    (while (asm-blox-in-box-p)
      (forward-char -1))
    (forward-char 1)))

(defun asm-blox--move-to-end-of-box (row col)
  "Move poin to the end of the box at ROW COL."
  (let ((end-pos (gethash (list row col) asm-blox--end-of-box-points)))
      (goto-char end-pos)))

(defun asm-blox--move-to-box (row col)
  "Move point to the point at ROW COL."
  (when (and (eql asm-blox--display-mode 'edit)
             (not asm-blox--disable-redraw))
    (let ((inhibit-read-only t))
      (asm-blox-redraw-game-board)))
  (let ((begin-pos (gethash (list row col) asm-blox--beginning-of-box-points)))
    (goto-char begin-pos)))

(defun asm-blox--move-to-box-point (row col)
  "Move point to ROW COL in current box."
  (unless (asm-blox-in-box-p)
    (error "Not in box"))
  (asm-blox--beginning-of-box)
  (let ((start-col (current-column)))
    (forward-line row)
    (move-to-column start-col)
    (forward-char col)))

(defun asm-blox--beginning-of-line ()
  "Move to the beginning of the current edit box."
  (while (asm-blox-in-box-p)
    (forward-char -1))
  (forward-char 1))

(defun asm-blox--replace-box-text (text)
  "Replace the text in the current box with TEXT."
  (let* ((start-pos (point))
         (lines (split-string text "\n"))
         (box-id (get-text-property (point) 'asm-blox-box-id))
         (row (nth 0 box-id))
         (col (nth 1 box-id))
         ;; (line (nth 2 box-id))
         (at-line-no 0)
         (start-line-col (current-column)))
    (asm-blox--beginning-of-box)
    (while (asm-blox-in-box-p)
      (let* ((at-line (or (car lines) ""))
             (padding (make-string (- asm-blox-box-width (length at-line)) ?\s)))
        (delete-char asm-blox-box-width)
        (insert (propertize (concat at-line padding)
                            'asm-blox-box-id
                            (list row col at-line-no)))
        (forward-line 1)
        (move-to-column start-line-col)
        (asm-blox--beginning-of-line)
        (setq lines (cdr lines))
        (setq at-line-no (1+ at-line-no))))
    (goto-char start-pos)))

(defun asm-blox--func-in-buffer (func)
  "Perform function FUNC as if in a buffer of the current box."
  (unless (asm-blox-in-box-p)
    (error "Not in box"))
  (let* ((box-id (get-text-property (point) 'asm-blox-box-id))
         (row (nth 0 box-id))
         (col (nth 1 box-id))
         (line (nth 2 box-id))
         (line-col (asm-blox-get-line-col-num))
         (text (asm-blox--get-box-content row col))
         (new-line)
         (new-col)
         (new-text))
    (with-current-buffer (get-buffer-create asm-blox--mirror-buffer-name)
      (erase-buffer)
      (insert text)
      (goto-char (point-min))
      (forward-line line)
      (move-to-column line-col)
      (funcall func)
      (if (or (> (length (buffer-substring-no-properties
                          (line-beginning-position)
                          (line-end-position)))
                 19) ;; TODO: 19 is magic number
              (> (length (split-string (buffer-string) "\n")) 11))
          (ding)
        (setq new-text (buffer-string))
        (setq new-line (1- (line-number-at-pos)))
        (setq new-col (current-column))))
    (when new-text
      (let ((inhibit-read-only t))
        (asm-blox--set-box-content row col new-text)
        (asm-blox--replace-box-text new-text)
        (asm-blox--move-to-box-point new-line new-col)))))

(defmacro asm-blox--in-buffer (code)
  "Perform CODE as if inside buffer of code box."
  `(asm-blox--func-in-buffer
    (lambda ()
      ,code)))

(defun asm-blox-self-insert-command ()
  "Insert the character you type."
  (interactive)
  (if (not (asm-blox-in-box-p))
      (ding)
    (asm-blox--in-buffer
     (insert (this-command-keys)))
    (asm-blox--push-undo-stack-value)))

(defun asm-blox-backward-delete-char ()
  "Delete the character to the left of the point."
  (interactive)
  (asm-blox--in-buffer
   (backward-delete-char 1))
  (asm-blox--push-undo-stack-value))

(defun asm-blox-delete-char ()
  "Delete the character at the point."
  (interactive)
  (asm-blox--in-buffer
   (delete-char 1))
  (asm-blox--push-undo-stack-value))

(defun asm-blox-kill-word ()
  "Kill characters forward until encountering the end of a word."
  (interactive)
  (asm-blox--in-buffer
   (kill-word 1))
  (asm-blox--push-undo-stack-value))

(defun asm-blox-kill-line ()
  "Kill until the end of the current line."
  (interactive)
  (asm-blox--in-buffer
   (kill-line))
  (asm-blox--push-undo-stack-value))

(defun asm-blox-move-beginning-of-line ()
  "Move the point to the beginning of the line."
  (interactive)
  (if (asm-blox-in-box-p)
      (asm-blox--in-buffer
       (move-beginning-of-line 1))
    (beginning-of-line)))

(defun asm-blox-move-end-of-line ()
  "Move the point to the end of the line."
  (interactive)
  (if (asm-blox-in-box-p)
      (asm-blox--in-buffer
       (move-end-of-line 1))
    (end-of-line)))

(defun asm-blox-beginning-of-buffer ()
  "Move the point to the beginning of the buffer."
  (interactive)
  (if (asm-blox-in-box-p)
      (asm-blox--in-buffer
       (goto-char (point-min)))
    (goto-char (point-min))))

(defun asm-blox-end-of-buffer ()
  "Move the point to the end of the buffer."
  (interactive)
  (if (asm-blox-in-box-p)
      (asm-blox--in-buffer
       (goto-char (point-max)))
    (goto-char (point-max))))

(defun asm-blox--newline ()
  "Insert a new line."
  (interactive)
  (asm-blox--in-buffer
   (newline))
  (asm-blox--push-undo-stack-value))

(defun asm-blox--forward-line ()
  "Move forward a line."
  (interactive)
  (asm-blox--in-buffer
   (forward-line)))

(defun asm-blox--shift-box (drow dcol)
  "Shift contents of current box with that of box in direction DROW DCOL."
  (let* ((box-id (get-text-property (point) 'asm-blox-box-id))
         (row (car box-id))
         (col (cadr box-id))
         (next-col (mod (+ (+ col dcol)
                           asm-blox--gameboard-col-ct)
                        asm-blox--gameboard-col-ct))
         (next-row (mod (+ (+ row drow) asm-blox--gameboard-row-ct)
                        asm-blox--gameboard-row-ct))
         (line (caddr box-id))
         (line-col (asm-blox-get-line-col-num)))
    (setq asm-blox-parse-errors nil)
    (asm-blox--swap-box-contents row col next-row next-col)
    (asm-blox--swap-undo-stacks row col next-row next-col)
    (asm-blox--move-to-box next-row next-col)
    (asm-blox--move-to-box-point line line-col)
    (let ((inhibit-read-only t))
      (asm-blox-redraw-game-board))))

(defun asm-blox-shift-box-right ()
  "Shift the current box with the one to the right."
  (interactive)
  (asm-blox--shift-box 0 1))

(defun asm-blox-shift-box-left ()
  "Shift the current box with the one to the left."
  (interactive)
  (asm-blox--shift-box 0 -1))

(defun asm-blox-shift-box-up ()
  "Shift the current box with the one to the top."
  (interactive)
  (asm-blox--shift-box -1 0))

(defun asm-blox-shift-box-down ()
  "Shift the current box with the one to the bottom."
  (interactive)
  (asm-blox--shift-box 1 0))


(defun asm-blox--kill (beg end &optional copy-only)
  "Kill the text from BEG to END.
If COPY-ONLY is non-nil, don't kill the text but add it to kill ring."
  (let* ((box-id-1 (get-text-property beg 'asm-blox-box-id))
         (_ (when (not box-id-1) (error "Can only kill region inside box")))
         (row-1 (car box-id-1))
         (col-1 (cadr box-id-1))
         (line-1 (caddr box-id-1))
         (line-col-1 (asm-blox-get-line-col-num beg))
         (box-id-2 (get-text-property end 'asm-blox-box-id))
         (_ (when (not box-id-2) (error "Can only kill region inside box")))
         (row-2 (car box-id-2))
         (col-2 (cadr box-id-2))
         (line-2 (caddr box-id-2))
         (line-col-2 (asm-blox-get-line-col-num end)))
    (when (or (not (= row-1 row-2))
              (not (= col-1 col-2)))
      (error "Can't kill region across boxes"))
    (let* ((text (asm-blox--get-box-content row-1 col-1))
           (new-text "")
           (kill-text "")
           (lines (split-string text "\n")))
      (cl-loop for i from 0
               for line in lines
               do (cond
                   ((= line-1 i line-2)
                    (let ((line-part
                           (concat (substring line 0 (min (length line)
                                                          line-col-1))
                                   (substring line (min (length line)
                                                        line-col-2))))
                          (kill-part
                           (substring line
                                      (min (length line) line-col-1)
                                      (min (length line) line-col-2))))
                      (setq new-text (concat new-text line-part "\n"))
                      (setq kill-text (concat kill-text kill-part))))
                   ((= line-1 i)
                    (let ((line-part
                           (substring line 0 (min (length line) line-col-1)))
                          (kill-part
                           (substring line (min (length line) line-col-1))))
                      (setq new-text (concat new-text line-part))
                      (setq kill-text (concat kill-text kill-part "\n"))))
                   ((= line-2 i)
                    (let ((line-part
                           (substring line (min (length line) line-col-2)))
                          (kill-part
                           (substring line 0 (min (length line) line-col-2))))
                      (setq new-text (concat new-text line-part "\n"))
                      (setq kill-text (concat kill-text kill-part))))
                   ((> line-2 i line-1)
                    (setq kill-text (concat kill-text line "\n")))
                   (t (setq new-text (concat new-text line "\n")))))
      (when (seq-find (lambda (l) (>= (length l) asm-blox-box-width))
                      (split-string new-text "\n"))
        (error "Killing region makes a line too long"))
      (kill-new kill-text)
      (deactivate-mark)
      (when (not copy-only)
        (asm-blox--set-box-content row-1 col-1 (string-trim-right new-text "\n"))
        (goto-char beg)
        (asm-blox--push-undo-stack-value)
        (let ((inhibit-read-only t))
          (asm-blox-redraw-game-board))))))

(defun asm-blox-kill-region (beg end)
  "Kill the region from BEG to END."
  (interactive "r")
  (asm-blox--kill beg end))

(defun asm-blox-copy-region (beg end)
  "Copy the region from BEG to END."
  (interactive "r")
  (asm-blox--kill beg end t))

(defun asm-blox-yank ()
  "Yank text to current point."
  (interactive)
  (push-mark)
  (let* ((kill-text (current-kill 0))
         (box-id (get-text-property (point) 'asm-blox-box-id))
         (_ (when (not box-id) (error "Can only kill region inside box")))
         (row (car box-id))
         (col (cadr box-id))
         (line-row (caddr box-id))
         (line-col (asm-blox-get-line-col-num (point)))
         (text (asm-blox--get-box-content row col))
         (lines (split-string text "\n"))
         (new-text ""))
    (cl-loop for line in lines
             for i from 0
             do (cond
                 ((= i line-row)
                  (let ((up-to-point
                         (substring line 0 (min (length line) line-col)))
                        (after-point
                         (substring line (min (length line) line-col))))
                    (setq new-text (concat new-text
                                           (concat up-to-point
                                                   kill-text
                                                   after-point
                                                   "\n")))))
                 (t (setq new-text (concat new-text line "\n")))))
    (setq new-text (string-trim-right new-text "\n"))
    (when (or (seq-find (lambda (l) (>= (length l) asm-blox-box-width))
                        (split-string new-text "\n"))
              (>= (length (split-string new-text "\n")) asm-blox-box-height))
      (error "Yanked text doesn't fit in box"))
    (asm-blox--set-box-content row col new-text)
    (asm-blox--push-undo-stack-value)
    (let ((inhibit-read-only t))
      (asm-blox-redraw-game-board))))

(defun asm-blox--refresh-contents ()
  "Redraw the gameboard contents."
  (interactive)
  (let ((inhibit-read-only t))
    (if (asm-blox-in-box-p)
        (asm-blox--in-buffer
         (forward-char 0)) ;; noop to redraw cell
      (asm-blox-redraw-game-board))))

(defun asm-blox--coords-to-end-of-box ()
  "Return (lines-down columns-right) to reach the end of the box."
  (unless (asm-blox-in-box-p)
    (error "Not in box"))
  (let* ((box-id (get-text-property (point) 'asm-blox-box-id))
         (row (car box-id))
         (col (cadr box-id))
         (text (asm-blox--get-box-content row col))
         (lines (split-string text "\n"))
         (line-ct (length lines))
         (last-line-len (length (car (last lines)))))
    (list line-ct last-line-len)))

(defun asm-blox--move-point-to-end-of-box-content ()
  "Move the point to the end of the content of the current box."
  (let* ((coords (asm-blox--coords-to-end-of-box))
         (lines (car coords))
         (cols (cadr coords)))
    (let ((col (current-column)))
      (forward-line (1- lines))
      (move-to-column col))
    (forward-char cols)))

(defun asm-blox--next-row-cell ()
  "Move the point to the end of the box in the next row."
  (interactive)
  (unless (asm-blox-in-box-p)
    (error "Not in box"))
  (let* ((box-id (get-text-property (point) 'asm-blox-box-id))
         (row (car box-id))
         (col (cadr box-id))
         (next-row (if (= row (1- asm-blox--gameboard-row-ct)) 0 (1+ row))))
    (asm-blox--move-to-box next-row col)
    (asm-blox--move-point-to-end-of-box-content)))

(defun asm-blox-next-cell ()
  "Move the point to the end of the next box."
  (interactive)
  (if (asm-blox-in-box-p)
      (let* ((box-id (get-text-property (point) 'asm-blox-box-id))
             (row (car box-id))
             (col (cadr box-id))
             (next-col (if (= col (1- asm-blox--gameboard-col-ct)) 0 (1+ col)))
             (next-row (if (= col (1- asm-blox--gameboard-col-ct)) (1+ row) row))
             (next-row (if (= next-row asm-blox--gameboard-row-ct) 0 next-row)))
        (asm-blox--move-to-box next-row next-col)
        (asm-blox--move-point-to-end-of-box-content))
    (while (and (not (asm-blox-in-box-p))
                (not (bobp)))
      (forward-char -1))
    (when (bobp)
      (asm-blox--move-to-box (1- asm-blox--gameboard-row-ct)
                             (1- asm-blox--gameboard-col-ct)))
    (asm-blox-next-cell)))

;; TODO: DRY this and next-cell up.
(defun asm-blox-prev-cell ()
  "Move the point to the end of the previous box."
  (interactive)
  (if (asm-blox-in-box-p)
      (let* ((box-id (get-text-property (point) 'asm-blox-box-id))
             (row (car box-id))
             (col (cadr box-id))
             (next-col (if (= col 0) (1- asm-blox--gameboard-col-ct) (1- col)))
             (next-row (if (= col 0) (1- row) row))
             (next-row (if (= next-row -1)
                           (1- asm-blox--gameboard-row-ct)
                         next-row)))
        (asm-blox--move-to-box next-row next-col)
        (asm-blox--move-point-to-end-of-box-content))
    (while (and (not (asm-blox-in-box-p))
                (not (eobp)))
      (forward-char 1))
    (when (eobp)
      (asm-blox--move-to-box 0 0))
    (asm-blox-prev-cell)))

(defun asm-blox--printable-char-p (c)
  "Retrun non-nil if C is a printable character."
  (<= 32 c 126))

(defconst asm-blox-mode-map
  (let ((map (make-keymap)))
    (prog1 map
      ;;(suppress-keymap map)
      (dotimes (i 128)
        (when (asm-blox--printable-char-p i)
          (define-key map (char-to-string i) #'asm-blox-self-insert-command)))
      (define-key map (kbd "DEL") #'asm-blox-backward-delete-char)
      (define-key map (kbd "SPC") #'asm-blox-self-insert-command)
      (define-key map (kbd "RET") #'asm-blox--newline)
      (define-key map (kbd "C-c C-c") #'asm-blox-start-execution)
      (define-key map (kbd "M-d") #'asm-blox-kill-word)
      (define-key map (kbd "C-k") #'asm-blox-kill-line)
      (define-key map (kbd "C-a") #'asm-blox-move-beginning-of-line)
      (define-key map (kbd "C-e") #'asm-blox-move-end-of-line)
      (define-key map (kbd "C-d") #'asm-blox-delete-char)
      (define-key map (kbd "M-<") #'asm-blox-beginning-of-buffer)
      (define-key map (kbd "M->") #'asm-blox-end-of-buffer)
      (define-key map (kbd "<tab>") #'asm-blox-next-cell)
      (define-key map (kbd "<backtab>") #'asm-blox-prev-cell)
      (define-key map (kbd "<S-return>") #'asm-blox--next-row-cell)
      (define-key map (kbd "s-z") #'asm-blox-undo)
      (define-key map (kbd "s-y") #'asm-blox-redo)
      (define-key map (kbd "<s-up>") #'asm-blox-shift-box-up)
      (define-key map (kbd "<s-down>") #'asm-blox-shift-box-down)
      (define-key map (kbd "<s-left>") #'asm-blox-shift-box-left)
      (define-key map (kbd "<s-right>") #'asm-blox-shift-box-right)
      (define-key map [remap undo] #'asm-blox-undo)
      (define-key map (kbd "C-w") #'asm-blox-kill-region)
      (define-key map (kbd "M-w") #'asm-blox-copy-region)
      (define-key map (kbd "C-y") #'asm-blox-yank))))

(defun asm-blox-execution-next-command ()
  "Perform a single step of execution."
  (interactive)
  (when (not (asm-blox--gameboard-in-final-state-p))
    (asm-blox--step))
  (let ((inhibit-read-only t))
    (asm-blox-redraw-game-board)
    (asm-blox-execution-code-highlight)
    (asm-blox-execution-draw-stack))
  (asm-blox-check-winning-conditions)
  (asm-blox--draw-win-message)
  (goto-char (point-min)))

(defvar asm-blox-multi-step-ct 10)

(defun asm-blox--execution-next-multiple-commands ()
  "Perform a multiple instructions of execution according to `asm-blox-multi-step-ct'."
  (interactive)
  (dotimes (_ asm-blox-multi-step-ct)
    (when (not (asm-blox--gameboard-in-final-state-p))
      (asm-blox--step)
      (asm-blox-check-winning-conditions)))
  (let ((inhibit-read-only t))
    (asm-blox-redraw-game-board)
    (asm-blox-execution-code-highlight)
    (asm-blox-execution-draw-stack))
  (asm-blox-check-winning-conditions)
  (asm-blox--draw-win-message)
  (goto-char (point-min)))

(defun asm-blox--execution-run ()
  "Continuously run execution setps."
  (interactive)
  (while (and (not (asm-blox--gameboard-in-final-state-p))
              (not (input-pending-p)))
    (asm-blox--step)
    (asm-blox-check-winning-conditions))
  (let ((inhibit-read-only t))
    (asm-blox-redraw-game-board)
    (asm-blox-execution-code-highlight)
    (asm-blox-execution-draw-stack)
    (asm-blox--draw-win-message)))

(defconst asm-blox-execution-mode-map
  (let ((map (make-keymap)))
    (prog1 map
      (define-key map "n" #'asm-blox-execution-next-command)
      (define-key map "N" #'asm-blox--execution-next-multiple-commands)
      (define-key map "r" #'asm-blox--execution-run)
      (define-key map "q" #'quit-window))))

(defconst asm-blox-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?| "-" st)
    st)
  "Syntax table for `asm-blox' mode.")

(defun asm-blox--create-execution-buffer (box-contents extra-cells)
  "Create new gamebuffer to display execution of BOX-CONTENTS with EXTRA-CELLS."
  (let ((buffer (get-buffer-create "*asm-blox-execution*"))
        (origin-file-buffer (current-buffer)))
    (with-current-buffer buffer
      (asm-blox-execution-mode)
      (setq asm-blox-box-contents box-contents)
      (setq asm-blox--extra-gameboard-cells extra-cells)
      (let ((inhibit-read-only t))
        (asm-blox-redraw-game-board)
        (asm-blox-execution-code-highlight)
        (asm-blox-execution-draw-stack)))
    (switch-to-buffer buffer)
    (setq asm-blox-execution-origin-buffer origin-file-buffer)))

(defun asm-blox-execution-code-highlight ()
  "Add highlight face to where runtime's pc is."
  (let ((inhibit-read-only t))
    (dotimes (row asm-blox--gameboard-row-ct)
      (dotimes (col asm-blox--gameboard-col-ct)
        (let* ((at-runtime (asm-blox--cell-at-row-col row col))
               (at-instr (asm-blox--cell-runtime-current-instruction at-runtime))
               (start-pos (asm-blox-code-node-start-pos at-instr))
               (end-pos (asm-blox-code-node-end-pos at-instr)))
          (when (and start-pos end-pos)
            (asm-blox--move-to-box row col)
            (asm-blox--box-point-forward (1- start-pos))
            (let* ((text (asm-blox--get-box-content row col))
                   (hl-text (substring-no-properties text (1- start-pos) (1- end-pos)))
                   (hl-lines (split-string hl-text "\n")))
              (while hl-lines
                (put-text-property (point) (+ (point) (length (car hl-lines)))
                                   'font-lock-face 'asm-blox-highlight-face)
                (let ((at-col (current-column)))
                  (forward-line 1)
                  (move-to-column at-col)
                  (while (not (looking-back "│" nil))
                    (forward-char -1)))
                (setq hl-lines (cdr hl-lines))))))))))

(defun asm-blox-execution-draw-stack ()
  "Display the stack for the current cell-runtimes."
  (let ((inhibit-read-only t))
    (dotimes (row asm-blox--gameboard-row-ct)
      (dotimes (col asm-blox--gameboard-col-ct)
        (let* ((at-runtime (asm-blox--cell-at-row-col row col))
               (stack (reverse (asm-blox--cell-runtime-stack at-runtime))))
          (asm-blox--move-to-end-of-box row col)
          (while stack
            (let ((stack-top (car stack)))
              (forward-char -4)
              (delete-char 4)
              (if stack-top
                  (insert (format "%4d" stack-top))
                (insert "    "))
              (forward-char -5))
            (setq stack (cdr stack))))))))

(defun asm-blox--create-widges-from-gameboard ()
  "Create widget objects from sources and sinks of gameboard."
  (let ((sources (asm-blox--problem-spec-sources asm-blox--extra-gameboard-cells))
        (sinks (asm-blox--problem-spec-sinks asm-blox--extra-gameboard-cells))
        (widgets))
    (dolist (source sources)
      (setq widgets (cons (asm-blox--make-source-widget source)
                          widgets)))
    (dolist (sink sinks)
      (if (asm-blox--cell-sink-editor-text sink)
          (setq widgets (cons (asm-blox--make-editor-widget sink)
                              widgets))
        (setq widgets (cons (asm-blox--make-sink-widget sink)
                            widgets))))
    (setq asm-blox--current-widgets (reverse widgets))))

(defun asm-blox-start-execution ()
  "Parse gameboard, displaying any errors, and display code execution buffer."
  (interactive)
  ;; parse the current buffer to make sure that the text we are
  ;; running is the actual text of the buffer.
  (setq asm-blox-parse-errors nil)
  (let ((parse-errors))
    (maphash
     (lambda (coords code-text)
       (let ((parse-result (asm-blox--parse-cell coords code-text)))
         (cond
          ((asm-blox--parse-error-p parse-result)
           (setq parse-errors (cons (cons coords parse-result) parse-errors)))
          ((asm-blox--cell-runtime-p parse-result)
           (asm-blox--set-cell-at-row-col (car coords) (cadr coords) parse-result))
          (t (error "Unknown result from asm-blox--parse-cell %s" parse-result)))))
     asm-blox-box-contents)
    (if parse-errors
        (progn
          (setq asm-blox-parse-errors parse-errors)
          (let ((inhibit-read-only t))
            (asm-blox-redraw-game-board)))
      (setq asm-blox-parse-errors nil)
      (let ((inhibit-read-only t))
        (asm-blox-redraw-game-board))
      (asm-blox--backup-file-for-current-buffer)
      (asm-blox--reset-extra-gameboard-cells-state)
      (asm-blox--create-widges-from-gameboard)
      (let ((box-contents asm-blox-box-contents)
            (extra-cells asm-blox--extra-gameboard-cells))
        (asm-blox--create-execution-buffer box-contents extra-cells)
        (goto-char (point-min))))))

(define-derived-mode asm-blox-execution-mode fundamental-mode "asm-blox-execution"
  "Major mode for viewing the execution of an asm blox puzzle.

The follwoing commadns are defined:

\\{asm-blox-execution-mode-map}"
  :syntax-table asm-blox-mode-syntax-table
  (setq buffer-read-only t)
  (setq-local truncate-lines 0
              asm-blox--display-mode 'execute)
  (setq header-line-format "ASM-BLOX EXECUTION")
  (setq asm-blox-runtime-error nil)
  (setq asm-blox--gameboard-state nil))

(defvar asm-blox--skip-initial-parsing nil
  "When non-nil, don't parse the initial gameboard.")

(defvar asm-blox--show-pair-idle-timer nil
  "Idle-timer for showing matching parenthesis.")

(define-derived-mode asm-blox-mode fundamental-mode "asm-blox"
  "Major mode for editing `asm-blox' puzzles.

This mode provides capabilities for editing `asm-blox' code.  Since
the code is split into multiple cells, this mode in
necessary to be able to edit such cells.  The mode defines
editing functions as well as a function to compile and run the puzzle.

The following commands are available:

\\{asm-blox-mode-map}"
  :syntax-table asm-blox-mode-syntax-table
  (setq buffer-read-only t)
  (setq asm-blox-parse-errors nil)
  (setq-local truncate-lines 0)
  (unless asm-blox--skip-initial-parsing
    (condition-case nil
        (asm-blox--parse-saved-buffer)
      (error
       (if (asm-blox--restore-backup)
           (message "Save file corrupted, restoring from backup.")
         (error "Save file corrupted (could not be parsed)"))))
    (let ((inhibit-read-only t))
      (asm-blox-redraw-game-board)))
  (unless asm-blox--show-pair-idle-timer
    (setq asm-blox--show-pair-idle-timer
          (run-with-idle-timer 0.125 t #'asm-blox--highlight-pairs))))

;;;###autoload
(add-to-list 'auto-mode-alist (cons "\\.asbx\\'" 'asm-blox-mode))

;;; Undo ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar asm-blox--undo-stacks nil
  "Hashmap of stacks containing undo history of each buffer.")

(cl-defstruct (asm-blox--undo-state
               (:constructor asm-blox--undo-state-create)
               (:copier nil))
  "Struct representing an undo-state."
  text box-row box-col redo-list)

(defun asm-blox--initialize-undo-stacks ()
  "Initialize all undo-stacks to be empty."
  (setq asm-blox--undo-stacks (make-hash-table :test 'equal))
  (dotimes (row 3)
    (dotimes (col asm-blox--gameboard-col-ct)
      (let* ((current-value (asm-blox--get-box-content row col)))
        (puthash (list row col)
                 (list (asm-blox--undo-state-create :text current-value
                                                   :box-row 0
                                                   :box-col 0
                                                   :redo-list '()))
                 asm-blox--undo-stacks)))))

(defun asm-blox--swap-undo-stacks (row-1 col-1 row-2 col-2)
  "Swap the undo stacks of (ROW-1 COL-1) and (ROW-2 COL-2)."
  (let ((stack-1 (gethash (list row-1 col-1) asm-blox--undo-stacks))
        (stack-2 (gethash (list row-2 col-2) asm-blox--undo-stacks)))
    (puthash (list row-1 col-1) stack-2 asm-blox--undo-stacks)
    (puthash (list row-2 col-2) stack-1 asm-blox--undo-stacks)))

(defun asm-blox--push-undo-stack-value ()
  "Push the contents of the current box as a new undo frame."
  (unless asm-blox--undo-stacks
    (setq asm-blox--undo-stacks (make-hash-table :test #'equal)))
  (let* ((box-id (get-text-property (point) 'asm-blox-box-id))
         (row (car box-id))
         (col (cadr box-id))
         (line-row (caddr box-id))
         (text (asm-blox--get-box-content row col))
         (key (list row col))
         (states (gethash key asm-blox--undo-stacks))
         (top-text (and states (asm-blox--undo-state-text (car states)))))
    (unless (equal top-text text)
      (let* ((line-col (asm-blox-get-line-col-num))
             (state (asm-blox--undo-state-create :text text
                                                :box-row line-row
                                                :box-col line-col)))
        (puthash key (cons state states) asm-blox--undo-stacks)))))

(defun asm-blox-undo ()
  "Perform an undo in the current box."
  (interactive)
  (if (not (asm-blox-in-box-p))
      (ding)
    (let* ((box-id (get-text-property (point) 'asm-blox-box-id))
           (row (car box-id))
           (col (cadr box-id)))
      (let* ((stack (gethash (list row col) asm-blox--undo-stacks))
             (prev-state (cadr stack)))
        (if (<= (length stack) 1)
            (ding)
          ;; handle redo
          (let* ((at-state-redo (asm-blox--undo-state-redo-list (car stack)))
                 (new-redo (cons (car stack) at-state-redo)))
            (setf (asm-blox--undo-state-redo-list prev-state) new-redo))
          (puthash (list row col) (cdr stack) asm-blox--undo-stacks)
          (let ((text (asm-blox--undo-state-text prev-state))
                (box-row (asm-blox--undo-state-box-row prev-state))
                (box-col (asm-blox--undo-state-box-col prev-state)))
            (asm-blox--set-box-content row col text)
            ;; code-smell: always inhibiting read only
            (let ((inhibit-read-only t))
              (asm-blox-redraw-game-board))
            (asm-blox--move-to-box-point box-row box-col)))))))

(defun asm-blox-redo ()
  "Perform a redo in the current box."
  (interactive)
  (if (not (asm-blox-in-box-p))
      (ding)
    (let* ((box-id (get-text-property (point) 'asm-blox-box-id))
           (row (car box-id))
           (col (cadr box-id)))
      (let* ((stack (gethash (list row col) asm-blox--undo-stacks))
             (top-state (car stack))
             (redo-list (asm-blox--undo-state-redo-list top-state)))
        (if (not redo-list)
            (ding)
          (let ((next-redo (car redo-list))
                (rest-redos (cdr redo-list)))
            (setf (asm-blox--undo-state-redo-list next-redo) rest-redos)
            (puthash (list row col) (cons next-redo stack) asm-blox--undo-stacks)
            (let ((text (asm-blox--undo-state-text next-redo))
                  (box-row (asm-blox--undo-state-box-row next-redo))
                  (box-col (asm-blox--undo-state-box-col next-redo)))
              (asm-blox--set-box-content row col text)
              (let ((inhibit-read-only t))
                (asm-blox-redraw-game-board))
              (asm-blox--move-to-box-point box-row box-col))))))))


;;; Parenthesis match code ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar asm-blox-pair-overlays nil
  "List of overlays used to highlight parenthesis pairs.")

(defun asm-blox--find-closing-match ()
  "Find the closing paren match of point."
  (let* ((box-id (get-text-property (point) 'asm-blox-box-id))
         (row (car box-id))
         (col (cadr box-id))
         (line (caddr box-id))
         (line-col (asm-blox-get-line-col-num))
         (text (asm-blox--get-box-content row col)))
    (with-temp-buffer
      (insert text)
      (goto-char (point-min))
      (forward-line line)
      (forward-char line-col)
      (let ((ct 1))
        (while (and (not (eobp))
                    (not (zerop ct)))
          (forward-char 1)
          (when (looking-at "(")
            (setq ct (1+ ct)))
          (when (looking-at ")")
            (setq ct (1- ct)))))
      (when (not (eobp))
        (let* ((new-col (current-column))
               (new-row (1- (line-number-at-pos)))
               (dcol (- new-col line-col))
               (drow (- new-row line)))
          (list drow dcol))))))

(defun asm-blox--find-opening-match ()
  "Find the opening paren match of point."
  (let* ((box-id (get-text-property (point) 'asm-blox-box-id))
         (row (car box-id))
         (col (cadr box-id))
         (line (caddr box-id))
         (line-col (asm-blox-get-line-col-num))
         (text (asm-blox--get-box-content row col)))
    (with-temp-buffer
      (insert text)
      (goto-char (point-min))
      (forward-line line)
      (forward-char line-col)
      (forward-char -1) ;; to highlight if after closing
      (let ((ct 1))
        (while (and (not (bobp))
                    (not (zerop ct)))
          (forward-char -1)
          (when (looking-at ")")
            (setq ct (1+ ct)))
          (when (looking-at "(")
            (setq ct (1- ct)))))
      (when (or (looking-at "(") (not (bobp)))
        (let* ((new-col (current-column))
               (new-row (1- (line-number-at-pos)))
               (dcol (- new-col line-col))
               (drow (- new-row line)))
          (list drow dcol))))))

(defun  asm-blox--pair-delete-overlays ()
  "Remove both show pair overlays."
  (when asm-blox-pair-overlays
    (dolist (overlay asm-blox-pair-overlays)
      (delete-overlay overlay))
    (setq asm-blox-pair-overlays nil)))

(defun asm-blox--pair-create-overlays (start end)
  "Create the show pair overlays for parens at point START and END."
  (when asm-blox-pair-overlays
    (asm-blox--pair-delete-overlays))
  (let ((oleft (make-overlay start (1+ start) nil t nil))
        (oright (make-overlay end (1+ end) nil t nil)))
    (setq asm-blox-pair-overlays (list oleft oright))
    (overlay-put oleft 'face 'asm-blox-show-paren-match-face)
    (overlay-put oright 'face 'asm-blox-show-paren-match-face)
    (overlay-put oleft 'type 'show-pair)))

(defun asm-blox--highlight-pairs ()
  "Highlight any relevant pairs at point."
  (when (equal mode-name "asm-blox") ;; TODO: use eql to make this faster
    (if (asm-blox-in-box-p)
        (save-excursion
          (save-match-data
            (while-no-input
              (cond
               ((looking-back ")" nil)
                (let* ((start-point (1- (point)))
                       (opening-match-coords (asm-blox--find-opening-match))
                       (d-row (car opening-match-coords))
                       (d-col (cadr opening-match-coords))
                       (at-col (current-column)))
                  (forward-line d-row)
                  (move-to-column at-col)
                  (forward-char d-col)
                  (let ((end-point (point)))
                    (asm-blox--pair-create-overlays start-point end-point))))
               ((looking-at "(")
                (let* ((start-point (point))
                       (closing-match-coords (asm-blox--find-closing-match)))
                  (if closing-match-coords
                      (let ((d-row (car closing-match-coords))
                            (d-col (cadr closing-match-coords))
                            (at-col (current-column)))
                        (forward-line d-row)
                        (move-to-column at-col)
                        (forward-char d-col)
                        (let ((end-point (point)))
                          (asm-blox--pair-create-overlays start-point end-point)))
                    (when asm-blox-pair-overlays
                       ;; TODO: should display red match instead
                      (asm-blox--pair-delete-overlays)))))
               (asm-blox-pair-overlays
                (asm-blox--pair-delete-overlays))))))
      (when asm-blox-pair-overlays
        (asm-blox--pair-delete-overlays)))))


;;; Puzzle Selection

(defvar asm-blox-puzzles nil
  "List of puzzles to be loaded on puzzle selection page.")

(defun asm-blox--get-puzzle-by-id (name)
  "Given a puzzle NAME, return tis puzzle generation function."
  (seq-find (lambda (puzzle-fn)
              (let ((n (asm-blox--problem-spec-name (funcall puzzle-fn))))
                (equal name n)))
            asm-blox-puzzles))

(defun asm-blox--puzzle-selection-setup-buffer (id)
  "Setup the puzzle buffer for the puzzle at ID."
  (let ((puzzle (asm-blox--get-puzzle-by-id id)))
    (unless puzzle
      (error "No puzzle found with id %s" id))
      ;; TODO: allow for 1+ puzzles at once
    (let ((buffer (get-buffer-create "*asm-blox*"))
          (file-name (asm-blox--generate-new-puzzle-filename id)))
      (switch-to-buffer buffer)
      (let ((inhibit-read-only t)
            (asm-blox--skip-initial-parsing t))
        (set-visited-file-name file-name)
        ;; make sure buffer-local variables set properly
        (asm-blox-mode)
        (setq asm-blox--extra-gameboard-cells (funcall puzzle))
        (asm-blox--initialize-box-contents)
        (asm-blox-redraw-game-board)
        (save-buffer)))))

(defun asm-blox-select-puzzle ()
  "Start the puzzle for the puzzle under the point."
  (interactive)
  (let ((at-puzzle-id
         (get-text-property (point) 'asm-blox-puzzle-selection-id))
        (at-puzzle-filename
         (get-text-property (point) 'asm-blox-puzzle-selection-filename)))
    (unless at-puzzle-id
      (error "No puzzle under point"))
    (if at-puzzle-filename
        (find-file at-puzzle-filename)
      (asm-blox--puzzle-selection-setup-buffer at-puzzle-id))
    ;; refresh the puzzle selection buffer so
    ;; the user can see that their file was created.
    (asm-blox-puzzle-selection-prepare-buffer)))

(defconst asm-blox-puzzle-selection-mode-map
  (let ((map (make-sparse-keymap)))
    (prog1 map
      (define-key map "q" #'quit-window)
      (define-key map "g" #'asm-blox-puzzle-selection-refresh)
      (define-key map (kbd "RET") #'asm-blox-select-puzzle)))
  "Mode map for selecting a `asm-blox' puzzle.")

(defun asm-blox--font-for-difficulty (difficulty)
  "Return font color from DIFFICULTY symbol."
  (pcase difficulty
    ('tutorial "LavenderBlush2")
    ('easy "forest green")
    ('medium "goldenrod")
    ('hard "orange red")))

(defun asm-blox--puzzles-by-difficulty ()
  "Return the puzzles in order of difficulty."
  (let* ((difficulty-to-num
          (lambda (d)
            (pcase d
              ('tutorial 0)
              ('easy 1)
              ('medium 2)
              ('hard 3))))
         (compare-difficulty
          (lambda (a b)
            (< (funcall difficulty-to-num
                        (asm-blox--problem-spec-difficulty (funcall a)))
               (funcall difficulty-to-num
                        (asm-blox--problem-spec-difficulty (funcall b)))))))
    (seq-sort compare-difficulty asm-blox-puzzles)))

(defun asm-blox-puzzle-selection-prepare-buffer ()
  "Prepare the puzzle selection buffer."
  (with-current-buffer (get-buffer-create "*asm-blox-puzzle-selection*")
    (let ((inhibit-read-only t))
      (erase-buffer)
      (when (zerop (length asm-blox-puzzles))
        (insert "It looks like you have no puzzles loaded.  Did you forget to load asm-blox-puzzles.el?"))
      (dolist (puzzle-fn (asm-blox--puzzles-by-difficulty))
        (let* ((puzzle (funcall puzzle-fn))
               (name (asm-blox--problem-spec-name puzzle))
               (difficulty (asm-blox--problem-spec-difficulty puzzle))
               (difficulty-color (asm-blox--font-for-difficulty difficulty))
               (description (replace-regexp-in-string
                             "\n"
                             " "
                             (asm-blox--problem-spec-description puzzle)))
               (line-str (format "%3s %-8s %-25s %-60s   "
                                 (if (asm-blox--puzzle-won-p name) "[x]" "[ ]")
                                 (propertize (symbol-name difficulty)
                                             'font-lock-face
                                             `(:foreground ,difficulty-color))
                                 name
                                 (truncate-string-to-width description 60
                                                           nil nil t))))
          (insert (propertize line-str 'asm-blox-puzzle-selection-id name))
          (let ((saved-file-ids (asm-blox--saved-puzzle-ct-ids name)))
            (dolist (i saved-file-ids)
              (insert
               (propertize (format "[%d]" i)
                           'asm-blox-puzzle-selection-id name
                           'asm-blox-puzzle-selection-filename
                           (asm-blox--make-puzzle-idx-file-name name i)))
              (insert (propertize " " 'asm-blox-puzzle-selection-id name)))))
        (insert "\n")))))

(defun asm-blox-puzzle-selection-refresh ()
  "Refresh the contents of the puzzle display buffer."
  (interactive)
  (let ((line-num (line-number-at-pos))
        (col-num (current-column)))
    (asm-blox-puzzle-selection-prepare-buffer)
    (goto-char (point-min))
    (forward-line (1- line-num))
    (forward-char col-num)))

(defun asm-blox-puzzle-selection-mode ()
  "Activate mode for selecting `asm-blox' puzzles."
  (interactive)
  (kill-all-local-variables)
  (use-local-map asm-blox-puzzle-selection-mode-map)
  (setq mode-name "asm-blox-puzzle-selection"
        buffer-read-only t)
  (setq header-line-format
        (format "     %-8s %-25s %-60s   %s"
                "STRAIN"
                "PUZZLE NAME"
                "DESCRIPTION"
                "SAVED FILES"))
  (setq-local truncate-lines 0)
  (hl-line-mode t))

(require 'asm-blox-puzzles)

;;;###autoload
(defun asm-blox ()
  "Open `asm-blox' puzzle selection screen."
  (interactive)
  (let ((buffer (get-buffer-create "*asm-blox-puzzle-selection*")))
    (switch-to-buffer buffer)
    (asm-blox-puzzle-selection-mode)
    (asm-blox-puzzle-selection-prepare-buffer)
    (goto-char (point-min))))

(provide 'asm-blox)

;;; asm-blox.el ends here
