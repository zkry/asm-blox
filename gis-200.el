;;; gis-200.el --- Grided Intelligence System -*- lexical-binding: t -*-

;; Author: Zachary Romero
;; Maintainer: Zachary Romero
;; Version: 0.0.1
;; Package-Requires: ()
;; Homepage: ((emacs "25.1") (yaml "0.3.4"))
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
(require 'yaml)

(defconst gis-200-box-height 12)
(defconst gis-200-column-ct 4)
(defconst gis-200-box-width 20)
(defconst gis-200--gameboard-col-ct 4)
(defconst gis-200--gameboard-row-ct 3)

(defvar gis-200-box-contents nil)
(defvar gis-200--gameboard nil)
(defvar gis-200--extra-gameboard-cells nil)
(defvar gis-200--gameboard-state nil
  "Contains the state of the board whether it be victory or error.")
(defvar gis-200--parse-depth nil)
(defvar gis-200--branch-labels nil)

(defvar gis-200-runtime-error nil
  "If non-nil, contains the runtime error encountered.
The format of the error is (list message row column).")

(defvar-local gis-200-execution-origin-buffer nil)

(cl-defstruct (gis-200-code-node
               (:constructor gis-200--code-node-create)
               (:copier nil))
  children start-pos end-pos)

(defun gis-200--parse-error-p (err)
  "Return non-nil if ERR is a parse error."
  (and (listp err) (eql 'error (car err))))

(defun gis-200--parse-cell (coords code)
  "Parse a the CODE of a text box at COORDS.  This may be YAML or WAT."
  (let* ((first-char
          (and (not (string-empty-p (string-trim code)))
               (substring-no-properties (string-trim-left code) 0 1)))
         ;; There is currently no switch the user can use to indicate
         ;; filetype, thus the need of heuristic.
         (wat-p (or (not first-char)
                    (string= first-char "(")
                    (string= first-char ")")
                    (string= first-char ";"))))
    (if wat-p
        (gis-200--parse-assembly code)
      (gis-200--create-yaml-code-node (car coords) (cadr coords) code))))

(defun gis-200--parse-assembly (code)
  "Parse ASM CODE returning a list of instructions."
  (with-temp-buffer
    (erase-buffer)
    (insert code)
    (goto-char (point-min))
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
                            (<= ?A c ?Z)
                            (= ?_ c)))
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
                                          (_ (throw 'error `(error ,(point) "INVALID CHAR")))))
                                      (forward-char 1)
                                      (push c elements)))
                                   ;; Symbol
                                   ((symbol-char-p at-char)
                                    (let ((start (point)))
                                      (forward-char 1)
                                      (while (and (not (eobp))
                                                  (symbol-char-p (current-char)))
                                        (forward-char 1))
                                      (let ((symbol (intern (upcase (buffer-substring-no-properties start (point))))))
                                        (push symbol elements))))

                                   ;; digit
                                   ((or (digit-char-p at-char) (eql at-char ?-))
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
      (catch 'error (parse-element t)))))

(defconst gis-200-base-operations
  '(GET SET TEE CONST NULL IS_NULL DROP
        NOP ADD INC DEC SUB MUL DIV REM AND OR EQZ GZ LZ
        EQ NE LT GT GE LE SEND PUSH POP
        CLR NOT DUP ABS))

(defconst gis-200-command-specs
  '((SET integerp gis-200--subexpressions)
    (CLR) 
    (CONST integerp)
    (DUP gis-200--subexpressions)
    (ABS gis-200--subexpressions)
    (ADD gis-200--subexpressions)
    (SUB gis-200--subexpressions)
    (MUL gis-200--subexpressions)
    (DIV gis-200--subexpressions)
    (NEG gis-200--subexpressions)
    (REM gis-200--subexpressions)
    (AND gis-200--subexpressions)
    (NOT gis-200--subexpressions)
    (OR gis-200--subexpressions)
    (EQ gis-200--subexpressions)
    (NE gis-200--subexpressions)
    (LT gis-200--subexpressions)
    (LE gis-200--subexpressions)
    (GT gis-200--subexpressions)
    (GE gis-200--subexpressions)
    (GZ gis-200--subexpressions)
    (LZ gis-200--subexpressions)
    (EQZ gis-200--subexpressions)
    (BLOCK gis-200--subexpressions)
    (LOOP gis-200--subexpressions)
    (INC integerp)
    (DEC integerp)
    (BR_IF integerp)
    (BR integerp)
    (NOP)
    (DROP gis-200--subexpressions)
    (SEND gis-200--portp gis-200--subexpressions)
    (GET (lambda (x) (or (gis-200--portp x) (integerp x))))
    (LEFT)  ;; TODO: Should these be in final game?
    (RIGHT) ;; TODO
    (UP)    ;; TODO
    (DOWN)  ;; TODO
    (FN t)  ;; FN needs special verification code
    )
  "List of commands and specifications for command arguments.")

(defun gis-200--portp (x)
  "Return non-nil if X is a port direction."
  (memq x '(UP DOWN LEFT RIGHT)))

(defun gis-200--code-node-validate (code-node)
  "Determine if CODE-NODE adheres to the corresponding specification."
  (let* ((children (gis-200-code-node-children code-node))
         (start-pos (gis-200-code-node-start-pos code-node))
         (end-pos (gis-200-code-node-end-pos code-node))
         (first-child (car children))
         (cmd-spec (assoc first-child gis-200-command-specs)))
    (cond
     ((not first-child)
      `(error ,start-pos "No command found"))
     ((not cmd-spec)
      `(error ,start-pos "Command not found"))
     (t
      (let* ((specs (cdr cmd-spec))
             (rest-children (cdr children))
             (at-spec (car specs)))
        (catch 'err
          (while (or specs rest-children)
            (cond
             ((eql at-spec 'gis-200--subexpressions)
              (if (seq-every-p #'gis-200-code-node-p rest-children)
                  (setq rest-children nil)
                (throw 'err `(error ,start-pos "bad end expressions"))))
             ((and rest-children (not specs))
              (throw 'err `(error ,start-pos "too many args")))
             ((and specs (not rest-children))
              (throw 'err `(error ,start-pos "not enough args")))
             (t
              (let* ((at-child (car rest-children))
                     (ok-p (funcall at-spec at-child)))
                (if ok-p
                    (setq rest-children (cdr rest-children))
                  (let ((msg (format "bad arg to '%s'" first-child)))
                    (throw 'err `(error ,start-pos ,msg)))))))
            (setq specs (cdr specs))
            (setq at-spec (car specs)))))))))

(defun gis-200--make-label ()
  "Depending on the parse-depth create a label for the various goto statements."
  (intern (concat "L_"
                  (number-to-string (random 100000))
                  "_"
                  (number-to-string gis-200--parse-depth))))

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
      (let ((err (gis-200--code-node-validate parse)))
        (if err
            (throw 'err err)
          (let* ((children (gis-200-code-node-children parse))
                 (start-pos (gis-200-code-node-start-pos parse))
                 (end-pos (gis-200-code-node-end-pos parse))
                 (first-child (car children))
                 (rest-children (cdr children)))
            (cond
             ((not first-child)
              (throw 'err `(error ,start-pos "No cmd found")))

             ((eql first-child 'BLOCK)
              (let* ((label-symbol (gis-200--make-label))
                     (gis-200--branch-labels
                      (cons (cons gis-200--parse-depth label-symbol)
                            gis-200--branch-labels))
                     (rest-asm-stmts (mapcar #'gis-200--parse-tree-to-asm*
                                             rest-children)))
                (append rest-asm-stmts
                        (list (gis-200--code-node-create
                               :children (list 'LABEL label-symbol)
                               :start-pos nil
                               :end-pos nil)))))

             ((eql first-child 'LOOP)
              (let* ((label-symbol (gis-200--make-label))
                     (gis-200--branch-labels
                      (cons (cons gis-200--parse-depth label-symbol)
                            gis-200--branch-labels))
                     (rest-asm-stmts
                      (mapcar #'gis-200--parse-tree-to-asm* rest-children)))
                (append (list (gis-200--code-node-create
                               :children (list 'LABEL label-symbol)
                               :start-pos nil
                               :end-pos nil))
                        rest-asm-stmts)))
             ((eql first-child 'BR)
              (let* ((br-num (car rest-children))
                     (lbl-ref-level (- gis-200--parse-depth br-num 1))
                     (label-symbol
                      (or (cdr (assoc lbl-ref-level
                                      gis-200--branch-labels))
                          (concat "NOT_FOUND_"
                                  (number-to-string br-num)
                                  "_"
                                  (number-to-string gis-200--parse-depth)
                                  "_"
                                  (assoc br-num gis-200--branch-labels)))))
                (gis-200--code-node-create
                 :children (list 'JMP label-symbol)
                 :start-pos start-pos
                 :end-pos end-pos)))

             ((eql first-child 'BR_IF)
              (let* ((br-num (car rest-children))
                     (lbl-ref-level (- gis-200--parse-depth br-num 1))
                     (label-symbol
                      (or (cdr (assoc lbl-ref-level gis-200--branch-labels))
                          (concat "NOT_FOUND_"
                                  (number-to-string br-num)
                                  "_"
                                  (number-to-string gis-200--parse-depth)
                                  "_"
                                  (assoc br-num gis-200--branch-labels)))))
                (gis-200--code-node-create
                 :children (list 'JMP_IF label-symbol)
                 :start-pos start-pos
                 :end-pos end-pos)))

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
                        (seq-map #'gis-200--parse-tree-to-asm*
                                 (cdr (gis-200-code-node-children then-case)))
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
                        (seq-map #'gis-200--parse-tree-to-asm*
                                 (cdr (gis-200-code-node-children else-case)))
                      nil)
                  ,(gis-200--code-node-create
                    :children (list 'LABEL end-label)
                    :start-pos nil
                    :end-pos nil))))

             ((assoc first-child gis-200-command-specs)
              (let* ((cmd-spec (assoc first-child gis-200-command-specs))
                     (spec (cdr cmd-spec))
                     (rest-children (cdr children))
                     (children-cmds '()))
                ;; Determine which children are commands
                ;; that run before the command we're at.
                (while (and spec rest-children)
                  (when (eql (car spec) 'gis-200--subexpressions)
                    (setq children-cmds (seq-map #'gis-200--parse-tree-to-asm*
                                                 rest-children)))
                  (setq spec (cdr spec))
                  (setq rest-children (cdr rest-children)))
                (append children-cmds (list parse))))


             (t `(error ,start-pos ,(format "Bad cmd: %s " first-child)))))))))))

(defun gis-200--parse-tree-to-asm (parse)
  "Generate game bytecode from tree of PARSE, resolving labels."
  (catch 'err
    (let ((asm (flatten-list (gis-200--parse-tree-to-asm* parse))))
      (gis-200--resolve-labels asm)
      asm)))

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

;;; RUNTIME ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl-defstruct (gis-200--cell-runtime
               (:constructor gis-200--cell-runtime-create)
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
  run-function
  run-spec
  run-state)



(defun gis-200--cell-runtime-current-instruction (cell-runtime)
  "Return the current instruction of CELL-RUNTIME based in pc."
  (let* ((pc (gis-200--cell-runtime-pc cell-runtime))
         (instrs (gis-200--cell-runtime-instructions cell-runtime))
         (instrs (if (not (listp instrs)) (list instrs) instrs)))
    (if (not instrs)
        (gis-200--code-node-create :children '(_EMPTY))
      (and (>= pc (length instrs)) (error "End of program error"))
      (car (nthcdr pc instrs)))))

(defun gis-200--gameboard-in-final-state-p ()
  "Return non-nil if the gameboard is in a finalized state."
  ;; If gis-200--gameboard-state is not nil then it is in finalized state.
  gis-200--gameboard-state)

;; TODO - consolidate this function with gis-200--cell-at-moved-row-col
(defun gis-200--cell-at-row-col (row col)
  "Return the cell at index ROW COL from the gameboard."
  (aref gis-200--gameboard
        (+ (* row gis-200--gameboard-col-ct)
           col)))

(defun gis-200--set-cell-at-row-col (row col cell-runtime)
  "Set board cell at ROW, COL to CELL-RUNTIME."
  (when (not gis-200--gameboard)
    (setq gis-200--gameboard
          (make-vector (* gis-200--gameboard-col-ct gis-200--gameboard-row-ct)
                       nil)))
  (setf (aref gis-200--gameboard (+ (* row gis-200--gameboard-col-ct) col))
        cell-runtime))

(defun gis-200--set-cell-asm-at-row-col (row col asm)
  "Create a runtime from ASM at set board cell at ROW, COL to it."
  (let* ((asm (if (not (listp asm)) (list asm) asm))
         (runtime (gis-200--cell-runtime-create
                   :instructions asm
                   :pc 0
                   :stack '()
                   :row row
                   :col col
                   :up nil
                   :down nil
                   :left nil
                   :right nil)))
    (gis-200--set-cell-at-row-col row col runtime)))

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

(defun gis-200--get-value-from-staging-direction (cell-runtime direction)
  "Dynamically look up and return value at DIRECTION on CELL-RUNTIME."
  (pcase direction
    ('UP (gis-200--cell-runtime-staging-up cell-runtime))
    ('RIGHT (gis-200--cell-runtime-staging-right cell-runtime))
    ('DOWN (gis-200--cell-runtime-staging-down cell-runtime))
    ('LEFT (gis-200--cell-runtime-staging-left cell-runtime))))

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

(defun gis-200--cell-runtime-merge-ports-with-staging (cell-runtime)
  "For CELL-RUNTIME, if the staging region has a value, move it to port.
If the port does't have a value, set staging to nil."
  ;; This function is needed to prevent execution order from tampering with the
  ;; execution results.
  (dolist (direction '(UP DOWN LEFT RIGHT))
    (let ((staging-value
           (gis-200--get-value-from-staging-direction cell-runtime direction))
          (value (gis-200--get-value-from-direction cell-runtime direction)))
      (when (and (eql staging-value 'sent) (not value))
        (gis-200--cell-runtime-set-staging-value-from-direction cell-runtime direction nil)
        (setq staging-value nil))
      (when (and staging-value (not value))
        (gis-200--cell-runtime-set-value-from-direction cell-runtime direction staging-value)
        (gis-200--cell-runtime-set-staging-value-from-direction cell-runtime direction 'sent)))))

(defun gis-200--remove-value-from-staging-direction (cell-runtime direction)
  "Dynamically look up and return value at staging DIRECTION on CELL-RUNTIME."
  (pcase direction
    ('UP (setf (gis-200--cell-runtime-staging-up cell-runtime) nil))
    ('RIGHT (setf (gis-200--cell-runtime-staging-right cell-runtime) nil))
    ('DOWN (setf (gis-200--cell-runtime-staging-down cell-runtime) nil))
    ('LEFT (setf (gis-200--cell-runtime-staging-left cell-runtime) nil))))

(defun gis-200--remove-value-from-direction (cell-runtime direction)
  "Dynamically look up and return value at DIRECTION on CELL-RUNTIME."
  (pcase direction
    ('UP (setf (gis-200--cell-runtime-up cell-runtime) nil))
    ('RIGHT (setf (gis-200--cell-runtime-right cell-runtime) nil))
    ('DOWN (setf (gis-200--cell-runtime-down cell-runtime) nil))
    ('LEFT (setf (gis-200--cell-runtime-left cell-runtime) nil))))

;; TODO don't repeat this logic elsewhere
(defun gis-200--cell-runtime-set-value-from-direction (cell-runtime direction value)
  "Dynamically set the DIRECTION port of CELL-RUNTIME to VALUE."
  (pcase direction
    ('UP (setf (gis-200--cell-runtime-up cell-runtime) value))
    ('RIGHT (setf (gis-200--cell-runtime-right cell-runtime) value))
    ('DOWN (setf (gis-200--cell-runtime-down cell-runtime) value))
    ('LEFT (setf (gis-200--cell-runtime-left cell-runtime) value))))

(defun gis-200--cell-runtime-set-staging-value-from-direction (cell-runtime direction value)
  "Dynamically set the staging DIRECTION port of CELL-RUNTIME to VALUE."
  (pcase direction
    ('UP (setf (gis-200--cell-runtime-staging-up cell-runtime) value))
    ('RIGHT (setf (gis-200--cell-runtime-staging-right cell-runtime) value))
    ('DOWN (setf (gis-200--cell-runtime-staging-down cell-runtime) value))
    ('LEFT (setf (gis-200--cell-runtime-staging-left cell-runtime) value))))

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
  (let* ((stack (gis-200--cell-runtime-stack cell-runtime)))
    (when (>= (length stack) 4)
      (let ((row (gis-200--cell-runtime-row cell-runtime))
            (col (gis-200--cell-runtime-col cell-runtime)))
        (setq gis-200-runtime-error ;; TODO: extract the logic here to separate function
              (list "Stack overflow" row col))
        (setq gis-200--gameboard-state 'error)
        (message "Stack overflow error at (%d, %d)" row col))) ;; Stack size hardcoded.
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
         (res (funcall function v2 v1)))
    (gis-200--cell-runtime-push cell-runtime res)))

(defun gis-200--cell-runtime-set-stack (cell-runtime offset &optional op)
  "Set the stack at a given OFFSET of CELL-RUNTIME to top stack value.

If OP is a symbol, perform special logic."
  (let* ((row (gis-200--cell-runtime-row cell-runtime))
         (col (gis-200--cell-runtime-col cell-runtime))
         (stack (gis-200--cell-runtime-stack cell-runtime))
         (offset (if (< offset 0) (+ offset (length stack)) offset))
         (curr-val (nth (- (length stack) offset 1) stack))
         (v (cond
             ((eql op 'INC) (1+ curr-val))
             ((eql op 'DEC) (1- curr-val))
             (t (gis-200--cell-runtime-pop cell-runtime)))))
    (when (or (< offset 0) (>= offset (length stack)))
      (setq gis-200-runtime-error  ;; TODO: extract this logic
            (list "Idx out of bounds" row col))
      (setq gis-200--gameboard-state 'error))
    (setcar (nthcdr (- (length stack) offset 1) stack) v)))

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
      ('UP (setq current-val (gis-200--cell-runtime-staging-up cell-runtime)))
      ('DOWN (setq current-val (gis-200--cell-runtime-staging-down cell-runtime)))
      ('LEFT (setq current-val (gis-200--cell-runtime-staging-left cell-runtime)))
      ('RIGHT (setq current-val (gis-200--cell-runtime-staging-right cell-runtime))))
    (let ((result
           (if current-val
               ;; item is blocked
               'blocked
             (gis-200--cell-runtime-set-staging-value-from-direction cell-runtime direction v))))
      (when (eql result 'blocked)
        (gis-200--cell-runtime-push cell-runtime v))
      result)))

(defun gis-200--cell-runtime-get-extra (cell-runtime direction)
  "Perform the GET command on CELL-RUNTIME outside the gameboard at DIRECTION."
  (let* ((at-row (gis-200--cell-runtime-row cell-runtime))
         (at-col (gis-200--cell-runtime-col cell-runtime))
         (source (gis-200--gameboard-source-at-pos at-row at-col direction)))
    (if (or (not source) (not (gis-200--cell-source-current-value source)))
        'blocked
      (let ((v (gis-200--cell-source-pop source)))
        (gis-200--cell-runtime-push cell-runtime v)))))

(defun gis-200--cell-runtime-stack-get (cell-runtime loc)
  "Perform a variant of the GET command, grabbing the LOC value from CELL-RUNTIME's stack."
  (let ((row (gis-200--cell-runtime-row cell-runtime))
        (col (gis-200--cell-runtime-col cell-runtime))
        (stack (seq-reverse (gis-200--cell-runtime-stack cell-runtime)))
        (val))
    ;; Error checking
    (if (>= loc 0)
        (progn
          (when (>= loc (length stack))
            (setq gis-200-runtime-error ;; TODO: extract the logic here to separate function
                  (list (format "Bad idx %d/%d" loc (length stack)) row col)))
          (setq val (nth loc stack)))
      (when (> (- loc) (length stack))
        (setq gis-200-runtime-error ;; TODO: extract the logic here to separate function
                  (list (format "Bad idx %d/%d" loc (length stack)) row col)))
      (setq val (nth (+ (length stack) loc) stack)))
    (gis-200--cell-runtime-push cell-runtime val)))

(defun gis-200--cell-runtime-get (cell-runtime direction)
  "Perform the GET command running from CELL-RUNTIME, recieving from DIRECTION."
  (if (integerp direction)
      (gis-200--cell-runtime-stack-get cell-runtime direction)
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
            'blocked))))))

(defun gis-200--true-p (v)
  "Return non-nil if V is truthy."
  (not (= 0 v)))

(defun gis-200--cell-runtime-skip-labels (cell-runtime)
  "Skip pc over any label instructions for CELL-RUNTIME.
This logic  is needed to display current command properly."
  (while (let* ((current-instr (gis-200--cell-runtime-current-instruction cell-runtime))
                (code-data (gis-200-code-node-children current-instr))
                (cmd (car code-data)))
           (eql cmd 'LABEL))
    ;; TODO: check case of all LABEL commands
    (gis-200--cell-runtime-pc-inc cell-runtime)))

(defun gis-200--cell-runtime-step (cell-runtime)
  "Perform one step of CELL-RUNTIME."
  (let* ((current-instr (gis-200--cell-runtime-current-instruction cell-runtime))
         (code-data (gis-200-code-node-children current-instr))
         (cmd (car code-data))
         (status
          (pcase cmd
            ('_EMPTY 'blocked)
            ('CONST (let ((const (cadr code-data)))
                      (gis-200--cell-runtime-push cell-runtime const)))
            ('SET (let ((stack-offset (cadr code-data)))
                    (gis-200--cell-runtime-set-stack cell-runtime stack-offset)))
            ('INC (let ((stack-offset (cadr code-data)))
                    (gis-200--cell-runtime-set-stack cell-runtime stack-offset 'INC)))
            ('DEC (let ((stack-offset (cadr code-data)))
                    (gis-200--cell-runtime-set-stack cell-runtime stack-offset 'DEC)))
            ('CLR (setf (gis-200--cell-runtime-stack cell-runtime) nil))
            ('DUP (let ((stack (gis-200--cell-runtime-stack cell-runtime)))
                     (setf (gis-200--cell-runtime-stack cell-runtime) (append stack stack))))
            ('ADD (gis-200--binary-operation cell-runtime #'+))
            ('SUB (gis-200--binary-operation cell-runtime #'-))
            ('MUL (gis-200--binary-operation cell-runtime #'*))
            ('DIV (gis-200--binary-operation cell-runtime #'/))
            ('REM (gis-200--binary-operation cell-runtime #'%))
            ('AND (gis-200--binary-operation cell-runtime(lambda (a b) (if (and (gis-200--true-p a)
                                                                                (gis-200--true-p b))
                                                                           1 0))))
            ('NOT (gis-200--unary-operation cell-runtime (lambda (x) (if (gis-200--true-p x) 0 1))))
            ('NEG (gis-200--unary-operation cell-runtime (lambda (x) (- x))))
            ('ABS (gis-200--unary-operation cell-runtime (lambda (x) (abs x))))
            ('OR (gis-200--binary-operation cell-runtime (lambda (a b) (if (or (gis-200--true-p a)
                                                                               (gis-200--true-p b))
                                                                           1 0))))
            ('EQZ (gis-200--unary-operation cell-runtime (lambda (x) (if (= 0 x) 1 0))))

            ('LZ (gis-200--unary-operation cell-runtime (lambda (x) (if (< x 0) 1 0))))
            ('GZ (gis-200--unary-operation cell-runtime (lambda (x) (if (> x 0) 1 0))))

            ('EQ (gis-200--binary-operation cell-runtime (lambda (a b) (if (= a b) 1 0))))
            ('NE (gis-200--binary-operation cell-runtime (lambda (a b) (if (not (= a b)) 1 0))))
            ('LT (gis-200--binary-operation cell-runtime (lambda (a b) (if (< a b) 1 0))))
            ('LE (gis-200--binary-operation cell-runtime (lambda (a b) (if (<= a b) 1 0))))
            ('GT (gis-200--binary-operation cell-runtime (lambda (a b) (if (> a b) 1 0))))
            ('GE (gis-200--binary-operation cell-runtime (lambda (a b) (if (>= a b) 1 0))))
            ('NOP (ignore))
            ('DROP (gis-200--cell-runtime-pop cell-runtime))
            ('SEND (gis-200--cell-runtime-send cell-runtime (cadr code-data)))
            ('GET (gis-200--cell-runtime-get cell-runtime (cadr code-data)))
            ('RIGHT (gis-200--cell-runtime-get cell-runtime 'RIGHT))
            ('LEFT (gis-200--cell-runtime-get cell-runtime 'LEFT))
            ('UP (gis-200--cell-runtime-get cell-runtime 'UP))
            ('DOWN (gis-200--cell-runtime-get cell-runtime 'DOWN))
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
      (_ (gis-200--cell-runtime-pc-inc cell-runtime)))
    (gis-200--cell-runtime-skip-labels cell-runtime)))

(defun gis-200--step ()
  "Perform the operations needed to progress the game one step."
  (gis-200--gameboard-step)
  (gis-200--resolve-port-values)
  (gis-200--extra-gameboard-step))

(defun gis-200--extra-gameboard-step ()
  "Perform step on all things not on the gameboard."
  (let ((sinks (gis-200--problem-spec-sinks gis-200--extra-gameboard-cells)))
    (dolist (sink sinks)
      (gis-200--cell-sink-get sink))))

(defun gis-200--gameboard-step ()
  "Perform step on all cells on the gameboard."
  (let ((last-cell-fns '()))
    (dotimes (idx (length gis-200--gameboard))
      (let ((cell (aref gis-200--gameboard idx)))
        (let ((fn (gis-200--cell-runtime-run-function cell)))
          (if (functionp fn)
              (setq last-cell-fns (cons (cons fn cell) last-cell-fns))
            (gis-200--cell-runtime-step cell)))))
    ;; We need to run the non-code cells last because they directly
    ;; manipulate their ports.
    (dolist (fn+cell last-cell-fns)
      (funcall (car fn+cell) (cdr fn+cell)))))

(defun gis-200--resolve-port-values ()
  "Move staging port values to main, propogate nils up to staging."
  (dotimes (idx (length gis-200--gameboard))
    (let ((cell (aref gis-200--gameboard idx)))
      (gis-200--cell-runtime-merge-ports-with-staging cell))))

(defun gis-200-check-winning-conditions ()
  "Return non-nil if all sinks are full."
  (when (not (gis-200--gameboard-in-final-state-p))
    (let ((sinks (gis-200--problem-spec-sinks gis-200--extra-gameboard-cells))
          (win-p t))
      (while (and sinks win-p)
        (let* ((sink (car sinks))
               (expected-data (gis-200--cell-sink-expected-data sink))
               (idx (gis-200--cell-sink-idx sink))
               (err-val (gis-200--cell-sink-err-val sink)))
          (if (gis-200--cell-sink-expected-text sink)
              ;; If expected text exists we are dealing with an editor sink
              (let* ((expected-text (string-trim-right (gis-200--cell-sink-expected-text sink)))
                     (expected-lines (split-string expected-text "\n"))
                     (text (string-trim-right (gis-200--cell-sink-editor-text sink)))
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
        (gis-200--win-file-for-current-buffer)
        ;; TODO: Do something else for the victory.
        (setq gis-200--gameboard-state 'win)
        (message "Congragulations, you won!")))))

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
          (gis-200--cell-source-current-value source))))
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
          (gis-200--cell-source-current-value source))))
     (t
      (let* ((cell-runtime (gis-200--cell-at-row-col cell-row col)))
        (gis-200--get-value-from-direction cell-runtime direction))))))

(defun gis-200--get-source-idx-at-position (row col)
  "Return name of source at position ROW, COL if exists."
  (let ((sources (gis-200--problem-spec-sources gis-200--extra-gameboard-cells))
        (found))
    (while sources
      (let ((source (car sources)))
        (if (and (= (gis-200--cell-source-row source) row)
                 (= (gis-200--cell-source-col source) col))
            (progn (setq sources nil)
                   (setq found (gis-200--cell-source-name source)))
          (setq sources (cdr sources)))))
    found))

(defun gis-200--get-sink-name-at-position (row col)
  "Return name of sink at position ROW, COL if exists."
  (let ((sinks (gis-200--problem-spec-sinks gis-200--extra-gameboard-cells))
        (found))
    (while sinks
      (let ((sink (car sinks)))
        (if (and (= (gis-200--cell-sink-row sink) row)
                 (= (gis-200--cell-sink-col sink) col))
            (progn (setq sinks nil)
                   (setq found (gis-200--cell-sink-name sink)))
          (setq sinks (cdr sinks)))))
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
  row col data idx name)

(cl-defstruct (gis-200--cell-sink
               (:constructor gis-200--cell-sink-create)
               (:copier nil))
  row col expected-data idx name err-val
  default-editor-text
  editor-text editor-point expected-text)

(cl-defstruct (gis-200--problem-spec
               (:constructor gis-200--problem-spec-create)
               (:copier nil))
  sources sinks name description difficulty)

(defun gis-200--reset-extra-gameboard-cells-state ()
  "Reset the state of all cells not in the grid (sources and sinks)."
  (let ((sources (gis-200--problem-spec-sources gis-200--extra-gameboard-cells))
        (sinks (gis-200--problem-spec-sinks gis-200--extra-gameboard-cells)))
    (dolist (source sources)
      (setf (gis-200--cell-source-idx source) 0))
    (dolist (sink sinks)
      (setf (gis-200--cell-sink-idx sink) 0)
      (setf (gis-200--cell-sink-err-val sink) nil)
      (when (gis-200--cell-sink-expected-text sink)
        (if (gis-200--cell-sink-default-editor-text sink)
            (setf (gis-200--cell-sink-editor-text sink)
                  (gis-200--cell-sink-default-editor-text sink))
          (setf (gis-200--cell-sink-editor-text sink) ""))
        (setf (gis-200--cell-sink-editor-point sink) 1)))))

(defun gis-200--cell-sink-get (sink)
  "Grab a value and put it into SINK from the gameboard."
  (let* ((row (gis-200--cell-sink-row sink))
         (col (gis-200--cell-sink-col sink))
         (direction (cond
                     ((>= col gis-200--gameboard-col-ct) 'LEFT)
                     ((> 0 col) 'RIGHT)
                     ((> 0 row) 'DOWN)
                     ((>= row gis-200--gameboard-row-ct) 'UP)))
         (opposite-direction (gis-200--mirror-direction direction))
         (cell-runtime (gis-200--cell-at-moved-row-col row col direction))
         (v (gis-200--get-value-from-direction cell-runtime opposite-direction)))
    (if v
        (let* ((data (gis-200--cell-sink-expected-data sink))
               (idx (gis-200--cell-sink-idx sink))
               (expected-value (nth idx data)))
          (when (not (equal expected-value v))
            ;; TODO - do something here
            (setq gis-200--gameboard-state 'error)
            (setf (gis-200--cell-sink-err-val sink) v)
            (message "Unexpected value"))
          (setf (gis-200--cell-sink-idx sink) (1+ idx))
          (gis-200--remove-value-from-direction cell-runtime opposite-direction))
      'blocked)))

(defun gis-200--cell-sink-insert-character (sink char)
  "For a textual SINK, insert CHAR."
  (let ((text (gis-200--cell-sink-editor-text sink))
        (point (gis-200--cell-sink-editor-point sink)))
    (cond
     ((<= 32 char 126)
      (setf (gis-200--cell-sink-editor-text sink)
            (concat (substring text 0 (1- point))
                    (char-to-string char)
                    (substring text (1- point))))
      (setf (gis-200--cell-sink-editor-point sink)
            (1+ point)))
     ((= char ?\n)
      (setf (gis-200--cell-sink-editor-text sink)
            (concat (substring text 0 (1- point))
                    (char-to-string char)
                    (substring text (1- point))))
      (setf (gis-200--cell-sink-editor-point sink)
            (1+ point)))
     ((or (= char ?\b) (= char -1))
      (when (not (= 1 point))
        (setf (gis-200--cell-sink-editor-text sink)
              (concat (substring text 0 (- point 2))
                      (substring text (- point 1))))
        (setf (gis-200--cell-sink-editor-point sink)
              (max (1- point) 1))))
     ((or (= char -2))
      (when (not (= 1 point))
        (setf (gis-200--cell-sink-editor-text sink)
              (concat (substring text 0 (- point 1))
                      (substring text point))))))))

(defun gis-200--cell-sink-move-point (sink point)
  "For a textual SINK, move the point to POINT."
  (let* ((text (gis-200--cell-sink-editor-text sink))
         (bounded-pt (max (min point (1+ (length text))) 1)))
    (setf (gis-200--cell-sink-editor-point sink) bounded-pt)))

(defun gis-200--cell-source-current-value (source)
  "Return the value of SOURCE that will be taken next."
  (unless (gis-200--cell-source-p source)
    (error "Cell-source-pop type error"))
  (let* ((data (gis-200--cell-source-data source))
         (idx (gis-200--cell-source-idx source))
         (top (nth idx data)))
    top))

(defun gis-200--cell-source-pop (source)
  "Pop a value from the data of SOURCE."
  (unless (gis-200--cell-source-p source)
    (error "Cell-source-pop type error"))
  (let* ((data (gis-200--cell-source-data source))
         (idx (gis-200--cell-source-idx source))
         (top (nth idx data)))
    (setf (gis-200--cell-source-idx source) (1+ idx))
    top))

(defun gis-200--problem-list-of-lists-to-lisp (lists)
  "Return a list of LISTS from 0-terminated list of number lists."
  (thread-last (list '() '())
    (seq-reduce (lambda (acc x)
                  (let ((lol (car acc))
                        (curr-list (cadr acc)))
                    (if (= x 0)
                        (list (cons (reverse curr-list) lol) '())
                      (list lol (cons  x curr-list)))))
                lists)
    (car)
    (reverse)))

(defun gis-200--problem-random-list-of-lists ()
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

(defun gis-200--problem--indentation ()
  "Generate a problem of indenting a code sequence properly."
  (let* ((input (seq-map (lambda (_) (+ 1 (random 10))) (make-list 40 nil)))
         (expected-output (seq-map (lambda (x) (/ (* x (+ 1 x)) 2))input)))
    (gis-200--problem-spec-create
     :name "Indentation I"
     :difficulty 'medium
     :sinks
     (list (gis-200--cell-sink-create :row 2
                                      :col 4
                                      :expected-data expected-output
                                      :idx 0
                                      :name "O"
                                      :default-editor-text "func main () {\nfmt.Println(\"hello world\")\nreturn\n}"
                                      :editor-point 1
                                      :expected-text "func main () {\n  fmt.Println(\"hello world\")\n  return\n}"))
     :description "<editor> Edit text to match the target.")))

(defun gis-200--problem--number-sum ()
  "Generate a problem of calculating y=x(x+1)/2."
  (let* ((input (seq-map (lambda (_) (+ 1 (random 10))) (make-list 40 nil)))
         (expected-output (seq-map (lambda (x) (/ (* x (+ 1 x)) 2))input)))
    (gis-200--problem-spec-create
     :name "Number Sum"
     :difficulty 'easy
     :sources (list (gis-200--cell-source-create :row -1
                                                 :col 3
                                                 :data input
                                                 :idx 0
                                                 :name "I"))
     :sinks
     (list (gis-200--cell-sink-create :row 2
                                      :col 4
                                      :expected-data expected-output
                                      :idx 0
                                      :name "O"
                                      :editor-text nil
                                      :editor-point nil
                                      :expected-text nil))
     :description "Read a number from I, send to O the sum of numbers
from 0 to the read number. (ex. 3->6, 4->10, 5->15)")))

(defun gis-200--problem--meeting-point ()
  "Generate a problem of finding the point that minimizes movement."
  (let* ((input (seq-map (lambda (_) (+ 1 (random 10))) (make-list 10 nil)))
         (expected-output (list (cl-loop for i from 1 to 1000
                                         minimize (cl-loop for d in input
                                                           sum (abs (- i d)))))))
    (gis-200--problem-spec-create
     :name "Meeting point"
     :difficulty 'hard
     :sources (list (gis-200--cell-source-create :row 1
                                                 :col -1
                                                 :data input
                                                 :idx 0
                                                 :name "N"))
     :sinks
     (list (gis-200--cell-sink-create :row 2
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

(defun gis-200--problem--simple-graph ()
  "Generate a problem for the user to draw a simple graph."
  (let* ((input (seq-map (lambda (_) (+ 1 (random 10))) (make-list 10 nil)))
         (expected-text (string-join
                         (seq-map (lambda (x) (make-string x ?#)) input) "\n")))
    (gis-200--problem-spec-create
     :name "Simple Graph"
     :difficulty 'medium
     :sources (list (gis-200--cell-source-create :row 1
                                                 :col -1
                                                 :data input
                                                 :idx 0
                                                 :name "A"))
     :sinks
     (list (gis-200--cell-sink-create :row 1
                                      :col 5
                                      :expected-data nil
                                      :idx 0
                                      :name "O"
                                      :editor-text ""
                                      :editor-point 1
                                      :expected-text expected-text))
     :description
     "<editor> Read a number from A, draw a line with that many '#' characters.")))

(defun gis-200--problem--hello-world ()
  "Generate a problem involving writing Hello World to the srceen."
  (gis-200--problem-spec-create
   :name "Editor Basics"
   :difficulty 'tutorial
   :sources (list )
   :sinks
   (list (gis-200--cell-sink-create :row 1
                                    :col 5
                                    :expected-data nil
                                    :idx 0
                                    :name "O"
                                    :editor-text "01"
                                    :editor-point 3
                                    :expected-text "Hello World"))
   :description "<editor> Write the string \"Hello World\" to the editor."))

(defun gis-200--problem--upcase ()
  "generate a problem involving upcasing characters."
  (let* ((input-1 (seq-map (lambda (_)
                             (+ (random 95) 32))
                           (make-list 40 nil)))
         (expected (string-to-list (upcase (apply #'string input-1)))))
    (gis-200--problem-spec-create
     :name "Upcase"
     :difficulty 'easy
     :sources (list (gis-200--cell-source-create :row 1
                                                 :col -1
                                                 :data input-1
                                                 :idx 0
                                                 :name "C"))
     :sinks
     (list (gis-200--cell-sink-create :row 1
                                      :col 4
                                      :expected-data expected
                                      :idx 0
                                      :name "O"))
     :description "Read a character from C and send it to O,
upcasing it if it is a lowercase letter.")))

(defun gis-200--problem--inc-ct ()
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
    (gis-200--problem-spec-create
     :name "Increment Cout"
     :difficulty 'medium
     :sources (list (gis-200--cell-source-create :row 1
                                                 :col -1
                                                 :data input-1
                                                 :idx 0
                                                 :name "I"))
     :sinks
     (list (gis-200--cell-sink-create :row 1
                                      :col 4
                                      :expected-data expected
                                      :idx 0
                                      :name "O"))
     :description
     "Return the number of times subsequent values of I increase.
ex. 1  2  0  5  6  4
     +  -  +  +  -     3 increses")))

(defun gis-200--problem--tax ()
  "Generate a simple tax problem."
  (let* ((high-start-ct (random 20))
         (start-seq (seq-map (lambda (_) (random 999)) (make-list high-start-ct nil)))
         (high-seq (seq-map (lambda (_) (+ 500 (random 499))) (make-list 12 nil)))
         (rest-seq (seq-map (lambda (_) (random 999)) (make-list (- 40 high-start-ct 12) nil)))
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
    (gis-200--problem-spec-create
     :name "Tax"
     :difficulty 'hard
     :sources (list (gis-200--cell-source-create :row 1
                                                 :col -1
                                                 :data input-1
                                                 :idx 0
                                                 :name "I"))
     :sinks
     (list (gis-200--cell-sink-create :row 1
                                      :col 4
                                      :expected-data expected
                                      :idx 0
                                      :name "O"))
     :description
     "Read values from I. After the 12th consecutive value is greater than or equal to 500, return that 12th value divided by 40.")))

(defun gis-200--problem--list-reverse ()
  "Generate a simple addition problem."
  (let* ((input-1 (gis-200--problem-random-list-of-lists))
         (lists (gis-200--problem-list-of-lists-to-lisp input-1))
         (expected (flatten-list (seq-map (lambda (l)
                                       (append (reverse l) (list 0)))
                                     lists))))
    (gis-200--problem-spec-create
     :name "List Reverse"
     :difficulty 'medium
     :sources (list (gis-200--cell-source-create :row -1
                                                 :col 2
                                                 :data input-1
                                                 :idx 0
                                                 :name "L"))
     :sinks
     (list (gis-200--cell-sink-create :row 3
                                      :col 1
                                      :expected-data expected
                                      :idx 0
                                      :name "R"))
     :description
     "Lists are 0 terminated.
Read a list from L, reverse it, and send it to R (terminating it with 0).")))

(defun gis-200--problem--list-length ()
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
    (gis-200--problem-spec-create
     :name "List Length"
     :difficulty 'medium
     :sources (list (gis-200--cell-source-create :row 1
                                                 :col -1
                                                 :data nums
                                                 :idx 0
                                                 :name "I"))
     :sinks
     (list (gis-200--cell-sink-create :row 1
                                      :col 4
                                      :expected-data lengths
                                      :idx 0
                                      :name "O"))
     :description "Lists are 0 terminated. Read a list from I, calculate its length and send it to O.")))

(defun gis-200--problem--turing ()
  "Generate a simple Brain****-like puzzle."
  (let* ((input-1 (list ?> ?> ?> ?+ ?+ ?. ?. ?< ?+ ?. ?> ?. ?+ ?. ?> ?> ?.))
         (expected (list 2 2 1 2 3 0)))
    (gis-200--problem-spec-create
     :name "Turing"
     :difficulty 'hard
     :sources (list (gis-200--cell-source-create :row 0
                                                 :col -1
                                                 :data input-1
                                                 :idx 0
                                                 :name "X"))
     :sinks
     (list (gis-200--cell-sink-create :row 1
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


(defun gis-200--problem--merge-step ()
  "Generate a simple addition problem."
  (let* ((input-1 (seq-sort #'< (seq-map (lambda (_) (random 100))
                                         (make-list 20 nil))))
         (input-2 (seq-sort #'< (seq-map (lambda (_) (random 100))
                                         (make-list 20 nil))))
         (expected (seq-sort #'< (append input-1 input-2))))
    (gis-200--problem-spec-create
     :name "Merge Step"
     :difficulty 'hard
     :sources (list (gis-200--cell-source-create :row 0
                                                 :col -1
                                                 :data input-1
                                                 :idx 0
                                                 :name "A")
                    (gis-200--cell-source-create :row 2
                                                 :col -1
                                                 :data input-2
                                                 :idx 0
                                                 :name "B"))
     :sinks
     (list (gis-200--cell-sink-create :row 1
                                      :col 4
                                      :expected-data expected
                                      :idx 0
                                      :name "C"))
     :description "Numbers in A and B are sorted. Read numbers from A and B,
combine them sorted and send it them to C.")))

(defun gis-200--problem--filter ()
  "Generate a simple addition problem."
  (let* ((input-1 (seq-map (lambda (_) (random 100)) (make-list 40 nil)))
         (expected (seq-map (lambda (x)
                              (if (= (mod x 2) 0)
                                  0
                                x))
                               input-1)))
    (gis-200--problem-spec-create
     :name "Number Filter"
     :difficulty 'easy
     :sources (list (gis-200--cell-source-create :row 1
                                                 :col -1
                                                 :data input-1
                                                 :idx 0
                                                 :name "I"))
     :sinks
     (list (gis-200--cell-sink-create :row 1
                                      :col 4
                                      :expected-data expected
                                      :idx 0
                                      :name "O"))
     :description
     "Read a value from I. If it is even send 0 to O, else send the value.")))

(defun gis-200--problem--clock ()
  "Generate a simple addition problem."
  (let* ((input-1 (seq-map (lambda (_) (random 24)) (make-list 40 nil)))
         (expected (cdr (seq-reverse
                         (seq-reduce (lambda (acc x)
                                       (let ((top (car acc)))
                                         (cons (mod (+ top x) 24)
                                               acc)))
                                     input-1
                                     '(0))))))
    (gis-200--problem-spec-create
     :name "Clock Hours"
     :difficulty 'easy
     :sources (list (gis-200--cell-source-create :row 1
                                                 :col -1
                                                 :data input-1
                                                 :idx 0
                                                 :name "H"))
     :sinks
     (list (gis-200--cell-sink-create :row 3
                                      :col 1
                                      :expected-data expected
                                      :idx 0
                                      :name "T"))
     :description "On a clock with hours 0 to 23, read a value from H and add
that value to the current time which starts at 0.

Write the current time to T for every time you move the current time.")))

(defun gis-200--problem--add ()
  "Generate a simple addition problem."
  (let* ((input-1 (seq-map (lambda (_) (random 10)) (make-list 40 nil)))
         (input-2 (seq-map (lambda (_) (random 10)) (make-list 40 nil)))
         (input-3 (seq-map (lambda (_) (random 10)) (make-list 40 nil)))
         (expected (seq-mapn #'+ input-1 input-2 input-3)))
    (gis-200--problem-spec-create
     :name "Number Addition"
     :difficulty 'easy
     :sources (list (gis-200--cell-source-create :row -1
                                                 :col 0
                                                 :data input-1
                                                 :idx 0
                                                 :name "A")
                    (gis-200--cell-source-create :row -1
                                                 :col 1
                                                 :data input-2
                                                 :idx 0
                                                 :name "B")
                    (gis-200--cell-source-create :row -1
                                                 :col 2
                                                 :data input-3
                                                 :idx 0
                                                 :name "C"))
     :sinks
     (list (gis-200--cell-sink-create :row 3
                                      :col 1
                                      :expected-data expected
                                      :idx 0
                                      :name "S"))
     :description
     "Take input from A, B, and C, add the three together, and send it to S.")))

(defun gis-200--problem--number-sorter ()
  "Generate problem for comparing two numbers and sending them in different places."
  (let* ((input-1 (seq-map (lambda (_) (random 10)) (make-list 40 nil)))
         (input-2 (seq-map (lambda (_) (random 10)) (make-list 40 nil)))
         (expected-1 (seq-mapn (lambda (a b) (if (> a b) a 0)) input-1 input-2))
         (expected-2 (seq-mapn (lambda (a b) (if (> b a) b 0)) input-1 input-2)))
    (gis-200--problem-spec-create
     :name "Number Chooser"
     :difficulty 'easy
     :sources (list (gis-200--cell-source-create :row -1
                                                 :col 0
                                                 :data input-1
                                                 :idx 0
                                                 :name "A")
                    (gis-200--cell-source-create :row -1
                                                 :col 1
                                                 :data input-2
                                                 :idx 0
                                                 :name "B"))
     :sinks
     (list (gis-200--cell-sink-create :row 0
                                      :col 4
                                      :expected-data expected-1
                                      :idx 0
                                      :name "L")
           (gis-200--cell-sink-create :row 2
                                      :col 4
                                      :expected-data expected-2
                                      :idx 0
                                      :name "R"))
     :description "Take an input from A and B. If A>B then send A to L, 0 to R;
If B>A then send B to R, 0 to L. If A=B send 0 to L and R.")))

(defun gis-200--problem--constant ()
  "Generate a simple addition problem."
  (let* ((expected (make-list 40 1)))
    (gis-200--problem-spec-create
     :name "Constant Generator"
     :difficulty 'tutorial
     :sources (list )
     :sinks
     (list (gis-200--cell-sink-create :row 0
                                      :col 4
                                      :expected-data expected
                                      :idx 0
                                      :name "N"))
     :description "Repeatedly send the number 1 to N. There are no inputs.")))

(defun gis-200--problem--identity ()
  "Generate a simple addition problem."
  (let* ((input-1 (seq-map (lambda (_) (random 10)) (make-list 40 nil)))
         (expected input-1))
    (gis-200--problem-spec-create
     :name "Identity"
     :difficulty 'tutorial
     :sources (list (gis-200--cell-source-create :row -1
                                                 :col 0
                                                 :data input-1
                                                 :idx 0
                                                 :name "X"))
     :sinks
     (list (gis-200--cell-sink-create :row 3
                                      :col 3
                                      :expected-data expected
                                      :idx 0
                                      :name "X"))
     :description "Take an input from the input X and send it to the output X.")))

(defvar gis-200-puzzles (list
                         #'gis-200--problem--indentation
                         #'gis-200--problem--constant
                         #'gis-200--problem--identity
                         #'gis-200--problem--add
                         #'gis-200--problem--filter
                         #'gis-200--problem--number-sum
                         #'gis-200--problem--number-sorter
                         #'gis-200--problem--clock
                         #'gis-200--problem--tax
                         #'gis-200--problem--list-length
                         #'gis-200--problem--list-reverse
                         #'gis-200--problem--inc-ct
                         #'gis-200--problem--upcase
                         #'gis-200--problem--merge-step
                         #'gis-200--problem--hello-world
                         #'gis-200--problem--simple-graph
                         #'gis-200--problem--meeting-point
                         #'gis-200--problem--turing))

(defun gis-200--get-puzzle-by-id (name)
  "Given a puzzle NAME, return tis puzzle generation function."
  (seq-find (lambda (puzzle-fn)
              (let ((n (gis-200--problem-spec-name (funcall puzzle-fn))))
                (equal name n)))
            gis-200-puzzles))

;;; File Saving ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst gis-200-save-directory-name
  (expand-file-name ".gis-200" user-emacs-directory))

(defun gis-200--generate-new-puzzle-filename (name)
  "For puzzle NAME, determine the name for a new puzzle."
  ;; TODO: This work when deleting puzzles.
  (ignore-errors
    (make-directory gis-200-save-directory-name))
  (let* ((dir-files (directory-files gis-200-save-directory-name))
         (puzzle-files (seq-filter (lambda (file-name)
                                     (string-prefix-p name file-name))
                                   dir-files))
         (new-idx (number-to-string (1+ (length puzzle-files)))))
    (expand-file-name (concat name "-" new-idx ".gis")
                      gis-200-save-directory-name)))

(defun gis-200--parse-saved-buffer ()
  "Extract the puzzle contents from the saved buffer setting up state."
  (save-excursion
    (goto-char (point-min))
    (setq gis-200-box-contents (make-hash-table :test 'equal))
    ;; Get the literal contents of each box.
    (let ((match-regexp (format "│\\(.\\{%d\\}\\)│" gis-200-box-width)))
     (dotimes (i (* gis-200--gameboard-row-ct
                    gis-200--gameboard-col-ct
                    gis-200-box-height))
       (search-forward-regexp match-regexp)
       (let* ((col (mod i gis-200--gameboard-col-ct))
              (row (/ (/ i gis-200--gameboard-col-ct) gis-200-box-height))
              (match (string-trim-right (match-string-no-properties 1)))
              (current-text (gethash (list row col) gis-200-box-contents))
              (next-text (if current-text
                             (concat current-text "\n" match)
                           match)))
         (puthash (list row col) next-text gis-200-box-contents))))
    ;; Trim the ends of the boxes, assume user doesn't want it.
    (dotimes (row gis-200--gameboard-row-ct)
      (dotimes (col gis-200--gameboard-col-ct)
        (let ((prev-val (gethash (list row col) gis-200-box-contents)))
          (puthash (list row col)
                   (string-trim-right prev-val)
                   gis-200-box-contents))))
    ;; Find the name of the puzzle.
    (search-forward-regexp "^\\([[:alnum:] -]+\\):")
    (let ((match (match-string 1)))
      (unless match
        (error "Bad file format, no puzzle name found"))
      (let ((puzzle (gis-200--get-puzzle-by-id match)))
        (unless puzzle
          (error "No puzzle with name %s found" puzzle))
        (setq gis-200--extra-gameboard-cells (funcall puzzle))))))

(defun gis-200--saved-puzzle-ct-by-id (id)
  "Return the number of saved puzzles that start with the puzzle ID."
  (length (seq-filter (lambda (file-name)
                        (string-prefix-p id file-name))
                      (directory-files gis-200-save-directory-name))))

(defun gis-200--make-puzzle-idx-file-name (id idx)
  "Create a file name for puzzle with ID and IDX."
  (expand-file-name (concat id "-" (number-to-string idx) ".gis")
                    gis-200-save-directory-name))

(defun gis-200--win-file-for-current-buffer ()
  "Return the name of the win-backup for the current execution buffer."
  (unless (equal (buffer-name) "*gis-200-execution*")
    (error "Unable to win from non-execution buffer"))
  (unless (bufferp gis-200-execution-origin-buffer)
    (error "Unable to find buffer with winning solution"))
  (let* ((buffer-contents (with-current-buffer gis-200-execution-origin-buffer
                            (buffer-string)))
         (bfn (with-current-buffer gis-200-execution-origin-buffer
                            (buffer-file-name)))
         (name (file-name-nondirectory bfn))
         (path (file-name-directory bfn))
         (new-name (concat path "." name ".win.txt")))
    (save-window-excursion
      (let ((inhibit-read-only t))
        (find-file new-name)
        (insert buffer-contents)
        (save-buffer)
        (kill-buffer)))))

(defun gis-200--backup-file-for-current-buffer ()
  "Create a backup file for the current buffer."
  (let* ((buffer-contents (buffer-string))
         (bfn (buffer-file-name))
         (name (file-name-nondirectory bfn))
         (path (file-name-directory bfn))
         (new-name (concat path "." name ".backup.txt")))
    (save-window-excursion
      (let ((inhibit-read-only t))
        (find-file new-name)
        (insert buffer-contents)
        (save-buffer)
        (kill-buffer)))))

(defun gis-200--puzzle-won-p (puzzle-name)
  "Return non-nil if a win file exists for puzzle with name PUZZLE-NAME."
  (seq-find (lambda (n) (and (string-match (regexp-quote puzzle-name) n)
                             (string-match "\\.win" n)))
            (directory-files gis-200-save-directory-name)))

;;; YAML Blocks

(defun gis-200--create-yaml-code-node (row col code)
  "Create a runtime for the parsed CODE, located at ROW COL."
  (let-alist (yaml-parse-string code :object-type 'alist)
    ;; .apiVersion .kind .metadata .spec
    (unless (equal "v1" .apiVersion)
      (error "Unknown api version: %s" .apiVersion))
    (pcase .kind
      ("Stack" (gis-200--yaml-create-stack row col .metadata .spec))
      ("Controller" (gis-200--yaml-create-controller row col .metadata .spec))
      ("Container" (error "Container not implemented"))
      ("Network" (error "Network not implemented"))
      ("Heap" (gis-200--yaml-create-heap row col .metadata .spec)))))

(defun gis-200--yaml-step-stack (cell-runtime)
  "Perform the step operation for the CELL-RUNTIME of kind Stack."
  (let ((row (gis-200--cell-runtime-row cell-runtime))
        (col (gis-200--cell-runtime-col cell-runtime))
        (spec (gis-200--cell-runtime-run-spec cell-runtime))
        (state (gis-200--cell-runtime-run-state cell-runtime)))
    (let-alist spec
      ;; .inputPorts .outputPort .sizePort .size .logLevel
      ;; First put port back on the stack
      (let* ((port-sym (intern (upcase .outputPort)))
             (size-port-sym (and .sizePort (intern (upcase .sizePort))))
             (val (gis-200--get-value-from-direction cell-runtime port-sym)))
        (when val
          (gis-200--remove-value-from-direction cell-runtime port-sym)
          (setq state (cons val state)))
        (when .sizePort
          (gis-200--remove-value-from-direction cell-runtime size-port-sym)))

      ;; Then add new values to stack
      (seq-do
       (lambda (port)
         (let* ((port-sym (intern (upcase port)))
                (from-cell (gis-200--cell-at-moved-row-col row col port-sym))
                (opposite-port (gis-200--mirror-direction port-sym))
                (recieve-val (gis-200--get-value-from-direction from-cell opposite-port)))
           (when recieve-val
             (gis-200--remove-value-from-direction from-cell opposite-port)
             (setq state (cons recieve-val state))
             (when (> (length state) (or .size 20)) ;; TODO: find a good way of setting defualts.
               (error "Stack overflow %d/%d" (length state) (or .size 20))))))
       .inputPorts)

      ;; Add size to sizePort
      (when .sizePort
        (gis-200--cell-runtime-set-staging-value-from-direction
         cell-runtime
         (intern (upcase .sizePort))
         (length state)))

      ;; Add top value of stack to port
      (when state
        (gis-200--cell-runtime-set-staging-value-from-direction
         cell-runtime
         (intern (upcase .outputPort))
         (car state))
        (setq state (cdr state)))

      ;; Persist state for next rount
      (setf (gis-200--cell-runtime-run-state cell-runtime) state))))

(defun gis-200--yaml-get-editor-sink (cell-runtime)
  "Return the sink corresponding to CELL-RUNTIME."
  ;; For now there will only be one editor.
  (car (gis-200--problem-spec-sinks gis-200--extra-gameboard-cells)))

(defun gis-200--yaml-step-controller (cell-runtime)
  "Perform runtime step for a CELL-RUNTIME of kind YAML Controller."
  (let ((row (gis-200--cell-runtime-row cell-runtime))
        (col (gis-200--cell-runtime-col cell-runtime))
        (spec (gis-200--cell-runtime-run-spec cell-runtime))
        (state (gis-200--cell-runtime-run-state cell-runtime))
        (sink (gis-200--yaml-get-editor-sink cell-runtime)))
    (let-alist spec
      ;; .inputPort  .setPointPort
      ;; .charAtPort .pointPort
      ;; First consume setPoint port
      (when .setPointPort
        (let* ((port-sym (intern (upcase .setPointPort)))
               (from-cell (gis-200--cell-at-moved-row-col row col port-sym))
               (opposite-port (gis-200--mirror-direction port-sym))
               (recieve-val (gis-200--get-value-from-direction from-cell opposite-port)))
          (when recieve-val
            (gis-200--remove-value-from-direction from-cell opposite-port)
            (gis-200--cell-sink-move-point sink recieve-val))))
      
      ;; Next consume inputPort
      (when .inputPort
        (let* ((port-sym (intern (upcase .inputPort)))
               (from-cell (gis-200--cell-at-moved-row-col row col port-sym))
               (opposite-port (gis-200--mirror-direction port-sym))
               (recieve-val (gis-200--get-value-from-direction from-cell opposite-port)))
          (when recieve-val
            (gis-200--remove-value-from-direction from-cell opposite-port)
            (gis-200--cell-sink-insert-character sink recieve-val))))

      ;; Get the point and atChar and send them to respective ports
      (when .charAtPort
        (let* ((port-sym (intern (upcase .charAtPort)))
               (val (gis-200--get-value-from-direction cell-runtime port-sym))
               (point (gis-200--cell-sink-editor-point sink))
               (text (gis-200--cell-sink-editor-text sink)))
          (when val
            (gis-200--remove-value-from-direction cell-runtime port-sym))
          (gis-200--cell-runtime-set-staging-value-from-direction
           cell-runtime
           (intern (upcase .charAtPort))
           (aref text (1- point)))))
      
      (when .pointPort
        (let* ((port-sym (intern (upcase .pointPort)))
               (val (gis-200--get-value-from-direction cell-runtime port-sym))
               (point (gis-200--cell-sink-editor-point sink)))
          (when val
            (gis-200--remove-value-from-direction cell-runtime port-sym))
          (gis-200--cell-runtime-set-staging-value-from-direction
           cell-runtime
           (intern (upcase .pointPort))
           point))))))

(defun gis-200--yaml-step-heap (cell-runtime)
  "Perform runtime step for CELL-RUNTIME of type YAML HEAP."
  (let* ((row (gis-200--cell-runtime-row cell-runtime))
         (col (gis-200--cell-runtime-col cell-runtime))
         (spec (gis-200--cell-runtime-run-spec cell-runtime))
         (state (gis-200--cell-runtime-run-state cell-runtime))
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
        (when (not (gis-200--get-value-from-direction cell-runtime (intern (upcase .readPort))))
          (setq offset (1+ offset))))
      (when .seekPort
        (let* ((port-sym (intern (upcase .seekPort)))
               (from-cell (gis-200--cell-at-moved-row-col row col port-sym))
               (opposite-port (gis-200--mirror-direction port-sym))
               (recieve-val (gis-200--get-value-from-direction from-cell opposite-port))
               (stage-recieve-val (gis-200--get-value-from-staging-direction from-cell opposite-port)))
          (cond
           (recieve-val
            (gis-200--remove-value-from-direction from-cell opposite-port)
            (setq offset recieve-val))
           (stage-recieve-val
            (gis-200--remove-value-from-staging-direction from-cell opposite-port)
            (setq offset stage-recieve-val)))))
      (when .writePort
        (let* ((port-sym (intern (upcase .writePort)))
               (from-cell (gis-200--cell-at-moved-row-col row col port-sym))
               (opposite-port (gis-200--mirror-direction port-sym))
               (recieve-val (gis-200--get-value-from-direction from-cell opposite-port)))
          (when recieve-val
            (gis-200--remove-value-from-direction from-cell opposite-port)
            (aset data offset recieve-val)
            (setq offset (1+ offset)))))
      (when .setPort
        (let* ((port-sym (intern (upcase .setPort)))
               (from-cell (gis-200--cell-at-moved-row-col row col port-sym))
               (opposite-port (gis-200--mirror-direction port-sym))
               (recieve-val (gis-200--get-value-from-direction from-cell opposite-port)))
          (when recieve-val
            (gis-200--remove-value-from-direction from-cell opposite-port)
            (aset data offset recieve-val))))
      (when .offsetPort
        (let* ((port-sym (intern (upcase .offsetPort)))
               (val (gis-200--get-value-from-direction cell-runtime port-sym)))
          (when val
            (gis-200--remove-value-from-direction cell-runtime port-sym))
          (gis-200--cell-runtime-set-staging-value-from-direction
           cell-runtime
           (intern (upcase .offsetPort))
           offset)))
      (when .readPort
        (let* ((port-sym (intern (upcase .readPort)))
               (val (gis-200--get-value-from-direction cell-runtime port-sym))
               (datum (if (>= offset (length data)) -1 (aref data offset))))
          (when val
            (gis-200--remove-value-from-direction cell-runtime port-sym))
          (gis-200--cell-runtime-set-staging-value-from-direction
           cell-runtime
           (intern (upcase .readPort))
           datum)))
      (when .peekPort
        (when (= -1 offset) (setq offset 0)) ;; another hack related to the way readPort works
        (let* ((port-sym (intern (upcase .peekPort)))
               (val (gis-200--get-value-from-direction cell-runtime port-sym))
               (datum (if (>= offset (length data)) -1 (aref data offset))))
          (when val
            (gis-200--remove-value-from-direction cell-runtime port-sym))
          (gis-200--cell-runtime-set-staging-value-from-direction
           cell-runtime
           (intern (upcase .peekPort))
           datum)))
      (setf (gis-200--cell-runtime-run-state cell-runtime) (cons offset data)))))

(defun gis-200--yaml-create-stack (row col metadata spec)
  "Return a Stack runtime according to SPEC with METADATA at ROW COL."
  ;; .inputPorts .outputPort .sizePort .size .logLevel
  (gis-200--cell-runtime-create
   :instructions nil
   :pc nil
   :row row
   :col col
   :run-function #'gis-200--yaml-step-stack
   :run-spec spec))

(defun gis-200--yaml-create-controller (row col metadata spec)
  "Return a Controller runtime according to SPEC with METADATA at ROW COL."
  (gis-200--cell-runtime-create
   :instructions nil
   :pc nil
   :row row
   :col col
   :run-function #'gis-200--yaml-step-controller
   :run-spec spec))

(defun gis-200--yaml-create-heap (row col metadata spec)
  "Return a Stack runtime according to SPEC with METADATA at ROW COL."
  ;; .size
  (let-alist spec
    (let ((data (make-vector (or .size 20) 0)))
      (cl-loop for elt across .data
               for i from 0
               do (aset data i elt))
      (gis-200--cell-runtime-create
       :instructions nil
       :pc nil
       :row row
       :col col
       :run-function #'gis-200--yaml-step-heap
       ;; -1 because used hack to increment offset which will
       ;; run once at the start of the game.
       :run-state (cons -1 data)
       :run-spec spec))))

(provide 'gis-200)

;;; gis-200.el ends here
