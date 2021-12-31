;;; asm-blox-el.el --- Grided Intelligence System -*- lexical-binding: t -*-

;; Author: Zachary Romero

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

(defconst asm-blox-box-height 12)
(defconst asm-blox-column-ct 4)
(defconst asm-blox-box-width 20)
(defconst asm-blox--gameboard-col-ct 4)
(defconst asm-blox--gameboard-row-ct 3)

(defvar-local asm-blox-box-contents nil)
(defvar asm-blox--gameboard nil)
(defvar-local asm-blox--extra-gameboard-cells nil)
(defvar asm-blox--gameboard-state nil
  "Contains the state of the board whether it be victory or error.")
(defvar asm-blox--parse-depth nil)
(defvar asm-blox--branch-labels nil)

(defvar asm-blox-runtime-error nil
  "If non-nil, contains the runtime error encountered.
The format of the error is (list message row column).")

(defvar-local asm-blox-execution-origin-buffer nil)

(cl-defstruct (asm-blox-code-node
               (:constructor asm-blox--code-node-create)
               (:copier nil))
  children start-pos end-pos)

(defun asm-blox--parse-error-p (err)
  "Return non-nil if ERR is a parse error."
  (and (listp err) (eql 'error (car err))))

(defun asm-blox--parse-cell (coords code)
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
        (asm-blox--parse-assembly code)
      (asm-blox--create-yaml-code-node (car coords) (cadr coords) code))))

(defun asm-blox--parse-assembly (code)
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
                                             (node (asm-blox--code-node-create :children children
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
         (end-pos (asm-blox-code-node-end-pos code-node))
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

(defun asm-blox--parse-tree-to-asm (parse)
  "Generate game bytecode from tree of PARSE, resolving labels."
  (catch 'error
    (let ((asm (flatten-list (asm-blox--parse-tree-to-asm* parse))))
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

(defun asm-blox--set-cell-asm-at-row-col (row col asm)
  "Create a runtime from ASM at set board cell at ROW, COL to it."
  (let* ((asm (if (not (listp asm)) (list asm) asm))
         (runtime (asm-blox--cell-runtime-create
                   :instructions asm
                   :pc 0
                   :stack '()
                   :row row
                   :col col
                   :up nil
                   :down nil
                   :left nil
                   :right nil)))
    (asm-blox--set-cell-at-row-col row col runtime)))

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
  (let* ((row (asm-blox--cell-runtime-row cell-runtime))
         (col (asm-blox--cell-runtime-col cell-runtime))
         (v1 (asm-blox--cell-runtime-pop cell-runtime))
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
  "Perform a variant of the GET command, grabbing the LOC value from CELL-RUNTIME's stack."
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
  (assert (or (eql 'LEFT direction) (eql 'RIGHT direction)))
  (assert (<= 0 row (1- asm-blox--gameboard-row-ct)))
  (assert (<= 0 col asm-blox--gameboard-col-ct))
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
  (assert (or (eql 'UP direction) (eql 'DOWN direction)))
  (assert (<= 0 row asm-blox--gameboard-row-ct))
  (assert (<= 0 col (1- asm-blox--gameboard-col-ct)))
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
  row col data idx name)

(cl-defstruct (asm-blox--cell-sink
               (:constructor asm-blox--cell-sink-create)
               (:copier nil))
  row col expected-data idx name err-val
  default-editor-text
  editor-text editor-point expected-text)

(cl-defstruct (asm-blox--problem-spec
               (:constructor asm-blox--problem-spec-create)
               (:copier nil))
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

(defconst asm-blox-save-directory-name
  (expand-file-name ".asm-blox" user-emacs-directory))

(defun asm-blox--generate-new-puzzle-filename (name)
  "For puzzle NAME, determine the name for a new puzzle."
  ;; TODO: This work when deleting puzzles.
  (ignore-errors
    (make-directory asm-blox-save-directory-name))
  (let* ((dir-files (directory-files asm-blox-save-directory-name))
         (puzzle-files (seq-filter (lambda (file-name)
                                     (string-prefix-p name file-name))
                                   dir-files))
         (new-idx (number-to-string (1+ (length puzzle-files)))))
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

(defun asm-blox--saved-puzzle-ct-by-id (id)
  "Return the number of saved puzzles that start with the puzzle ID."
  (length (seq-filter (lambda (file-name)
                        (string-prefix-p id file-name))
                      (directory-files asm-blox-save-directory-name))))

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
        (insert buffer-contents)
        (save-buffer)
        (kill-buffer)))))

(defun asm-blox--restore-backup ()
  "Create a backup file for the current buffer."
  (let* ((buffer-contents (buffer-string))
         (bfn (buffer-file-name))
         (name (file-name-nondirectory bfn))
         (path (file-name-directory bfn))
         (new-name (concat path "." name ".backup.txt")))
    (when (f-exists? new-name)
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
   (let-alist (yaml-parse-string code :object-type 'alist)
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

(defun asm-blox--yaml-message-stack (cell-runtime)
  (let* ((state (asm-blox--cell-runtime-run-state cell-runtime))
         (spec (asm-blox--cell-runtime-run-spec cell-runtime))
         (size (cdr (assoc 'size spec)))
         (output-port (intern (upcase (cdr (assoc 'outputPort spec)))))
         (val (asm-blox--get-value-from-direction cell-runtime output-port)))
    (when val
      (setq state (cons val state)))
    (if (= 0 (length state))
        "empty stack"
      (format "top:%d size:%d/%d" (car state) (length state) size))))

(defun asm-blox--yaml-step-stack (cell-runtime)
  "Perform the step operation for the CELL-RUNTIME of kind Stack."
  (let ((row (asm-blox--cell-runtime-row cell-runtime))
        (col (asm-blox--cell-runtime-col cell-runtime))
        (spec (asm-blox--cell-runtime-run-spec cell-runtime))
        (state (asm-blox--cell-runtime-run-state cell-runtime)))
    (let-alist spec
      ;; .inputPorts .outputPort .sizePort .size .logLevel
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
       .inputPorts)

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

(defun asm-blox--yaml-get-editor-sink (cell-runtime)
  "Return the sink corresponding to CELL-RUNTIME."
  ;; For now there will only be one editor.
  (car (asm-blox--problem-spec-sinks asm-blox--extra-gameboard-cells)))

(defun asm-blox--yaml-step-controller (cell-runtime)
  "Perform runtime step for a CELL-RUNTIME of kind YAML Controller."
  (let ((row (asm-blox--cell-runtime-row cell-runtime))
        (col (asm-blox--cell-runtime-col cell-runtime))
        (spec (asm-blox--cell-runtime-run-spec cell-runtime))
        (state (asm-blox--cell-runtime-run-state cell-runtime))
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
            (throw 'runtime-error `(error "idx out of bounds" ,row ,col)))
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

(defun asm-blox--yaml-create-stack (row col metadata spec)
  "Return a Stack runtime according to SPEC with METADATA at ROW COL."
  ;; .inputPorts .outputPort .sizePort .size .logLevel
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
  "Throw error for PORT with NAME if not-empty and not a port. "
  (when (and port (or (eql port ':null)
                      (not (asm-blox--portp (upcase port)))))
    (throw 'error `(error 0 ,(format "invalid %s" name)))))

(defun asm-blox--verify-stack (spec)
  (when (eql spec ':null)
    (throw 'error '(error 0 "spec can't be empty")))
  (let ((input-ports (cdr (assoc 'inputPorts spec)))
        (output-port (cdr (assoc 'outputPort spec)))
        (size-port (cdr (assoc 'sizePort spec)))
        (size (cdr (assoc 'size spec))))
    (when (not input-ports)
      (throw 'error `(error 0 "missing inputPorts")))
    (when (eql input-ports ':null)
      (throw 'error `(error 0 "inputPorts is empty")))
    
    (when (not output-port)
      (throw 'error `(error 0 "missing outputPort")))
    (when (eql output-port ':null)
      (throw 'error `(error 0 "outputPort is empty"))) ;; TODO: test this
    
    (when (and size-port (or (eql size-port ':null)
                             (not (asm-blox--portp (upcase size-port)))))
      (throw 'error `(error 0 "invalid sizePort")))

    (when (and size (or (eql write-port ':null)
                        (not (numberp size))))
      (throw 'error '(error 0 "invalid size")))
    (when (and size-port output-port (eql size-port output-port))
      (error 'error `(error 0 "same OUT ports")))))

(defun asm-blox--verify-controller (spec)
  (when (eql spec ':null)
    (throw 'error '(error 0 "spec can't be empty")))
  (let ((input-port (cdr (assoc 'inputPort spec)))
        (set-point-port (cdr (assoc 'setPointPort spec)))
        (char-at-point (cdr (assoc 'charAtPoint spec)))
        (point-port (cdr (assoc 'pointPort spec))))
    
    (asm-blox--verify-port "inputPort" input-port)
    (asm-blox--verify-port "setPointPort" set-point-port)
    (asm-blox--verify-port "charAtPoint" char-at-point)
    (asm-blox--verify-port "pointPort" point-port)

    (when (and input-port set-point-port (eql input-port set-point-port))
      (throw 'error `(error 0 "same IN ports")))
    (when (and char-at-port point-port (eql char-at-port point-port))
      (throw 'error `(error 0 "same OUT ports")))))

(defun asm-blox--yaml-create-controller (row col metadata spec)
  "Return a Controller runtime according to SPEC with METADATA at ROW COL."
  (asm-blox--cell-runtime-create
   :instructions nil
   :pc nil
   :row row
   :col col
   :run-function #'asm-blox--yaml-step-controller
   :run-spec spec))

(defun asm-blox--verify-heap (spec)
  (when (eql spec ':null)
    (throw 'error '(error 0 "spec can't be empty")))
  (dolist (prop '(writePort seekPort setPort offsetPort peekPort readPort))
    (let ((port (cdr (assoc prop spec))))
      (asm-blox--verify-port prop port)))
  (dolist (props '(("IN" writePort seekPort setPort) ("OUT" offsetPort peekPort readPort)))
    (let ((type (car props))
          (ports (seq-map
                  (lambda (n) (intern n))
                  (seq-filter
                   #'stringp
                   (seq-map (lambda (prop-name) (cdr (assoc prop-name spec))) (cdr props))))))
      (while ports
        (let ((top (car ports))
              (rest (cdr ports)))
          (when (and top (memq top rest))
            (throw 'error `(error 0 ,(format "same port: %s" (symbol-name top))))))
        (setq ports (cdr ports))))))

(defun asm-blox--yaml-create-heap (row col metadata spec)
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

(provide 'asm-blox-exec)

;;; asm-blox.el ends here
