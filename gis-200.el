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


;;; structure definitions and helpers

(cl-defstruct (gis-200-code-cell
               (:constructor gis-200--cell-create)
               (:copier nil))
  coord code pc acc bak up right down left)

(defun gis-200-code-cell-get-port (cell port)
  "Return the item in PORT of CELL."
  (unless (gis-200-port-p port)
    (error "Invalid port %s" port))
  (pcase port
    ('up (gis-200-code-cell-up cell))
    ('right (gis-200-code-cell-right cell))
    ('down (gis-200-code-cell-down cell))
    ('left (gis-200-code-cell-left cell))))

(defun gis-200--code-cell-init (code x y)
  "Return an instantiated code-cell with CODE, at coords X, Y."
  (gis-200--cell-create
   :coord (list x y)
   :code (gis-200--parse-code code)
   :pc 0 :acc 0 :bak 0 :up nil :right nil :down nil :left nil))

(defun gis-200--code-cell-update (cell location val)
  "Return a new code cell from CELL with LOCATION updated with VAL."
  (let ((coord (gis-200-code-cell-coord cell))
        (code (gis-200-code-cell-code cell))
        (pc (gis-200-code-cell-pc cell))
        (acc (gis-200-code-cell-acc cell))
        (bak (gis-200-code-cell-bak cell))
        (up (gis-200-code-cell-up cell))
        (right (gis-200-code-cell-right cell))
        (down (gis-200-code-cell-down cell))
        (left (gis-200-code-cell-left cell)))
    (pcase location
      ('pc (setq pc val))
      ('acc (setq acc val))
      ('up (setq up val))
      ('right (setq right val))
      ('down (setq down val))
      ('left (setq left val)))
    (gis-200--cell-create
     :coord coord
     :code code
     :pc pc :acc acc :bak bak :up up :right right :down down :left left)))

(defun gis-200--code-cell-pc-inc (cell)
  (let* ((program (gis-200-code-cell-code cell))
         (old-pc (gis-200-code-cell-pc cell))
         (line-ct (length program))
         (new-pc (mod (1+ old-pc) line-ct)))
    (gis-200--code-cell-update cell 'pc new-pc)))

;;; code parsing

(defun gis-200--parse-code (code)
  "Return parsed structure of CODE."
  (read (concat "(" code ")")))

;;; port/coordinates logic

(defun gis-200--port-inverse (port)
  "Return the opposite direction of PORT."
  (unless (gis-200-port-p port)
    (error "Invalid port %s" port))
  (pcase port
    ('up 'down)
    ('down 'up)
    ('right 'left)
    ('left 'right)))

(defun gis-200-port-p (sym)
  "Return non-nil of SYM is up, right, down, or left."
  (pcase sym
    ('up t)
    ('right t)
    ('down t)
    ('left t)
    (_ nil)))

(defun gis-200--coord-move (at dir)
  "Return the coordinate by going in direction DIR from AT."
  (let ((x (car at))
        (y (cadr at)))
    (pcase dir
      ('up (list x (1- y)))
      ('down (list x (1+ y)))
      ('right (list (1+ x) y))
      ('left (list (1- x) y))
      (_ (error "Invalid direction %s" dir)))))

;;; Game board

(defvar gis-200--gameboard nil
  "The current state of the current played level.

The board should be a two dimentional grid of `gis-200--code-cell' items.")

(defun gis-200--fetch-cell (coord)
  "Return board-cell of COORD."
  ;; TODO: write me
  (let ((x (car coord))
        (y (cadr coord)))
    (nth x (nth y gis-200--gameboard))))

(defun gis-200--init-board ()
  "Helper function to initialize the board to a test state."
  (setq gis-200--gameboard (gis-200--parse-game-board)))

(defun gis-200--cell-free-p (cell)
  "Return non-nil if CELL is blocked on a send."
  (not (or (gis-200-code-cell-up cell)
           (gis-200-code-cell-down cell)
           (gis-200-code-cell-left cell)
           (gis-200-code-cell-right cell))))

(defun gis-200--unblocked-board-positions ()
  "Return a sequence of all board position."
  (seq-map #'gis-200-code-cell-coord
           (seq-filter #'gis-200--cell-free-p (seq-reduce #'append gis-200--gameboard '()))))

(defun gis-200--update-new-cells (cells)
  "Replace the gameboard cells with CELLS."
  (let ((new-cells cells))
    (while new-cells
      (let* ((new-cell    (car new-cells))
             (cell-coords (gis-200-code-cell-coord new-cell)))
        (setq gis-200--gameboard
         (seq-map
          (lambda (row)
            (seq-map (lambda (cell)
                       (if (equal (gis-200-code-cell-coord cell) cell-coords)
                           new-cell
                         cell))
                     row))
          gis-200--gameboard)))
      (setq new-cells (cdr new-cells)))))

;;; Execution Logic

(defun gis-200--recv (cell from-port)
  "Attempt to receive the item on the FROM-PORT from CELL.

If result is non-nil, the item will have been removed from the
port."
  (let* ((at-coord (gis-200-code-cell-coord cell))
         (recv-coord (gis-200--coord-move at-coord from-port))
         (recv-cell (gis-200--fetch-cell recv-coord))
         (from-port-inv (gis-200--port-inverse from-port))
         (port-val (gis-200-code-cell-get-port recv-cell from-port-inv)))
    (if (not port-val)
        ;; If port-val is nil do nothing
        nil
      ;; else consume the value
      (gis-200--update-new-cells (list (gis-200--code-cell-update recv-cell from-port-inv nil)))
      ;; TODO: maybe find a better way to do this
      port-val)))

(defun gis-200--move (cell cmd)
  "Perform the move CMD on CELL."
  (let* ((from (nth 1 cmd))
         (to (nth 2 cmd))
         (from-res (cond
                    ((numberp from) from)
                    ((eql 'acc from) (gis-200-code-cell-acc cell))
                    ((eql 'nil from) 0)
                    ((eql 'any from) (error "Any not implemented"))
                    ((gis-200-port-p from) (gis-200--recv cell from)))))
    (if (not from-res)
        nil ;; no value for from, block
      (gis-200--code-cell-update cell to from-res))))

(defun gis-200--cell-step (cell)
  "Run one step of the program in CELL, returning a new cell."
  (let* ((at-cmd (nth (gis-200-code-cell-pc cell) (gis-200-code-cell-code cell)))
         (res (pcase (car at-cmd) ('mov (gis-200--move cell at-cmd)))))
    (when res
      (setq res (gis-200--code-cell-pc-inc res)))
    res))

(defun gis-200--board-step ()
  "Run one step of the program for each cell on the current board."
  (let ((unblocked-cells-coords (gis-200--unblocked-board-positions))
        new-cells)
    (while unblocked-cells-coords
      (let* ((next-coord (car unblocked-cells-coords))
             (next-cell (gis-200--fetch-cell next-coord))
             (next-cell-step (gis-200--cell-step next-cell)))
        (when next-cell-step
          (setq new-cells (cons next-cell-step new-cells))))
      (setq unblocked-cells-coords (cdr unblocked-cells-coords)))
    (gis-200--update-new-cells new-cells)))

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

(defun gis-200--parse-assembly (code)
  "Parse ASM CODE returning a list of instructions."
  (with-temp-buffer
    (insert code)
    (goto-char (point-min))
    (let* ((top-body '()))
      (cl-labels ((whitespace-p (c)
                                (or (eql c ?\s)
                                    (eql c ?\t)
                                    (eql c ?\n)))
                  (end-of-buffer-p ()
                                   (eql (point) (point-max)))
                  (current-char ()
                                (char-after (point)))
                  (consume-space ()
                                 (while (and (whitespace-p (current-char))
                                             (not (end-of-buffer-p)))
                                   (forward-char)))
                  (symbol-char-p (c)
                                 (or (<= ?a c ?z)
                                     (<= ?A c ?Z)))
                  (digit-char-p (c) (<= ?0 c ?9))
                  (parse-element ()
                                 (let ((elements '()))
                                   (catch 'end
                                     (while t
                                       (consume-space)
                                       (if (end-of-buffer-p)
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
                                             (forward-char 1)
                                             (throw 'end nil))
                                            ;; Symbol
                                            ((symbol-char-p at-char)
                                             (let ((start (point)))
                                               (forward-char 1)
                                               (while (and (symbol-char-p (current-char))
                                                           (not (end-of-buffer-p)))
                                                 (forward-char 1))
                                               (let ((symbol (intern (buffer-substring-no-properties start (point)))))
                                                 (push symbol elements))))

                                            ;; digit
                                            ((digit-char-p at-char)
                                             (let ((start (point)))
                                               (forward-char 1)
                                               (while (and (digit-char-p (current-char))
                                                           (not (end-of-buffer-p)))
                                                 (forward-char 1))
                                               (let ((number (string-to-number (buffer-substring-no-properties start (point)))))
                                                 (push number elements)))))))))
                                   (reverse elements))))
        (parse-element)))))

(defconst gis-200-base-operations
  '(GET SET TEE CONST NULL IS_NULL DROP
        NOP ADD SUB MUL DIV REM AND OR EQZ
        EQ NE LT GT GE LE SEND PUSH))

(defvar gis-200--parse-depth nil)
(defvar gis-200--branch-labels nil)

(defun gis-200--make-label ()
  (intern (concat "L_" (number-to-string (random 100000)) "_" (number-to-string gis-200--parse-depth))))

;; TODO: flatten result of this
(defun gis-200--parse-tree-to-asm (parse)
  "Convert PARSE into a list of ASM instructions."
  (let ((gis-200--parse-depth (if gis-200--parse-depth
                                  (1+ gis-200--parse-depth)
                                0)))
    (cond
     ((listp parse)
      (let ((asm-stmts (mapcar #'gis-200--parse-tree-to-asm parse)))
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
                 (rest-asm-stmts (mapcar #'gis-200--parse-tree-to-asm rest-children)))
            (append rest-asm-stmts
                    (list (gis-200--code-node-create
                           :children (list 'LABEL label-symbol)
                           :start-pos nil
                           :end-pos nil)))))
         
         ((eql first-child 'LOOP)
          (let* ((label-symbol (gis-200--make-label))
                 (gis-200--branch-labels (cons (cons gis-200--parse-depth label-symbol) gis-200--branch-labels))
                 (rest-asm-stmts (mapcar #'gis-200--parse-tree-to-asm rest-children)))
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
                    (seq-map #'gis-200--parse-tree-to-asm (cdr (gis-200-code-node-children then-case)))
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
                    (seq-map #'gis-200--parse-tree-to-asm (cdr (gis-200-code-node-children else-case)))
                  nil)
              ,(gis-200--code-node-create
               :children (list 'LABEL end-label)
               :start-pos nil
               :end-pos nil))))))))))

(defun gis-200--pprint-asm (instructions)
  (message "%s" "======INSTRUCTIONS=======")
  (let ((i 1))
    (dolist (instr instructions nil)
      (let ((instr (gis-200-code-node-children instr)))
        (message "%d %s" i instr)
        (setq i (1+ i)))))
  (message "%s" "======INSTRUCTIONS_END=======")
  nil)


;;;

(defconst gis-200--gameboard-col-ct 4)
(defconst gis-200--gameboard-row-ct 3)

(defvar gis-200--gameboard nil)

(defun gis-200--cell-at-row-col (row col)
  (aref gis-200--gameboard
        (+ (* row gis-200--gameboard-row-ct)
           col)))

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

(cl-defstruct (gis-200--cell-runtime
               (:constructor gis-200--cell-runtime-create)
               (:copier nil))
  instructions pc stack row col up down left right)

(defun gis-200--get-value-from-direction (cell-runtime direction)
  "Dynamically look up and return value at DIRECTION on CELL-RUNTIME."
  (pcase direction
    ('UP (gis-200--cell-runtime-up cell-runtime))
    ('RIGHT (gis-200--cell-runtime-right cell-runtime))
    ('DOWN (gis-200--cell-runtime-down cell-runtime))
    ('LEFT (gis-200--cell-runtime-left cell-runtime))))

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
  (let ((v (gis-200--cell-runtime-pop cell-runtime))
        (current-val)
        (set-fn))
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
        ('DOWN (setf (gis-200--cell-runtime-up cell-runtime) v))
        ('LEFT (setf (gis-200--cell-runtime-up cell-runtime) v))
        ('RIGHT (setf (gis-200--cell-runtime-up cell-runtime) v))))))

(defun gis-200--cell-runtime-get (cell-runtime direction)
  "Perform the GET command running from CELL-RUNTIME, recieving from DIRECTION."
  (let* ((at-row (gis-200--cell-runtime-row cell-runtime))
         (at-col (gis-200--cell-runtime-col cell-runtime))
         (opposite-direction (gis-200--mirror-direction direction))
         (from-cell (gis-200--cell-at-moved-row-col at-row at-col direction))
         (recieve-val (gis-200--get-value-from-direction from-cell opposite-direction)))
    (if recieve-val
        (progn
          (gis-200--cell-runtime-push cell-runtime recieve-val)
          (gis-200--remove-value-from-direction from-cell opposite-direction))
      'blocked)))

(defun gis-200--true-p (v)
  "Return non-nil if V is truthy."
  (not (= 0 v)))

(defun gis-200--cell-runtime-step (cell-runtime)
  "Perform one step of CELL-RUNTIME."
  (let* ((pc (gis-200--cell-runtime-pc cell-runtime))
         (instrs (gis-200--cell-runtime-instructions cell-runtime))
         (_ (and (>= pc (length instrs)) (error "End of program error"))) ;; TODO: move pc to beg?
         (current-instr (car (nthcdr pc instrs)))
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
      (_ (setf (gis-200--cell-runtime-pc cell-runtime) (1+ pc))))))

(defmacro comment (&rest x) nil)

(comment
 (let* ((parsed (gis-200--parse-assembly "(GET RIGHT)"))
        (asm (gis-200--parse-tree-to-asm parsed))
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
                    :instructions '()
                    :pc 0
                    :stack nil
                    :row 0
                    :col 0
                    :up nil
                    :down nil
                    :left 77
                    :right nil))
        (gis-200--gameboard (vector runtime runtime-2)))
   (gis-200--cell-runtime-step runtime)
   runtime-2
   )
 )

;;; 

(defun gis-200-mode ()
  "Major mode for playing gis-200 game."
  (interactive)
  (kill-all-local-variables)
  ;;(use-local-map )
  (setq mode-name "gis-200"
        buffer-read-only t
        truncate-lines t)
  (buffer-disable-undo)
  (gis-200--display))

(defun gis-200 ()
  "Open the game buffer."
  (interactive)
  (switch-to-buffer "*gis-200*")
  (if (equal mode-name "gis-200")
      nil ;; TODO: refresh the buffer
    (gis-200-mode)))

(provide 'gis-200)

;;; gis-200.el ends here
