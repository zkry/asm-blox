;;; asm-blox-display.el --- Display code for asm-blox Game -*- lexical-binding: t -*-

;; Author: Zachary Romero
;; Maintainer: Zachary Romero

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

;; This packages contains display code for asm-blox game.

;; commentary

;;; Code:

(require 'asm-blox)

(defface asm-blox-error-face
  '((((class color) (background light)) (:foreground "DarkGoldenrod4"))
    (((class color) (background dark))  (:foreground "DarkGoldenrod1")))
  "?"
  :group 'asm-blox)

(defface asm-blox-show-paren-match-face
  '((t (:inherit show-paren-match)))
  "`asm-blox-mode' face used for a matching paren pair."
  :group 'asm-blox)

(defface asm-blox-region-face
  '((t (:inherit region)))
  "`asm-blox-mode' face used for a matching paren pair."
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
      (car asm-blox-runtime-error)
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
  (let ((width 32)
        (sink-widget-offset-ct 2))
    (lambda (msg)
      (pcase msg
        ('width width)
        (`(display ,n)
         (let* ((box-top (concat "┌" (make-string 30 ?─) "┐"))
                (box-bottom (concat "└" (make-string 30 ?─) "┘"))
                (spacing (make-string 32 ?\s))
                (data (asm-blox--cell-sink-expected-data sink))
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
    (dotimes (col asm-blox-column-ct)
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

(defun asm-blox-display-game-board ()
  "Insert the characters of the board to the buffer."
  (setq asm-blox--widget-row-idx 0)
  (when (not (hash-table-p asm-blox--end-of-box-points))
    (setq asm-blox--end-of-box-points (make-hash-table :test 'equal)))
  (when (not (hash-table-p asm-blox--beginning-of-box-points))
    (setq asm-blox--beginning-of-box-points (make-hash-table :test 'equal)))
  (let* ((display-mode 'edit) ;; TODO - finalize where this comes from. can be 'edit or 'execute
         (arrow-up "↑")
         (arrow-down "↓")
         (arrow-right "→")
         (arrow-left "←")
         (box-horizontal ?─)
         (box-vertical ?│)
         (box-top-left ?┌)
         (box-top-right ?┐)
         (box-bottom-right ?┘)
         (box-bottom-left ?└)
         (space-start (make-string 6 ?\s))
         (space-between (make-string 5 ?\s))
         (box-line-top-bottom (make-string asm-blox-box-width box-horizontal))
         (box-inside (make-string asm-blox-box-width ?\s)))
    (let ((insert-row-top
           (lambda (row)
             "Draw the  ┌───┐┌───┐┌───┐┌───┐ part of the board."
             (insert space-start)
             (insert space-between)
             (dotimes (col asm-blox-column-ct)
               (let ((err (asm-blox--get-error-at-cell row col)))
                 (if err
                     (progn
                       (insert (propertize (char-to-string box-top-left)
                                           'font-lock-face
                                           '(:foreground "red")))
                       (insert (propertize box-line-top-bottom
                                           'font-lock-face
                                           '(:foreground "red")))
                       (insert (propertize (char-to-string box-top-right)
                                           'font-lock-face
                                           '(:foreground "red"))))
                   (insert box-top-left)
                   (insert box-line-top-bottom)
                   (insert box-top-right))
                 (insert space-between)))
             (when (eql asm-blox--display-mode 'execute)
               (asm-blox--display-widget))
             (insert "\n")))
          (insert-row-bottom
           (lambda (row)
             "Draw the  └───┘└───┘└───┘└───┘ part of the board. "
             (insert space-start)
             (insert space-between)
             (dotimes (col asm-blox-column-ct)
               (let ((err (asm-blox--get-error-at-cell row col)))
                 (if err
                     (progn (insert (propertize (char-to-string box-bottom-left)
                                                'font-lock-face
                                                '(:foreground "red")))
                            (insert (propertize box-line-top-bottom
                                                'font-lock-face
                                                '(:foreground "red")))
                            (insert (propertize (char-to-string box-bottom-right)
                                                'font-lock-face
                                                '(:foreground "red"))))
                   (insert box-bottom-left)
                   (insert box-line-top-bottom)
                   (insert box-bottom-right))
                 (insert space-between)))
             (when (eql asm-blox--display-mode 'execute)
               (asm-blox--display-widget))
             (insert "\n")))
          (insert-v-border
           (lambda (position)
             "Draw the very top and very bottom lines."
             (let* ((left-of-arrows-len
                     (1+ (- (/ (length box-line-top-bottom) 2) 1)))
                    (right-of-arrows-len
                     (1+ (- (length box-line-top-bottom) 2 left-of-arrows-len))))
               (insert space-start)
               (insert space-between)
               (dotimes (col asm-blox-column-ct)
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
                 (insert space-between)))
             (when (eql asm-blox--display-mode 'execute)
               (asm-blox--display-widget))
             (insert "\n")))
          (insert-row-middle
           (lambda (row box-row)
             "Draw the ⇋|   |⇋|   |⇋|   |⇋|   |⇋ part of the board."
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
             (dotimes (col asm-blox-column-ct)
               (let ((err (asm-blox--get-error-at-cell row col)))
                 (if err
                     (insert (propertize (char-to-string box-vertical)
                                         'font-lock-face
                                         '(:foreground "red")))
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
                   (if (and (= 11 box-row) err (eql asm-blox--display-mode 'edit))
                       (let ((err-text (nth 2 (asm-blox--get-error-at-cell row col))))
                         (insert err-text)
                         (insert (make-string (- (length box-inside)
                                                 (length err-text))
                                              ?\s)))
                     (insert (propertize text 'asm-blox-box-id (list row col box-row)))
                     (insert (propertize spacing
                                         'asm-blox-box-id (list row col box-row)
                                         'asm-blox-text-type 'spacing))))
                 (when (= box-row (1- asm-blox-box-height))
                   (puthash (list row col) (point) asm-blox--end-of-box-points))
                 (let ((pipe-str (cond
                                  ((= box-row (1- asm-blox-box-height))
                                   (if err
                                       (propertize (char-to-string box-vertical)
                                                   'asm-blox-text-type
                                                   `(box-end ,row ,col)
                                                   'font-lock-face
                                                   '(:foreground "red"))
                                     (propertize (char-to-string box-vertical)
                                                 'asm-blox-text-type
                                                 `(box-end ,row ,col))))
                                  (err
                                   (propertize (char-to-string box-vertical)
                                               'font-lock-face
                                               '(:foreground "red")))
                                  (t
                                   (char-to-string box-vertical)))))
                   (insert pipe-str)))
               (when (< col (1- asm-blox-column-ct))
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
                                                             asm-blox-column-ct
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
                                                             asm-blox-column-ct
                                                             'LEFT)))
                 (insert display)))
              (t
               (insert space-between)))
             (when (eql asm-blox--display-mode 'execute)
               (asm-blox--display-widget))
             (insert "\n")))
          (insert-middle-row-space
           (lambda (row)
             "Draw the  ↑↓    ↑↓    ↑↓    ↑↓ part of the board."
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
               (dotimes (col asm-blox-column-ct)
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
               (insert "\n")))))
      (funcall insert-v-border 'top)
      (funcall insert-middle-row-space 0)
      (dotimes (row 3)
        (funcall insert-row-top row)
        (dotimes (box-row asm-blox-box-height)
          (funcall insert-row-middle row box-row))
        (funcall insert-row-bottom row)
        (when (not (= 2 row))
          (funcall insert-middle-row-space (1+ row))))
      (funcall insert-middle-row-space 3)
      (funcall insert-v-border 'bottom)

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
      (asm-blox--propertize-errors))))

(defun asm-blox--draw-win-message ()
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

(defvar asm-blox--beginning-of-box-points nil
  "Contains a hashmap of the points where each box begins.
This was added for performance reasons.")

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
         (line (nth 2 box-id))
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
        (next-line 1)
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
    (with-current-buffer (get-buffer-create asm-blox--mirror-buffer-name) ;; TODO: use temp buffer?
      (erase-buffer)
      (insert text)
      (goto-char (point-min))
      (goto-line (1+ line))
      (move-to-column line-col)
      (funcall func)
      (if (> (length (buffer-substring-no-properties
                      (line-beginning-position)
                      (line-end-position)))
             19) ;; TODO: 19 is magic number
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

(defun asm-blox--move-beginning-of-line ()
  "Move the point to the beginning of the line."
  (interactive)
  (if (asm-blox-in-box-p)
      (asm-blox--in-buffer
       (move-beginning-of-line 1))
    (beginning-of-line)))

(defun asm-blox--move-end-of-line ()
  "Move the point to the end of the line."
  (interactive)
  (if (asm-blox-in-box-p)
      (asm-blox--in-buffer
       (move-end-of-line 1))
    (end-of-line)))

(defun asm-blox--beginning-of-buffer ()
  "Move the point to the beginning of the buffer."
  (interactive)
  (if (asm-blox-in-box-p)
      (asm-blox--in-buffer
       (beginning-of-buffer))
    (beginning-of-buffer)))

(defun asm-blox--end-of-buffer ()
  "Move the point to the end of the buffer."
  (interactive)
  (if (asm-blox-in-box-p)
      (asm-blox--in-buffer
       (end-of-buffer))
    (end-of-buffer)))

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
           (line-no 0)
           (lines (split-string text "\n")))
      (cl-loop for i from 0
               for line in lines
               do (cond
                   ((= line-1 i line-2)
                    (let ((line-part
                           (concat (substring line 0 (min (length line) line-col-1))
                                   (substring line (min (length line) line-col-2))))
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

(defun asm-blox--kill-region (beg end)
  "Kill the region from BEG to END."
  (interactive "r")
  (asm-blox--kill beg end))

(defun asm-blox--copy-region (beg end)
  "Copy the region from BEG to END."
  (interactive "r")
  (asm-blox--kill beg end t))

(defun asm-blox--yank ()
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

(defun asm-blox--next-cell ()
  "Move the point to the end of the next box."
  (interactive)
  (if (asm-blox-in-box-p)
      (let* ((box-id (get-text-property (point) 'asm-blox-box-id))
             (row (car box-id))
             (col (cadr box-id))
             (next-col (if (= col (1- asm-blox-column-ct)) 0 (1+ col)))
             (next-row (if (= col (1- asm-blox-column-ct)) (1+ row) row))
             (next-row (if (= next-row asm-blox--gameboard-row-ct) 0 next-row)))
        (asm-blox--move-to-box next-row next-col)
        (asm-blox--move-point-to-end-of-box-content))
    (while (and (not (asm-blox-in-box-p))
                (not (bobp)))
      (forward-char -1))
    (when (bobp)
      (asm-blox--move-to-box (1- asm-blox--gameboard-row-ct) (1- asm-blox--gameboard-col-ct)))
    (asm-blox--next-cell)))

;; TODO: DRY this and next-cell up.
(defun asm-blox--prev-cell ()
  "Move the point to the end of the previous box."
  (interactive)
  (if (asm-blox-in-box-p)
      (let* ((box-id (get-text-property (point) 'asm-blox-box-id))
             (row (car box-id))
             (col (cadr box-id))
             (next-col (if (= col 0) (1- asm-blox-column-ct) (1- col)))
             (next-row (if (= col 0) (1- row) row))
             (next-row (if (= next-row -1) (1- asm-blox--gameboard-row-ct) next-row)))
        (asm-blox--move-to-box next-row next-col)
        (asm-blox--move-point-to-end-of-box-content))
    (while (and (not (asm-blox-in-box-p))
                (not (eobp)))
      (forward-char 1))
    (when (eobp)
      (asm-blox--move-to-box 0 0))
    (asm-blox--prev-cell)))

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
      (define-key map (kbd "C-c C-g") #'asm-blox--refresh-contents)
      (define-key map (kbd "C-c C-c") #'asm-blox-start-execution)
      (define-key map (kbd "M-d") #'asm-blox-kill-word)
      (define-key map (kbd "C-k") #'asm-blox-kill-line)
      (define-key map (kbd "C-a") #'asm-blox--move-beginning-of-line)
      (define-key map (kbd "C-d") #'asm-blox-delete-char)
      (define-key map (kbd "C-e") #'asm-blox--move-end-of-line)
      (define-key map (kbd "M-<") #'asm-blox--beginning-of-buffer)
      (define-key map (kbd "M->") #'asm-blox--end-of-buffer)
      (define-key map (kbd "<tab>") #'asm-blox--next-cell)
      (define-key map (kbd "<backtab>") #'asm-blox--prev-cell)
      (define-key map (kbd "<S-return>") #'asm-blox--next-row-cell)
      (define-key map (kbd "s-z") #'asm-blox--undo)
      (define-key map (kbd "s-y") #'asm-blox--redo)
      (define-key map (kbd "<s-up>") #'asm-blox-shift-box-up)
      (define-key map (kbd "<s-down>") #'asm-blox-shift-box-down)
      (define-key map (kbd "<s-left>") #'asm-blox-shift-box-left)
      (define-key map (kbd "<s-right>") #'asm-blox-shift-box-right)
      (define-key map [remap undo] #'asm-blox--undo)
      (define-key map (kbd "C-w") #'asm-blox-kill-region)
      (define-key map (kbd "M-w") #'asm-blox--copy-region)
      (define-key map (kbd "C-y") #'asm-blox--yank))))

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
  (asm-blox--draw-win-message))

(defvar asm-blox-multi-step-ct 10)

(defun asm-blox--execution-next-multiple-commands ()
  "Perform a multiple steps of execution according to `asm-blox-multi-step-ct'."
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
  (asm-blox--draw-win-message))

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
  "Syntax table for asm-blox mode.")

(defconst asm-blox-mode-highlights
  '(("\\<setq\\>" . 'font-lock-function-name-face)
    ("[0-9]+" . (1 'font-lock-constant-face))))

(defun asm-blox--create-execution-buffer ()
  "Create a new uneditable gamebuffer for displaing execution of puzzles."
  (let ((buffer (get-buffer-create "*asm-blox-execution*"))
        (origin-file-buffer (current-buffer)))
    (with-current-buffer buffer
      (asm-blox-execution-mode)
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
                                   'font-lock-face '(:background "#555"))
                (let ((at-col (current-column)))
                  (forward-line 1)
                  (move-to-column at-col)
                  (while (not (looking-back "│"))
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
              (delete-forward-char 4)
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
  (asm-blox--parse-saved-buffer)
  (setq asm-blox-parse-errors nil)
  (let ((parse-errors)
        (parses))
    (maphash
     (lambda (coords code-text)
       (let ((parse-result (asm-blox--parse-cell coords code-text)))
         (cond
          ((asm-blox--parse-error-p parse-result)
           (setq parse-errors (cons (cons coords parse-result) parse-errors)))
          ((asm-blox--cell-runtime-p parse-result)
           (asm-blox--set-cell-at-row-col (car coords) (cadr coords) parse-result))
          (t (let ((asm (asm-blox--parse-tree-to-asm parse-result)))
               (if (asm-blox--parse-error-p asm)
                   (setq parse-errors (cons (cons coords asm) parse-errors)))
               (setq parses (cons (cons coords asm) parses)))))))
     asm-blox-box-contents)
    (if parse-errors
        (progn
          (setq asm-blox-parse-errors parse-errors)
          (let ((inhibit-read-only t))
            (asm-blox-redraw-game-board)))
      (setq asm-blox-parse-errors nil)
      (let ((inhibit-read-only t))
        (asm-blox-redraw-game-board))
      (dolist (parse parses)
        (let* ((coords (car parse))
               (row (car coords))
               (col (cadr coords))
               (asm (cdr parse)))
          (assert (numberp col))
          (asm-blox--set-cell-asm-at-row-col row col asm)))
      (asm-blox--backup-file-for-current-buffer)
      (asm-blox--reset-extra-gameboard-cells-state)
      (asm-blox--create-widges-from-gameboard)
      (asm-blox--create-execution-buffer))))

(defun asm-blox-execution-mode ()
  "Activate asm-blox execution mod."
  (kill-all-local-variables)
  (use-local-map asm-blox-execution-mode-map)
  (setq mode-name "asm-blox-execution"
        buffer-read-only t)
  (setq-local truncate-lines 0
              asm-blox--display-mode 'execute)
  (setq font-lock-defaults asm-blox-mode-highlights)
  (setq header-line-format "ASM-BLOX EXECUTION")
  (setq asm-blox-runtime-error nil)
  (setq asm-blox--gameboard-state nil)
  (set-syntax-table asm-blox-mode-syntax-table))

(defvar asm-blox--skip-initial-parsing nil
  "When non-nil, don't parse the initial gameboard.")

(defvar asm-blox--show-pair-idle-timer nil
  "Idle-timer for showing matching parenthesis.")

(defun asm-blox-mode ()
  "Activate asm-blox editing mode."
  (interactive)
  (kill-all-local-variables)
  (use-local-map asm-blox-mode-map)
  (setq mode-name "asm-blox"
        buffer-read-only t)
  (setq asm-blox-parse-errors nil)
  (setq-local truncate-lines 0)
  (setq font-lock-defaults asm-blox-mode-highlights)
  (set-syntax-table asm-blox-mode-syntax-table)
  (unless asm-blox--skip-initial-parsing
    (asm-blox--parse-saved-buffer)
    (let ((inhibit-read-only t))
      (asm-blox-redraw-game-board)))
  (unless asm-blox--show-pair-idle-timer
    (setq asm-blox--show-pair-idle-timer
          (run-with-idle-timer 0.125 t 'asm-blox--highlight-pairs))))

;;;###autoload
(add-to-list 'auto-mode-alist (cons "\\.gis\\'" 'asm-blox-mode))

;;; Undo ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar asm-blox--undo-stacks nil
  "Hashmap of stacks containing undo history of each buffer.")

(cl-defstruct (asm-blox--undo-state
               (:constructor asm-blox--undo-state-create)
               (:copier nil))
  "Struct representing a undo-state."
  text box-row box-col redo-list)

(defun asm-blox--initialize-undo-stacks ()
  "Initialize all undo-stacks to be empty."
  (setq asm-blox--undo-stacks (make-hash-table :test 'equal))
  (dotimes (row 3)
    (dotimes (col asm-blox-column-ct)
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

(defun asm-blox--undo ()
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

(defun asm-blox--redo ()
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
              (let ((inhibit-read-only t)) ;; code-smell: always inhibiting read only
                (asm-blox-redraw-game-board))
              (asm-blox--move-to-box-point box-row box-col))))))))


;;; Parenthesis match code ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar asm-blox-pair-overlays nil "List of overlays used to highlight parenthesis pairs.")

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
               ((looking-back ")")
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
                      (asm-blox--pair-delete-overlays))))) ;; TODO: should display red match instead
               (asm-blox-pair-overlays
                (asm-blox--pair-delete-overlays))))))
      (when asm-blox-pair-overlays
        (asm-blox--pair-delete-overlays)))))

;;; Puzzle Selection

(defun asm-blox--puzzle-selection-setup-buffer (id)
  "Setup the puzzle buffer for the puzzle at ID."
  (let ((puzzle (asm-blox--get-puzzle-by-id id)))
    (unless puzzle
      (error "No puzzle found with id %s" id))
    (let ((buffer (get-buffer-create "*asm-blox*"))  ;; TODO: allow for 1+ puzzles at once
          (file-name (asm-blox--generate-new-puzzle-filename id)))
      (asm-blox--initialize-box-contents)
      (setq asm-blox--extra-gameboard-cells (funcall puzzle))
      (switch-to-buffer buffer)
      (let ((inhibit-read-only t)
            (asm-blox--skip-initial-parsing t))
        (set-visited-file-name file-name)
        (asm-blox-redraw-game-board)
        (asm-blox-mode)
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
  "Mode map for selecting a asm-blox puzzle.")

(defun asm-blox--font-for-difficulty (difficulty)
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
          (let ((saved-file-ct (asm-blox--saved-puzzle-ct-by-id name)))
            (dotimes (i saved-file-ct)
              (insert
               (propertize (format "[%d]" (1+ i))
                           'asm-blox-puzzle-selection-id name
                           'asm-blox-puzzle-selection-filename
                           (asm-blox--make-puzzle-idx-file-name name (1+ i))))
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
  "Activate mode for selecting asm-blox puzzles."
  (interactive)
  (kill-all-local-variables)
  (use-local-map asm-blox-puzzle-selection-mode-map)
  (setq mode-name "asm-blox-puzzle-selection"
        buffer-read-only t)
  (setq header-line-format
        (format "     %-8s %-25s %-60s   %s" "STRAIN" "PUZZLE NAME" "DESCRIPTION" "SAVED FILES"))
  (setq-local truncate-lines 0)
  (hl-line-mode t))

(defun asm-blox ()
  "Open asm-blox puzzle selection screen."
  (interactive)
  (let ((buffer (get-buffer-create "*asm-blox-puzzle-selection*")))
    (switch-to-buffer buffer)
    (asm-blox-puzzle-selection-mode)
    (asm-blox-puzzle-selection-prepare-buffer)
    (goto-char (point-min))))

(provide 'asm-blox-display)

;;; asm-blox-display.el ends here
