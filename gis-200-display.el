;;; gis-200-display.el --- Display code for GIS-200 Game -*- lexical-binding: t -*-

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

;; This packages contains display code for GIS-200 game.

;; commentary

;;; Code:

(defface gis-200-error-face
  '((((class color) (background light)) (:foreground "DarkGoldenrod4"))
    (((class color) (background dark))  (:foreground "DarkGoldenrod1")))
  "?"
  :group 'gis-200)

(defface gis-200-show-paren-match-face
  '((t (:inherit show-paren-match)))
  "`gis-200-mode' face used for a matching paren pair."
  :group 'gis-200)

(defconst gis-200-column-ct 4)
(defconst gis-200-box-width 20)
(defconst gis-200-box-height 12)

(defvar gis-200-parse-errors nil)
(defvar gis-200-runtime-error nil
  "If non-nil, contains the runtime error encountered. The format
  of the error is (list message row column).")

(defvar-local gis-200--display-mode 'edit)

(defun gis-200--get-parse-error-at-cell (row col)
  (let ((err (assoc (list row col) gis-200-parse-errors)))
    (cdr err)))

(defun gis-200--get-error-at-cell (row col)
  (if (and (eql gis-200--display-mode 'execute)
           gis-200-runtime-error
           (equal (list row col) (cdr gis-200-runtime-error)))
      (car gis-200-runtime-error)
    (let ((err (assoc (list row col) gis-200-parse-errors)))
      (cdr err))))

(defvar gis-200-box-contents nil)

;;; Widget display

(defvar gis-200--widget-row-idx nil)
(defvar gis-200--current-widgets nil
  "List of widgets to display on right of gameboard.")


(defun gis-200--test-widget (msg)
  (pcase msg
    ('width 6)
    (`(display ,n)
     (pcase n
       (0 " IN.A ")
       (1 "┌────┐")
       (47 "└────┘")
       (_ (format "│%4d│" n))))))

(defun gis-200--make-source-widget (source)
  "Return a widget displaying a source."
  (let ((source-widget-offset-ct 2))
    (lambda (msg)
      (pcase msg
        ('width 6)
        (`(display ,n)
         (let ((data (gis-200--cell-source-data source))
               (idx (gis-200--cell-source-idx source))
               (name (gis-200--cell-source-name source)))
           (pcase n
             (0 (format "IN: %s" name))
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

(defun gis-200--make-sink-widget (sink)
  "Return a widget displaying a source."
  (let ((sink-widget-offset-ct 2))
    (lambda (msg)
      (pcase msg
        ('width 6)
        (`(display ,n)
         (let* ((data (gis-200--cell-sink-expected-data sink))
                (err-val (gis-200--cell-sink-err-val sink))
                (idx (gis-200--cell-sink-idx sink))
                (last-idx-p (= (1- idx) (- n sink-widget-offset-ct)))
                (name (gis-200--cell-sink-name sink)))
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

(defun gis-200--display-widget ()
  "Display the current line of the active widget.
This should normally be called when the point is at the end of the display."
  (dolist (widget gis-200--current-widgets)
    (let ((widget-text (funcall widget (list 'display gis-200--widget-row-idx))))
      (insert widget-text)
      (insert "  ")))
  (setq gis-200--widget-row-idx (1+ gis-200--widget-row-idx)))


;;; Main grid display

(defun gis-200--initialize-box-contents ()
  (setq gis-200-box-contents (make-hash-table :test 'equal))
  (dotimes (row 3)
    (dotimes (col gis-200-column-ct)
      (puthash (list row col) "" gis-200-box-contents))))

(defun gis-200--get-box-content (row col)
  (gethash (list row col) gis-200-box-contents))

(defun gis-200--set-box-content (row col text)
  (puthash (list row col) text gis-200-box-contents))

(defun gis-200--get-box-line-content (row col line)
  (let ((text (gis-200--get-box-content row col)))
    (or (nth line (split-string text "\n")) "")))

(defun gis-200--insert-box-line-content (row col line line-col char)
  (let* ((text gis-200-box-content)
         (lines (split-string text "\n"))
         (at-line (nth line lines))
         (new-at-line (concat (substring at-line 0 line-col)
                              (char-to-string char) 
                              (substring at-line line-col))))
    (setcar (nthcdr line lines) new-at-line)
    (setq gis-200-box-content (string-join lines "\n"))))

(defun gis-200--row-register-display (row col direction)
  ""
  (if (eql 'execute gis-200--display-mode)
      (let ((val (gis-200--get-direction-row-registers row col direction)))
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
(defun gis-200--col-register-display (row col direction)
  ""
  ;; Note: the space between the cells is 5 splaces.
  (if (eql 'execute gis-200--display-mode)
      (let ((val (gis-200--get-direction-col-registers row col direction)))
        (cond
         ((not val) "     ")
         ((numberp val)
          (propertize
           (format "%4d " val)
           'font-lock-face '(:weight bold)))
         ((symbolp val)
          (format "%4s " (symbol-name val)))))
    "     "))

(defun gis-200--source-sink-idx-to-name (type idx)
  (let ((start-char (if (eql type 'source) ?A ?W)))
    (format "%c" (+ start-char idx))))

(defun gis-200--row-arrow-label-display (position type col)
  (let* ((row (if (eql position 'top) -1 3))
         (name (if (eql type 'source)
                   (gis-200--get-source-idx-at-position row col)
                 (gis-200--get-sink-idx-at-position row col))))
    (or name " ")))

(defun gis-200--col-arrow-label-display (position type row)
  (let* ((col (if (eql position 'left) -1 4))
         (name (if (eql type 'source)
                   (gis-200--get-source-idx-at-position row col)
                 (gis-200--get-sink-idx-at-position row col))))
    (or name " ")))

(defun gis-200-display-game-board ()
  (setq gis-200--widget-row-idx 0)
  (when (not (hash-table-p gis-200--end-of-box-points))
    (setq gis-200--end-of-box-points (make-hash-table :test 'equal)))
  (when (not (hash-table-p gis-200--beginning-of-box-points))
    (setq gis-200--beginning-of-box-points (make-hash-table :test 'equal)))
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
         (box-line-top-bottom (make-string gis-200-box-width box-horizontal))
         (box-inside (make-string gis-200-box-width ?\s)))
    (let ((insert-row-top
           (lambda (row)
             "Draw the  ┌───┐┌───┐┌───┐┌───┐ part of the board."
             (insert space-start)
             (insert space-between)
             (dotimes (col gis-200-column-ct)
               (let ((err (gis-200--get-error-at-cell row col)))
                 (if err
                     (progn
                       (insert (propertize (char-to-string box-top-left) 'font-lock-face '(:foreground "red")))
                       (insert (propertize box-line-top-bottom 'font-lock-face '(:foreground "red")))
                       (insert (propertize (char-to-string box-top-right) 'font-lock-face '(:foreground "red")) ))
                   (insert box-top-left)
                   (insert box-line-top-bottom)
                   (insert box-top-right))
                 (insert space-between)))
             (when (eql gis-200--display-mode 'execute)
               (gis-200--display-widget))
             (insert "\n")))
          (insert-row-bottom
           (lambda (row)
             "Draw the  └───┘└───┘└───┘└───┘ part of the board. "
             (insert space-start)
             (insert space-between)
             (dotimes (col gis-200-column-ct)
               (let ((err (gis-200--get-error-at-cell row col)))
                 (if err
                     (progn (insert (propertize (char-to-string box-bottom-left) 'font-lock-face '(:foreground "red")))
                            (insert (propertize box-line-top-bottom 'font-lock-face '(:foreground "red")))
                            (insert (propertize (char-to-string box-bottom-right) 'font-lock-face '(:foreground "red"))))
                   (insert box-bottom-left)
                   (insert box-line-top-bottom)
                   (insert box-bottom-right))
                 (insert space-between)))
             (when (eql gis-200--display-mode 'execute)
               (gis-200--display-widget))
             (insert "\n")))
          (insert-v-border
           (lambda (position)
             "Draw the very top and very bottom lines."
             (let* ((left-of-arrows-len (1+ (- (/ (length box-line-top-bottom) 2) 1)))
                    (right-of-arrows-len (1+ (- (length box-line-top-bottom) 2 left-of-arrows-len))))
               (insert space-start)
               (insert space-between)
               (dotimes (col gis-200-column-ct)
                 (insert (make-string left-of-arrows-len ?\s))
                 (let ((sink-char (gis-200--row-arrow-label-display position 'sink col))
                       (source-char (gis-200--row-arrow-label-display position 'source col)))
                   (if (eql position 'top)
                       (insert (format "%s %s" sink-char source-char))
                     (insert (format "%s %s" source-char sink-char))))
                 (insert (make-string right-of-arrows-len ?\s))
                 (insert space-between)))
             (when (eql gis-200--display-mode 'execute)
               (gis-200--display-widget))
             (insert "\n")))
          (insert-row-middle
           (lambda (row box-row)
             "Draw the ⇋|   |⇋|   |⇋|   |⇋|   |⇋ part of the board."
             (insert space-start)
             (cond
              ((= 4 box-row)
               (let ((display (gis-200--col-register-display row 0 'RIGHT))) ; NOTE: capital LEFT indicates arrow direction
                 (insert display)))                                          ;       while lower-case represents board side.
              ((= 5 box-row)
               (let ((label (gis-200--col-arrow-label-display 'left 'source row)))
                 (if (equal label " ")
                     (insert space-between)
                   (progn (insert "  ") (insert label) (insert arrow-right) (insert ?\s)))))
              ((= 7 box-row)
               (let ((label (gis-200--col-arrow-label-display 'left 'sink row)))
                 (if (equal label " ")
                     (insert space-between)
                   (progn (insert "  ") (insert label) (insert arrow-left) (insert ?\s)))))
              ((= 8 box-row)
               (let ((display (gis-200--col-register-display row 0 'LEFT)))
                 (insert display)))
              (t (insert space-between)))
             ;; Draw each box column
             (dotimes (col gis-200-column-ct)
               (let ((err (gis-200--get-error-at-cell row col)))
                 (if err
                     (insert (propertize (char-to-string box-vertical) 'font-lock-face '(:foreground "red")))
                   (insert box-vertical))
                 (when (= box-row 0)
                   (puthash (list row col) (point) gis-200--beginning-of-box-points))
                 ;; Draw the inner contents of the box
                 (let* ((text (gis-200--get-box-line-content row col box-row))
                        (spacing (make-string (- (length box-inside) (length text)) ?\s)))
                   (if (and (= 11 box-row) err (eql gis-200--display-mode 'edit))
                       (let ((err-text (nth 2 (gis-200--get-error-at-cell row col)))) 
                         (insert err-text)
                         (insert (make-string (- (length box-inside) (length err-text)) ?\s)))
                     (insert (propertize text 'gis-200-box-id (list row col box-row)))
                     (insert (propertize spacing
                                         'gis-200-box-id (list row col box-row)
                                         'gis-200-text-type 'spacing))))
                 (when (= box-row (1- gis-200-box-height))
                   (puthash (list row col) (point) gis-200--end-of-box-points))
                 (let ((pipe-str (cond
                                  ((= box-row (1- gis-200-box-height))
                                   (if err 
                                       (propertize (char-to-string box-vertical)
                                                   'gis-200-text-type `(box-end ,row ,col)
                                                   'font-lock-face '(:foreground "red"))
                                     (propertize (char-to-string box-vertical)
                                                   'gis-200-text-type `(box-end ,row ,col))))
                                  (err
                                   (propertize (char-to-string box-vertical)
                                               'font-lock-face '(:foreground "red")))
                                  (t
                                   (char-to-string box-vertical)))))
                   (insert pipe-str)))
               (when (< col (1- gis-200-column-ct))
                 (cond
                  ((= 4 box-row)
                   (let ((display (gis-200--col-register-display row (1+ col) 'RIGHT)))
                     (insert display)))
                  ((= 5 box-row)
                   (progn (insert "  ") (insert arrow-right) (insert "  ")))
                  ((= 7 box-row)
                   (progn (insert "  ") (insert arrow-left) (insert "  ")))
                  ((= 8 box-row)
                   (let ((display (gis-200--col-register-display row (1+ col) 'LEFT)))
                     (insert display)))
                  (t
                   (insert space-between)))))
             (cond
              ((= 4 box-row)
               (let ((display (gis-200--col-register-display row gis-200-column-ct 'RIGHT)))
                 (insert display)))
              ((= 5 box-row)
               (let ((label (gis-200--col-arrow-label-display 'right 'sink row)))
                 (if (equal label " ")
                     (insert space-between)
                   (progn (insert ?\s) (insert arrow-right) (insert label) (insert "  ")))))
              ((= 7 box-row)
               (let ((label (gis-200--col-arrow-label-display 'right 'source row)))
                 (if (equal label " ")
                     (insert space-between)
                   (progn (insert ?\s) (insert arrow-left) (insert label) (insert "  ")))))
              ((= 8 box-row)
               (let ((display (gis-200--col-register-display row gis-200-column-ct 'LEFT)))
                 (insert display)))
              (t
               (insert space-between)))
             (when (eql gis-200--display-mode 'execute)
               (gis-200--display-widget))
             (insert "\n")))
          (insert-middle-row-space
           (lambda (row)
             "Draw the  ↑↓    ↑↓    ↑↓    ↑↓ part of the board."
             (let* ((left-of-arrows-len (1+ (- (/ (length box-line-top-bottom) 2) 1)))
                    (right-of-arrows-len (1+ (- (length box-line-top-bottom) 2 left-of-arrows-len)))
                    (padding-space-left (make-string (- left-of-arrows-len 5) ?\s))
                    (padding-space-right (make-string (- right-of-arrows-len 5) ?\s)))
               (insert space-start)
               (insert space-between)
               (dotimes (col gis-200-column-ct)
                 (insert padding-space-left)
                 ;; logic to display arrow and contnents
                 (let* ((up-arrow-display (gis-200--row-register-display row col 'UP))
                        (down-arrow-display (gis-200--row-register-display row col 'DOWN))
                        (label-position (cond ((= 0 row) 'top) ((= 3 row) 'bottom) (t nil)))
                        (source-label
                         (and label-position
                              (gis-200--row-arrow-label-display label-position
                                                                (if (eql label-position 'top)
                                                                    'source 'sink)
                                                                col)))
                        (sink-label
                         (and label-position
                              (gis-200--row-arrow-label-display label-position
                                                                (if (eql label-position 'top)
                                                                    'sink 'source)
                                                                col))))
                   (insert up-arrow-display)
                   (insert " ")
                   (if (and label-position (or (not sink-label) (equal sink-label " ")))
                       (insert " ")
                     (insert arrow-up))
                   (insert ?\s)
                   (if (and label-position (or (not source-label) (equal source-label " ")))
                       (insert " ")
                     (insert arrow-down))
                   (insert " ")
                   (insert down-arrow-display))
                 (insert padding-space-right)
                 (insert space-between))
               (when (eql gis-200--display-mode 'execute)
                 (gis-200--display-widget))
               (insert "\n")))))
      (funcall insert-v-border 'top)
      (funcall insert-middle-row-space 0)
      (dotimes (row 3)
        (funcall insert-row-top row)
        (dotimes (box-row gis-200-box-height)
          (funcall insert-row-middle row box-row))
        (funcall insert-row-bottom row)
        (when (not (= 2 row))
          (funcall insert-middle-row-space (1+ row))))
      (funcall insert-middle-row-space 3)
      (funcall insert-v-border 'bottom)

      (insert "\n\n")
      (let ((name (gis-200--problem-spec-name gis-200--extra-gameboard-cells))
            (description (gis-200--problem-spec-description gis-200--extra-gameboard-cells)))
        (insert name ":\n")
        (insert description "\n"))
      (when (and (eql gis-200--display-mode 'execute) gis-200-runtime-error)
        (insert (format "\nERROR: %s at cell (%d, %d)\n"
                        (car gis-200-runtime-error)
                        (cadr gis-200-runtime-error)
                        (caddr gis-200-runtime-error))))))
  (gis-200--propertize-errors))

(defun gis-200--box-point-forward (ct)
  "With the point in a text box, move forward a point in box-buffer."
  (while (> ct 0)
    (cond
     ((eql (get-text-property (point) 'gis-200-text-type) 'spacing)
      (forward-char))
     ((not (gis-200-in-box-p))
      (let ((col (- (current-column) gis-200-box-width)))
        (forward-line 1)
        (move-to-column col)
        (setq ct (1- ct))))
     (t (forward-char)
        (setq ct (1- ct))))))

(defvar gis-200--disable-redraw nil
  "If non-nil, commands should not opt-in to redrawing the gameboard.")

(defun gis-200--propertize-errors ()
  "Add text properties to errors."
  (let ((errs gis-200-parse-errors))
    (dolist (err errs)
      (let ((row (caar err))
            (col (cadar err))
            (pt (1- (caddr err)))
            (gis-200--disable-redraw t))
        (gis-200--move-to-box row col)
        ;; move to the point where the error occured.
        ;; TODO: create box-point-forward to save this logic
        (while (> pt 0)
          (cond
           ((eql (get-text-property (point) 'gis-200-text-type) 'spacing)
            (forward-char))
           ((not (gis-200-in-box-p))
            (let ((col (- (current-column) gis-200-box-width)))
              (forward-line 1)
              (move-to-column col)
              (setq pt (1- pt))))
           (t (forward-char)
              (setq pt (1- pt)))))
        (let ((inhibit-read-only t))
          (put-text-property (point) (1+ (point)) 'font-lock-face '(:underline (:color "red" :style wave))))))))

(defun gis-200-redraw-game-board ()
  (erase-buffer)
  (gis-200-display-game-board))

(defun gis-200-in-box-p ()
  (get-text-property (point) 'gis-200-box-id))

(defun gis-200-get-line-col-num ()
  "Return the line column number in the current box."
  (unless (gis-200-in-box-p)
    (error "not in text box"))
  (let ((i 0))
    (save-excursion
      (while (get-text-property (point) 'gis-200-box-id)
        (forward-char -1)
        (setq i (1+ i))))
    (1- i)))

(defun gis-200-insert-char (char)
  "Insert a character inside a box."
  (let ((box-id (get-text-property (point) 'gis-200-box-id)))
    (when box-id
      (let ((row (nth 0 box-id))
            (col (nth 1 box-id))
            (line-no (nth 2 box-id))
            (line-col-no (gis-200-get-line-col-num)))
        (gis-200--insert-box-line-content row col line-no line-col-no char)
        (gis-200-redraw-game-board)))))

(defun gis-200--beginning-of-box ()
  "Move the point to the beginning of the box."
  (unless (gis-200-in-box-p)
    (error "not in box 1"))
  (while (gis-200-in-box-p)
    (next-line -1)) ;; TODO - is this command buggy?
  (next-line 1)     ;; TODO - use forward line
  (while (gis-200-in-box-p)
    (forward-char -1))
  (forward-char 1))

(defvar gis-200--end-of-box-points nil
  "Contains a hashmap of the points where each box ends.
This was added for performance reasons.")

(defun gis-200--move-to-end-of-box (row col)
  (let ((end-pos (gethash (list row col) gis-200--end-of-box-points)))
      (goto-char end-pos)))

(defvar gis-200--beginning-of-box-points nil
  "Contains a hashmap of the points where each box begins.
This was added for performance reasons.")

(defun gis-200--move-to-box (row col)
  (when (and (eql gis-200--display-mode 'edit)
             (not gis-200--disable-redraw))
    (let ((inhibit-read-only t))
      (gis-200-redraw-game-board)))
  (let ((begin-pos (gethash (list row col) gis-200--beginning-of-box-points)))
    (goto-char begin-pos)))

(defun gis-200--move-to-box-point (row col)
  (unless (gis-200-in-box-p)
    (error "not in box 2"))
  (gis-200--beginning-of-box)
  (let ((start-col (current-column)))
    (next-line row)
    (move-to-column start-col)
    (forward-char col)))

(defun gis-200--beginning-of-line ()
  (while (gis-200-in-box-p)
    (forward-char -1))
  (forward-char 1))

(defun gis-200--replace-box-text (text)
  (let* ((start-pos (point))
         (lines (split-string text "\n"))
         (box-id (get-text-property (point) 'gis-200-box-id))
         (row (nth 0 box-id))
         (col (nth 1 box-id))
         (line (nth 2 box-id))
         (at-line-no 0)
         (start-line-col (current-column)))
    (gis-200--beginning-of-box)
    (while (gis-200-in-box-p)
      (let* ((at-line (or (car lines) ""))
             (padding (make-string (- gis-200-box-width (length at-line)) ?\s)))
        (delete-char gis-200-box-width)
        (insert (propertize (concat at-line padding)
                            'gis-200-box-id
                            (list row col at-line-no)))
        (next-line 1)
        (move-to-column start-line-col)
        (gis-200--beginning-of-line)
        (setq lines (cdr lines))
        (setq at-line-no (1+ at-line-no))))
    (goto-char start-pos)))

(defconst gis-200--mirror-buffer-name "*gis-200-temp*")

(defun gis-200--func-in-buffer (func)
  (unless (gis-200-in-box-p)
    (error "not in box 3"))
  (let* ((box-id (get-text-property (point) 'gis-200-box-id))
         (row (nth 0 box-id))
         (col (nth 1 box-id))
         (line (nth 2 box-id))
         (line-col (gis-200-get-line-col-num))
         (text (gis-200--get-box-content row col))
         (new-line)
         (new-col)
         (new-text))
    (with-current-buffer (get-buffer-create gis-200--mirror-buffer-name) ;; TODO: use temp buffer?
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
        (gis-200--set-box-content row col new-text)
        (gis-200--replace-box-text new-text)
        (gis-200--move-to-box-point new-line new-col)))))

(defmacro gis-200--in-buffer (code)
  `(gis-200--func-in-buffer
    (lambda ()
      ,code)))

(defun gis-200--self-insert-command ()
  ""
  (interactive)
  (gis-200--in-buffer
   (insert (this-command-keys))))

(defun gis-200--backward-delete-char ()
  ""
  (interactive)
  (gis-200--in-buffer
   (backward-delete-char 1)))

(defun gis-200--kill-word ()
  ""
  (interactive)
  (gis-200--in-buffer
   (kill-word 1)))

(defun gis-200--kill-line ()
  ""
  (interactive)
  (gis-200--in-buffer
   (kill-line)))

(defun gis-200--move-beginning-of-line ()
  ""
  (interactive)
  (gis-200--in-buffer
   (move-beginning-of-line 1)))

(defun gis-200--move-end-of-line ()
  ""
  (interactive)
  (gis-200--in-buffer
   (move-end-of-line 1)))

(defun gis-200--beginning-of-buffer ()
  ""
  (interactive)
  (if (gis-200-in-box-p)
      (gis-200--in-buffer
       (beginning-of-buffer))
    (beginning-of-buffer)))

(defun gis-200--end-of-buffer ()
  ""
  (interactive)
  (if (gis-200-in-box-p)
      (gis-200--in-buffer
       (end-of-buffer))
    (end-of-buffer)))

(defun gis-200--newline ()
  ""
  (interactive)
  (gis-200--in-buffer
   (newline)))

(defun gis-200--forward-line ()
  ""
  (interactive)
  (gis-200--in-buffer
   (forward-line)))

(defun gis-200--refresh-contents ()
  ""
  (interactive)
  (let ((inhibit-read-only t))
    (if (gis-200-in-box-p)
        (gis-200--in-buffer
         (forward-char 0))
      (gis-200-redraw-game-board))))

(defun gis-200--coords-to-end-of-box ()
  "Return (lines-down columns-right) to reach the end of the box."
  (unless (gis-200-in-box-p)
    (error "not in box"))
  (let* ((box-id (get-text-property (point) 'gis-200-box-id))
         (row (car box-id))
         (col (cadr box-id))
         (text (gis-200--get-box-content row col))
         (lines (split-string text "\n"))
         (line-ct (length lines))
         (last-line-len (length (car (last lines)))))
    (list line-ct last-line-len)))

(defun gis-200--move-point-to-end-of-box-content ()
  "Move the point to the end of the content of the current box."
  (let* ((coords (gis-200--coords-to-end-of-box))
         (lines (car coords))
         (cols (cadr coords)))
    (let ((col (current-column)))
      (forward-line (1- lines))
      (move-to-column col))
    (forward-char cols)))

(defun gis-200--next-row-cell ()
  "Move the point to the end of the box in the next row."
  (interactive)
  (unless (gis-200-in-box-p)
    (error "not in box"))
  (let* ((box-id (get-text-property (point) 'gis-200-box-id))
         (row (car box-id))
         (col (cadr box-id))
         (next-row (if (= row (1- gis-200--gameboard-row-ct)) 0 (1+ row))))
    (gis-200--move-to-box next-row col)
    (gis-200--move-point-to-end-of-box-content)))

(defun gis-200--next-cell ()
  "Move the point to the end of the next box."
  (interactive)
  (if (gis-200-in-box-p)
      (let* ((box-id (get-text-property (point) 'gis-200-box-id))
             (row (car box-id))
             (col (cadr box-id))
             (next-col (if (= col (1- gis-200-column-ct)) 0 (1+ col)))
             (next-row (if (= col (1- gis-200-column-ct)) (1+ row) row))
             (next-row (if (= next-row gis-200--gameboard-row-ct) 0 next-row)))
        (gis-200--move-to-box next-row next-col)
        (gis-200--move-point-to-end-of-box-content))
    (while (and (not (gis-200-in-box-p))
                (not (bobp)))
      (forward-char -1))
    (when (bobp)
      (gis-200--move-to-box (1- gis-200--gameboard-row-ct) (1- gis-200--gameboard-col-ct)))
    (gis-200--next-cell)))

;; TODO: DRY this and next-cell up.
(defun gis-200--prev-cell ()
  "Move the point to the end of the previous box."
  (interactive)
  (if (gis-200-in-box-p)
      (let* ((box-id (get-text-property (point) 'gis-200-box-id))
             (row (car box-id))
             (col (cadr box-id))
             (next-col (if (= col 0) (1- gis-200-column-ct) (1- col)))
             (next-row (if (= col 0) (1- row) row))
             (next-row (if (= next-row -1) (1- gis-200--gameboard-row-ct) next-row)))
        (gis-200--move-to-box next-row next-col)
        (gis-200--move-point-to-end-of-box-content))
    (while (and (not (gis-200-in-box-p))
                (not (eobp)))
      (forward-char 1))
    (when (eobp)
      (gis-200--move-to-box 0 0))
    (gis-200--prev-cell)))

(defconst gis-200-mode-map
  (let ((map (make-keymap)))
    (prog1 map
      ;;(suppress-keymap map)
      (define-key map "a" #'gis-200--self-insert-command)
      (define-key map "b" #'gis-200--self-insert-command)
      (define-key map "c" #'gis-200--self-insert-command)
      (define-key map "d" #'gis-200--self-insert-command)
      (define-key map "e" #'gis-200--self-insert-command)
      (define-key map "f" #'gis-200--self-insert-command)
      (define-key map "g" #'gis-200--self-insert-command)
      (define-key map "h" #'gis-200--self-insert-command)
      (define-key map "i" #'gis-200--self-insert-command)
      (define-key map "j" #'gis-200--self-insert-command)
      (define-key map "k" #'gis-200--self-insert-command)
      (define-key map "l" #'gis-200--self-insert-command)
      (define-key map "m" #'gis-200--self-insert-command)
      (define-key map "n" #'gis-200--self-insert-command)
      (define-key map "o" #'gis-200--self-insert-command)
      (define-key map "p" #'gis-200--self-insert-command)
      (define-key map "q" #'gis-200--self-insert-command)
      (define-key map "r" #'gis-200--self-insert-command)
      (define-key map "s" #'gis-200--self-insert-command)
      (define-key map "t" #'gis-200--self-insert-command)
      (define-key map "u" #'gis-200--self-insert-command)
      (define-key map "v" #'gis-200--self-insert-command)
      (define-key map "w" #'gis-200--self-insert-command)
      (define-key map "x" #'gis-200--self-insert-command)
      (define-key map "y" #'gis-200--self-insert-command)
      (define-key map "z" #'gis-200--self-insert-command)
      (define-key map "A" #'gis-200--self-insert-command)
      (define-key map "B" #'gis-200--self-insert-command)
      (define-key map "C" #'gis-200--self-insert-command)
      (define-key map "D" #'gis-200--self-insert-command)
      (define-key map "E" #'gis-200--self-insert-command)
      (define-key map "F" #'gis-200--self-insert-command)
      (define-key map "G" #'gis-200--self-insert-command)
      (define-key map "H" #'gis-200--self-insert-command)
      (define-key map "I" #'gis-200--self-insert-command)
      (define-key map "J" #'gis-200--self-insert-command)
      (define-key map "K" #'gis-200--self-insert-command)
      (define-key map "L" #'gis-200--self-insert-command)
      (define-key map "M" #'gis-200--self-insert-command)
      (define-key map "N" #'gis-200--self-insert-command)
      (define-key map "O" #'gis-200--self-insert-command)
      (define-key map "P" #'gis-200--self-insert-command)
      (define-key map "Q" #'gis-200--self-insert-command)
      (define-key map "R" #'gis-200--self-insert-command)
      (define-key map "S" #'gis-200--self-insert-command)
      (define-key map "T" #'gis-200--self-insert-command)
      (define-key map "U" #'gis-200--self-insert-command)
      (define-key map "V" #'gis-200--self-insert-command)
      (define-key map "W" #'gis-200--self-insert-command)
      (define-key map "X" #'gis-200--self-insert-command)
      (define-key map "Y" #'gis-200--self-insert-command)
      (define-key map "Z" #'gis-200--self-insert-command)
      (define-key map "0" #'gis-200--self-insert-command)
      (define-key map "1" #'gis-200--self-insert-command)
      (define-key map "2" #'gis-200--self-insert-command)
      (define-key map "3" #'gis-200--self-insert-command)
      (define-key map "4" #'gis-200--self-insert-command)
      (define-key map "5" #'gis-200--self-insert-command)
      (define-key map "6" #'gis-200--self-insert-command)
      (define-key map "7" #'gis-200--self-insert-command)
      (define-key map "8" #'gis-200--self-insert-command)
      (define-key map "9" #'gis-200--self-insert-command)
      (define-key map "," #'gis-200--self-insert-command)
      (define-key map "(" #'gis-200--self-insert-command)
      (define-key map ")" #'gis-200--self-insert-command)
      (define-key map ":" #'gis-200--self-insert-command)
      (define-key map "-" #'gis-200--self-insert-command)
      (define-key map "[" #'gis-200--self-insert-command)
      (define-key map "]" #'gis-200--self-insert-command)
      (define-key map "\"" #'gis-200--self-insert-command)
      (define-key map "'" #'gis-200--self-insert-command)
      (define-key map ";" #'gis-200--self-insert-command)
      (define-key map "#" #'gis-200--self-insert-command)
      (define-key map "|" #'gis-200--self-insert-command)
      (define-key map "<" #'gis-200--self-insert-command)
      (define-key map ">" #'gis-200--self-insert-command)
      (define-key map "{" #'gis-200--self-insert-command)
      (define-key map "}" #'gis-200--self-insert-command)
      (define-key map "=" #'gis-200--self-insert-command)
      (define-key map "+" #'gis-200--self-insert-command)
      (define-key map "_" #'gis-200--self-insert-command)
      (define-key map (kbd "DEL") #'gis-200--backward-delete-char)
      (define-key map (kbd "SPC") #'gis-200--self-insert-command)
      (define-key map (kbd "RET") #'gis-200--newline)
      (define-key map (kbd "C-c g") #'gis-200--refresh-contents)
      (define-key map (kbd "C-c C-c") #'gis-200-start-execution)
      (define-key map (kbd "M-d") #'gis-200--kill-word)
      (define-key map (kbd "C-k") #'gis-200--kill-line)
      (define-key map (kbd "C-a") #'gis-200--move-beginning-of-line)
      (define-key map (kbd "C-e") #'gis-200--move-end-of-line)
      (define-key map (kbd "M-<") #'gis-200--beginning-of-buffer)
      (define-key map (kbd "M->") #'gis-200--end-of-buffer)
      (define-key map (kbd "<tab>") #'gis-200--next-cell)
      (define-key map (kbd "<backtab>") #'gis-200--prev-cell)
      (define-key map (kbd "<S-return>") #'gis-200--next-row-cell))))

(defun gis-200--execution-next-command ()
  ""
  (interactive)
  (when (not (gis-200--gameboard-in-final-state-p))
    (gis-200--gameboard-step)
    (gis-200--resolve-port-values)    ;; ports need to be resolved before extra-cells run
    (gis-200--extra-gameboard-step))  ;; so they can consume immediately 
  (let ((inhibit-read-only t))
    (gis-200-redraw-game-board)
    (gis-200-execution-code-highlight)
    (gis-200-execution-draw-stack))
  (gis-200-check-winning-conditions))

(defconst gis-200-execution-mode-map
  (let ((map (make-keymap)))
    (prog1 map
      ;;(suppress-keymap map)
      (define-key map "n" #'gis-200--execution-next-command)
      (define-key map "q" #'quit-window))))

(defconst gis-200-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?| "-" st)
    st)
  "Syntax table for gis-200 mode.")

(defconst gis-200-mode-highlights
  '(("\\<setq\\>" . 'font-lock-function-name-face)
    ("[0-9]+" . (1 'font-lock-constant-face))))

(defun gis-200--create-execution-buffer ()
  "Create a new uneditable gamebuffer for displaing execution of puzzles."
  (let ((buffer (get-buffer-create "*gis-200-execution*")))
    (with-current-buffer buffer
      (gis-200-execution-mode)
      (let ((inhibit-read-only t))
        (gis-200-redraw-game-board)
        (gis-200-execution-code-highlight)
        (gis-200-execution-draw-stack)))
    (switch-to-buffer buffer)))

(defun gis-200-execution-code-highlight ()
  "Adds highlight face to where runtime's pc is "
  (let ((inhibit-read-only t))
    (dotimes (row gis-200--gameboard-row-ct)
      (dotimes (col gis-200--gameboard-col-ct)
        (let* ((at-runtime (gis-200--cell-at-row-col row col))
               (at-instr (gis-200--cell-runtime-current-instruction at-runtime))
               (start-pos (gis-200-code-node-start-pos at-instr))
               (end-pos (gis-200-code-node-end-pos at-instr)))
          (when (and start-pos end-pos)
            (gis-200--move-to-box row col)
            (gis-200--box-point-forward (1- start-pos))
            (let* ((text (gis-200--get-box-content row col))
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

(defun gis-200-execution-draw-stack ()
  "Display the stack for the current cell-runtimes."
  (let ((inhibit-read-only t))
    (dotimes (row gis-200--gameboard-row-ct)
      (dotimes (col gis-200--gameboard-col-ct)
        (let* ((at-runtime (gis-200--cell-at-row-col row col))
               ;; makes more sense to reverse stack 
               (stack (reverse (gis-200--cell-runtime-stack at-runtime))))
          (gis-200--move-to-end-of-box row col)
          (while stack
            (let ((stack-top (car stack)))
              (forward-char -4)
              (delete-forward-char 4)
              (if stack-top
                  (insert (format "%4d" stack-top))
                (insert "    "))
              (forward-char -5))
            (setq stack (cdr stack))))))))

(defun gis-200--create-widges-from-gameboard ()
  ""
  (let ((sources (gis-200--problem-spec-sources gis-200--extra-gameboard-cells))
        (sinks (gis-200--problem-spec-sinks gis-200--extra-gameboard-cells))
        (widgets))
    (dolist (source sources)
      (setq widgets (cons (gis-200--make-source-widget source)
                          widgets)))
    (dolist (sink sinks)
      (setq widgets (cons (gis-200--make-sink-widget sink)
                          widgets)))
    (setq gis-200--current-widgets (reverse widgets))))

(defun gis-200-start-execution ()
  "Parse gameboard, displaying any errors, and display code execution buffer."
  (interactive)
  (setq gis-200-parse-errors nil)
  (let ((parse-errors)
        (parses))
    (maphash
     (lambda (coords code-text)
       (let ((parse-result (gis-200--parse-cell coords code-text)))
         (cond
          ((gis-200--parse-error-p parse-result)
           (setq parse-errors (cons (cons coords parse-result) parse-errors)))
          ((gis-200--cell-runtime-p parse-result)
           (gis-200--set-cell-at-row-col (car coords) (cadr coords) parse-result))
          (t (let ((asm (gis-200--parse-tree-to-asm parse-result)))
               (if (gis-200--parse-error-p asm)
                   (setq parse-errors (cons (cons coords asm) parse-errors)))
               (setq parses (cons (cons coords asm) parses)))))))
     gis-200-box-contents)
    (if parse-errors
        (progn 
          (setq gis-200-parse-errors parse-errors)
          (let ((inhibit-read-only t))
            (gis-200-redraw-game-board)))
      (setq gis-200-parse-errors nil)
      (let ((inhibit-read-only t))
        (gis-200-redraw-game-board))
      (dolist (parse parses)
        (let* ((coords (car parse))
               (row (car coords))
               (col (cadr coords))
               (asm (cdr parse)))
          (assert (numberp col))
          (gis-200--set-cell-asm-at-row-col row col asm)))
      (gis-200--reset-extra-gameboard-cells-state)
      (gis-200--create-widges-from-gameboard)
      (gis-200--create-execution-buffer))))

(defun gis-200-execution-mode ()
  (kill-all-local-variables)
  (use-local-map gis-200-execution-mode-map)
  (setq mode-name "gis-200-execution"
        buffer-read-only t)
  (setq-local truncate-lines 0
              gis-200--display-mode 'execute)
  (buffer-disable-undo)
  (setq font-lock-defaults gis-200-mode-highlights)
  (setq header-line-format "GIS-200 EXECUTION")
  (setq gis-200-runtime-error nil)
  (setq gis-200--gameboard-state nil)
  (set-syntax-table gis-200-mode-syntax-table))

(defvar gis-200--skip-initial-parsing nil
  "When non-nil, don't parse the initial gameboard.")

(defvar gis-200--show-pair-idle-timer nil)

(defun gis-200-mode ()
  (interactive)
  (kill-all-local-variables)
  (use-local-map gis-200-mode-map)
  (setq mode-name "gis-200"
        buffer-read-only t)
  (setq gis-200-parse-errors nil)
  (setq-local truncate-lines 0)
  (buffer-disable-undo)
  (setq font-lock-defaults gis-200-mode-highlights)
  (set-syntax-table gis-200-mode-syntax-table)
  (unless gis-200--skip-initial-parsing
    (gis-200--parse-saved-buffer)
    (let ((inhibit-read-only t))
      (gis-200-redraw-game-board)))
  (unless gis-200--show-pair-idle-timer
    (setq gis-200--show-pair-idle-timer
          (run-with-idle-timer 0.125 t 'gis-200--highlight-pairs))))

;;;###autoload
(add-to-list 'auto-mode-alist (cons "\\.gis\\'" 'gis-200-mode))

;;; Parenthesis match code

(defvar gis-200-pair-overlays nil "List of overlays used to highlight parenthesis pairs.")

(defun gis-200--find-closing-match ()
  (let* ((box-id (get-text-property (point) 'gis-200-box-id))
         (row (car box-id))
         (col (cadr box-id))
         (line (caddr box-id))
         (line-col (gis-200-get-line-col-num))
         (text (gis-200--get-box-content row col)))
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

(defun gis-200--find-opening-match ()
  (let* ((box-id (get-text-property (point) 'gis-200-box-id))
         (row (car box-id))
         (col (cadr box-id))
         (line (caddr box-id))
         (line-col (gis-200-get-line-col-num))
         (text (gis-200--get-box-content row col)))
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

(defun  gis-200--pair-delete-overlays ()
  "Remove both show pair overlays."
  (when gis-200-pair-overlays
    (dolist (overlay gis-200-pair-overlays)
      (delete-overlay overlay))
    (setq gis-200-pair-overlays nil)))

(defun gis-200--pair-create-overlays (start end)
  "Create the show pair overlays."
  (when gis-200-pair-overlays
    (gis-200--pair-delete-overlays))
  (let ((oleft (make-overlay start (1+ start) nil t nil))
        (oright (make-overlay end (1+ end) nil t nil)))
    (setq gis-200-pair-overlays (list oleft oright))
    (overlay-put oleft 'face 'gis-200-show-paren-match-face)
    (overlay-put oright 'face 'gis-200-show-paren-match-face)
    (overlay-put oleft 'type 'show-pair)))

(defun gis-200--highlight-pairs ()
  (when (equal mode-name "gis-200") ;; TODO: use eql to make this faster
    (if (gis-200-in-box-p)
        (save-excursion
          (save-match-data
            (while-no-input 
              (cond
               ((looking-back ")")
                (let* ((start-point (1- (point)))
                       (opening-match-coords (gis-200--find-opening-match))
                       (d-row (car opening-match-coords))
                       (d-col (cadr opening-match-coords))
                       (at-col (current-column)))
                  (forward-line d-row)
                  (move-to-column at-col)
                  (forward-char d-col)
                  (let ((end-point (point)))
                    (gis-200--pair-create-overlays start-point end-point))))
               ((looking-at "(")
                (let* ((start-point (point))
                       (closing-match-coords (gis-200--find-closing-match)))
                  (if closing-match-coords
                      (let ((d-row (car closing-match-coords))
                            (d-col (cadr closing-match-coords))
                            (at-col (current-column)))
                        (forward-line d-row)
                        (move-to-column at-col)
                        (forward-char d-col)
                        (let ((end-point (point)))
                          (gis-200--pair-create-overlays start-point end-point)))
                    (when gis-200-pair-overlays
                      (gis-200--pair-delete-overlays))))) ;; TODO: should display red match instead
               (gis-200-pair-overlays
                (gis-200--pair-delete-overlays))))))
      (when gis-200-pair-overlays
        (gis-200--pair-delete-overlays)))))

;;; Puzzle Selection

(defun gis-200--puzzle-selection-setup-buffer (id)
  "Setup the puzzle buffer for the puzzle at ID."
  (let ((puzzle (gis-200--get-puzzle-by-id id)))
    (unless puzzle
      (error "no puzzle found with id %s" id))
    (let ((buffer (get-buffer-create "*gis-200*"))  ;; TODO: allow for 1+ puzzles at once
          (file-name (gis-200--generate-new-puzzle-filename id)))
      (gis-200--initialize-box-contents)
      (setq gis-200--extra-gameboard-cells (funcall puzzle))
      (switch-to-buffer buffer)
      (let ((inhibit-read-only t)
            (gis-200--skip-initial-parsing t))
        (set-visited-file-name file-name)
        (gis-200-redraw-game-board)
        (gis-200-mode)))))

(defun gis-200-select-puzzle ()
  "Start the puzzle for the puzzle under the point."
  (interactive)
  (let ((at-puzzle-id (get-text-property (point) 'gis-200-puzzle-selection-id))
        (at-puzzle-filename (get-text-property (point) 'gis-200-puzzle-selection-filename)))
    (unless at-puzzle-id
      (error "no puzzle under point"))
    (if at-puzzle-filename
        (find-file at-puzzle-filename)
      (gis-200--puzzle-selection-setup-buffer at-puzzle-id))))

(defconst gis-200-puzzle-selection-mode-map
  (let ((map (make-sparse-keymap)))
    (prog1 map
      (define-key map "q" #'quit-window)
      (define-key map (kbd "RET") #'gis-200-select-puzzle))))

(defun gis-200-puzzle-selection-prepare-buffer ()
  "Prepare the puzzle selection buffer."
  (with-current-buffer (get-buffer-create "*gis-200-puzzle-selection*")
    (let ((inhibit-read-only t))
      (erase-buffer)
      (dolist (puzzle-fn gis-200-puzzles)
        (let* ((puzzle (funcall puzzle-fn))
               (name (gis-200--problem-spec-name puzzle))
               (description (gis-200--problem-spec-description puzzle))
               (line-str (format "%-25s %-60s   "
                                 name
                                 (truncate-string-to-width description 60
                                                           nil nil t))))
          (insert (propertize line-str 'gis-200-puzzle-selection-id name))
          (let ((saved-file-ct (gis-200--saved-puzzle-ct-by-id name)))
            (dotimes (i saved-file-ct)
              (insert (propertize (format "[%d]" (1+ i))
                                  'gis-200-puzzle-selection-id name
                                  'gis-200-puzzle-selection-filename (gis-200--make-puzzle-idx-file-name name (1+ i))))
              (insert (propertize " " 'gis-200-puzzle-selection-id name)))))
        (insert "\n")))))

(defun gis-200-puzzle-selection-mode ()
  (interactive)
  (kill-all-local-variables)
  (use-local-map gis-200-puzzle-selection-mode-map)
  (setq mode-name "gis-200-puzzle-selection"
        buffer-read-only t)
  (setq header-line-format
        (format " %-25s %-60s   %s" "PUZZLE NAME" "DESCRIPTION" "SAVED FILES"))
  (setq-local truncate-lines 0)
  (hl-line-mode t)
  (buffer-disable-undo))

(defun gis-200 ()
  (interactive)
  (let ((buffer (get-buffer-create "*gis-200-puzzle-selection*")))
    (switch-to-buffer buffer)
    (gis-200-puzzle-selection-mode)
    (gis-200-puzzle-selection-prepare-buffer)
    (goto-char (point-min))))

(provide 'gis-200-display)

;;; gis-200-display.el ends here
