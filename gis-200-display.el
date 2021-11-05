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


(defconst gis-200-column-ct 4)
(defconst gis-200-box-width 20)
(defconst gis-200-box-height 12)

(defvar gis-200-parse-errors nil)

(defun gis-200--get-parse-error-at-cell (row col)
  (let ((err (assoc (list row col) gis-200-parse-errors)))
    (cdr err)))

(defvar gis-200-box-contents nil)

(defun gis-200--initialize-box-contents ()
  (setq gis-200-box-contents (make-hash-table :test 'equal))
  (dotimes (row 3)
    (dotimes (col gis-200-column-ct)
      (puthash (list row col) "" gis-200-box-contents))))

(defun gis-200--get-box-content (row col)
  (gethash (list row col) gis-200-box-contents))

(defun gis-200--set-box-content (row col text)
  (puthash (list row col) text gis-200-box-contents))

(defun gis-200--parse-game-board ()
  "Parse each cell in gameboard, returning Lisp object of result."
  (let ((res '()))
    (dotimes (row 3)
      (let ((parsed-row '()))
        (dotimes (col gis-200-column-ct)
          (let* ((at-text (gis-200--get-box-content row col))
                 (parsed (gis-200--code-cell-init at-text col row)))
            (push parsed parsed-row)))
        (push (reverse parsed-row) res)))
    (reverse res)))

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

(defun gis-200--resolve-moved-point (row col line line-col)
  (let* ((text gis-200-box-content)
         (lines (split-string text "\n")))
    (if (>= line (length lines))
        'end
      (let ((at-line (nth line lines)))
        (if (> line-col (length at-line))
            (gis-200--resolve-moved-point row col (1+ line) 0)
          (list line line-col))))))

(defun gis-200--row-register-display (row col direction)
  ""
  "    "
  ;; "????"
  ;; (let ((val (gis-200--get-direction-row-registers row col direction)))
  ;;   (cond
  ;;    ((not val) "    ")
  ;;    ((numberp val) (format "%4d" val))))
  )

(defun gis-200--source-sink-idx-to-name (type idx)
  (let ((start-char (if (eql type 'source) ?A ?W)))
    (format "%c" (+ start-char idx))))

(defun gis-200--row-arrow-label-display (position type col)
  (let* ((row (if (eql position 'top) -1 3))
         (idx (if (eql type 'source)
                   (gis-200--get-source-idx-at-position row col)
                (gis-200--get-sink-idx-at-position row col))))
    (if idx
        (gis-200--source-sink-idx-to-name type idx)
      " ")))

(defun gis-200--col-arrow-label-display (position type row)
  (let* ((col (if (eql position 'left) -1 4))
         (idx (if (eql type 'source)
                  (gis-200--get-source-idx-at-position row col)
                (gis-200--get-sink-idx-at-position row col))))
    (if idx
        (gis-200--source-sink-idx-to-name type idx)
      " ")))

(defun gis-200-display-game-board ()
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
               (let ((err (gis-200--get-parse-error-at-cell row col)))
                 (if err
                     (progn
                       (insert (propertize (char-to-string box-top-left) 'font-lock-face '(:foreground "red")))
                       (insert (propertize box-line-top-bottom 'font-lock-face '(:foreground "red")))
                       (insert (propertize (char-to-string box-top-right) 'font-lock-face '(:foreground "red")) ))
                   (insert box-top-left)
                   (insert box-line-top-bottom)
                   (insert box-top-right))
                 (insert space-between)))
             (insert "\n")))
          (insert-row-bottom
           (lambda (row)
             "Draw the  └───┘└───┘└───┘└───┘ part of the board. "
             (insert space-start)
             (insert space-between)
             (dotimes (col gis-200-column-ct)
               (let ((err (gis-200--get-parse-error-at-cell row col)))
                 (if err
                     (progn (insert (propertize (char-to-string box-bottom-left) 'font-lock-face '(:foreground "red")))
                            (insert (propertize box-line-top-bottom 'font-lock-face '(:foreground "red")))
                            (insert (propertize (char-to-string box-bottom-right) 'font-lock-face '(:foreground "red"))))
                   (insert box-bottom-left)
                   (insert box-line-top-bottom)
                   (insert box-bottom-right))
                 (insert space-between)))
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
             (insert "\n")))
          (insert-row-middle
           (lambda (row box-row)
             "Draw the ⇋|   |⇋|   |⇋|   |⇋|   |⇋ part of the board."
             (insert space-start)
             (cond
              ((= 5 box-row)
               (let ((label (gis-200--col-arrow-label-display 'left 'source row)))
                 (if (equal label " ")
                     (insert space-between)
                   (progn (insert label) (insert arrow-right) (insert ?\s)))))
              ((= 7 box-row)
               (let ((label (gis-200--col-arrow-label-display 'left 'sink row)))
                 (if (equal label " ")
                     (insert space-between)
                   (progn (insert label) (insert arrow-left) (insert ?\s)))))
              (t (insert space-between)))
             (dotimes (col gis-200-column-ct)
               (let ((err (gis-200--get-parse-error-at-cell row col)))
                 (if err
                     (insert (propertize (char-to-string box-vertical) 'font-lock-face '(:foreground "red")))
                   (insert box-vertical))
                 ;; Draw the inner contents of the box
                 (let* ((text (gis-200--get-box-line-content row col box-row))
                        (spacing (make-string (- (length box-inside) (length text)) ?\s)))
                   (if (and (= 11 box-row) err)
                       (let ((err-text (caddr (gis-200--get-parse-error-at-cell row col)))) 
                         (insert err-text)
                         (insert (make-string (- (length box-inside) (length err-text)) ?\s)))
                     (insert (propertize text 'gis-200-box-id (list row col box-row)))
                     (insert (propertize spacing
                                         'gis-200-box-id (list row col box-row)
                                         'gis-200-text-type 'spacing))))
                 (if err
                     (insert (propertize (char-to-string box-vertical) 'font-lock-face '(:foreground "red")))
                   (insert box-vertical)))
               (when (< col (1- gis-200-column-ct))
                 (cond
                  ((= 5 box-row)
                   (progn (insert "  ") (insert arrow-right) (insert "  ")))
                  ((= 7 box-row)
                   (progn (insert "  ") (insert arrow-left) (insert "  ")))
                  (t
                   (insert space-between)))))
             (cond
              ((= 5 box-row)
               (let ((label (gis-200--col-arrow-label-display 'right 'sink row)))
                 (if (equal label " ")
                     (insert space-between)
                   (progn (insert ?\s) (insert arrow-right) (insert label)))))
              ((= 7 box-row)
               (let ((label (gis-200--col-arrow-label-display 'right 'source row)))
                 (if (equal label " ")
                     (insert space-between)
                   (progn (insert ?\s) (insert arrow-left) (insert label)))))
              (t
               (insert space-between)))
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
               (insert (format "%d" row))
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

      ;;; DEBUG INFORMATION
      (insert "\n\n")
      (insert (format "%s" gis-200-parse-errors)))))

(defun gis-200--propertize-errors ()
  "Add text properties to errors."
  (let ((errs gis-200-parse-errors))
    (dolist (err errs)
      (let ((row (caar err))
            (col (cadar err))
            (pt (1- (caddr err))))
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
    (next-line -1))
  (next-line 1)
  (while (gis-200-in-box-p)
    (forward-char -1))
  (forward-char 1))

(defun gis-200--move-to-box (row col)
  (goto-char (point-min))
  (while (let* ((prop (get-text-property (point) 'gis-200-box-id))
                (at-row (car prop))
                (at-col (cadr prop)))
           (and (not (eobp))
            (or (not prop)
                (not (and (= row at-row)
                          (= col at-col))))))
    (forward-char))
  (when (eobp)
    (error "box %d, %d not found" row col)))

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
  (gis-200--in-buffer
   (beginning-of-buffer)))

(defun gis-200--end-of-buffer ()
  ""
  (interactive)
  (gis-200--in-buffer
   (end-of-buffer)))

(defun gis-200--newline ()
  ""
  (interactive)
  (gis-200--in-buffer
   (newline)))

(defun gis-200--refresh-contents ()
  ""
  (interactive)
  (let ((inhibit-read-only t))
    (if (gis-200-in-box-p)
        (gis-200--in-buffer
         (forward-char 0))
      (gis-200-redraw-game-board))))

(defun gis-200-forward-char ()
  (let ((start-pos (point)))
    (if (not (gis-200-in-box-p))
        (while (and (not (= (point-max) (point)))
                    (not (gis-200-in-box-p)))
          (forward-char 1))
      (forward-char 1)
      (when (not (gis-200-in-box-p))
        (let ((column (current-column)))
          (next-line 1)
          (move-to-column column))
        (forward-char (- gis-200-box-width))))
    (let* ((box-id (get-text-property (point) 'gis-200-box-id))
           (row (nth 0 box-id))
           (col (nth 1 box-id))
           (line-no (nth 2 box-id))
           (line-col-no (gis-200-get-line-col-num))
           (new-pt (gis-200--resolve-moved-point row col line-no line-col-no)))
      (if (eql 'end new-pt)
          nil
        (apply #'gis-200--move-to-box-point new-pt)))))

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
      (define-key map (kbd "DEL") #'gis-200--backward-delete-char)
      (define-key map (kbd "SPC") #'gis-200--self-insert-command)
      (define-key map (kbd "RET") #'gis-200--newline)
      (define-key map (kbd "C-c g") #'gis-200--refresh-contents)
      (define-key map (kbd "M-d") #'gis-200--kill-word)
      (define-key map (kbd "C-k") #'gis-200--kill-line)
      (define-key map (kbd "C-a") #'gis-200--move-beginning-of-line)
      (define-key map (kbd "C-e") #'gis-200--move-end-of-line)
      (define-key map (kbd "M-<") #'gis-200--beginning-of-buffer)
      (define-key map (kbd "M->") #'gis-200--end-of-buffer))))

(defconst gis-200-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?| "-" st)
    st)
  "Syntax table for Go mode.")

(defconst gis-200-mode-highlights
  '(("\\<setq\\>" . 'font-lock-function-name-face)
    ("[0-9]+" . (1 'font-lock-constant-face))))

(defun gis-200-start-execution ()
  "Parse gameboard, displaying any errors, and display code execution buffer."
  (interactive)
  (setq gis-200-parse-errors nil)
  (let ((parse-errors))
    (maphash
     (lambda (coords code-text)
       (let ((parse-result (gis-200--parse-assembly code-text)))
         (if (gis-200--parse-error-p parse-result)
             (setq parse-errors (cons (cons coords parse-result) parse-errors)))))
     gis-200-box-contents)
    (when parse-errors
      (setq gis-200-parse-errors parse-errors))
    (let ((inhibit-read-only t))
      (gis-200-redraw-game-board))))

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
  (set-syntax-table gis-200-mode-syntax-table))

(setq gis-200--extra-gameboard-cells
      (gis-200--problem-spec-create :sources (list (gis-200--cell-source-create :row 3 :col 2 :data '(44 55 66)))
                                    :sinks (list (gis-200--cell-sink-create :row 0 :col 4 :expected-data '(1 2)))))



(provide 'gis-200-display)

;;; gis-200-display.el ends here
