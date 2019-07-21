
(defun uniq-lines (beg end)
  "Unique lines in region.
Called from a program, there are two arguments:
BEG and END (region to sort)."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (while (not (eobp))
        (kill-line 1)
        (yank)
        (let ((next-line (point)))
          (while
              (re-search-forward
               (format "^%s" (regexp-quote (car kill-ring))) nil t)
            (replace-match "" nil nil))
          (goto-char next-line))))))

(defun unfill-region (beg end)
  "Unfill the region, joining text paragraphs into a single
    logical line.  This is useful, e.g., for use with
    `visual-line-mode'."
  (interactive "*r")
  (let ((fill-column (point-max)))
    (fill-region beg end)))

(defun key-binding-at-point (key)
  (mapcar (lambda (keymap) (when (keymapp keymap)
                             (lookup-key keymap key)))
          (list
           ;; More likely
           (get-text-property (point) 'keymap)
           (mapcar (lambda (overlay)
                     (overlay-get overlay 'keymap))
                   (overlays-at (point)))
           ;; Less likely
           (get-text-property (point) 'local-map)
           (mapcar (lambda (overlay)
                     (overlay-get overlay 'local-map))
                   (overlays-at (point))))))

(defun locate-key-binding (key)
  "Determine in which keymap KEY is defined."
  (interactive "kPress key: ")
  (let ((ret
         (list
          (key-binding-at-point key)
          (minor-mode-key-binding key)
          (local-key-binding key)
          (global-key-binding key))))
    (when (called-interactively-p 'any)
      (message "At Point: %s\nMinor-mode: %s\nLocal: %s\nGlobal: %s"
               (or (nth 0 ret) "")
               (or (mapconcat (lambda (x) (format "%s: %s" (car x) (cdr x)))
                              (nth 1 ret) "\n             ")
                   "")
               (or (nth 2 ret) "")
               (or (nth 3 ret) "")))
    ret))

;; kill buffers which file does not exists
(defun s9g-kill-orphan-buffers ()
  (interactive)
  (loop for buf in (buffer-list) do
        (let ((bfn (buffer-file-name buf)))
          (if (and bfn
                   (not (file-exists-p bfn)))
              (kill-buffer buf)))))


(defun indent-by (ind &optional beg end)
  (interactive)
  (when (or (and beg end) (region-active-p))
    (let* ((deactivate-mark)
           (beg (or beg (region-beginning)))
           (end (or end (region-end)))
           (mark-after (region-active-p))
           (bline (line-number-at-pos beg))
           (eline (line-number-at-pos end)))
      (flet ((getlinepos (line)
                         (save-excursion
                           (goto-line line)
                           (beginning-of-line)
                           (point))))
        (indent-rigidly (getlinepos bline)
                        (getlinepos eline) ind)
        (when mark-after
          (push-mark (getlinepos bline))
          (goto-char (getlinepos eline))
          (activate-mark))))))

(defun s9g-indent-up ()
  (interactive)
  (indent-by 1))

(defun s9g-indent-down ()
  (interactive)
  (indent-by -1))

(defun display-buffer-and-raise (buffer &optional action frame)
  (let ((window (display-buffer buffer action frame)))
    (when window
      (select-window window)
      (select-frame-set-input-focus (window-frame window)))))

(defun display-buffer-pop-up-window-all-frames (buffer alist)
  "Display BUFFER by popping up new window in any frame.
Cycles over all available frames and call
`display-buffer-pop-up-window' in each frame, returns first
non-nil result.  ALIST is passed down to
`display-buffer-pop-up-window'"
  (let* ((curr-frame (selected-frame))
         (frames (cons curr-frame (remove curr-frame (frame-list)))))
    (or (loop for frame in frames do
              (select-frame frame)
              (let ((ret (display-buffer-pop-up-window buffer alist)))
                (when ret (return ret))))
        (progn
          (select-frame curr-frame)     ; select originally selected
                                        ; frame to save editor's state
          '()))
    ))

(defun whack-whitespace (arg)
  "Delete all white space from point to the next word.  With prefix ARG
    delete across newlines as well.  The only danger in this is that you
    don't have to actually be at the end of a word to make it work.  It
    skips over to the next whitespace and then whacks it all to the next
    word."
  (interactive "P")
  (let ((regexp (if arg "[ \t\n]+" "[ \t]+")))
    (re-search-forward regexp nil t)
    (replace-match "" nil nil)))

(defun smart-delete-forward (arg)
  (interactive "P")
  (let* ((c (string (char-after)))
         (spacep (string-match-p "\\s-" c)))
    (if spacep
        (whack-whitespace arg)
      (kill-word arg))))

(defun delete-horizontal-and-surround-space ()
  (interactive)
  (delete-horizontal-space)
  (insert " ")
  (save-excursion
    (insert " ")))


(defun comment-dwim-line (&optional arg)
  "Replacement for the comment-dwim command.
        If no region is selected and current line is not blank and we are not at the end of the line,
        then comment current line.
        Replaces default behaviour of comment-dwim, when it inserts comment at the end of the line."
  (interactive "*P")
  (comment-normalize-vars)
  (if (not (region-active-p))
      (comment-or-uncomment-region (line-beginning-position) (line-end-position))
    (comment-dwim arg)))

(defun kill-line-or-region (&optional lines-count)
  "Kill whole line if region is not selected. Kill region otherwise."
  (interactive "p")
  (if mark-active
      (delete-region (region-beginning)
                     (region-end))
    (kill-whole-line lines-count)))

(defun kill-comment-region ()
  (interactive)
  (if (region-active-p)
      (save-excursion
        (let* ((beg (line-number-at-pos (region-beginning)))
               (end (line-number-at-pos (region-end)))
               (dif (- end beg)))
          (goto-char (region-beginning))
          (comment-kill dif)))
    (comment-kill 1)))

(defun split-window-sensibly-reversed (&optional window)
  "Same as split-window-sensibly but with reversed logic about
  horizontal/vertical split."
  (let ((window (or window (selected-window))))
    (or (and (window-splittable-p window t)
             ;; Split window horizontally.
             (with-selected-window window
               (split-window-right)))
        (and (window-splittable-p window)
             ;; Split window vertically.
             (with-selected-window window
               (split-window-below)))
        (and (eq window (frame-root-window (window-frame window)))
             (not (window-minibuffer-p window))
             ;; If WINDOW is the only window on its frame and is not the
             ;; minibuffer window, try to split it vertically disregarding
             ;; the value of `split-height-threshold'.
             (let ((split-height-threshold 0))
               (when (window-splittable-p window)
                 (with-selected-window window
                   (split-window-below))))))))


(defun fit-window-to-buffer-width (&optional window max-width min-width)
  "Fit WINDOW according to its buffer's width.
WINDOW, MAX-WIDTH and MIN-WIDTH have the same meaning as in
`fit-window-to-buffer'."
  (interactive)
  (let ((fit-window-to-buffer-horizontally 'only))
    (fit-window-to-buffer window nil nil max-width min-width)))

(defun fit-window-to-buffer-height (&optional window max-height min-height)
  "Fit WINDOW according to its buffer's height.
WINDOW, MAX-HEIGHT and MIN-HEIGHT have the same meaning as in
`fit-window-to-buffer'."
  (interactive)
  (let ((fit-window-to-buffer-horizontally nil))
    (fit-window-to-buffer window max-height min-height nil nil)))

(defun fit-window-to-buffer-height-or-width
    (&optional window max-height min-height max-width min-width)
  "Fit WINDOW according to its buffer's height and width.
WINDOW, MAX-HEIGHT, MIN-HEIGHT, MAX-WIDTH and MIN-WIDTH have the same meaning
as in `fit-window-to-buffer'."
  (interactive)
  (let ((fit-window-to-buffer-horizontally t))
    (fit-window-to-buffer window max-height min-height max-width min-width)))

(defun insert-commented-keyword (field)
  (interactive)
  (comment-dwim nil)
  (unless (looking-at (format "%s:" field))
    (insert (format " %s: " field)))
  (progn
    (delete-trailing-whitespace)
    (end-of-line)
    (insert " ")))

(defun aw-switch-relative-window (newidx)
  (let* ((w (selected-window))
         (wlist (aw-window-list))
         (idx (cl-position w wlist))
         (prev-w (nth (funcall newidx idx) wlist)))
    (unless (null prev-w)
      (aw-switch-to-window prev-w))))

(defun aw-switch-prev-window ()
  (interactive)
  (aw-switch-relative-window
   (lambda (idx)
     (max 0 (- idx 1)))))

(defun aw-switch-next-window ()
  (interactive)
  (aw-switch-relative-window
   (lambda (idx)
     (+ idx 1))))

(provide 's9-defuns)
