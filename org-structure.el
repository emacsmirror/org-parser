;;; org-structure.el -- parse org files to give an appropriate structure.

;;; Commentary:

;;; Code:

(defun org-structure/hash-tables-equal (table1 table2)
  "Return t if TABLE1 and TABLE2 are equal, nil if they arent'."
  (and (= (hash-table-count table1)
          (hash-table-count table2))
       (every (lambda (key)
                (equal (gethash key table1)
                       (gethash key table2)))
              (hash-table-keys table1))))

(defun org-structure/hash-table-subset (subset superset)
  "Return t if every (key, value) pair in SUBSET is also in SUPERSET."
  (every (lambda (key)
           (equal (gethash key superset)
                  (gethash key subset)))
         (hash-table-keys subset)))


(defun org-structure-buffer (buffer)
  "Return the org structure of BUFFER."
  (with-current-buffer buffer
    (org-structure (buffer-string))))

(defun org-structure-file (filename)
  "Return the org structure of FILENAME."
  (with-temp-buffer
    (insert-file-contents filename)
    (org-structure-buffer (current-buffer))))

(defun org-structure (text)
  "Return the org structure of TEXT."
  (org-structure/parse text 1))

(defun org-structure/parse (text level)
  "Return the org-structure of TEXT, a bunch of blocks of nesting level LEVEL.

This returns a list of blocks."
  (mapcar (lambda (text-block)
            (org-structure/parse-block text-block level))
          (org-structure/get-blocks text level)))

(defun org-structure/parse-block (text-block level)
  "Parse the TEXT-BLOCK -- a single block -- which is at level LEVEL.

Return a single block."
  (let ((table (make-hash-table))
        (end-of-text (search "\n" text-block)))
    (puthash :text (substring text-block 0 end-of-text) table)
    (puthash :children
             (if (and end-of-text
                      (< (1+ end-of-text)
                         (length text-block)))
                 (org-structure/parse (substring text-block end-of-text) (1+ level))
               nil)
             table)
    (puthash :level level table)
    table))

(defun org-structure/get-blocks (text level)
  "Get the blocks for TEXT, at nesting level LEVEL.

For example:

* first
** second
* back to first

would have two blocks at nesting level one; the first block having two lines, and the second: one."
  (let ((asterisks-string (format "\\*\\{%s\\}" level)))
    (split-string text
                  (format "\n%s "
                          asterisks-string)
                  t
                  (format "\\(^%s \\|\n\\)"
                          asterisks-string))))

;;; org-structure.el ends here
