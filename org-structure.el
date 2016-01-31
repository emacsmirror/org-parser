;;; org-structure.el -- parse org files to give an appropriate structure.

;;; Commentary:

;;; Code:

(defun org-structure/hash-tables-equal (table1 table2)
  "Return t if TABLE1 and TABLE2 are equal, nil if they arent'."
  (message (format "first table has size %s; second %s"
                   (hash-table-count table1)
                   (hash-table-count table2)))
  (and (= (hash-table-count table1)
          (hash-table-count table2))
       (every (lambda (key)
                (equal (gethash key table1)
                       (gethash key table2)))
              (hash-table-keys table1))))


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
  "Take TEXT -- which is at nesting level LEVEL -- and return the org-structure.

This returns a list of blocks."
  (mapcar (lambda (text-block)
            (org-structure/parse-block text-block (1+ level)))
          (org-structure/get-blocks text level)))

(defun org-structure/parse-block (text-block level)
  "Parse the TEXT-BLOCK, assuming it's at level LEVEL.

This returns a single block."
  (let ((table (make-hash-table))
        (end-of-text (search "\n" text-block)))
    (puthash :text (substring text-block 0 end-of-text) table)
    (puthash :children
             (if end-of-text
                 (mapcar (lambda (child) ;;too many lists in result. Why?
                           (org-structure/parse child level)
                           ;; (let ((child-table (make-hash-table)))
                           ;;   (puthash :text child child-table)
                           ;;   child-table)
                           )
                         (split-string (substring text-block end-of-text)
                                       "\n"
                                       t))
               nil)
             table)
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
                  (format "^%s "
                          asterisks-string))))

;;; org-structure.el ends here
