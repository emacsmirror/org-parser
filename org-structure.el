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
  "Parse TEXT-BLOCK -- a single block with CURRENT-BULLET at level LEVEL.

Return a single block."
  (let ((table (make-hash-table))
        (end-of-text (search "\n" text-block))
        (full-bullet (org-structure/get-bullet text-block)))
    (puthash :text (substring text-block
                              (length full-bullet)
                              end-of-text)
             table)
    (puthash :children
             (if (and end-of-text
                      (< (1+ end-of-text)
                         (length text-block)))
                 (org-structure/parse (substring text-block end-of-text) (1+ level))
               nil)
             table)
    (puthash :level level table)
    (puthash :bullet (org-structure/bullet-type full-bullet) table)
    table))

(defun org-structure/bullet-type (full-bullet)
  "Return the bullet-type of FULL-BULLET.

For example, \"** \" has a bullet type of ?*.
Plain lists are the leading symbol (+ or -).
Ordered lists are ?#"
  (cond ((string-match "^\\*+ " full-bullet)
         ?*)
        ((string-match "^\s*\\([+-]\\) " full-bullet)
         (elt (match-string 1 full-bullet) 0))
        ((string-match "^\s*[[:digit:]]+\\([.)]\\) " full-bullet)
         (elt (match-string 1 full-bullet) 0))))

(defun org-structure/get-blocks (text level)
  "Get the blocks for TEXT, at nesting level LEVEL.

For example:

* first
** second
* back to first

would have two blocks at nesting level one; the first block having two lines, and the second: one."
  (let ((bullet (org-structure/get-bullet text)))
    (mapcar (lambda (block-without-substring)
              (format "%s%s" bullet block-without-substring))
         (split-string text
                       (regexp-quote (format "\n%s" bullet))
                       t
                       (format "\\(%s\\|\n\\)" (regexp-quote bullet))))))

(defun org-structure/to-string (structure-list)
  "Convert STRUCTURE-LIST, a list of structure hash tables, to a string.

This should be identical to the org file parsed to create the structure."
  (string-join (mapcar #'org-structure/single-to-string
                       structure-list)))

(defun org-structure/single-to-string (structure)
  "Convert STRUCTURE, a single structure hash table, to a string.

This should be identical to the org file parsed to create the structure."
  (format "%s %s\n%s"
          (make-string (gethash :level structure)
                       (gethash :bullet structure))
          (gethash :text structure)
          (org-structure/to-string (gethash :children structure))))

(defun org-structure/get-nested-children (structure &rest children-indices)
  "Get children recursively from STRUCTURE; at each level, take the nth child, where n is the next element in CHILDREN-INDICES."
  (if (not children-indices)
      structure
    (apply #'org-structure/get-nested-children
           (elt (gethash :children structure)
                (first children-indices))
           (rest children-indices))))

(defun org-structure/get-bullet (text)
  "Get the bullet form from TEXT, including the space after.

If TEXT does not start with a bullet form, this will error."
  (cond ((string-match "^\\(\\*+ \\)" text)
         (match-string 1 text))
        ((string-match "^\\(\s*[+-] \\)" text)
         (match-string 1 text))
        ((string-match "^\\(\s*[[:digit:]]+[.)]\s\\)" text)
         (match-string 1 text))
        (t (error "Error calling org-structure/get-bullet on a string that doesn't have bullets"))))

;;; org-structure.el ends here
