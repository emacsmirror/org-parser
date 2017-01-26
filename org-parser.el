;;; org-parser.el --- parse org files into structured datatypes.

;; Version: 0.1

;; Copyright (C) 2016-2017 Zachary Kanfer

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;; This file is not part of GNU Emacs.

;; Keywords: languages
;; Homepage: https://bitbucket.org/zck/org-parser.el

;;; Commentary:

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'cl-lib)

;;;###autoload
(defun org-parser/parse-buffer (buffer)
  "Parse BUFFER into a list of structure items."
  (with-current-buffer buffer
    (org-parser/parse-string (buffer-string))))

;;;###autoload
(defun org-parser/parse-file (filename)
  "Parse FILENAME into a list of structure items."
  (with-temp-buffer
    (insert-file-contents filename)
    (org-parser/parse-buffer (current-buffer))))

;;;###autoload
(defun org-parser/parse-string (string)
  "Parse STRING into a list of structure items."
  (org-parser/convert-text-tree (org-parser/make-text-tree (org-parser/split-into-blocks (substring-no-properties string)))))

(defun org-parser/split-into-blocks (text)
  "Split TEXT into blocks; one block for each headline or plain list."
  (let ((current-block "")
        (blocks nil))
    (seq-do (lambda (line)
              (if (org-parser/title-line? line)
                  (progn (unless (equal current-block "")
                           (push current-block blocks))
                         (setq current-block line))
                (setq current-block
                      (if (equal current-block "")
                          line
                        (format "%s\n%s" current-block line)))))
            (split-string text "\n" t))
    (push current-block blocks)
    (reverse blocks)))


(defun org-parser/guess-level (text)
  "Attempts to guess the level of the TEXT.

This method will definitely work for headlines,
and will probably work for plain lists.

The situations where this breaks are where there have been multiple
ordered lists in parents for TEXT, as the bullet for ordered lists
is more than one character."
  (cond ((string-match "\\`\\(\\*+\\) " text)
         (length (match-string 1 text)))
        ((string-match "\\`\\(\s*\\)[-+[:digit:]]" text)
         (1+ (/ (length (match-string 1 text)) 2)))
        (t 1)))


(defun org-parser/bullet-type (full-bullet)
  "Return the bullet-type of FULL-BULLET.

For example, \"** \" has a bullet type of ?*.
Plain lists are the leading symbol (+ or -).
Ordered lists are ?. or ?)"
  (cond ((string-match "\\`\\*+ " full-bullet)
         ?*)
        ((string-match "\\`\s*\\([+-]\\) " full-bullet)
         (elt (match-string 1 full-bullet) 0))
        ((string-match "\\`\s*[[:digit:]]+\\([.)]\\) " full-bullet)
         (elt (match-string 1 full-bullet) 0))))


(defun org-parser/convert-text-tree (text-tree)
  "Convert TEXT-TREE, a list generated by make-text-tree, to a list of org structures."
  (mapcar #'org-parser/convert-text-block
          text-tree))

(defun org-parser/convert-text-block (text-block)
  "Convert TEXT-BLOCK to an org structure.

Return a single structure.  A structure has the following keywords:

:text -- the text on the first line of the block.
:body -- the text on following lines of the block, as a list, where each line
    is represented by a list of items.
    For example:
    * this is the 'text'
      This is the 'body', which can
      continue to multiple lines.

    Results in:
    '((\"This is the 'body', which can\") (\"continue to multiple lines.\"))
:children -- a list of child blocks.
:bullet-type -- a character indicating the type of bullet used,
    either ?*, ?-, ?+, ?., or ?) .  For ordered lists --
    (either ?\) or ?.) -- this is the character /after/ the number.
    For other types of blocks, the bullet is the entire number."
  (let ((table (make-hash-table))
        (text (car text-block)))
    (puthash :text (org-parser/get-text text) table)
    (puthash :body (org-parser/get-body text) table)
    (puthash :bullet-type (org-parser/bullet-type text) table)
    (puthash :children (org-parser/convert-text-tree (cdr text-block)) table)
    table))

(defun org-parser/remove-bullet (text)
  "Return TEXT without the bullet at the beginning."
  (cond ((string-match "\\`\\*+ ?\\(\\(.\\|\n\\)+\\)" text)
         (match-string 1 text))
        ((string-match "\\` *[-+*] ?\\(\\(.\\|\n\\)+\\)" text)
         (match-string 1 text))
        ((string-match "\\` *[[:digit:]]+[.)] ?\\(\\(.\\|\n\\)+\\)" text)
         (match-string 1 text))))

(defun org-parser/parse-for-markup (text)
  "Parse TEXT into its structure, respecting markup.

This handles things like links and italic text.

This will return a list of things.  Each thing in this list will be
either a string (for no markup), or a hash, with a :type key to
indicate what the type of the markup is.

Possible :type values are :link."
  (let ((result-list nil)
        (remaining-text text))
    (if (string-match "\\(.*?\\)\\[\\[\\([^][]+\\)\\]\\[\\([^][]+\\)\\]\\]\\(.*\\)"
                      remaining-text)
        (let* ((text-before-link (match-string 1 text))
               (target-text (match-string 2 text))
               (link-text (match-string 3 text))
               (link-hash (org-parser/make-link-hash target-text link-text))
               (text-after-link (match-string 4 text)))
          (if (string-empty-p text-before-link)
              (cons link-hash
                    (org-parser/parse-for-markup text-after-link))
            (cl-list* text-before-link
                   link-hash
                   (org-parser/parse-for-markup text-after-link))))
      (unless (string-empty-p text) (list text)))))

(defun org-parser/make-link-hash (target-text link-text)
  "Make a link hash pointing to TARGET-TEXT with text LINK-TEXT.

It will have keys :target, :text, and :type.  The :type value will be :link."
  (let ((link-hash (make-hash-table)))
    (puthash :target target-text link-hash)
    (puthash :text link-text link-hash)
    (puthash :type :link link-hash)
    link-hash))

(defun org-parser/get-text (text)
  "Return the first line of TEXT without the bullet, parsed for org formatting.

This is a list of items."
  (let* ((text-without-bullet (org-parser/remove-bullet text))
         (first-line-text (car (split-string text-without-bullet "\n" t))))
    (org-parser/parse-for-markup first-line-text)))

(defun org-parser/get-body (text)
  "Return the body of a given TEXT.

This method will drop initial newlines of TEXT, then treat everything
after a newline as the body.

The body is returned as a list, where each item in the list represents
a line in TEXT.  Each line in TEXT is a list of items itself."

  (let ((lines (split-string text "\n" t)))
    (when (cdr lines)
      (mapcar #'org-parser/parse-for-markup (cdr lines)))))

(defun org-parser/make-text-tree (lines)
  "Organize the given LINES into the overall tree structure.

Return a list that represents the structure of LINES.  Each element is either
a list or a string."
  (when lines
    (let* ((first-line (car lines))
           (first-block (seq-take-while (lambda (line)
                                          (org-parser/descendent? first-line line))
                                        (cdr lines))))
      (cons (cons first-line
                  (org-parser/make-text-tree first-block))
            (org-parser/make-text-tree (seq-drop lines
                                                (+ 1 (length first-block))))))))

(defun org-parser/descendent? (root possible-descendent)
  "Whether ROOT and POSSIBLE-DESCENDENT should be in the same block.

For example, a block that starts '* headline' should be in the same block
 at '** nested', but not the same block as '* another headline.'"
  (if (org-parser/headline? root)
      (or (and (org-parser/headline? possible-descendent)
               (< (org-parser/guess-level root)
                  (org-parser/guess-level possible-descendent)))
          (org-parser/plain-list? possible-descendent))
    (and (org-parser/plain-list? possible-descendent)
         (< (org-parser/guess-level root)
            (org-parser/guess-level possible-descendent)))))

(defun org-parser/title-line? (line)
  "Return whether LINE corresponds to a title line.

A title line is the first line of a headline or plain list."

  (or (org-parser/headline? line)
      (org-parser/plain-list? line)))

(defun org-parser/headline? (line-or-char)
  "Return t if LINE-OR-CHAR is a headline.

LINE-OR-CHAR can be either a line, or the character in a structure
indicating the bullet type."
  (if (characterp line-or-char)
      (equal line-or-char ?*)
    (and (> (length line-or-char)
            0)
         (equal (elt line-or-char 0)
                ?*))))

(defun org-parser/plain-list? (line-or-char)
  "Return t if LINE-OR-CHAR is a plain list.

LINE-OR-CHAR can be either a line, or the character in a structure
indicating the bullet type."
  (if (characterp line-or-char)
      (not (org-parser/headline? line-or-char))
    (and (> (length line-or-char)
            0)
         (or (org-parser/ordered-list? line-or-char)
             (string-match "\\`\s*[-*+] " line-or-char))
         (not (org-parser/headline? line-or-char)))))

(defun org-parser/ordered-list? (line-or-char)
  "Return t if LINE-OR-CHAR is an ordered list.

LINE-OR-CHAR can be either a line, or the character in a structure
indicating the bullet type."
  (if (characterp line-or-char)
      (or (= ?. line-or-char)
          (= ?\) line-or-char))
    (and (string-match "\\` *[[:digit:]]+[.)] " line-or-char) t)))

(defun org-parser/make-bullet (structure parent-bullet older-sibling-count)
  "Return the string representing the bullet for STRUCTURE.

PARENT-BULLET is used to determine indentation.

There should be OLDER-SIBLING-COUNT siblings before this one.  This only matters for ordered lists."
  (cond ((org-parser/headline? (gethash :bullet-type structure))
         ;;we have a headline, so we must be under a headline (right?)
         (if (string-match "\\`\\(\\*+ \\)$" parent-bullet)
             (format "*%s" (match-string 1 parent-bullet))
           "* "))
        ((org-parser/ordered-list? (gethash :bullet-type structure))
         (format "%s%d%c "
                 (org-parser/get-nested-whitespace parent-bullet)
                 (1+ older-sibling-count)
                 (gethash :bullet-type structure)))
        ((org-parser/plain-list? (gethash :bullet-type structure))
         ;;zck plain lists can be under headlines, or under other plain lists
         (if t ;; (org-parser/headline? parent-bullet)
             (format "%s%c "
                     (org-parser/get-nested-whitespace parent-bullet)
                     (gethash :bullet-type structure))))
        (t 'whaaat?)))

(defun org-parser/get-nested-whitespace (bullet)
  "Gets the nested whitespace for a plain list under BULLET.

BULLET can be the bullet for a plain list or a headline."
  (if (org-parser/headline? bullet)
      ""
    (if (string-match "\\`\\(\s*[^\s]+\\)\s" bullet)
        (make-string (1+ (length (match-string 1 bullet)))
                     ?\s)
      "")))

(defun org-parser/to-string (structure-list)
  "Convert STRUCTURE-LIST, a list of structure hash tables, to a string.

This should be identical to the org file parsed to create the structure."
  (org-parser/to-string-helper structure-list ""))

(defun org-parser/to-string-helper (structure-list parent-bullet)
  "Convert STRUCTURE-LIST, a list of structure hash tables, to a string.

These structure hash tables all have the same parent, whose bullet
is PARENT-BULLET.

This should be identical to the org file parsed to create the structure."
  (string-join (cl-mapcar (lambda (structure siblings-before-this-one)
                            (org-parser/single-to-string structure parent-bullet siblings-before-this-one))
                          structure-list
                          (number-sequence 0
                                           (1- (length structure-list))))))

(defun org-parser/single-to-string (structure parent-headline siblings-before-this-one)
  "Create the string for STRUCTURE, with parent having PARENT-HEADLINE.

SIBLINGS-BEFORE-THIS-ONE is the count of older siblings with the same parent."
  (let* ((this-bullet (org-parser/make-bullet structure parent-headline siblings-before-this-one))
         (title-line (format "%s%s"
                             this-bullet
                             (org-parser/format-text (gethash :text structure))))
         (children-text (org-parser/to-string-helper (gethash :children structure)
                                                        this-bullet)))
    (if (gethash :body structure)
        (format "%s\n%s\n%s"
                title-line
                (org-parser/format-body (gethash :body structure))
                children-text)
      (format "%s\n%s" title-line children-text))))

(defun org-parser/format-text (structure-text)
  "Format STRUCTURE-TEXT into a string.

STRUCTURE-TEXT is either a single string (in which case it returns
unchanged), or a list of structure items, in which case this returns a
string that's the formatted representation of the list."
  (if (stringp structure-text)
      structure-text
    (string-join (mapcar #'org-parser/format-text-single-item
                         ;;should this add newlines between items? Probably not. But does that mean that if we have a structure object with a body with multiple things in it, what happens?
                         structure-text))))

(defun org-parser/format-body (body-list)
  "Format the body represented by BODY-LIST.

Each element of BODY-LIST should be a list itself."
  (string-join (mapcar #'org-parser/format-body-line
                       body-list)
               "\n"))

(defun org-parser/format-body-line (body-line)
  "Format BODY-LINE into a string."
  (string-join (mapcar #'org-parser/format-text-single-item
                       body-line)))

(defun org-parser/format-text-single-item (structure-item)
  "Format STRUCTURE-ITEM, a string or hash, into a string."
  (cond ((stringp structure-item)
         structure-item)

        ;;for now, assume it's a link. Obviously this will have to
        ;;change later, for other types of structured text.
        ((hash-table-p structure-item)
         (format "[[%s][%s]]"
                 (gethash :target structure-item)
                 (gethash :text structure-item)))
        (t "")))




(defun org-parser/get-nested-children (structure &rest children-indices)
  "Get children recursively from STRUCTURE; at each level, take the nth child, where n is the next element in CHILDREN-INDICES."
  (if (not children-indices)
      structure
    (apply #'org-parser/get-nested-children
           (elt (gethash :children structure)
                (cl-first children-indices))
           (cl-rest children-indices))))

(defun org-parser/get-bullet (text)
  "Get the bullet form from TEXT, including the space after.

If TEXT does not start with a bullet form, this will error."
  (cond ((string-match "\\`\\(\\*+ \\)" text)
         (match-string 1 text))
        ((string-match "\\`\\(\s*[+-] \\)" text)
         (match-string 1 text))
        ((string-match "\\`\\(\s*[[:digit:]]+[.)]\s\\)" text)
         (match-string 1 text))
        (t (error "Error calling org-parser/get-bullet on a string that doesn't have bullets"))))

(provide 'org-parser)

;;; org-parser.el ends here
