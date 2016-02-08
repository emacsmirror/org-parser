;;; tests.el -- tests for org-structure

;;; Commentary:

;;; Code:

;;;; hash table tests

(ert-deftest empty-hash-tables-are-equal ()
  (should (org-structure/hash-tables-equal (make-hash-table)
                                           (make-hash-table))))

(ert-deftest one-item-hash-tables-are-equal ()
  (should (org-structure/hash-tables-equal #s(hash-table data (1 2))
                                           #s(hash-table data (1 2)))))

(ert-deftest hash-tables-different-keys-different ()
  (should-not (org-structure/hash-tables-equal #s(hash-table data (1 2))
                                               #s(hash-table data (2 2)))))

(ert-deftest hash-tables-different-values-different ()
  (should-not (org-structure/hash-tables-equal #s(hash-table data (1 2))
                                               #s(hash-table data (1 3)))))

(ert-deftest hash-tables-first-subset-second-different ()
  (should-not (org-structure/hash-tables-equal #s(hash-table data (1 2))
                                               #s(hash-table data (1 2 4 5)))))

(ert-deftest hash-tables-second-subset-first-different ()
  (should-not (org-structure/hash-tables-equal #s(hash-table data (1 2 4 5))
                                               #s(hash-table data (1 2)))))

(ert-deftest two-item-hash-tables-are-equal ()
  (should (org-structure/hash-tables-equal #s(hash-table data (1 2 4 5))
                                           #s(hash-table data (1 2 4 5)))))

(ert-deftest subset/empty-tables-are-subset ()
  (should (org-structure/hash-table-subset (make-hash-table)
                                           (make-hash-table))))

(ert-deftest subset/empty-table-subset-of-table-with-data ()
  (should (org-structure/hash-table-subset (make-hash-table)
                                           #s(hash-table data (1 2 3 4)))))

(ert-deftest subset/equal-tables-are-subsets ()
  (should (org-structure/hash-table-subset #s(hash-table data (1 2 3 4))
                                           #s(hash-table data (1 2 3 4)))))

(ert-deftest subset/simple-subset ()
  (should (org-structure/hash-table-subset #s(hash-table data (1 2))
                                           #s(hash-table data (1 2 3 4)))))


;;;; org structure tests

(ert-deftest single-line-has-only-one-block ()
  (should (equal 1
                 (length (org-structure "* header")))))

(ert-deftest single-line-text ()
  (should (equal "header"
                 (gethash :text (car (org-structure "* header"))))))

(ert-deftest single-line-children ()
  (should-not (gethash :children (car (org-structure "* header")))))

(ert-deftest single-line-level ()
  (should (equal 1
                 (gethash :level (car (org-structure "* header"))))))

(ert-deftest single-line-bullet ()
  (should (equal ?*
                 (gethash :bullet (car (org-structure "* header"))))))

(ert-deftest with-newline-single-line-text ()
  (should (equal "header"
                 (gethash :text (car (org-structure "* header\n"))))))

(ert-deftest with-newline-single-line-children ()
  (should-not (gethash :children (car (org-structure "* header\n")))))

(ert-deftest with-newline-single-line-level ()
  (should (equal 1
                 (gethash :level (car (org-structure "* header\n"))))))

(ert-deftest with-newline-single-line-bullet ()
  (should (equal ?*
                 (gethash :bullet (car (org-structure "* header\n"))))))


(ert-deftest children-dont-create-new-block ()
  (should (equal 1
                 (length (org-structure "* header\n** nested")))))

(ert-deftest two-children ()
  (should (equal 2
                 (length (gethash :children
                                  (car (org-structure "* header\n** first child\n** second child")))))))

(ert-deftest two-blocks ()
  (should (equal 2
                 (length (org-structure "* header\n* nested")))))



(ert-deftest child-block-single-line-text ()
  (should (equal "I'm a child!"
                 (gethash :text (car (gethash :children (elt (org-structure "* ignored\n** I'm a child!") 0)))))))

(ert-deftest child-block-single-line-children ()
  (should-not (gethash :children (car (gethash :children (elt (org-structure "* ignored\n** I'm a child!") 0))))))

(ert-deftest child-block-single-line-level ()
  (should (equal 2
                 (gethash :level (car (gethash :children (elt (org-structure "* ignored\n** I'm a child!") 0)))))))

(ert-deftest child-block-single-line-bullet ()
  (should (equal ?*
                 (gethash :bullet (car (gethash :children (elt (org-structure "* ignored\n** I'm a child!") 0)))))))


(ert-deftest second-child-block-single-line-text ()
  (should (equal "I'm the younger, forgotten child."
                 (gethash :text (elt (gethash :children (elt (org-structure "* ignored\n** I'm a child!\n** I'm the younger, forgotten child.") 0))
                                     1)))))

(ert-deftest second-child-block-single-line-children ()
  (should-not (gethash :children (elt (gethash :children (elt (org-structure "* ignored\n** I'm a child!\n** I'm the younger, forgotten child.") 0))
                                      1))))

(ert-deftest second-child-block-single-line-level ()
  (should (equal 2
                 (gethash :level (elt (gethash :children (elt (org-structure"* ignored\n** I'm a child!\n** I'm the younger, forgotten child.") 0))
                                      1)))))

(ert-deftest second-child-block-single-line-bullet ()
  (should (equal ?*
                 (gethash :bullet (elt (gethash :children (elt (org-structure "* ignored\n** I'm a child!\n** I'm the younger, forgotten child.") 0))
                                       1)))))



(ert-deftest thirdly-nested-child-blocks ()
  (should (equal 2
                 (length (gethash :children
                                  (elt (gethash :children
                                                (elt (org-structure "* header\n* second header\n** first child\n*** I'm forgotten about\n** second child\n*** this is the test grandchild\n*** this is the other test grandchild.")
                                                     1))
                                       1))))))


(ert-deftest third-nested-child-block-single-line-text ()
  (should (equal "this is the other test grandchild."
                 (gethash :text
                          (elt (gethash :children
                                        (elt (gethash :children
                                                      (elt (org-structure "* header\n* second header\n** first child\n*** I'm forgotten about\n** second child\n*** this is the test grandchild\n*** this is the other test grandchild.")
                                                           1))
                                             1))
                               1)))))

(ert-deftest third-nested-child-block-single-line-children ()
  (should-not (gethash :children
                       (elt (gethash :children
                                        (elt (gethash :children
                                                      (elt (org-structure "* header\n* second header\n** first child\n*** I'm forgotten about\n** second child\n*** this is the test grandchild\n*** this is the other test grandchild.")
                                                           1))
                                             1))
                               1))))

(ert-deftest third-nested-child-block-single-line-level ()
  (should (equal 3
                 (gethash :level
                          (elt (gethash :children
                                        (elt (gethash :children
                                                      (elt (org-structure "* header\n* second header\n** first child\n*** I'm forgotten about\n** second child\n*** this is the test grandchild\n*** this is the other test grandchild.")
                                                           1))
                                             1))
                               1)))))

(ert-deftest third-nested-child-block-single-line-bullet ()
  (should (equal ?*
                 (gethash :bullet
                          (elt (gethash :children
                                        (elt (gethash :children
                                                      (elt (org-structure "* header\n* second header\n** first child\n*** I'm forgotten about\n** second child\n*** this is the test grandchild\n*** this is the other test grandchild.")
                                                           1))
                                             1))
                               1)))))


;;;; get-blocks tests

(ert-deftest get-blocks-one-block ()
  (should (equal '("just one block")
                 (org-structure/get-blocks "* just one block"
                                           1))))

(ert-deftest get-blocks-newline-at-eof-is-ok ()
  (should (equal '("just one block")
                 (org-structure/get-blocks "* just one block\n"
                                           1))))

(ert-deftest get-blocks-two-block ()
  (should (equal '("first block" "second block")
                 (org-structure/get-blocks "* first block\n* second block"
                                           1))))

(ert-deftest get-blocks-keeps-children ()
  (should (equal '("first block\n** nested" "second block")
                 (org-structure/get-blocks "* first block\n** nested\n* second block"
                                           1))))

(ert-deftest get-blocks-second-level ()
  (should (equal '("first block" "second block")
                 (org-structure/get-blocks "** first block\n** second block"
                                           2))))

(ert-deftest get-blocks-second-level-keeps-children ()
  (should (equal '("first block\n*** nested" "second block")
                 (org-structure/get-blocks "** first block\n*** nested\n** second block"
                                           2))))

(ert-deftest get-blocks-ignores-way-nested-children ()
  (should (equal '("first block\n*** nested\n**** way nested" "second block")
                 (org-structure/get-blocks "** first block\n*** nested\n**** way nested\n** second block"
                                           2))))

(ert-deftest get-bullet/one-level-headline ()
  (should (equal "* "
                 (org-structure/get-bullet "* headline\n"))))

(ert-deftest get-bullet/two-level-headline ()
  (should (equal "** "
                 (org-structure/get-bullet "** headline\n"))))

(ert-deftest get-bullet/three-level-headline ()
  (should (equal "*** "
                 (org-structure/get-bullet "*** headline\n"))))

(ert-deftest get-bullet/looks-at-first-line ()
  (should (equal "** "
                 (org-structure/get-bullet "** headline\n*** second thing\n"))))

(ert-deftest get-bullet/headline-with-initial-space-fails ()
  (should-error (org-structure/get-bullet " ** not matching!")))

(ert-deftest get-bullet/headline-with-no-endingspace-fails ()
  (should-error (org-structure/get-bullet "**not matching!")))

(ert-deftest get-bullet/headline-with-no-bullets-fails ()
  (should-error (org-structure/get-bullet "not matching!")))

(ert-deftest get-bullet/plain-list-simple ()
  (should (equal "- "
                 (org-structure/get-bullet "- asrt\n"))))

(ert-deftest get-bullet/plain-list-single-nested ()
  (should (equal "  - "
                 (org-structure/get-bullet "  - asrt\n"))))

(ert-deftest get-bullet/plain-list-doubly-nested ()
  (should (equal "    - "
                 (org-structure/get-bullet "    - asrt\n"))))

(ert-deftest get-bullet/plain-list-nested-plus ()
  (should (equal "  + "
                 (org-structure/get-bullet "  + asrt\n"))))

(ert-deftest get-bullet/ordered-list-period ()
  (should (equal "1. "
                 (org-structure/get-bullet "1. what"))))

(ert-deftest get-bullet/ordered-list-period ()
  (should (equal "4) "
                 (org-structure/get-bullet "4) what"))))

(ert-deftest get-bullet/ordered-list-nested-period ()
  (should (equal "  2. "
                 (org-structure/get-bullet "  2. what"))))

(ert-deftest get-bullet/ordered-list-nested-paren ()
  (should (equal "  1) "
                 (org-structure/get-bullet "  1) what"))))

(ert-deftest get-bullet/ordered-list-two-digits ()
    (should (equal "  42) "
                 (org-structure/get-bullet "  42) what"))))

(ert-deftest get-bullet/plain-list-dash-after-ordered-list ()
  (should (equal "   - "
                 (org-structure/get-bullet "   - the /parent/ of this node is an ordered list, so there are *three* spaces"))))


;;;; to-string tests

(ert-deftest to-string/just-one-block ()
  (should (equal "* header\n"
                 (org-structure/to-string '(#s(hash-table data (:text "header"
                                                                      :children nil
                                                                      :level 1
                                                                      :bullet ?*)))))))

;;;; tests that go from a string to a structure to a string
(ert-deftest to-structure-to-string/just-one-block ()
  (should (equal "* header\n"
                 (org-structure/to-string (org-structure "* header\n")))))

(ert-deftest to-structure-to-string/simply-nested ()
  (should (equal "* header\n** nested\n"
                 (org-structure/to-string (org-structure "* header\n** nested\n")))))

(ert-deftest to-structure-to-string/two-children ()
  (should (equal "* header\n** first child\n** second child\n"
                 (org-structure/to-string (org-structure "* header\n** first child\n** second child\n")))))

(ert-deftest to-structure-to-string/two-blocks ()
  (should (equal "* header\n* second\n"
                 (org-structure/to-string (org-structure "* header\n* second\n")))))

(ert-deftest to-structure-to-string/three-levels ()
  (should (equal "* header\n* second header\n** first child\n*** I'm forgotten about\n** second child\n*** this is the test grandchild\n*** this is the other test grandchild.\n"
                 (org-structure/to-string (org-structure "* header\n* second header\n** first child\n*** I'm forgotten about\n** second child\n*** this is the test grandchild\n*** this is the other test grandchild.\n")))))


(ert-deftest nested-children/no-indices ()
  (should (org-structure/hash-tables-equal #s(hash-table data (:children 'children :text "whatever"))
                                           (org-structure/get-nested-children #s(hash-table data (:children 'children :text "whatever"))))))

(ert-deftest nested-children/single-index ()
  (should (equal 3
                 (org-structure/get-nested-children #s(hash-table data (:children (2 3 4)))
                                                    1))))

(ert-deftest nested-children/missing-index ()
  (should-not (org-structure/get-nested-children #s(hash-table data (:children (2 3 4)))
                                                 14)))

(ert-deftest nested-children/too-many-indices ()
  (should-not (org-structure/get-nested-children #s(hash-table data (:children (2 #s(hash-table data (:children nil :text "whatever")) 4)))
                                                 1 1)))

(ert-deftest nested-children/two-indices ()
  (should (equal :im-nested
                 (org-structure/get-nested-children #s(hash-table data (:children (2 #s(hash-table data (:children (0 1 :im-nested))))))
                                                    1 2))))

;;; tests.el ends here
