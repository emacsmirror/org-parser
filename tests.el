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


;;;; org structure tests

(ert-deftest single-line-has-only-one-block ()
  (should (equal 1
                 (length (org-structure "* header")))))

(ert-deftest single-line-should-be-ok ()
  (should (org-structure/hash-tables-equal #s(hash-table data (:text "header"
                                                                     :children nil))
                                           (car (org-structure "* header")))))

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

(ert-deftest child-block ()
  (should (org-structure/hash-tables-equal #s(hash-table data (:text "I'm a child!"
                                                                     :children nil))
                                           (car (gethash :children
                                                         (car (org-structure "* ignored\n** I'm a child!")))))))

;;;; get-blocks tests

(ert-deftest get-blocks-one-block ()
  (should (equal '("just one block")
                 (org-structure/get-blocks "* just one block"
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

;;; tests.el ends here
