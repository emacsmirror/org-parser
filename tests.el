;;; tests.el -- tests for org-structure

;;; Commentary:

;;; Code:


;;;; org structure tests

(ert-deftest single-line-has-only-one-block ()
  (should (equal 1
                 (length (org-structure "* header")))))

(ert-deftest single-line-text ()
  (should (equal "header"
                 (gethash :text (car (org-structure "* header"))))))

(ert-deftest single-line-children ()
  (should-not (gethash :children (car (org-structure "* header")))))

(ert-deftest single-line-bullet-type ()
  (should (equal ?*
                 (gethash :bullet-type (car (org-structure "* header"))))))

(ert-deftest single-plain-list-has-only-one-block ()
  (should (equal 1
                 (length (org-structure "- header")))))

(ert-deftest single-plain-list-text ()
  (should (equal "header"
                 (gethash :text (car (org-structure "- header"))))))

(ert-deftest single-plain-list-children ()
  (should-not (gethash :children (car (org-structure "- header")))))

(ert-deftest single-plain-list-bullet-type ()
  (should (equal ?-
                 (gethash :bullet-type (car (org-structure "- header"))))))

(ert-deftest nested-headline-bullet-type ()
  (should (equal ?*
                 (gethash :bullet-type (car (org-structure "** header"))))))

(ert-deftest nested-plain-list-bullet-type ()
  (should (equal ?+
                 (gethash :bullet-type (car (org-structure "   + header"))))))

(ert-deftest ordered-list-bullet-type ()
  (should (equal ?.
                 (gethash :bullet-type (car (org-structure "14. header"))))))

(ert-deftest nested-ordered-list-bullet-type ()
  (should (equal ?.
                 (gethash :bullet-type (car (org-structure "  3. header"))))))

(ert-deftest with-newline-single-line-text ()
  (should (equal "header"
                 (gethash :text (car (org-structure "* header\n"))))))

(ert-deftest with-newline-single-line-children ()
  (should-not (gethash :children (car (org-structure "* header\n")))))

(ert-deftest with-newline-single-line-bullet-type ()
  (should (equal ?*
                 (gethash :bullet-type (car (org-structure "* header\n"))))))


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
                 (gethash :text (org-structure/get-nested-children (car (org-structure "* ignored\n** I'm a child!"))
                                                                   0)))))

(ert-deftest child-block-single-line-children ()
  (should-not (gethash :children (org-structure/get-nested-children (car (org-structure "* ignored\n** I'm a child!"))
                                                                    0))))

(ert-deftest child-block-single-line-bullet-type ()
  (should (equal ?*
                 (gethash :bullet-type (org-structure/get-nested-children (car (org-structure "* ignored\n** I'm a child!"))
                                                                          0)))))



(ert-deftest second-child-block-single-line-text ()
  (should (equal "I'm the younger, forgotten child."
                 (gethash :text (org-structure/get-nested-children (car (org-structure "* ignored\n** I'm a child!\n** I'm the younger, forgotten child."))
                                                                   1)))))

(ert-deftest second-child-block-single-line-children ()
  (should-not (gethash :children (org-structure/get-nested-children (car (org-structure "* ignored\n** I'm a child!\n** I'm the younger, forgotten child."))
                                                                    1))))

(ert-deftest second-child-block-single-line-bullet-type ()
  (should (equal ?*
                 (gethash :bullet-type (org-structure/get-nested-children (car (org-structure "* ignored\n** I'm a child!\n** I'm the younger, forgotten child."))
                                                                          1)))))



(ert-deftest thirdly-nested-child-blocks ()
  (should (equal 2
                 (length (gethash :children
                                  (org-structure/get-nested-children (elt (org-structure "* header\n* second header\n** first child\n*** I'm forgotten about\n** second child\n*** this is the test grandchild\n*** this is the other test grandchild.")
                                                                          1)
                                                                     1))))))


(ert-deftest third-nested-child-block-single-line-text ()
  (should (equal "this is the other test grandchild."
                 (gethash :text
                          (org-structure/get-nested-children (elt (org-structure "* header\n* second header\n** first child\n*** I'm forgotten about\n** second child\n*** this is the test grandchild\n*** this is the other test grandchild.")
                                                                  1)
                                                             1
                                                             1)))))

(ert-deftest third-nested-child-block-single-line-children ()
  (should-not (gethash :children
                       (org-structure/get-nested-children (elt (org-structure "* header\n* second header\n** first child\n*** I'm forgotten about\n** second child\n*** this is the test grandchild\n*** this is the other test grandchild.")
                                                               1)
                                                          1
                                                          1))))

(ert-deftest third-nested-child-block-single-line-bullet-type ()
  (should (equal ?*
                 (gethash :bullet-type
                          (org-structure/get-nested-children (elt (org-structure "* header\n* second header\n** first child\n*** I'm forgotten about\n** second child\n*** this is the test grandchild\n*** this is the other test grandchild.")
                                                                  1)
                                                             1
                                                             1)))))



(ert-deftest plain-child-lists-of-mixed-types-are-blocked-properly ()
  (should (equal 1
                 (length (gethash :children
                                  (car (org-structure "* whatever\n1. what\n   - the /parent/ of this node is an ordered list, so there are *three* spaces")))))))

(ert-deftest indented-ordered-lists-are-blocked-properly ()
  (should (equal 1
                 (length (org-structure "* whatever\n1. what\n   - the /parent/ of this node is an ordered list, so there are *three* spaces")))))

(ert-deftest lists-of-mixed-types-are-blocked-properly ()
  (should (equal 2
                 (length (gethash :children
                                  (car (org-structure "* top\n- first level\n** also first level")))))))

(ert-deftest headline-with-body-text-has-correct-text ()
  (should (equal "I'm in the body"
                 (gethash :body
                          (car (org-structure "* I'm the headline\nI'm in the body"))))))

(ert-deftest headline-with-body-text-has-no-siblings ()
  (should (equal 1
                 (length (org-structure "* I'm the headline\nI'm in the body")))))

(ert-deftest headline-with-body-text-has-no-children ()
  (should (equal 0
                 (length (gethash :children (car (org-structure "* I'm the headline\nI'm in the body")))))))


;;;; get-bullet tests
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

(ert-deftest get-bullet/ordered-list-with-child ()
  (should (equal "1. "
                 (org-structure/get-bullet "1. what\n   - the /parent/ of this node is an ordered list, so there are *three* spaces"))))



(ert-deftest make-bullet/simple-headline ()
  (should (equal "* "
                 (org-structure/make-bullet #s(hash-table data (:text "whatever" :bullet-type ?* :children nil))
                                            ""
                                            0))))

(ert-deftest make-bullet/already-nested-headline ()
  (should (equal "** "
                 (org-structure/make-bullet #s(hash-table data (:text "whatever" :bullet-type ?* :children nil))
                                            "* "
                                            0))))

(ert-deftest make-bullet/doubly-nested-headline ()
  (should (equal "*** "
                 (org-structure/make-bullet #s(hash-table data (:text "whatever" :bullet-type ?* :children nil))
                                            "** "
                                            0))))

(ert-deftest make-bullet/simple-plain-list ()
  (should (equal "- "
                 (org-structure/make-bullet #s(hash-table data (:text "whatever" :bullet-type ?- :children nil))
                                            ""
                                            0))))

(ert-deftest make-bullet/plain-list-under-headline ()
  (should (equal "- "
                 (org-structure/make-bullet #s(hash-table data (:text "whatever" :bullet-type ?- :children nil))
                                            "* "
                                            0))))

(ert-deftest make-bullet/plain-list-under-nested-headline ()
  (should (equal "- "
                 (org-structure/make-bullet #s(hash-table data (:text "whatever" :bullet-type ?- :children nil))
                                            "*** "
                                            0))))

(ert-deftest make-bullet/plain-list-under-ordered-list ()
  (should (equal "   - "
                 (org-structure/make-bullet #s(hash-table data (:text "whatever" :bullet-type ?- :children nil))
                                            "7. "
                                            0))))

(ert-deftest make-bullet/plain-list-under-long-ordered-list ()
  (should (equal "    - "
                 (org-structure/make-bullet #s(hash-table data (:text "whatever" :bullet-type ?- :children nil))
                                            "10. "
                                            0))))

(ert-deftest make-bullet/nested-plain-list ()
  (should (equal "  - "
                 (org-structure/make-bullet #s(hash-table data (:text "whatever" :bullet-type ?- :children nil))
                                            "- "
                                            0))))

(ert-deftest make-bullet/toplevel-ordered-list-period-first-item ()
  (should (equal "1. "
                 (org-structure/make-bullet #s(hash-table data (:text "whatever" :bullet-type ?. :children nil))
                                            ""
                                            0))))

(ert-deftest make-bullet/toplevel-ordered-list-paren-first-item ()
  (should (equal "1) "
                 (org-structure/make-bullet #s(hash-table data (:text "whatever" :bullet-type ?\) :children nil))
                                            ""
                                            0))))

(ert-deftest make-bullet/toplevel-ordered-list-period-second-item ()
  (should (equal "2. "
                 (org-structure/make-bullet #s(hash-table data (:text "whatever" :bullet-type ?. :children nil))
                                            ""
                                            1))))

(ert-deftest make-bullet/toplevel-ordered-list-paren-fifth-item ()
  (should (equal "5) "
                 (org-structure/make-bullet #s(hash-table data (:text "whatever" :bullet-type ?\) :children nil))
                                            ""
                                            4))))

(ert-deftest make-bullet/indented-ordered-list-period-first-item ()
  (should (equal "   1. "
                 (org-structure/make-bullet #s(hash-table data (:text "whatever" :bullet-type ?\. :children nil))
                                            "1) "
                                            0))))

(ert-deftest make-bullet/indented-ordered-list-paren-first-item ()
  (should (equal "   1) "
                 (org-structure/make-bullet #s(hash-table data (:text "whatever" :bullet-type ?\) :children nil))
                                            "7. "
                                            0))))

(ert-deftest make-bullet/indented-ordered-list-period-first-item-under-long-parent ()
  (should (equal "    1. "
                 (org-structure/make-bullet #s(hash-table data (:text "whatever" :bullet-type ?. :children nil))
                                            "12) "
                                            0))))

(ert-deftest make-bullet/indented-ordered-list-paren-first-item-under-long-parent ()
  (should (equal "    1) "
                 (org-structure/make-bullet #s(hash-table data (:text "whatever" :bullet-type ?\) :children nil))
                                            "79. "
                                            0))))

(ert-deftest make-bullet/indented-ordered-list-period-tenth-item ()
  (should (equal "   10. "
                 (org-structure/make-bullet #s(hash-table data (:text "whatever" :bullet-type ?\. :children nil))
                                            "1) "
                                            9))))

(ert-deftest make-bullet/indented-ordered-list-paren-third-item ()
  (should (equal "   3) "
                 (org-structure/make-bullet #s(hash-table data (:text "whatever" :bullet-type ?\) :children nil))
                                            "7. "
                                            2))))


(ert-deftest nested-whitespace/under-headline ()
  (should (equal ""
                 (org-structure/get-nested-whitespace "* "))))

(ert-deftest nested-whitespace/under-nested-headline ()
  (should (equal ""
                 (org-structure/get-nested-whitespace "** "))))

(ert-deftest nested-whitespace/under-nothing ()
  (should (equal ""
                 (org-structure/get-nested-whitespace ""))))

(ert-deftest nested-whitespace/plain-list-dash ()
  (should (equal (make-string 2 ?\s)
                 (org-structure/get-nested-whitespace "- "))))

(ert-deftest nested-whitespace/plain-list-plus ()
  (should (equal (make-string 2 ?\s)
                 (org-structure/get-nested-whitespace "+ "))))

(ert-deftest nested-whitespace/ordered-list-paren ()
  (should (equal (make-string 3 ?\s)
                 (org-structure/get-nested-whitespace "1) "))))

(ert-deftest nested-whitespace/ordered-list-period ()
  (should (equal (make-string 3 ?\s)
                 (org-structure/get-nested-whitespace "1. "))))

(ert-deftest nested-whitespace/ordered-list-two-digits ()
  (should (equal (make-string 4 ?\s)
                 (org-structure/get-nested-whitespace "21. "))))

(ert-deftest nested-whitespace/plain-list-dash-indented ()
  (should (equal (make-string 4 ?\s)
                 (org-structure/get-nested-whitespace "  - "))))

(ert-deftest nested-whitespace/plain-list-plus-indented ()
  (should (equal (make-string 4 ?\s)
                 (org-structure/get-nested-whitespace "  + "))))

(ert-deftest nested-whitespace/ordered-list-paren-indented ()
  (should (equal (make-string 5 ?\s)
                 (org-structure/get-nested-whitespace "  1) "))))

(ert-deftest nested-whitespace/ordered-list-period-indented ()
  (should (equal (make-string 5 ?\s)
                 (org-structure/get-nested-whitespace "  1. "))))

(ert-deftest nested-whitespace/ordered-list-indented-two-digits ()
  (should (equal (make-string 6 ?\s)
                 (org-structure/get-nested-whitespace "  21. "))))

(ert-deftest nested-whitespace/plain-list-dash-indented-under-long-ordered-list ()
  (should (equal (make-string 5 ?\s)
                 (org-structure/get-nested-whitespace "   - "))))

(ert-deftest nested-whitespace/plain-list-plus-indented-under-long-ordered-list ()
  (should (equal (make-string 5 ?\s)
                 (org-structure/get-nested-whitespace "   + "))))

(ert-deftest nested-whitespace/ordered-list-paren-indented-under-long-ordered-list ()
  (should (equal (make-string 6 ?\s)
                 (org-structure/get-nested-whitespace "   1) "))))

(ert-deftest nested-whitespace/ordered-list-period-indented-under-long-ordered-list ()
  (should (equal (make-string 6 ?\s)
                 (org-structure/get-nested-whitespace "   1. "))))

(ert-deftest nested-whitespace/ordered-list-long-paren-indented-under-long-ordered-list ()
  (should (equal (make-string 7 ?\s)
                 (org-structure/get-nested-whitespace "   14) "))))

(ert-deftest nested-whitespace/plain-list-dash-double-indented ()
  (should (equal (make-string 6 ?\s)
                 (org-structure/get-nested-whitespace "    - "))))

(ert-deftest nested-whitespace/plain-list-plus-double-indented ()
  (should (equal (make-string 6 ?\s)
                 (org-structure/get-nested-whitespace "    + "))))

(ert-deftest nested-whitespace/ordered-list-paren-double-indented ()
  (should (equal (make-string 7 ?\s)
                 (org-structure/get-nested-whitespace "    1) "))))

(ert-deftest nested-whitespace/ordered-list-period-double-indented ()
  (should (equal (make-string 7 ?\s)
                 (org-structure/get-nested-whitespace "    1. "))))

(ert-deftest nested-whitespace/ordered-list-long-period-double-indented ()
  (should (equal (make-string 8 ?\s)
                 (org-structure/get-nested-whitespace "    31. "))))


;;;; to-string tests

(ert-deftest to-string/just-one-block ()
  (should (equal "* header\n"
                 (org-structure/to-string '(#s(hash-table data (:text "header"
                                                                      :children nil
                                                                      :bullet-type ?*)))))))

(ert-deftest to-string/just-one-plain-list-dash ()
  (should (equal "- header\n"
                 (org-structure/to-string '(#s(hash-table data (:text "header"
                                                                      :children nil
                                                                      :bullet-type ?-)))))))

(ert-deftest to-string/just-one-plain-list-plus ()
  (should (equal "+ header\n"
                 (org-structure/to-string '(#s(hash-table data (:text "header"
                                                                      :children nil
                                                                      :bullet-type ?+)))))))

(ert-deftest to-string/just-one-ordered-list-period ()
  (should (equal "1. header\n"
                 (org-structure/to-string '(#s(hash-table data (:text "header"
                                                                      :children nil
                                                                      :bullet-type ?.)))))))

(ert-deftest to-string/just-one-ordered-list-paren ()
  (should (equal "1) header\n"
                 (org-structure/to-string '(#s(hash-table data (:text "header"
                                                                      :children nil
                                                                      :bullet-type ?\))))))))

(ert-deftest to-string/two-headlines ()
  (should (equal "* header\n* header2\n"
                 (org-structure/to-string '(#s(hash-table data (:text "header"
                                                                      :children nil
                                                                      :bullet-type ?*))
                                              #s(hash-table data (:text "header2"
                                                                        :children nil
                                                                        :bullet-type ?*)))))))

(ert-deftest to-string/two-plain-lists ()
  (should (equal "+ header\n+ header2\n"
                 (org-structure/to-string '(#s(hash-table data (:text "header"
                                                                      :children nil
                                                                      :bullet-type ?+))
                                              #s(hash-table data (:text "header2"
                                                                        :children nil
                                                                        :bullet-type ?+)))))))

(ert-deftest to-string/two-ordered-lists ()
  (should (equal "1. header\n2. header2\n"
                 (org-structure/to-string '(#s(hash-table data (:text "header"
                                                                      :children nil
                                                                      :bullet-type ?.))
                                              #s(hash-table data (:text "header2"
                                                                        :children nil
                                                                        :bullet-type ?.)))))))


(ert-deftest single-to-string/headline-and-body ()
  (should (equal "* whatever\nHere's a body\n"
                 (org-structure/single-to-string #s(hash-table data (:text "whatever"
                                                                           :children nil
                                                                           :bullet-type ?*
                                                                           :body "Here's a body"))
                                                 ""
                                                 0))))


;;;; tests that go all the way around -- from a string to a structure to the original string
(ert-deftest to-structure-to-string/just-one-block ()
  (should (equal "* header\n"
                 (org-structure/to-string (org-structure "* header\n")))))

(ert-deftest to-structure-to-string/just-one-block-with-body ()
  (should (equal "* header\nwith body\n"
                 (org-structure/to-string (org-structure "* header\nwith body\n")))))

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

(ert-deftest to-structure-to-string/lots-of-bullet-types ()
  (should (equal "* whatever\n** two\n1. what\n   - the /parent/ of this node is an ordered list, so there are *three* spaces\n"
                 (org-structure/to-string (org-structure "* whatever\n** two\n1. what\n   - the /parent/ of this node is an ordered list, so there are *three* spaces")))))

(ert-deftest to-structure-to-string/headline-then-plain-list ()
  (should (equal "* first thing\n+ nested thing\n"
                 (org-structure/to-string (org-structure "* first thing\n+ nested thing\n")))))

(ert-deftest to-structure-to-string/nested-headline-then-plain-list ()
  (should (equal "* first thing\n** nested headline\n+ nested thing\n"
                 (org-structure/to-string (org-structure "* first thing\n** nested headline\n+ nested thing\n")))))

(ert-deftest to-structure-to-string/headline-then-double-nested-plain-list ()
  (should (equal "* first thing\n+ nested headline\n  + nested thing\n"
                 (org-structure/to-string (org-structure "* first thing\n+ nested headline\n  + nested thing\n")))))

(ert-deftest to-structure-to-string/just-one-plain-list-plus ()
  (should (equal "+ header\n"
                 (org-structure/to-string (org-structure "+ header\n")))))

(ert-deftest to-structure-to-string/just-one-plain-list-dash ()
  (should (equal "- header\n"
                 (org-structure/to-string (org-structure "- header\n")))))

(ert-deftest to-structure-to-string/just-one-ordered-list-period ()
  (should (equal "1. header\n"
                 (org-structure/to-string (org-structure "1. header\n")))))

(ert-deftest to-structure-to-string/just-one-ordered-list-paren ()
  (should (equal "1) header\n"
                 (org-structure/to-string (org-structure "1) header\n")))))

(ert-deftest to-structure-to-string/ordered-lists ()
  (should (equal "1. first\n2. second\n"
                 (org-structure/to-string (org-structure "1. first\n2. second\n")))))

(ert-deftest to-structure-to-string/plain-list-under-ordered ()
  (should (equal "1. thing\n   + yep, nested\n"
                 (org-structure/to-string (org-structure "1. thing\n   + yep, nested\n")))))

(ert-deftest to-structure-to-string/plain-list-under-ordered ()
  (should (equal "1. thing\n   + yep, nested\n"
                 (org-structure/to-string (org-structure "1. thing\n   + yep, nested\n")))))

(ert-deftest to-structure-to-string/long-ordered-list-with-child ()
  (should (equal "1. thing\n2. thing\n3. thing\n4. thing\n5. thing\n6. thing\n7. thing\n8. thing\n9. thing\n10. thing\n    + yep, nested\n"
                 (org-structure/to-string (org-structure "1. thing\n2. thing\n3. thing\n4. thing\n5. thing\n6. thing\n7. thing\n8. thing\n9. thing\n10. thing\n    + yep, nested\n")))))



(ert-deftest nested-children/no-indices ()
  (let ((looked-up-table (org-structure/get-nested-children #s(hash-table data (:children children :text "whatever")))))
    (should looked-up-table)
    (should (equal 2
                   (hash-table-count looked-up-table)))
    (should (equal 'children
                   (gethash :children looked-up-table)))
    (should (equal "whatever"
                   (gethash :text looked-up-table)))))

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


(ert-deftest bullet-type-headline ()
  (should (equal ?*
                 (org-structure/bullet-type "* "))))

(ert-deftest bullet-type-nested-headline ()
  (should (equal ?*
                 (org-structure/bullet-type "** "))))

(ert-deftest bullet-type-plain-list-dash ()
  (should (equal ?-
                 (org-structure/bullet-type "- "))))

(ert-deftest bullet-type-plain-list-nested-dash ()
  (should (equal ?-
                 (org-structure/bullet-type "  - "))))

(ert-deftest bullet-type-plain-list-nested-plus ()
  (should (equal ?+
                 (org-structure/bullet-type "     + "))))

(ert-deftest bullet-type-plain-list-number-paren ()
  (should (equal ?\)
                 (org-structure/bullet-type "14) "))))

(ert-deftest bullet-type-plain-list-number-period ()
  (should (equal ?.
                 (org-structure/bullet-type "3. "))))

(ert-deftest bullet-type-plain-list-number-nested-paren ()
  (should (equal ?\)
                 (org-structure/bullet-type "  14) "))))

(ert-deftest bullet-type-plain-list-number-nested-period ()
  (should (equal ?.
                 (org-structure/bullet-type "     3. "))))


(ert-deftest get-text/simple-headline ()
  (should (equal "headline"
                 (org-structure/get-text "* headline"))))

(ert-deftest get-text/headline-2 ()
  (should (equal "my headline"
                 (org-structure/get-text "** my headline"))))

(ert-deftest get-text/headline-3 ()
  (should (equal "another headline"
                 (org-structure/get-text "*** another headline"))))

(ert-deftest get-text/plain-list ()
  (should (equal "a new list"
                 (org-structure/get-text "- a new list"))))

(ert-deftest get-text/indented-plain-list ()
  (should (equal "a new list"
                 (org-structure/get-text "  - a new list"))))

(ert-deftest get-text/plain-list-plus ()
  (should (equal "a new list"
                 (org-structure/get-text "+ a new list"))))

(ert-deftest get-text/indented-plain-list-plus ()
  (should (equal "a new list"
                 (org-structure/get-text "  + a new list"))))

(ert-deftest get-text/quite-indented-plain-list-plus ()
  (should (equal "a new list"
                 (org-structure/get-text "     + a new list"))))

(ert-deftest get-text/stupid-plain-list-with-asterisks ()
  (should (equal "this is a dumb kind of list"
                 (org-structure/get-text "  * this is a dumb kind of list"))))

(ert-deftest get-text/ordered-list ()
  (should (equal "a new list"
                 (org-structure/get-text "1. a new list"))))

(ert-deftest get-text/indented-ordered-list ()
  (should (equal "a new list"
                 (org-structure/get-text "  1. a new list"))))

(ert-deftest get-text/ordered-list-paren ()
  (should (equal "a new list"
                 (org-structure/get-text "11) a new list"))))

(ert-deftest get-text/indented-ordered-list-paren ()
  (should (equal "a new list"
                 (org-structure/get-text "  2) a new list"))))

(ert-deftest get-text/quite-ordered-plain-list-paren ()
  (should (equal "a new list"
                 (org-structure/get-text "     7) a new list"))))



(ert-deftest guess-level/headline-one ()
  (should (equal 1
                  (org-structure/guess-level "* headline"))))

(ert-deftest guess-level/headline-two ()
  (should (equal 2
                  (org-structure/guess-level "** headline"))))

(ert-deftest guess-level/headline-three ()
  (should (equal 3
                  (org-structure/guess-level "*** headline"))))

(ert-deftest guess-level/headline-children-dont-matter ()
  (should (equal 2
                  (org-structure/guess-level "** headline\n*** nested!"))))

(ert-deftest guess-level/plain-list-dash-one ()
  (should (equal 1
                  (org-structure/guess-level "- title"))))

(ert-deftest guess-level/plain-list-plus-one ()
  (should (equal 1
                  (org-structure/guess-level "+ title"))))

(ert-deftest guess-level/plain-list-dash-two ()
  (should (equal 2
                  (org-structure/guess-level "  - title"))))

(ert-deftest guess-level/plain-list-plus-two ()
  (should (equal 2
                  (org-structure/guess-level "  + title"))))

(ert-deftest guess-level/plain-list-dash-three ()
  (should (equal 3
                  (org-structure/guess-level "    - title"))))

(ert-deftest guess-level/plain-list-plus-three ()
  (should (equal 3
                  (org-structure/guess-level "    + title"))))

(ert-deftest guess-level/plain-list-dash-one-and-a-half ()
  (should (equal 2
                  (org-structure/guess-level "   - title"))))

(ert-deftest guess-level/plain-list-plus-one-and-a-half ()
  (should (equal 2
                  (org-structure/guess-level "   + title"))))

(ert-deftest guess-level/plain-list-dash-two-and-a-half ()
  (should (equal 3
                  (org-structure/guess-level "     - title"))))

(ert-deftest guess-level/plain-list-plus-two-and-a-half ()
  (should (equal 3
                 (org-structure/guess-level "     + title"))))




(ert-deftest make-text-tree/one-headline ()
  (should (equal '(("* one line"))
                 (org-structure/make-text-tree '("* one line")))))

(ert-deftest make-text-tree/one-headline-with-body ()
  (should (equal '(("* one line\nand a body"))
                 (org-structure/make-text-tree '("* one line\nand a body")))))

(ert-deftest make-text-tree/two-headlines ()
  (should (equal '(("* first headline") ("* second headline"))
                 (org-structure/make-text-tree '("* first headline" "* second headline")))))

(ert-deftest make-text-tree/one-nested-headline ()
  (should (equal '(("* first headline" ("** nested headline")))
                 (org-structure/make-text-tree '("* first headline" "** nested headline")))))

(ert-deftest make-text-tree/nested-plain-list ()
  (should (equal '(("* first headline" ("- I'm nested")))
                 (org-structure/make-text-tree '("* first headline" "- I'm nested")))))

(ert-deftest make-text-tree/nested-plain-list-and-nested-headline ()
  (should (equal '(("* first headline" ("- I'm nested") ("** I'm nested too")))
                 (org-structure/make-text-tree '("* first headline" "- I'm nested" "** I'm nested too")))))

(ert-deftest make-text-tree/nested-plain-list-and-nested-headline-with-followup-headline ()
  (should (equal '(("* first headline" ("- I'm nested") ("** I'm nested too")) ("* also first-level headline"))
                 (org-structure/make-text-tree '("* first headline" "- I'm nested" "** I'm nested too" "* also first-level headline")))))

(ert-deftest make-text-tree/multiple-headlines ()
  (should (equal '(("* first headline") ("* second headline") ("* third headline"))
                 (org-structure/make-text-tree '("* first headline" "* second headline" "* third headline")))))

(ert-deftest make-text-tree/multiple-nested-headlines ()
  (should (equal '(("* first headline" ("** nested headline")) ("* second top headline" ("** and more children") ("** and more more children")))
                 (org-structure/make-text-tree '("* first headline" "** nested headline" "* second top headline" "** and more children" "** and more more children")))))



(ert-deftest split-into-blocks/single-headline ()
  (should (equal '("* headline")
                 (org-structure/split-into-blocks "* headline\n"))))

(ert-deftest split-into-blocks/single-headline-with-text ()
  (should (equal '("* headline\nhere's some text")
                 (org-structure/split-into-blocks "* headline\nhere's some text"))))

(ert-deftest split-into-blocks/two-headlines ()
  (should (equal '("* headline" "* another headline")
                 (org-structure/split-into-blocks "* headline\n* another headline"))))

(ert-deftest split-into-blocks/nested-headlines ()
  (should (equal '("* headline" "** another headline")
                 (org-structure/split-into-blocks "* headline\n** another headline"))))

(ert-deftest split-into-blocks/single-plain-list ()
  (should (equal '("- plain-list")
                 (org-structure/split-into-blocks "- plain-list\n"))))

(ert-deftest split-into-blocks/single-plain-list-with-text ()
  (should (equal '("- plain-list\nhere's some text")
                 (org-structure/split-into-blocks "- plain-list\nhere's some text"))))


(ert-deftest split-into-blocks/two-plain-lists ()
  (should (equal '("- plain-list" "- another plain-list")
                 (org-structure/split-into-blocks "- plain-list\n- another plain-list"))))

(ert-deftest split-into-blocks/nested-plain-lists ()
  (should (equal '("- plain-list" "- another plain-list")
                 (org-structure/split-into-blocks "- plain-list\n- another plain-list"))))




(ert-deftest convert-text-block/simple-headline-text ()
  (should (equal "whatever"
                 (gethash :text (org-structure/convert-text-block '("* whatever"))))))

(ert-deftest convert-text-block/simple-headline-text-ignoring-body ()
  (should (equal "whatever"
                 (gethash :text (org-structure/convert-text-block '("* whatever\nbody to ignore"))))))

(ert-deftest convert-text-block/simple-headline-body ()
  (should (equal "I'm a body!"
                 (gethash :body (org-structure/convert-text-block '("* whatever\nI'm a body!"))))))

(ert-deftest convert-text-block/simple-headline-multiple-line-body ()
  (should (equal "I'm a body!\nAnd still I come"
                 (gethash :body (org-structure/convert-text-block '("* whatever\nI'm a body!\nAnd still I come"))))))

(ert-deftest convert-text-block/simple-headline-bullet-type ()
  (should (equal ?*
                 (gethash :bullet-type (org-structure/convert-text-block '("* whatever"))))))

(ert-deftest convert-text-block/simple-headline-children ()
  (should-not (gethash :children (org-structure/convert-text-block '("* whatever")))))

(ert-deftest convert-text-block/nested-headline-text ()
  (should (equal "whatever"
                 (gethash :text (org-structure/convert-text-block '("* whatever" ("** nested")))))))

(ert-deftest convert-text-block/nested-headline-bullet-type ()
  (should (equal ?*
                 (gethash :bullet-type (org-structure/convert-text-block '("* whatever" ("** nested")))))))

(ert-deftest convert-text-block/nested-headline-children ()
  (should (equal 1
                 (length (gethash :children (org-structure/convert-text-block '("* whatever" ("** nested"))))))))

(ert-deftest convert-text-block/nested-headline-child-text ()
  (should (equal "nested here!"
                 (gethash :text
                          (org-structure/get-nested-children (org-structure/convert-text-block '("* whatever" ("** nested here!")))
                                                             0)))))

(ert-deftest convert-text-block/nested-headline-child-bullet ()
  (should (equal ?*
                 (gethash :bullet-type
                          (org-structure/get-nested-children (org-structure/convert-text-block '("* whatever" ("** nested here!")))
                                                             0)))))

(ert-deftest convert-text-block/nested-headline-child-children ()
  (should-not (gethash :children
                       (org-structure/get-nested-children (org-structure/convert-text-block '("* whatever" ("** nested here!")))
                                                          0))))

(ert-deftest convert-text-block/multiple-nested-children ()
  (should (equal 3
                 (length (gethash :children (org-structure/convert-text-block '("* whatever" ("** nested") ("** nested two") ("**nested three!"))))))))



(ert-deftest get-text/headline-plain-text ()
  (should (equal "I'm the text"
                 (org-structure/get-text "* I'm the text"))))

(ert-deftest get-text/plain-list-plain-text ()
  (should (equal "I'm the text"
                 (org-structure/get-text "- I'm the text"))))

(ert-deftest get-text/headline-plain-text-with-body ()
  (should (equal "I'm the text"
                 (org-structure/get-text "* I'm the text\nbut I'm the body"))))

(ert-deftest get-text/plain-list-plain-text-with-body ()
  (should (equal "I'm the text"
                 (org-structure/get-text "- I'm the text\nbut I'm the body"))))

(ert-deftest get-text/headline-plain-text-with-multiline-body ()
  (should (equal "I'm the text"
                 (org-structure/get-text "* I'm the text\nbut I'm the body\nand so am I."))))

(ert-deftest get-text/plain-list-plain-text-with-multiline-body ()
  (should (equal "I'm the text"
                 (org-structure/get-text "- I'm the text\nbut I'm the body\nand so am I."))))



(ert-deftest get-body/headline-plain-text ()
  (should-not (org-structure/get-body "* I'm the text")))

(ert-deftest get-body/plain-list-plain-text ()
  (should-not (org-structure/get-body "- I'm the text")))

(ert-deftest get-body/headline-plain-text-with-body ()
  (should (equal "but I'm the body"
                 (org-structure/get-body "* I'm the text\nbut I'm the body"))))

(ert-deftest get-body/plain-list-plain-text-with-body ()
  (should (equal "but I'm the body"
                 (org-structure/get-body "- I'm the text\nbut I'm the body"))))

(ert-deftest get-body/headline-plain-text-with-multiline-body ()
  (should (equal "but I'm the body\nand so am I."
                 (org-structure/get-body "* I'm the text\nbut I'm the body\nand so am I."))))

(ert-deftest get-body/plain-list-plain-text-with-multiline-body ()
  (should (equal "but I'm the body\nand so am I."
                 (org-structure/get-body "- I'm the text\nbut I'm the body\nand so am I."))))




(ert-deftest convert-text-tree/one-headline ()
  (should (equal 1
                 (length (org-structure/convert-text-tree '(("* a single headline")))))))

(ert-deftest convert-text-tree/one-headline-with-children ()
  (should (equal 1
                 (length (org-structure/convert-text-tree '(("* a single headline" ("** but with children"))))))))



(ert-deftest descendent/headline-1-headline-1 ()
  (should-not (org-structure/descendent? "* headline 1" "* another headline")))

(ert-deftest descendent/headline-2-headline-2 ()
  (should-not (org-structure/descendent? "** headline 1" "** another headline")))

(ert-deftest descendent/headline-1-headline-2 ()
  (should (org-structure/descendent? "* headline 1" "** nested headline")))

(ert-deftest descendent/headline-1-headline-3 ()
  (should (org-structure/descendent? "* headline 1" "*** another headline")))

(ert-deftest descendent/headline-1-plain-1 ()
  (should (org-structure/descendent? "* headline 1" "- plain here")))

(ert-deftest descendent/headline-2-plain-1 ()
  (should (org-structure/descendent? "** headline 1" "- plain here")))

(ert-deftest descendent/headline-1-plain-2 ()
  (should (org-structure/descendent? "* headline 1" " - plain list")))

(ert-deftest descendent/headline-1-ordered-1 ()
  (should (org-structure/descendent? "* headline 1" "17. ordered")))

(ert-deftest descendent/plain-1-headline-1 ()
  (should-not (org-structure/descendent? "- plain" "* headline")))

(ert-deftest descendent/plain-1-headline-2 ()
  (should-not (org-structure/descendent? "- plain" "** nested")))



(ert-deftest headline?/empty ()
  (should-not (org-structure/headline? "")))

(ert-deftest headline?/asterisk-char ()
  (should (org-structure/headline? ?*)))

(ert-deftest headline?/dash-char ()
  (should-not (org-structure/headline? ?-)))

(ert-deftest headline?/plus-char ()
  (should-not (org-structure/headline? ?+)))

(ert-deftest headline?/paren-char ()
  (should-not (org-structure/headline? ?\))))

(ert-deftest headline?/period-char ()
  (should-not (org-structure/headline? ?.)))

(ert-deftest headline?/headline-1 ()
  (should (org-structure/headline? "* yep, I'm good")))

(ert-deftest headline?/headline-2 ()
  (should (org-structure/headline? "** yep, I'm good")))

(ert-deftest headline?/headline-3 ()
  (should (org-structure/headline? "*** yep, I'm good")))

(ert-deftest headline?/plain-1-dash ()
  (should-not (org-structure/headline? "- no way")))

(ert-deftest headline?/plain-2-dash ()
  (should-not (org-structure/headline? "  - no way")))

(ert-deftest headline?/plain-3-dash ()
  (should-not (org-structure/headline? "    - no way")))

(ert-deftest headline?/plain-1-plus ()
  (should-not (org-structure/headline? "+ no way")))

(ert-deftest headline?/plain-2-plus ()
  (should-not (org-structure/headline? "  + no way")))

(ert-deftest headline?/plain-3-plus ()
  (should-not (org-structure/headline? "    + no way")))

(ert-deftest headline?/ordered-1 ()
  (should-not (org-structure/headline? "1. no way")))

(ert-deftest headline?/ordered-2 ()
  (should-not (org-structure/headline? "  2) no way")))

(ert-deftest headline?/ordered-3 ()
  (should-not (org-structure/headline? "    4. no way")))

(ert-deftest headline?/plain-list-with-indented-asterisk ()
  (should-not (org-structure/headline? "  * no way")))



(ert-deftest plain-list?/empty ()
  (should-not (org-structure/plain-list? "")))

(ert-deftest plain-list?/asterisk-char ()
  (should-not (org-structure/plain-list? ?*)))

(ert-deftest plain-list?/dash-char ()
  (should (org-structure/plain-list? ?-)))

(ert-deftest plain-list?/plus-char ()
  (should (org-structure/plain-list? ?+)))

(ert-deftest plain-list?/paren-char ()
  (should (org-structure/plain-list? ?\))))

(ert-deftest plain-list?/period-char ()
  (should (org-structure/plain-list? ?.)))

(ert-deftest plain-list?/headline-1 ()
  (should-not (org-structure/plain-list? "* a headline?!")))

(ert-deftest plain-list?/headline-2 ()
  (should-not (org-structure/plain-list? "** a headline?!")))

(ert-deftest plain-list?/headline-3 ()
  (should-not (org-structure/plain-list? "*** a headline?!")))

(ert-deftest plain-list?/plain-1-dash ()
  (should (org-structure/plain-list? "- plain here, but not sad")))

(ert-deftest plain-list?/plain-2-dash ()
  (should (org-structure/plain-list? "  - plain here, but not sad")))

(ert-deftest plain-list?/plain-3-dash ()
  (should (org-structure/plain-list? "    - plain here, but not sad")))

(ert-deftest plain-list?/plain-1-plus ()
  (should (org-structure/plain-list? "+ plain here, but not sad")))

(ert-deftest plain-list?/plain-2-plus ()
  (should (org-structure/plain-list? "  + plain here, but not sad")))

(ert-deftest plain-list?/plain-3-plus ()
  (should (org-structure/plain-list? "    + plain here, but not sad")))

(ert-deftest plain-list?/ordered-1 ()
  (should (org-structure/plain-list? "1. plain here, but not sad")))

(ert-deftest plain-list?/ordered-2 ()
  (should (org-structure/plain-list? "  2) plain here, but not sad")))

(ert-deftest plain-list?/ordered-3 ()
  (should (org-structure/plain-list? "    4. plain here, but not sad")))

(ert-deftest plain-list?/plain-list-with-indented-asterisk ()
  (should (org-structure/plain-list? "  * plain here, but not sad")))

(ert-deftest plain-list?/random-text ()
  (should-not (org-structure/plain-list? "Not anything useful, even with * and -.")))



(ert-deftest title-line?/headline ()
  (should (org-structure/title-line? "* whatever")))

(ert-deftest title-line?/headline-with-body ()
  (should (org-structure/title-line? "* whatever\nhere's a body")))

(ert-deftest title-line?/double-headline ()
  (should (org-structure/title-line? "** whatever")))

(ert-deftest title-line?/plain-list ()
  (should (org-structure/title-line? "- whatever")))

(ert-deftest title-line?/indented-plain-list ()
  (should (org-structure/title-line? "  + whatever")))

(ert-deftest title-line?/plain-list-with-body ()
  (should (org-structure/title-line? "- whatever\nand here's a body")))

(ert-deftest title-line?/not-a-title ()
  (should-not (org-structure/title-line? "Not anything useful, even with * and -.")))

(ert-deftest title-line?/not-a-title-indented ()
  (should-not (org-structure/title-line? "   Not anything useful, even with * and -.")))



(ert-deftest ordered-list/paren-char ()
  (should (org-structure/ordered-list? ?\))))

(ert-deftest ordered-list/dot-char ()
  (should (org-structure/ordered-list? ?.)))

(ert-deftest ordered-list/dash-char ()
  (should-not (org-structure/ordered-list? ?-)))

(ert-deftest ordered-list/plus-char ()
  (should-not (org-structure/ordered-list? ?+)))

(ert-deftest ordered-list/asterisk-char ()
  (should-not (org-structure/ordered-list? ?*)))

(ert-deftest ordered-list/toplevel-paren ()
  (should (org-structure/ordered-list? "7) ")))

(ert-deftest ordered-list/toplevel-dot ()
  (should (org-structure/ordered-list? "3. ")))

(ert-deftest ordered-list/toplevel-two-digit ()
  (should (org-structure/ordered-list? "12) ")))

(ert-deftest ordered-list/toplevel-headline ()
  (should-not (org-structure/ordered-list? "* ")))

(ert-deftest ordered-list/toplevel-dash-list ()
  (should-not (org-structure/ordered-list? "- ")))

(ert-deftest ordered-list/toplevel-plus-list ()
  (should-not (org-structure/ordered-list? "+ ")))

(ert-deftest ordered-list/indented-paren ()
  (should (org-structure/ordered-list? "  7) ")))

(ert-deftest ordered-list/indented-dot ()
  (should (org-structure/ordered-list? "   3. ")))

(ert-deftest ordered-list/indented-two-digit ()
  (should (org-structure/ordered-list? "  12) ")))

(ert-deftest ordered-list/indented-headline ()
  (should-not (org-structure/ordered-list? "** ")))

(ert-deftest ordered-list/indented-dash-list ()
  (should-not (org-structure/ordered-list? "  - ")))

(ert-deftest ordered-list/indented-plus-list ()
  (should-not (org-structure/ordered-list? "  + ")))

;;; tests.el ends here
