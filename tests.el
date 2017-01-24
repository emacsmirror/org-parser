;;; tests.el -- tests for org-parser

;;; Commentary:

;;; Code:


;;;; org structure tests

(ert-deftest single-line-has-only-one-block ()
  (should (equal 1
                 (length (org-parser "* header")))))

(ert-deftest single-line-text ()
  (should (equal '("header")
                 (gethash :text (car (org-parser "* header"))))))

(ert-deftest single-line-children ()
  (should-not (gethash :children (car (org-parser "* header")))))

(ert-deftest single-line-bullet-type ()
  (should (equal ?*
                 (gethash :bullet-type (car (org-parser "* header"))))))

(ert-deftest single-plain-list-has-only-one-block ()
  (should (equal 1
                 (length (org-parser "- header")))))

(ert-deftest single-line-with-link-text-before-link ()
  (let ((text (gethash :text (first (org-parser "* here is [[http://zck.me/][my site]]")))))
    (should (listp text))
    (should (equal 2
                   (length text)))
    (should (equal "here is "
                   (first text)))
    (should (hash-table-p (second text)))
    (should (equal :link (gethash :type (second text))))
    (should (equal "http://zck.me/" (gethash :target (second text))))
    (should (equal "my site" (gethash :text (second text))))))

(ert-deftest single-line-with-link-text-link-text ()
  (let ((text (gethash :text (first (org-parser "* here is [[http://zck.me/][my site]]")))))
    (should (listp text))
    (should (equal 2
                   (length text)))
    (should (hash-table-p (second text)))
    (should (equal "my site"
                   (gethash :text (second text))))
    (should (equal "http://zck.me/"
                   (gethash :target (second text))))))


(ert-deftest single-plain-list-text ()
  (should (equal '("header")
                 (gethash :text (car (org-parser "- header"))))))

(ert-deftest single-plain-list-children ()
  (should-not (gethash :children (car (org-parser "- header")))))

(ert-deftest single-plain-list-bullet-type ()
  (should (equal ?-
                 (gethash :bullet-type (car (org-parser "- header"))))))

(ert-deftest nested-headline-bullet-type ()
  (should (equal ?*
                 (gethash :bullet-type (car (org-parser "** header"))))))

(ert-deftest nested-plain-list-bullet-type ()
  (should (equal ?+
                 (gethash :bullet-type (car (org-parser "   + header"))))))

(ert-deftest ordered-list-bullet-type ()
  (should (equal ?.
                 (gethash :bullet-type (car (org-parser "14. header"))))))

(ert-deftest nested-ordered-list-bullet-type ()
  (should (equal ?.
                 (gethash :bullet-type (car (org-parser "  3. header"))))))

(ert-deftest with-newline-single-line-text ()
  (should (equal '("header")
                 (gethash :text (car (org-parser "* header\n"))))))

(ert-deftest with-newline-single-line-children ()
  (should-not (gethash :children (car (org-parser "* header\n")))))

(ert-deftest with-newline-single-line-bullet-type ()
  (should (equal ?*
                 (gethash :bullet-type (car (org-parser "* header\n"))))))


(ert-deftest children-dont-create-new-block ()
  (should (equal 1
                 (length (org-parser "* header\n** nested")))))

(ert-deftest two-children ()
  (should (equal 2
                 (length (gethash :children
                                  (car (org-parser "* header\n** first child\n** second child")))))))

(ert-deftest two-blocks ()
  (should (equal 2
                 (length (org-parser "* header\n* nested")))))



(ert-deftest child-block-single-line-text ()
  (should (equal '("I'm a child!")
                 (gethash :text (org-parser/get-nested-children (car (org-parser "* ignored\n** I'm a child!"))
                                                                0)))))

(ert-deftest child-block-single-line-children ()
  (should-not (gethash :children (org-parser/get-nested-children (car (org-parser "* ignored\n** I'm a child!"))
                                                                 0))))

(ert-deftest child-block-single-line-bullet-type ()
  (should (equal ?*
                 (gethash :bullet-type (org-parser/get-nested-children (car (org-parser "* ignored\n** I'm a child!"))
                                                                       0)))))



(ert-deftest second-child-block-single-line-text ()
  (should (equal '("I'm the younger, forgotten child.")
                 (gethash :text (org-parser/get-nested-children (car (org-parser "* ignored\n** I'm a child!\n** I'm the younger, forgotten child."))
                                                                1)))))

(ert-deftest second-child-block-single-line-children ()
  (should-not (gethash :children (org-parser/get-nested-children (car (org-parser "* ignored\n** I'm a child!\n** I'm the younger, forgotten child."))
                                                                 1))))

(ert-deftest second-child-block-single-line-bullet-type ()
  (should (equal ?*
                 (gethash :bullet-type (org-parser/get-nested-children (car (org-parser "* ignored\n** I'm a child!\n** I'm the younger, forgotten child."))
                                                                       1)))))



(ert-deftest thirdly-nested-child-blocks ()
  (should (equal 2
                 (length (gethash :children
                                  (org-parser/get-nested-children (elt (org-parser "* header\n* second header\n** first child\n*** I'm forgotten about\n** second child\n*** this is the test grandchild\n*** this is the other test grandchild.")
                                                                       1)
                                                                  1))))))


(ert-deftest third-nested-child-block-single-line-text ()
  (should (equal '("this is the other test grandchild.")
                 (gethash :text
                          (org-parser/get-nested-children (elt (org-parser "* header\n* second header\n** first child\n*** I'm forgotten about\n** second child\n*** this is the test grandchild\n*** this is the other test grandchild.")
                                                               1)
                                                          1
                                                          1)))))

(ert-deftest third-nested-child-block-single-line-children ()
  (should-not (gethash :children
                       (org-parser/get-nested-children (elt (org-parser "* header\n* second header\n** first child\n*** I'm forgotten about\n** second child\n*** this is the test grandchild\n*** this is the other test grandchild.")
                                                            1)
                                                       1
                                                       1))))

(ert-deftest third-nested-child-block-single-line-bullet-type ()
  (should (equal ?*
                 (gethash :bullet-type
                          (org-parser/get-nested-children (elt (org-parser "* header\n* second header\n** first child\n*** I'm forgotten about\n** second child\n*** this is the test grandchild\n*** this is the other test grandchild.")
                                                               1)
                                                          1
                                                          1)))))



(ert-deftest plain-child-lists-of-mixed-types-are-blocked-properly ()
  (should (equal 1
                 (length (gethash :children
                                  (car (org-parser "* whatever\n1. what\n   - the /parent/ of this node is an ordered list, so there are *three* spaces")))))))

(ert-deftest indented-ordered-lists-are-blocked-properly ()
  (should (equal 1
                 (length (org-parser "* whatever\n1. what\n   - the /parent/ of this node is an ordered list, so there are *three* spaces")))))

(ert-deftest lists-of-mixed-types-are-blocked-properly ()
  (should (equal 2
                 (length (gethash :children
                                  (car (org-parser "* top\n- first level\n** also first level")))))))

(ert-deftest headline-with-body-text-has-correct-text ()
  (should (equal '(("I'm in the body"))
                 (gethash :body
                          (car (org-parser "* I'm the headline\nI'm in the body"))))))

(ert-deftest headline-multiline-body ()
  (should (equal '(("first line") ("second line"))
                 (gethash :body
                          (car (org-parser "* ignored headline\nfirst line\nsecond line"))))))

(ert-deftest headline-body-with-link ()
  (let ((body (gethash :body (car (org-parser "* ignored headline\nhere's a body with [[http://example.com][a link!]]")))))
    (should (equal 1
                   (length body)))
    (should (equal 2
                   (length (first body))))
    (should (equal "here's a body with "
                   (first (first body))))
    (let ((link-hash (second (first body))))
      (should (hash-table-p link-hash))
      (should (equal "http://example.com"
                     (gethash :target link-hash)))
      (should (equal "a link!"
                     (gethash :text link-hash))))))

(ert-deftest headline-with-body-text-has-no-siblings ()
  (should (equal 1
                 (length (org-parser "* I'm the headline\nI'm in the body")))))

(ert-deftest headline-with-body-text-has-no-children ()
  (should (equal 0
                 (length (gethash :children (car (org-parser "* I'm the headline\nI'm in the body")))))))


;;;; get-bullet tests
(ert-deftest get-bullet/one-level-headline ()
  (should (equal "* "
                 (org-parser/get-bullet "* headline\n"))))

(ert-deftest get-bullet/two-level-headline ()
  (should (equal "** "
                 (org-parser/get-bullet "** headline\n"))))

(ert-deftest get-bullet/three-level-headline ()
  (should (equal "*** "
                 (org-parser/get-bullet "*** headline\n"))))

(ert-deftest get-bullet/looks-at-first-line ()
  (should (equal "** "
                 (org-parser/get-bullet "** headline\n*** second thing\n"))))

(ert-deftest get-bullet/headline-with-initial-space-fails ()
  (should-error (org-parser/get-bullet " ** not matching!")))

(ert-deftest get-bullet/headline-with-no-endingspace-fails ()
  (should-error (org-parser/get-bullet "**not matching!")))

(ert-deftest get-bullet/headline-with-no-bullets-fails ()
  (should-error (org-parser/get-bullet "not matching!")))

(ert-deftest get-bullet/plain-list-simple ()
  (should (equal "- "
                 (org-parser/get-bullet "- asrt\n"))))

(ert-deftest get-bullet/plain-list-single-nested ()
  (should (equal "  - "
                 (org-parser/get-bullet "  - asrt\n"))))

(ert-deftest get-bullet/plain-list-doubly-nested ()
  (should (equal "    - "
                 (org-parser/get-bullet "    - asrt\n"))))

(ert-deftest get-bullet/plain-list-nested-plus ()
  (should (equal "  + "
                 (org-parser/get-bullet "  + asrt\n"))))

(ert-deftest get-bullet/ordered-list-period ()
  (should (equal "1. "
                 (org-parser/get-bullet "1. what"))))

(ert-deftest get-bullet/ordered-list-period ()
  (should (equal "4) "
                 (org-parser/get-bullet "4) what"))))

(ert-deftest get-bullet/ordered-list-nested-period ()
  (should (equal "  2. "
                 (org-parser/get-bullet "  2. what"))))

(ert-deftest get-bullet/ordered-list-nested-paren ()
  (should (equal "  1) "
                 (org-parser/get-bullet "  1) what"))))

(ert-deftest get-bullet/ordered-list-two-digits ()
  (should (equal "  42) "
                 (org-parser/get-bullet "  42) what"))))

(ert-deftest get-bullet/plain-list-dash-after-ordered-list ()
  (should (equal "   - "
                 (org-parser/get-bullet "   - the /parent/ of this node is an ordered list, so there are *three* spaces"))))

(ert-deftest get-bullet/ordered-list-with-child ()
  (should (equal "1. "
                 (org-parser/get-bullet "1. what\n   - the /parent/ of this node is an ordered list, so there are *three* spaces"))))



(ert-deftest make-bullet/simple-headline ()
  (should (equal "* "
                 (org-parser/make-bullet #s(hash-table data (:text "whatever" :bullet-type ?* :children nil))
                                         ""
                                         0))))

(ert-deftest make-bullet/already-nested-headline ()
  (should (equal "** "
                 (org-parser/make-bullet #s(hash-table data (:text "whatever" :bullet-type ?* :children nil))
                                         "* "
                                         0))))

(ert-deftest make-bullet/doubly-nested-headline ()
  (should (equal "*** "
                 (org-parser/make-bullet #s(hash-table data (:text "whatever" :bullet-type ?* :children nil))
                                         "** "
                                         0))))

(ert-deftest make-bullet/simple-plain-list ()
  (should (equal "- "
                 (org-parser/make-bullet #s(hash-table data (:text "whatever" :bullet-type ?- :children nil))
                                         ""
                                         0))))

(ert-deftest make-bullet/plain-list-under-headline ()
  (should (equal "- "
                 (org-parser/make-bullet #s(hash-table data (:text "whatever" :bullet-type ?- :children nil))
                                         "* "
                                         0))))

(ert-deftest make-bullet/plain-list-under-nested-headline ()
  (should (equal "- "
                 (org-parser/make-bullet #s(hash-table data (:text "whatever" :bullet-type ?- :children nil))
                                         "*** "
                                         0))))

(ert-deftest make-bullet/plain-list-under-ordered-list ()
  (should (equal "   - "
                 (org-parser/make-bullet #s(hash-table data (:text "whatever" :bullet-type ?- :children nil))
                                         "7. "
                                         0))))

(ert-deftest make-bullet/plain-list-under-long-ordered-list ()
  (should (equal "    - "
                 (org-parser/make-bullet #s(hash-table data (:text "whatever" :bullet-type ?- :children nil))
                                         "10. "
                                         0))))

(ert-deftest make-bullet/nested-plain-list ()
  (should (equal "  - "
                 (org-parser/make-bullet #s(hash-table data (:text "whatever" :bullet-type ?- :children nil))
                                         "- "
                                         0))))

(ert-deftest make-bullet/toplevel-ordered-list-period-first-item ()
  (should (equal "1. "
                 (org-parser/make-bullet #s(hash-table data (:text "whatever" :bullet-type ?. :children nil))
                                         ""
                                         0))))

(ert-deftest make-bullet/toplevel-ordered-list-paren-first-item ()
  (should (equal "1) "
                 (org-parser/make-bullet #s(hash-table data (:text "whatever" :bullet-type ?\) :children nil))
                                         ""
                                         0))))

(ert-deftest make-bullet/toplevel-ordered-list-period-second-item ()
  (should (equal "2. "
                 (org-parser/make-bullet #s(hash-table data (:text "whatever" :bullet-type ?. :children nil))
                                         ""
                                         1))))

(ert-deftest make-bullet/toplevel-ordered-list-paren-fifth-item ()
  (should (equal "5) "
                 (org-parser/make-bullet #s(hash-table data (:text "whatever" :bullet-type ?\) :children nil))
                                         ""
                                         4))))

(ert-deftest make-bullet/indented-ordered-list-period-first-item ()
  (should (equal "   1. "
                 (org-parser/make-bullet #s(hash-table data (:text "whatever" :bullet-type ?\. :children nil))
                                         "1) "
                                         0))))

(ert-deftest make-bullet/indented-ordered-list-paren-first-item ()
  (should (equal "   1) "
                 (org-parser/make-bullet #s(hash-table data (:text "whatever" :bullet-type ?\) :children nil))
                                         "7. "
                                         0))))

(ert-deftest make-bullet/indented-ordered-list-period-first-item-under-long-parent ()
  (should (equal "    1. "
                 (org-parser/make-bullet #s(hash-table data (:text "whatever" :bullet-type ?. :children nil))
                                         "12) "
                                         0))))

(ert-deftest make-bullet/indented-ordered-list-paren-first-item-under-long-parent ()
  (should (equal "    1) "
                 (org-parser/make-bullet #s(hash-table data (:text "whatever" :bullet-type ?\) :children nil))
                                         "79. "
                                         0))))

(ert-deftest make-bullet/indented-ordered-list-period-tenth-item ()
  (should (equal "   10. "
                 (org-parser/make-bullet #s(hash-table data (:text "whatever" :bullet-type ?\. :children nil))
                                         "1) "
                                         9))))

(ert-deftest make-bullet/indented-ordered-list-paren-third-item ()
  (should (equal "   3) "
                 (org-parser/make-bullet #s(hash-table data (:text "whatever" :bullet-type ?\) :children nil))
                                         "7. "
                                         2))))


(ert-deftest nested-whitespace/under-headline ()
  (should (equal ""
                 (org-parser/get-nested-whitespace "* "))))

(ert-deftest nested-whitespace/under-nested-headline ()
  (should (equal ""
                 (org-parser/get-nested-whitespace "** "))))

(ert-deftest nested-whitespace/under-nothing ()
  (should (equal ""
                 (org-parser/get-nested-whitespace ""))))

(ert-deftest nested-whitespace/plain-list-dash ()
  (should (equal (make-string 2 ?\s)
                 (org-parser/get-nested-whitespace "- "))))

(ert-deftest nested-whitespace/plain-list-plus ()
  (should (equal (make-string 2 ?\s)
                 (org-parser/get-nested-whitespace "+ "))))

(ert-deftest nested-whitespace/ordered-list-paren ()
  (should (equal (make-string 3 ?\s)
                 (org-parser/get-nested-whitespace "1) "))))

(ert-deftest nested-whitespace/ordered-list-period ()
  (should (equal (make-string 3 ?\s)
                 (org-parser/get-nested-whitespace "1. "))))

(ert-deftest nested-whitespace/ordered-list-two-digits ()
  (should (equal (make-string 4 ?\s)
                 (org-parser/get-nested-whitespace "21. "))))

(ert-deftest nested-whitespace/plain-list-dash-indented ()
  (should (equal (make-string 4 ?\s)
                 (org-parser/get-nested-whitespace "  - "))))

(ert-deftest nested-whitespace/plain-list-plus-indented ()
  (should (equal (make-string 4 ?\s)
                 (org-parser/get-nested-whitespace "  + "))))

(ert-deftest nested-whitespace/ordered-list-paren-indented ()
  (should (equal (make-string 5 ?\s)
                 (org-parser/get-nested-whitespace "  1) "))))

(ert-deftest nested-whitespace/ordered-list-period-indented ()
  (should (equal (make-string 5 ?\s)
                 (org-parser/get-nested-whitespace "  1. "))))

(ert-deftest nested-whitespace/ordered-list-indented-two-digits ()
  (should (equal (make-string 6 ?\s)
                 (org-parser/get-nested-whitespace "  21. "))))

(ert-deftest nested-whitespace/plain-list-dash-indented-under-long-ordered-list ()
  (should (equal (make-string 5 ?\s)
                 (org-parser/get-nested-whitespace "   - "))))

(ert-deftest nested-whitespace/plain-list-plus-indented-under-long-ordered-list ()
  (should (equal (make-string 5 ?\s)
                 (org-parser/get-nested-whitespace "   + "))))

(ert-deftest nested-whitespace/ordered-list-paren-indented-under-long-ordered-list ()
  (should (equal (make-string 6 ?\s)
                 (org-parser/get-nested-whitespace "   1) "))))

(ert-deftest nested-whitespace/ordered-list-period-indented-under-long-ordered-list ()
  (should (equal (make-string 6 ?\s)
                 (org-parser/get-nested-whitespace "   1. "))))

(ert-deftest nested-whitespace/ordered-list-long-paren-indented-under-long-ordered-list ()
  (should (equal (make-string 7 ?\s)
                 (org-parser/get-nested-whitespace "   14) "))))

(ert-deftest nested-whitespace/plain-list-dash-double-indented ()
  (should (equal (make-string 6 ?\s)
                 (org-parser/get-nested-whitespace "    - "))))

(ert-deftest nested-whitespace/plain-list-plus-double-indented ()
  (should (equal (make-string 6 ?\s)
                 (org-parser/get-nested-whitespace "    + "))))

(ert-deftest nested-whitespace/ordered-list-paren-double-indented ()
  (should (equal (make-string 7 ?\s)
                 (org-parser/get-nested-whitespace "    1) "))))

(ert-deftest nested-whitespace/ordered-list-period-double-indented ()
  (should (equal (make-string 7 ?\s)
                 (org-parser/get-nested-whitespace "    1. "))))

(ert-deftest nested-whitespace/ordered-list-long-period-double-indented ()
  (should (equal (make-string 8 ?\s)
                 (org-parser/get-nested-whitespace "    31. "))))


;;;; to-string tests

(ert-deftest to-string/just-one-block ()
  (should (equal "* header\n"
                 (org-parser/to-string '(#s(hash-table data (:text "header"
                                                                   :children nil
                                                                   :bullet-type ?*)))))))

(ert-deftest to-string/just-one-plain-list-dash ()
  (should (equal "- header\n"
                 (org-parser/to-string '(#s(hash-table data (:text "header"
                                                                   :children nil
                                                                   :bullet-type ?-)))))))

(ert-deftest to-string/just-one-plain-list-plus ()
  (should (equal "+ header\n"
                 (org-parser/to-string '(#s(hash-table data (:text "header"
                                                                   :children nil
                                                                   :bullet-type ?+)))))))

(ert-deftest to-string/just-one-ordered-list-period ()
  (should (equal "1. header\n"
                 (org-parser/to-string '(#s(hash-table data (:text "header"
                                                                   :children nil
                                                                   :bullet-type ?.)))))))

(ert-deftest to-string/just-one-ordered-list-paren ()
  (should (equal "1) header\n"
                 (org-parser/to-string '(#s(hash-table data (:text "header"
                                                                   :children nil
                                                                   :bullet-type ?\))))))))

(ert-deftest to-string/two-headlines ()
  (should (equal "* header\n* header2\n"
                 (org-parser/to-string '(#s(hash-table data (:text "header"
                                                                   :children nil
                                                                   :bullet-type ?*))
                                           #s(hash-table data (:text "header2"
                                                                     :children nil
                                                                     :bullet-type ?*)))))))

(ert-deftest to-string/two-plain-lists ()
  (should (equal "+ header\n+ header2\n"
                 (org-parser/to-string '(#s(hash-table data (:text "header"
                                                                   :children nil
                                                                   :bullet-type ?+))
                                           #s(hash-table data (:text "header2"
                                                                     :children nil
                                                                     :bullet-type ?+)))))))

(ert-deftest to-string/two-ordered-lists ()
  (should (equal "1. header\n2. header2\n"
                 (org-parser/to-string '(#s(hash-table data (:text "header"
                                                                   :children nil
                                                                   :bullet-type ?.))
                                           #s(hash-table data (:text "header2"
                                                                     :children nil
                                                                     :bullet-type ?.)))))))


(ert-deftest single-to-string/headline-and-body ()
  (should (equal "* whatever\nHere's a body\n"
                 (org-parser/single-to-string #s(hash-table data (:text "whatever"
                                                                        :children nil
                                                                        :bullet-type ?*
                                                                        :body (("Here's a body"))))
                                              ""
                                              0))))

(ert-deftest single-to-string/headline-and-multiline-body ()
  (should (equal "* whatever\nHere's a body\non two lines\n"
                 (org-parser/single-to-string #s(hash-table data (:text "whatever"
                                                                        :children nil
                                                                        :bullet-type ?*
                                                                        :body (("Here's a body") ("on two lines"))))
                                              ""
                                              0))))


;;;; tests that go all the way around -- from a string to a structure to the original string
(ert-deftest to-structure-to-string/just-one-block ()
  (should (equal "* header\n"
                 (org-parser/to-string (org-parser "* header\n")))))

(ert-deftest to-structure-to-string/just-one-block-with-body ()
  (should (equal "* header\nwith body\n"
                 (org-parser/to-string (org-parser "* header\nwith body\n")))))

(ert-deftest to-structure-to-string/just-one-block-with-multiline-body ()
  (should (equal "* header\nwith body\non two lines\n"
                 (org-parser/to-string (org-parser "* header\nwith body\non two lines")))))

(ert-deftest to-structure-to-string/simply-nested ()
  (should (equal "* header\n** nested\n"
                 (org-parser/to-string (org-parser "* header\n** nested\n")))))

(ert-deftest to-structure-to-string/two-children ()
  (should (equal "* header\n** first child\n** second child\n"
                 (org-parser/to-string (org-parser "* header\n** first child\n** second child\n")))))

(ert-deftest to-structure-to-string/two-blocks ()
  (should (equal "* header\n* second\n"
                 (org-parser/to-string (org-parser "* header\n* second\n")))))

(ert-deftest to-structure-to-string/three-levels ()
  (should (equal "* header\n* second header\n** first child\n*** I'm forgotten about\n** second child\n*** this is the test grandchild\n*** this is the other test grandchild.\n"
                 (org-parser/to-string (org-parser "* header\n* second header\n** first child\n*** I'm forgotten about\n** second child\n*** this is the test grandchild\n*** this is the other test grandchild.\n")))))

(ert-deftest to-structure-to-string/lots-of-bullet-types ()
  (should (equal "* whatever\n** two\n1. what\n   - the /parent/ of this node is an ordered list, so there are *three* spaces\n"
                 (org-parser/to-string (org-parser "* whatever\n** two\n1. what\n   - the /parent/ of this node is an ordered list, so there are *three* spaces")))))

(ert-deftest to-structure-to-string/headline-then-plain-list ()
  (should (equal "* first thing\n+ nested thing\n"
                 (org-parser/to-string (org-parser "* first thing\n+ nested thing\n")))))

(ert-deftest to-structure-to-string/nested-headline-then-plain-list ()
  (should (equal "* first thing\n** nested headline\n+ nested thing\n"
                 (org-parser/to-string (org-parser "* first thing\n** nested headline\n+ nested thing\n")))))

(ert-deftest to-structure-to-string/headline-then-double-nested-plain-list ()
  (should (equal "* first thing\n+ nested headline\n  + nested thing\n"
                 (org-parser/to-string (org-parser "* first thing\n+ nested headline\n  + nested thing\n")))))

(ert-deftest to-structure-to-string/just-one-plain-list-plus ()
  (should (equal "+ header\n"
                 (org-parser/to-string (org-parser "+ header\n")))))

(ert-deftest to-structure-to-string/just-one-plain-list-dash ()
  (should (equal "- header\n"
                 (org-parser/to-string (org-parser "- header\n")))))

(ert-deftest to-structure-to-string/just-one-ordered-list-period ()
  (should (equal "1. header\n"
                 (org-parser/to-string (org-parser "1. header\n")))))

(ert-deftest to-structure-to-string/just-one-ordered-list-paren ()
  (should (equal "1) header\n"
                 (org-parser/to-string (org-parser "1) header\n")))))

(ert-deftest to-structure-to-string/ordered-lists ()
  (should (equal "1. first\n2. second\n"
                 (org-parser/to-string (org-parser "1. first\n2. second\n")))))

(ert-deftest to-structure-to-string/plain-list-under-ordered ()
  (should (equal "1. thing\n   + yep, nested\n"
                 (org-parser/to-string (org-parser "1. thing\n   + yep, nested\n")))))

(ert-deftest to-structure-to-string/plain-list-under-ordered ()
  (should (equal "1. thing\n   + yep, nested\n"
                 (org-parser/to-string (org-parser "1. thing\n   + yep, nested\n")))))

(ert-deftest to-structure-to-string/long-ordered-list-with-child ()
  (should (equal "1. thing\n2. thing\n3. thing\n4. thing\n5. thing\n6. thing\n7. thing\n8. thing\n9. thing\n10. thing\n    + yep, nested\n"
                 (org-parser/to-string (org-parser "1. thing\n2. thing\n3. thing\n4. thing\n5. thing\n6. thing\n7. thing\n8. thing\n9. thing\n10. thing\n    + yep, nested\n")))))


(ert-deftest to-structure-to-string/link-in-headline ()
  (let ((input "* headline with [[http://example.com][link]]!\n"))
    (should (equal input
                   (org-parser/to-string (org-parser input))))))

(ert-deftest to-structure-to-string/link-in-body ()
  (let ((input "* headline no link\nUntil the [[http://example.com][body]]!\n"))
    (should (equal input
                   (org-parser/to-string (org-parser input))))))

(ert-deftest to-structure-to-string/link-in-nested-headline ()
  (let ((input "* headline\n** Second headline with [[http://example.com][a link]]!\n"))
    (should (equal input
                   (org-parser/to-string (org-parser input))))))

(ert-deftest to-structure-to-string/link-in-ordered-list ()
  (let ((input "* headline\n1. Here's an ordered list\n2. With a [[http://bitbucket.org/zck/org-parser.el][link in the ordering]] with a bunch more text after\n"))
    (should (equal input
                   (org-parser/to-string (org-parser input))))))


(ert-deftest format-text/basic-string ()
  (should (equal "Yep, basic text."
                 (org-parser/format-text "Yep, basic text."))))

(ert-deftest format-text/string-list ()
  (should (equal "Yep, basic text."
                 (org-parser/format-text (list "Yep, basic" " text.")))))

(ert-deftest format-text/link-list ()
  (should (equal "Hi there this is [[http://example.com][a link]]!"
                 (org-parser/format-text (list "Hi there this is "
                                               (org-parser/make-link-hash "http://example.com" "a link")
                                               "!")))))

(ert-deftest format-text/multiple-link-list ()
  (should (equal "Hi there this is [[http://example.com][one link]] [[http://zck.me][two links]]!"
                 (org-parser/format-text (list "Hi there this is "
                                               (org-parser/make-link-hash "http://example.com" "one link")
                                               " "
                                               (org-parser/make-link-hash "http://zck.me" "two links")
                                               "!")))))

(ert-deftest format-text-single-item/string ()
  (should (equal "Some text here."
                 (org-parser/format-text-single-item "Some text here."))))

(ert-deftest format-text-single-item/string ()
  (should (equal "[[http://example.com][I'm a link!]]"
                 (org-parser/format-text-single-item (org-parser/make-link-hash "http://example.com" "I'm a link!")))))

(ert-deftest format-body/single-line ()
  (should (equal "I'm a body!"
                 (org-parser/format-body '(("I'm a body!"))))))

(ert-deftest format-body/two-lines ()
  (should (equal "I'm a body\nAnd so am I!"
                 (org-parser/format-body '(("I'm a body")
                                           ("And so am I!"))))))

(ert-deftest format-body/single-line-with-link ()
  (should (equal "I've got [[http://example.com][a link]] inside me!"
                 (org-parser/format-body (list (list "I've got "
                                                     (org-parser/make-link-hash "http://example.com"
                                                                                "a link")
                                                     " inside me!"))))))

(ert-deftest format-body/multiple-lines-with-link ()
  (should (equal "One body line and then:\nI've got [[http://example.com][a link]] inside me!"
                 (org-parser/format-body (list (list "One body line and then:")
                                               (list "I've got "
                                                     (org-parser/make-link-hash "http://example.com"
                                                                                "a link")
                                                     " inside me!"))))))

(ert-deftest format-body-line/single-string ()
  (should (equal "Some text here"
                 (org-parser/format-body-line '("Some text here")))))

(ert-deftest format-body-line/multiple-strings ()
  (should (equal "I've got some text in the body."
                 (org-parser/format-body-line '("I've got" " some text " "in" " the body.")))))

(ert-deftest format-body-line/single-link ()
  (should (equal "[[http://example.com][I'm a link]]"
                 (org-parser/format-body-line (list (org-parser/make-link-hash "http://example.com"
                                                                               "I'm a link"))))))

(ert-deftest format-body-line/strings-and-link ()
  (should (equal "I'm text and [[http://example.com][I'm a link]] and I'm more text and [[http://example.com][I'm another link]]"
                 (org-parser/format-body-line (list "I'm text and "
                                                    (org-parser/make-link-hash "http://example.com"
                                                                               "I'm a link")
                                                    " and I'm more text and "
                                                    (org-parser/make-link-hash "http://example.com"
                                                                               "I'm another link"))))))



(ert-deftest nested-children/no-indices ()
  (let ((looked-up-table (org-parser/get-nested-children #s(hash-table data (:children children :text "whatever")))))
    (should looked-up-table)
    (should (equal 2
                   (hash-table-count looked-up-table)))
    (should (equal 'children
                   (gethash :children looked-up-table)))
    (should (equal "whatever"
                   (gethash :text looked-up-table)))))

(ert-deftest nested-children/single-index ()
  (should (equal 3
                 (org-parser/get-nested-children #s(hash-table data (:children (2 3 4)))
                                                 1))))

(ert-deftest nested-children/missing-index ()
  (should-not (org-parser/get-nested-children #s(hash-table data (:children (2 3 4)))
                                              14)))

(ert-deftest nested-children/too-many-indices ()
  (should-not (org-parser/get-nested-children #s(hash-table data (:children (2 #s(hash-table data (:children nil :text "whatever")) 4)))
                                              1 1)))

(ert-deftest nested-children/two-indices ()
  (should (equal :im-nested
                 (org-parser/get-nested-children #s(hash-table data (:children (2 #s(hash-table data (:children (0 1 :im-nested))))))
                                                 1 2))))


(ert-deftest bullet-type-headline ()
  (should (equal ?*
                 (org-parser/bullet-type "* "))))

(ert-deftest bullet-type-nested-headline ()
  (should (equal ?*
                 (org-parser/bullet-type "** "))))

(ert-deftest bullet-type-plain-list-dash ()
  (should (equal ?-
                 (org-parser/bullet-type "- "))))

(ert-deftest bullet-type-plain-list-nested-dash ()
  (should (equal ?-
                 (org-parser/bullet-type "  - "))))

(ert-deftest bullet-type-plain-list-nested-plus ()
  (should (equal ?+
                 (org-parser/bullet-type "     + "))))

(ert-deftest bullet-type-plain-list-number-paren ()
  (should (equal ?\)
                 (org-parser/bullet-type "14) "))))

(ert-deftest bullet-type-plain-list-number-period ()
  (should (equal ?.
                 (org-parser/bullet-type "3. "))))

(ert-deftest bullet-type-plain-list-number-nested-paren ()
  (should (equal ?\)
                 (org-parser/bullet-type "  14) "))))

(ert-deftest bullet-type-plain-list-number-nested-period ()
  (should (equal ?.
                 (org-parser/bullet-type "     3. "))))


(ert-deftest remove-bullet/simple-headline ()
  (should (equal "headline"
                 (org-parser/remove-bullet "* headline"))))

(ert-deftest remove-bullet/headline-2 ()
  (should (equal "my headline"
                 (org-parser/remove-bullet "** my headline"))))

(ert-deftest remove-bullet/headline-3 ()
  (should (equal "another headline"
                 (org-parser/remove-bullet "*** another headline"))))

(ert-deftest remove-bullet/plain-list ()
  (should (equal "a new list"
                 (org-parser/remove-bullet "- a new list"))))

(ert-deftest remove-bullet/indented-plain-list ()
  (should (equal "a new list"
                 (org-parser/remove-bullet "  - a new list"))))

(ert-deftest remove-bullet/plain-list-plus ()
  (should (equal "a new list"
                 (org-parser/remove-bullet "+ a new list"))))

(ert-deftest remove-bullet/indented-plain-list-plus ()
  (should (equal "a new list"
                 (org-parser/remove-bullet "  + a new list"))))

(ert-deftest remove-bullet/quite-indented-plain-list-plus ()
  (should (equal "a new list"
                 (org-parser/remove-bullet "     + a new list"))))

(ert-deftest remove-bullet/stupid-plain-list-with-asterisks ()
  (should (equal "this is a dumb kind of list"
                 (org-parser/remove-bullet "  * this is a dumb kind of list"))))

(ert-deftest remove-bullet/ordered-list ()
  (should (equal "a new list"
                 (org-parser/remove-bullet "1. a new list"))))

(ert-deftest remove-bullet/indented-ordered-list ()
  (should (equal "a new list"
                 (org-parser/remove-bullet "  1. a new list"))))

(ert-deftest remove-bullet/ordered-list-paren ()
  (should (equal "a new list"
                 (org-parser/remove-bullet "11) a new list"))))

(ert-deftest remove-bullet/indented-ordered-list-paren ()
  (should (equal "a new list"
                 (org-parser/remove-bullet "  2) a new list"))))

(ert-deftest remove-bullet/quite-ordered-plain-list-paren ()
  (should (equal "a new list"
                 (org-parser/remove-bullet "     7) a new list"))))

(ert-deftest remove-bullet/headline-plain-text-with-body ()
  (should (equal "I'm the text\nbut I'm the body"
                 (org-parser/remove-bullet "* I'm the text\nbut I'm the body"))))

(ert-deftest remove-bullet/plain-list-plain-text-with-body ()
  (should (equal "I'm the text\nbut I'm the body"
                 (org-parser/remove-bullet "- I'm the text\nbut I'm the body"))))

(ert-deftest remove-bullet/plain-list-plain-text-with-body ()
  (should (equal "I'm the text\nbut I'm the body"
                 (org-parser/remove-bullet "27. I'm the text\nbut I'm the body"))))

(ert-deftest remove-bullet/headline-plain-text-with-multiline-body ()
  (should (equal "I'm the text\nbut I'm the body\nand so am I."
                 (org-parser/remove-bullet "* I'm the text\nbut I'm the body\nand so am I."))))

(ert-deftest remove-bullet/plain-list-plain-text-with-multiline-body ()
  (should (equal "I'm the text\nbut I'm the body\nand so am I."
                 (org-parser/remove-bullet "- I'm the text\nbut I'm the body\nand so am I."))))

(ert-deftest remove-bullet/headline-dont-need-space-after-bullet ()
  (should (equal "I'm text"
                 (org-parser/remove-bullet "*I'm text"))))

(ert-deftest remove-bullet/plain-list-dont-need-space-after-bullet ()
  (should (equal "I'm text"
                 (org-parser/remove-bullet "-I'm text"))))

(ert-deftest remove-bullet/ordered-list-dont-need-space-after-bullet ()
  (should (equal "I'm text"
                 (org-parser/remove-bullet "7.I'm text"))))


(ert-deftest make-link-hash/basic ()
  (let ((link-hash (org-parser/make-link-hash "http://zck.me/" "This is a link!")))
    (should (hash-table-p link-hash))
    (should (equal "http://zck.me/"
                   (gethash :target link-hash)))
    (should (equal "This is a link!"
                   (gethash :text link-hash)))))


(ert-deftest parse-for-markup/no-markup ()
  (should (equal (list "I'm a headline")
                 (org-parser/parse-for-markup "I'm a headline"))))

(ert-deftest parse-for-markup/only-a-link ()
  (let ((parsed (org-parser/parse-for-markup "[[http://zck.me/][my site]]")))
    (should (equal 1 (length parsed)))
    (should (hash-table-p (first parsed)))
    (should (equal :link (gethash :type (first parsed))))
    (should (equal "http://zck.me/" (gethash :target (first parsed))))
    (should (equal "my site" (gethash :text (first parsed))))))

(ert-deftest parse-for-markup/text-before-link ()
  (let ((parsed (org-parser/parse-for-markup "Here's a link -> [[http://zck.me/][my site]]")))
    (should (equal 2 (length parsed)))
    (should (equal "Here's a link -> " (first parsed)))
    (should (hash-table-p (second parsed)))
    (should (equal :link (gethash :type (second parsed))))
    (should (equal "http://zck.me/" (gethash :target (second parsed))))
    (should (equal "my site" (gethash :text (second parsed))))))

(ert-deftest parse-for-markup/text-after-link ()
  (let ((parsed (org-parser/parse-for-markup "[[http://zck.me/][my site]] <- there it was")))
    (should (equal 2 (length parsed)))
    (should (hash-table-p (first parsed)))
    (should (equal :link (gethash :type (first parsed))))
    (should (equal "http://zck.me/" (gethash :target (first parsed))))
    (should (equal "my site" (gethash :text (first parsed))))
    (should (equal " <- there it was" (second parsed)))))

(ert-deftest parse-for-markup/text-before-and-after-link ()
  (let ((parsed (org-parser/parse-for-markup "Here's a link -> [[http://zck.me/][my site]] <- there it was")))
    (should (equal 3 (length parsed)))
    (should (equal "Here's a link -> " (first parsed)))
    (should (hash-table-p (second parsed)))
    (should (equal :link (gethash :type (second parsed))))
    (should (equal "http://zck.me/" (gethash :target (second parsed))))
    (should (equal "my site" (gethash :text (second parsed))))
    (should (equal " <- there it was" (third parsed)))))

(ert-deftest parse-for-markup/two-links ()
  (let ((parsed (org-parser/parse-for-markup "[[http://zck.me/][my site]][[https://www.gnu.org/software/emacs/][Emacs!]]")))
    (should (equal 2 (length parsed)))
    (should (equal :link (gethash :type (first parsed))))
    (should (equal "http://zck.me/" (gethash :target (first parsed))))
    (should (equal :link (gethash :type (second parsed))))
    (should (equal "https://www.gnu.org/software/emacs/" (gethash :target (second parsed))))))



(ert-deftest guess-level/headline-one ()
  (should (equal 1
                 (org-parser/guess-level "* headline"))))

(ert-deftest guess-level/headline-two ()
  (should (equal 2
                 (org-parser/guess-level "** headline"))))

(ert-deftest guess-level/headline-three ()
  (should (equal 3
                 (org-parser/guess-level "*** headline"))))

(ert-deftest guess-level/headline-children-dont-matter ()
  (should (equal 2
                 (org-parser/guess-level "** headline\n*** nested!"))))

(ert-deftest guess-level/plain-list-dash-one ()
  (should (equal 1
                 (org-parser/guess-level "- title"))))

(ert-deftest guess-level/plain-list-plus-one ()
  (should (equal 1
                 (org-parser/guess-level "+ title"))))

(ert-deftest guess-level/plain-list-dash-two ()
  (should (equal 2
                 (org-parser/guess-level "  - title"))))

(ert-deftest guess-level/plain-list-plus-two ()
  (should (equal 2
                 (org-parser/guess-level "  + title"))))

(ert-deftest guess-level/plain-list-dash-three ()
  (should (equal 3
                 (org-parser/guess-level "    - title"))))

(ert-deftest guess-level/plain-list-plus-three ()
  (should (equal 3
                 (org-parser/guess-level "    + title"))))

(ert-deftest guess-level/plain-list-dash-one-and-a-half ()
  (should (equal 2
                 (org-parser/guess-level "   - title"))))

(ert-deftest guess-level/plain-list-plus-one-and-a-half ()
  (should (equal 2
                 (org-parser/guess-level "   + title"))))

(ert-deftest guess-level/plain-list-dash-two-and-a-half ()
  (should (equal 3
                 (org-parser/guess-level "     - title"))))

(ert-deftest guess-level/plain-list-plus-two-and-a-half ()
  (should (equal 3
                 (org-parser/guess-level "     + title"))))




(ert-deftest make-text-tree/one-headline ()
  (should (equal '(("* one line"))
                 (org-parser/make-text-tree '("* one line")))))

(ert-deftest make-text-tree/one-headline-with-body ()
  (should (equal '(("* one line\nand a body"))
                 (org-parser/make-text-tree '("* one line\nand a body")))))

(ert-deftest make-text-tree/two-headlines ()
  (should (equal '(("* first headline") ("* second headline"))
                 (org-parser/make-text-tree '("* first headline" "* second headline")))))

(ert-deftest make-text-tree/one-nested-headline ()
  (should (equal '(("* first headline" ("** nested headline")))
                 (org-parser/make-text-tree '("* first headline" "** nested headline")))))

(ert-deftest make-text-tree/nested-plain-list ()
  (should (equal '(("* first headline" ("- I'm nested")))
                 (org-parser/make-text-tree '("* first headline" "- I'm nested")))))

(ert-deftest make-text-tree/nested-plain-list-and-nested-headline ()
  (should (equal '(("* first headline" ("- I'm nested") ("** I'm nested too")))
                 (org-parser/make-text-tree '("* first headline" "- I'm nested" "** I'm nested too")))))

(ert-deftest make-text-tree/nested-plain-list-and-nested-headline-with-followup-headline ()
  (should (equal '(("* first headline" ("- I'm nested") ("** I'm nested too")) ("* also first-level headline"))
                 (org-parser/make-text-tree '("* first headline" "- I'm nested" "** I'm nested too" "* also first-level headline")))))

(ert-deftest make-text-tree/multiple-headlines ()
  (should (equal '(("* first headline") ("* second headline") ("* third headline"))
                 (org-parser/make-text-tree '("* first headline" "* second headline" "* third headline")))))

(ert-deftest make-text-tree/multiple-nested-headlines ()
  (should (equal '(("* first headline" ("** nested headline")) ("* second top headline" ("** and more children") ("** and more more children")))
                 (org-parser/make-text-tree '("* first headline" "** nested headline" "* second top headline" "** and more children" "** and more more children")))))



(ert-deftest split-into-blocks/single-headline ()
  (should (equal '("* headline")
                 (org-parser/split-into-blocks "* headline\n"))))

(ert-deftest split-into-blocks/single-headline-with-text ()
  (should (equal '("* headline\nhere's some text")
                 (org-parser/split-into-blocks "* headline\nhere's some text"))))

(ert-deftest split-into-blocks/two-headlines ()
  (should (equal '("* headline" "* another headline")
                 (org-parser/split-into-blocks "* headline\n* another headline"))))

(ert-deftest split-into-blocks/nested-headlines ()
  (should (equal '("* headline" "** another headline")
                 (org-parser/split-into-blocks "* headline\n** another headline"))))

(ert-deftest split-into-blocks/single-plain-list ()
  (should (equal '("- plain-list")
                 (org-parser/split-into-blocks "- plain-list\n"))))

(ert-deftest split-into-blocks/single-plain-list-with-text ()
  (should (equal '("- plain-list\nhere's some text")
                 (org-parser/split-into-blocks "- plain-list\nhere's some text"))))


(ert-deftest split-into-blocks/two-plain-lists ()
  (should (equal '("- plain-list" "- another plain-list")
                 (org-parser/split-into-blocks "- plain-list\n- another plain-list"))))

(ert-deftest split-into-blocks/nested-plain-lists ()
  (should (equal '("- plain-list" "- another plain-list")
                 (org-parser/split-into-blocks "- plain-list\n- another plain-list"))))




(ert-deftest convert-text-block/simple-headline-text ()
  (should (equal '("whatever")
                 (gethash :text (org-parser/convert-text-block '("* whatever"))))))

(ert-deftest convert-text-block/simple-headline-text-ignoring-body ()
  (should (equal '("whatever")
                 (gethash :text (org-parser/convert-text-block '("* whatever\nbody to ignore"))))))

(ert-deftest convert-text-block/simple-headline-body ()
  (should (equal '(("I'm a body!"))
                 (gethash :body (org-parser/convert-text-block '("* whatever\nI'm a body!"))))))

(ert-deftest convert-text-block/simple-headline-multiple-line-body ()
  (should (equal '(("I'm a body!") ("And still I come"))
                 (gethash :body (org-parser/convert-text-block '("* whatever\nI'm a body!\nAnd still I come"))))))

(ert-deftest convert-text-block/simple-headline-bullet-type ()
  (should (equal ?*
                 (gethash :bullet-type (org-parser/convert-text-block '("* whatever"))))))

(ert-deftest convert-text-block/simple-headline-children ()
  (should-not (gethash :children (org-parser/convert-text-block '("* whatever")))))

(ert-deftest convert-text-block/nested-headline-text ()
  (should (equal '("whatever")
                 (gethash :text (org-parser/convert-text-block '("* whatever" ("** nested")))))))

(ert-deftest convert-text-block/nested-headline-bullet-type ()
  (should (equal ?*
                 (gethash :bullet-type (org-parser/convert-text-block '("* whatever" ("** nested")))))))

(ert-deftest convert-text-block/nested-headline-children ()
  (should (equal 1
                 (length (gethash :children (org-parser/convert-text-block '("* whatever" ("** nested"))))))))

(ert-deftest convert-text-block/nested-headline-child-text ()
  (should (equal '("nested here!")
                 (gethash :text
                          (org-parser/get-nested-children (org-parser/convert-text-block '("* whatever" ("** nested here!")))
                                                          0)))))

(ert-deftest convert-text-block/nested-headline-child-bullet ()
  (should (equal ?*
                 (gethash :bullet-type
                          (org-parser/get-nested-children (org-parser/convert-text-block '("* whatever" ("** nested here!")))
                                                          0)))))

(ert-deftest convert-text-block/nested-headline-child-children ()
  (should-not (gethash :children
                       (org-parser/get-nested-children (org-parser/convert-text-block '("* whatever" ("** nested here!")))
                                                       0))))

(ert-deftest convert-text-block/multiple-nested-children ()
  (should (equal 3
                 (length (gethash :children (org-parser/convert-text-block '("* whatever" ("** nested") ("** nested two") ("** nested three!"))))))))


(ert-deftest get-text/headline-plain-text ()
  (should (equal '("I'm the text")
                 (org-parser/get-text "* I'm the text"))))

(ert-deftest get-text/plain-list-plain-text ()
  (should (equal '("I'm the text")
                 (org-parser/get-text "- I'm the text"))))

(ert-deftest get-text/headline-plain-text-with-body ()
  (should (equal '("I'm the text")
                 (org-parser/get-text "** I'm the text\nbut I'm the body"))))

(ert-deftest get-text/plain-list-plain-text-with-body ()
  (should (equal '("I'm the text")
                 (org-parser/get-text "- I'm the text\nbut I'm the body"))))

(ert-deftest get-text/headline-plain-text-with-multiline-body ()
  (should (equal '("I'm the text")
                 (org-parser/get-text "* I'm the text\nbut I'm the body\nand so am I."))))

(ert-deftest get-text/plain-list-plain-text-with-multiline-body ()
  (should (equal '("I'm the text")
                 (org-parser/get-text "- I'm the text\nbut I'm the body\nand so am I."))))

(ert-deftest get-text/headline-with-link ()
  (let ((gotten-text (org-parser/get-text "* headline [[https://bitbucket.org/zck/org-parser.el][with a link]] and text after")))
    (should (listp gotten-text))
    (should (equal 3 (length gotten-text)))
    (should (stringp (first gotten-text)))
    (should (hash-table-p (second gotten-text)))
    (should (stringp (third gotten-text)))))

(ert-deftest get-text/headline-with-link-and-body ()
  (let ((gotten-text (org-parser/get-text "* headline [[https://bitbucket.org/zck/org-parser.el][with a link]] and text after\nand more stuff")))
    (should (listp gotten-text))
    (should (equal 3 (length gotten-text)))
    (should (stringp (first gotten-text)))
    (should (hash-table-p (second gotten-text)))
    (should (stringp (third gotten-text)))))



(ert-deftest get-body/headline-plain-text ()
  (should-not (org-parser/get-body "* I'm the text")))

(ert-deftest get-body/plain-list-plain-text ()
  (should-not (org-parser/get-body "- I'm the text")))

(ert-deftest get-body/headline-plain-text-with-body ()
  (should (equal '(("but I'm the body"))
                 (org-parser/get-body "* I'm the text\nbut I'm the body"))))

(ert-deftest get-body/plain-list-plain-text-with-body ()
  (should (equal '(("but I'm the body"))
                 (org-parser/get-body "- I'm the text\nbut I'm the body"))))

(ert-deftest get-body/headline-plain-text-with-multiline-body ()
  (should (equal '(("but I'm the body") ("and so am I."))
                 (org-parser/get-body "* I'm the text\nbut I'm the body\nand so am I."))))

(ert-deftest get-body/plain-list-plain-text-with-multiline-body ()
  (should (equal '(("but I'm the body") ("and so am I."))
                 (org-parser/get-body "- I'm the text\nbut I'm the body\nand so am I."))))

(ert-deftest get-body/headline-with-link-in-body ()
  (let ((gotten-text (org-parser/get-body "* headline\nWith a body [[https://bitbucket.org/zck/org-parser.el][with a link]] and text after")))
    (should (listp gotten-text))
    (should (equal 1 (length gotten-text)))
    (should (stringp (first (first gotten-text))))
    (should (hash-table-p (second (first gotten-text))))
    (should (stringp (third (first gotten-text))))))



(ert-deftest convert-text-tree/one-headline ()
  (should (equal 1
                 (length (org-parser/convert-text-tree '(("* a single headline")))))))

(ert-deftest convert-text-tree/one-headline-with-children ()
  (should (equal 1
                 (length (org-parser/convert-text-tree '(("* a single headline" ("** but with children"))))))))



(ert-deftest descendent/headline-1-headline-1 ()
  (should-not (org-parser/descendent? "* headline 1" "* another headline")))

(ert-deftest descendent/headline-2-headline-2 ()
  (should-not (org-parser/descendent? "** headline 1" "** another headline")))

(ert-deftest descendent/headline-1-headline-2 ()
  (should (org-parser/descendent? "* headline 1" "** nested headline")))

(ert-deftest descendent/headline-1-headline-3 ()
  (should (org-parser/descendent? "* headline 1" "*** another headline")))

(ert-deftest descendent/headline-1-plain-1 ()
  (should (org-parser/descendent? "* headline 1" "- plain here")))

(ert-deftest descendent/headline-2-plain-1 ()
  (should (org-parser/descendent? "** headline 1" "- plain here")))

(ert-deftest descendent/headline-1-plain-2 ()
  (should (org-parser/descendent? "* headline 1" " - plain list")))

(ert-deftest descendent/headline-1-ordered-1 ()
  (should (org-parser/descendent? "* headline 1" "17. ordered")))

(ert-deftest descendent/plain-1-headline-1 ()
  (should-not (org-parser/descendent? "- plain" "* headline")))

(ert-deftest descendent/plain-1-headline-2 ()
  (should-not (org-parser/descendent? "- plain" "** nested")))



(ert-deftest headline?/empty ()
  (should-not (org-parser/headline? "")))

(ert-deftest headline?/asterisk-char ()
  (should (org-parser/headline? ?*)))

(ert-deftest headline?/dash-char ()
  (should-not (org-parser/headline? ?-)))

(ert-deftest headline?/plus-char ()
  (should-not (org-parser/headline? ?+)))

(ert-deftest headline?/paren-char ()
  (should-not (org-parser/headline? ?\))))

(ert-deftest headline?/period-char ()
  (should-not (org-parser/headline? ?.)))

(ert-deftest headline?/headline-1 ()
  (should (org-parser/headline? "* yep, I'm good")))

(ert-deftest headline?/headline-2 ()
  (should (org-parser/headline? "** yep, I'm good")))

(ert-deftest headline?/headline-3 ()
  (should (org-parser/headline? "*** yep, I'm good")))

(ert-deftest headline?/plain-1-dash ()
  (should-not (org-parser/headline? "- no way")))

(ert-deftest headline?/plain-2-dash ()
  (should-not (org-parser/headline? "  - no way")))

(ert-deftest headline?/plain-3-dash ()
  (should-not (org-parser/headline? "    - no way")))

(ert-deftest headline?/plain-1-plus ()
  (should-not (org-parser/headline? "+ no way")))

(ert-deftest headline?/plain-2-plus ()
  (should-not (org-parser/headline? "  + no way")))

(ert-deftest headline?/plain-3-plus ()
  (should-not (org-parser/headline? "    + no way")))

(ert-deftest headline?/ordered-1 ()
  (should-not (org-parser/headline? "1. no way")))

(ert-deftest headline?/ordered-2 ()
  (should-not (org-parser/headline? "  2) no way")))

(ert-deftest headline?/ordered-3 ()
  (should-not (org-parser/headline? "    4. no way")))

(ert-deftest headline?/plain-list-with-indented-asterisk ()
  (should-not (org-parser/headline? "  * no way")))



(ert-deftest plain-list?/empty ()
  (should-not (org-parser/plain-list? "")))

(ert-deftest plain-list?/asterisk-char ()
  (should-not (org-parser/plain-list? ?*)))

(ert-deftest plain-list?/dash-char ()
  (should (org-parser/plain-list? ?-)))

(ert-deftest plain-list?/plus-char ()
  (should (org-parser/plain-list? ?+)))

(ert-deftest plain-list?/paren-char ()
  (should (org-parser/plain-list? ?\))))

(ert-deftest plain-list?/period-char ()
  (should (org-parser/plain-list? ?.)))

(ert-deftest plain-list?/headline-1 ()
  (should-not (org-parser/plain-list? "* a headline?!")))

(ert-deftest plain-list?/headline-2 ()
  (should-not (org-parser/plain-list? "** a headline?!")))

(ert-deftest plain-list?/headline-3 ()
  (should-not (org-parser/plain-list? "*** a headline?!")))

(ert-deftest plain-list?/plain-1-dash ()
  (should (org-parser/plain-list? "- plain here, but not sad")))

(ert-deftest plain-list?/plain-2-dash ()
  (should (org-parser/plain-list? "  - plain here, but not sad")))

(ert-deftest plain-list?/plain-3-dash ()
  (should (org-parser/plain-list? "    - plain here, but not sad")))

(ert-deftest plain-list?/plain-1-plus ()
  (should (org-parser/plain-list? "+ plain here, but not sad")))

(ert-deftest plain-list?/plain-2-plus ()
  (should (org-parser/plain-list? "  + plain here, but not sad")))

(ert-deftest plain-list?/plain-3-plus ()
  (should (org-parser/plain-list? "    + plain here, but not sad")))

(ert-deftest plain-list?/ordered-1 ()
  (should (org-parser/plain-list? "1. plain here, but not sad")))

(ert-deftest plain-list?/ordered-2 ()
  (should (org-parser/plain-list? "  2) plain here, but not sad")))

(ert-deftest plain-list?/ordered-3 ()
  (should (org-parser/plain-list? "    4. plain here, but not sad")))

(ert-deftest plain-list?/plain-list-with-indented-asterisk ()
  (should (org-parser/plain-list? "  * plain here, but not sad")))

(ert-deftest plain-list?/random-text ()
  (should-not (org-parser/plain-list? "Not anything useful, even with * and -.")))



(ert-deftest title-line?/headline ()
  (should (org-parser/title-line? "* whatever")))

(ert-deftest title-line?/headline-with-body ()
  (should (org-parser/title-line? "* whatever\nhere's a body")))

(ert-deftest title-line?/double-headline ()
  (should (org-parser/title-line? "** whatever")))

(ert-deftest title-line?/plain-list ()
  (should (org-parser/title-line? "- whatever")))

(ert-deftest title-line?/indented-plain-list ()
  (should (org-parser/title-line? "  + whatever")))

(ert-deftest title-line?/plain-list-with-body ()
  (should (org-parser/title-line? "- whatever\nand here's a body")))

(ert-deftest title-line?/not-a-title ()
  (should-not (org-parser/title-line? "Not anything useful, even with * and -.")))

(ert-deftest title-line?/not-a-title-indented ()
  (should-not (org-parser/title-line? "   Not anything useful, even with * and -.")))



(ert-deftest ordered-list/paren-char ()
  (should (org-parser/ordered-list? ?\))))

(ert-deftest ordered-list/dot-char ()
  (should (org-parser/ordered-list? ?.)))

(ert-deftest ordered-list/dash-char ()
  (should-not (org-parser/ordered-list? ?-)))

(ert-deftest ordered-list/plus-char ()
  (should-not (org-parser/ordered-list? ?+)))

(ert-deftest ordered-list/asterisk-char ()
  (should-not (org-parser/ordered-list? ?*)))

(ert-deftest ordered-list/toplevel-paren ()
  (should (org-parser/ordered-list? "7) ")))

(ert-deftest ordered-list/toplevel-dot ()
  (should (org-parser/ordered-list? "3. ")))

(ert-deftest ordered-list/toplevel-two-digit ()
  (should (org-parser/ordered-list? "12) ")))

(ert-deftest ordered-list/toplevel-headline ()
  (should-not (org-parser/ordered-list? "* ")))

(ert-deftest ordered-list/toplevel-dash-list ()
  (should-not (org-parser/ordered-list? "- ")))

(ert-deftest ordered-list/toplevel-plus-list ()
  (should-not (org-parser/ordered-list? "+ ")))

(ert-deftest ordered-list/indented-paren ()
  (should (org-parser/ordered-list? "  7) ")))

(ert-deftest ordered-list/indented-dot ()
  (should (org-parser/ordered-list? "   3. ")))

(ert-deftest ordered-list/indented-two-digit ()
  (should (org-parser/ordered-list? "  12) ")))

(ert-deftest ordered-list/indented-headline ()
  (should-not (org-parser/ordered-list? "** ")))

(ert-deftest ordered-list/indented-dash-list ()
  (should-not (org-parser/ordered-list? "  - ")))

(ert-deftest ordered-list/indented-plus-list ()
  (should-not (org-parser/ordered-list? "  + ")))


(ert-deftest two-lines-on-second-level/only-one-child ()
  (should (equal 1
                 (length (gethash :children (first (org-parser "* header\n** second level\nwith text")))))))

(ert-deftest two-lines-on-second-level/ ()
  (should (equal 1
                 (length (gethash :children (first (org-parser "* header\n** second level\nwith text")))))))

;;; tests.el ends here
