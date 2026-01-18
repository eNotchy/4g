;;; 4g-tests.el --- Tests for 4g -*- lexical-binding: t;  coding:utf-8 -*-
;;
;; Version: 0.8.1
;; URL: https://github.com/eNotchy/4g
;; Package-Requires: ((emacs "30.1"))
;; Keywords: comm extensions hypermedia org
;; SPDX-License-Identifier: GPL-3.0
;;
;; This file is NOT part of GNU Emacs.
;;
;;; Commentary:
;;
;; Tests for 4g.el
;;
;;; Code:

(require 'ert)
(require '4g)

(defvar 4g--filetestpost
  '(:no 107582981 :now "12/17/25(Wed)13:16:00" :name "Anonymous"
    :com "good on AMD yet?" :filename "img12345"
    :ext ".jpg" :w 1200 :h 675 :tn_w 125 :tn_h 70 :tim 1765995360846053
    :time 1765995360 :md5 "Z+W9DJ4XbL/2u334gt6CDw=="
    :fsize 49102 :resto 107581434))

(ert-deftest 4g-test-filenames ()
  (should
   (equal
    "img12345.Z-W9.jpg"
    (4g--gen-filename 4g--filetestpost)))
  (should
   (equal
    "[[4g-download:4chan/g/1765995360846053/.jpg][Download (48 KiB)]]"
    (let ((4g--sitename "4chan")
          (4g-image-download-directory nil))
      (4g--gen-download-link "g" 4g--filetestpost)))))

(ert-deftest 4g-test-country-emojification ()
  (should (equal nil (4g--emojify-country-code "asdf")))
  (should (equal "🇺🇸" (4g--emojify-country-code "US")))
  (should (equal "🇬🇧" (4g--emojify-country-code "GB"))))

(ert-deftest 4g-test-quote-link-orgification ()
  (should
   (equal
    "[[107164637][>>107164637]]\nMay I ask why monke sad?"
    (4g--orgify-com
     "<a href=\"#p107164637\" class=\"quotelink\">&gt;&gt;107164637</a><br>May I ask why monke sad?"))))

(ert-deftest 4g-test-filepath-generation ()
    (should
     (string= "thumbnails/K/KKKK________safdsdf12w==.jpg"
              (4g--thumbnail-file "KKKK////////safdsdf123==")))
    (should
     (string= "/dev/shm/4g/thumbnails/K/KKKK________safdsdf12w==.jpg"
              (cdr
               (let ((4g-directory "/dev/shm/4g"))
                 (4g--get-thumbnail-url+path
                  "g"
                  (list :md5 "KKKK////////safdsdf12w=="
                        :tim 1234))))))
    (should-not
     (let ((4g-directory nil))
       (4g--get-thumbnail-url+path
        "g"
        (list :md5 "KKKK////////safdsdf12w=="
              :tim 1234))))
    (should-error
     (let ((4g-directory "/dev/shm/4g"))
       (4g--get-thumbnail-url+path
        "g"
        (list :md5 "KKKKx////////safdsdf12w=="
              :tim 1234)))))

(ert-deftest 4g-test-url-parsing ()
  (should (equal '(:kind thread :board "g" :threadno "123" :postno "456")
                 (4g--parse-4chan-url "https://boards.4chan.org/g/thread/123#p456")))
  (should (equal '(:kind thread :board "g" :threadno "123" :postno nil)
                 (4g--parse-4chan-url "https://boards.4chan.org/g/thread/123")))
  (should (equal '(:kind catalog :board "g")
                 (4g--parse-4chan-url "https://boards.4chan.org/g/catalog")))
  (should (equal '(:kind catalog :board "g")
                 (4g--parse-4chan-url "https://boards.4chan.org/g/")))
  (should (equal '(:kind site)
                 (4g--parse-4chan-url "https://boards.4chan.org/"))))

(provide '4g-tests)

;;; 4g-tests.el ends here
