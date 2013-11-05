;;;; Copyright © 2013 Paul Stadig. All rights reserved.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this file,
;;;; You can obtain one at http://mozilla.org/MPL/2.0/.
;;;;
;;;; This Source Code Form is "Incompatible With Secondary Licenses", as defined
;;;; by the Mozilla Public License, v. 2.0.
(ns pjstadig.test.utf8
  (:require [clojure.core :as clj]
            [clojure.java.io :as io]
            [clojure.test :refer :all]
            [pjstadig.utf8 :refer :all]))

(deftest test-bytes->chars
  (is (= [\¢] (seq (bytes->chars [2r11000010 2r10100010]))))
  (is (= [\€] (seq (bytes->chars [2r11100010 2r10000010 2r10101100]))))
  (is (= [(char 0xd852) (char 0xdf62)]
         (seq (bytes->chars [2r11110000 2r10100100 2r10101101 2r10100010])))))

(deftest test-byte-predicates
  (is (cont-byte? 2r10111111))
  (is (not (cont-byte? 2r11111111)))
  (is (one-byte-start? 2r01111111))
  (is (not (one-byte-start? 2r11111111)))
  (is (two-byte-start? 2r11011111))
  (is (not (two-byte-start? 2r11111111)))
  (is (three-byte-start? 2r11101111))
  (is (not (three-byte-start? 2r11111111)))
  (is (four-byte-start? 2r11110111))
  (is (not (four-byte-start? 2r11111111))))

(deftest test-utf8-str
  (is (= "\ud852\udf62¢€" (str (utf8-str "\ud852\udf62¢€"))))
  (is (= "\ud852\udf62¢€" (str (utf8-str "\ud852" "\udf62" "¢" "€"))))
  (is (= "¢€" (str (.subSequence (utf8-str "\ud852\udf62¢€") 2 4))))
  (is (= (char 0xd852) (.charAt (utf8-str "\ud852\udf62¢€") 0)))
  (is (= (char 0xdf62) (.charAt (utf8-str "\ud852\udf62¢€") 1)))
  (is (= \¢ (.charAt (utf8-str "\ud852\udf62¢€") 2)))
  (is (= \€ (.charAt (utf8-str "\ud852\udf62¢€") 3)))
  (is (= 4 (.length (utf8-str "\ud852\udf62¢€")))))

(deftest test-char-seq
  (is (= [(char 0xd852) (char 0xdf62) \¢ \€]
         (char-seq (.vector (utf8-str "\ud852\udf62¢€")))))
  (is (= [(char 0xd852) (char 0xdf62) \¢ \€]
         (seq (utf8-str "\ud852\udf62¢€"))))
  (is (= [\? \? \? \¢ \€]
         (char-seq (drop 1 (.vector (utf8-str "\ud852\udf62¢€")))))))

(deftest test-byte-seq
  (is (= (map unchecked-byte [2r11110000 2r10100100 2r10101101 2r10100010
                              2r11000010 2r10100010
                              2r11100010 2r10000010 2r10101100])
         (byte-seq (seq "\ud852\udf62¢€")))))

(deftest test-utf8-str-as-coll
  (is (= (utf8-str "foob") (conj (utf8-str "foo") \b)))
  (is (= (utf8-str) (empty (utf8-str "foo"))))
  (is (= (utf8-str "foobar") (into (utf8-str "foo") (seq "bar"))))
  (is (= (utf8-str "foo\ud852\udf62¢€")
         (into (utf8-str "foo") (seq (utf8-str "\ud852\udf62¢€")))))
  (is (= (utf8-str "foo\ud852")
         (into (utf8-str "foo") (seq (utf8-str "\ud852"))))))

(deftest test-utf8-writer
  (is (= (utf8-str "foofooooffooo")
         (utf8-str (with-open [x (utf8-writer)]
                     (.write x "foo")
                     (.write x (char-array (seq "foo")))
                     (.write x (char-array (seq "foo")) 1 2)
                     (-> x
                         (.append \f)
                         (.append "foo")
                         (.append "foo" 1 2))
                     x)))))
