;;;; Copyright © 2013 Paul Stadig. All rights reserved.
;;;;
;;;; This Source Code Form is subject to the terms of the Mozilla Public
;;;; License, v. 2.0. If a copy of the MPL was not distributed with this file,
;;;; You can obtain one at http://mozilla.org/MPL/2.0/.
;;;;
;;;; This Source Code Form is "Incompatible With Secondary Licenses", as defined
;;;; by the Mozilla Public License, v. 2.0.
(ns pjstadig.utf8
  (:require [nio.core :as nio]
            [pjstadig.utf8.protocol :refer [IUtf8String] :as p])
  (:import (java.nio Buffer)
           (java.nio.charset Charset)))

(def ^Charset charset (java.nio.charset.Charset/forName "utf-8"))

(defn one-byte-start? [b]
  (let [b (int b)]
    (not (bit-test b 7))))

(defn two-byte-start? [b]
  (let [b (int b)]
    (== (bit-and b 2r11100000) 2r11000000)))

(defn three-byte-start? [b]
  (let [b (int b)]
    (== (bit-and b 2r11110000) 2r11100000)))

(defn four-byte-start? [b]
  (let [b (int b)]
    (== (bit-and b 2r11111000) 2r11110000)))

(defn cont-byte? [b]
  (let [b (int b)]
    (== (bit-and b 2r11000000) 2r10000000)))

(defn chars->bytes [& char-seq]
  (->> char-seq
       (map unchecked-char)
       char-array
       nio/char-buffer
       (.encode charset)
       nio/buffer-seq))

(defn bytes->chars [byte-seq]
  (->> byte-seq
       (map unchecked-byte)
       byte-array
       nio/byte-buffer
       (.decode charset)
       nio/buffer-seq))

(defn char-seq [byte-seq]
  (lazy-seq
   (when (seq byte-seq)
     (cond
      (one-byte-start? (first byte-seq))
      (concat [(char (first byte-seq))] (char-seq (rest byte-seq)))
      (two-byte-start? (first byte-seq))
      (concat (bytes->chars (take 2 byte-seq)) (char-seq (drop 2 byte-seq)))
      (three-byte-start? (first byte-seq))
      (concat (bytes->chars (take 3 byte-seq)) (char-seq (drop 3 byte-seq)))
      (four-byte-start? (first byte-seq))
      (concat (bytes->chars (take 4 byte-seq)) (char-seq (drop 4 byte-seq)))
      :else
      (concat [\?] (char-seq (rest byte-seq)))))))

(defn byte-seq [char-seq]
  (lazy-seq
   (when (seq char-seq)
     (if (Character/isHighSurrogate (unchecked-char (first char-seq)))
       (concat (chars->bytes (first char-seq) (second char-seq))
               (byte-seq (drop 2 char-seq)))
       (concat (chars->bytes (first char-seq))
               (byte-seq (drop 1 char-seq)))))))

(defn utf8-str
  ([]
     (utf8-str ""))
  ([obj]
     (p/utf8-str obj))
  ([obj & more]
     (into (utf8-str obj) (mapcat str more))))

(deftype Utf8String [vector surrogate]
  Object
  (equals [this obj]
    (.equiv this obj))
  (hashCode [this]
    (loop [h (int 0)
           s (seq (if surrogate
                    (into vector (chars->bytes surrogate))
                    vector))]
      (if (seq s)
        (recur (unchecked-add-int (unchecked-multiply-int h 31)
                                  (.hashCode ^Object (first s)))
               (rest s))
        h)))
  (toString [this]
    (String. (byte-array (if surrogate
                           (into vector (chars->bytes surrogate))
                           vector))
             "utf-8"))
  CharSequence
  (charAt [this i]
    (first (drop i (seq this))))
  (length [this]
    (count (seq this)))
  (subSequence [this start end]
    (Utf8String. (apply vector-of :byte
                        (byte-seq (take (- end start)
                                        (drop start (seq this)))))
                 nil))
  clojure.lang.IPersistentCollection
  (cons [this obj]
    (if surrogate
      (if (Character/isLowSurrogate obj)
        (Utf8String. (into vector
                           (chars->bytes surrogate obj))
                     nil)
        (if (Character/isHighSurrogate obj)
          (Utf8String. (into vector (chars->bytes surrogate)) obj)
          (Utf8String. (-> vector
                           (into (chars->bytes surrogate))
                           (into (chars->bytes obj)))
                       nil)))
      (if (Character/isHighSurrogate obj)
        (Utf8String. vector obj)
        (Utf8String. (into vector (chars->bytes obj)) nil))))
  (count [this]
    (count (seq vector)))
  (empty [this]
    (utf8-str ""))
  (equiv [this obj]
    (and (instance? Utf8String obj)
         (= (seq this) (seq obj))))
  (seq [this]
    (char-seq (if surrogate
                (into vector (chars->bytes surrogate))
                vector))))

(extend-protocol IUtf8String
  String
  (utf8-str [this]
    (into (Utf8String. (vector-of :byte) nil) (seq this))))

(defn utf8-writer []
  (let [s (atom (utf8-str ""))]
    (proxy [java.io.Writer pjstadig.utf8.protocol.IUtf8String] []
      (append
        ([c]
           (if (char? c)
             (do (swap! s conj c)
                 this)
             (do (swap! s into (seq c))
                 this)))
        ([csq start end]
           (swap! s into (take (- end start) (drop start csq)))
           this))
      (close [])
      (flush [])
      (write
        ([c]
           (cond
            (number? c)
            (swap! s conj c)
            :else
            (swap! s into (seq c))))
        ([cbuf off len]
           (swap! s into (take len (drop off cbuf)))))
      (utf8_str []
        @s))))
