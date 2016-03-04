(ns htm-mnist.core
  (:require [clojure.java.io :as io])
  (:import [java.nio ByteBuffer]))

(defn pos-unsigned-bytes
  "Takes an unsigned byte array which is represented in JVM as -128 to
  127; rescale to bytes from 0 to 127 with half the resolution."
  [bytes]
  (into (vector-of :byte)
        (map #(byte (-> (bit-and % 0xff)
                        (quot 2)))
             bytes)))

(defn read-mnist-images
  [file take-imgs]
  (with-open [in ; ??? java.util.zip.GZIPInputStream.
              (io/input-stream file)]
    (let [header-size 16 ;; bytes
          img-size (* 28 28)
          total (+ header-size (* take-imgs img-size))
          buf (byte-array total)
          n (.read in buf)
          bb (ByteBuffer/wrap buf)]
      (println "read" n "bytes")
      (assert (== n total))
      (let [magic (.getInt bb)
            n-imgs (.getInt bb)
            n-rows (.getInt bb)
            n-cols (.getInt bb)
            img-buf (byte-array img-size)]
        (assert (== magic 2051))
        (println "file has" n-imgs "images at" n-rows "x" n-cols)
        (loop [i (min take-imgs n-imgs)
               ans (transient [])]
          (if (pos? i)
            (do
              (.get bb img-buf)
              (recur (dec i)
                     (conj! ans (pos-unsigned-bytes img-buf))))
            ;; finished
            (persistent! ans)))))))

(defn read-mnist-labels
  [gz-file]
  (with-open [in (java.util.zip.GZIPInputStream.
                  (io/input-stream gz-file))]
    (let [header-n 8 ;; bytes
          buf (byte-array header-n)
          n (.read in buf)
          bb (ByteBuffer/allocate header-n)]
      (assert (== n header-n))
      (.put bb buf 0 n) ;; Fill ByteBuffer with array contents
      (.flip bb) ;; Prepare for reading
      (let [magic (.getInt bb)
            n-labs (.getInt bb)]
        (assert (== magic 2049))
        (println "file has" n-labs "labels")
        (let [buflen n-labs
              buf (byte-array buflen)
              n (.read in buf)]
          (into (vector-of :byte) buf))))))
