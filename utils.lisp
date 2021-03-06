#|
Copyright 2014 Guillaume LE VAILLANT

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 3.0 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library.
If not, see <http://www.gnu.org/licenses/>.
|#


(in-package :peercoin-blockchain-parser)


(defconstant +peercoin-version-byte+ 55)
(defconstant +peercoin-testnet-version-byte+ 111)
(defparameter *testnet* nil)
(defparameter +base58-symbols+ "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz")

(defun set-testnet (b)
  "If B is NIL, use main net constants. If not, use test net constants."
  (setf *testnet* (if b t nil)))

(defun pretty-print-hash (hash)
  "Make a string containing the big-endian hexadecimal representation of the little-endian byte array HASH."
  (if (and hash (> (length hash) 2))
      (reduce #'(lambda (x y) (concatenate 'string y x))
              (map 'vector
                   #'(lambda (x) (string-downcase (format nil "~2,'0x" x)))
                   hash))
      "NIL"))

(defun base58-encode (data)
  "Make a string containing the base58 encoding of the DATA."
  (let ((x 0)
        (len (length data))
        address)
    (dotimes (i len)
      (setf (ldb (byte 8 (* 8 i)) x) (aref data (- len 1 i))))
    
    (loop
       while (plusp x)
       do (multiple-value-bind (q r) (floor x 58)
            (setf x q)
            (push (elt +base58-symbols+ r) address)))

    (loop
       for i from 0 below len
       while (zerop (aref data i))
       do (push (elt +base58-symbols+ 0) address))

    (coerce address 'string)))

(defun pretty-print-address (hash)
  "Make a string containing the base58 encoding of an address given its HASH."
  (if (and hash (> (length hash) 2))
      (let ((script (vector (if *testnet*
                                +peercoin-testnet-version-byte+
                                +peercoin-version-byte+))))
        (setf script (concatenate 'vector script hash))
        (setf script (coerce script '(vector (unsigned-byte 8))))
        (setf script (concatenate 'vector script (subseq (sha256d script) 0 4)))
        (base58-encode script))
      "NIL"))

(defun bin-to-hex (bin)
  "Convert an octet vector BIN to a hexadecimal string."
  (let* ((hex (map 'vector #'(lambda (x) (string-downcase (format nil "~2,'0x" x))) bin))
         (len (length hex)))
    (cond ((= len 1) (coerce hex 'string))
          ((> len 1) (reduce #'(lambda (x y) (concatenate 'string x y)) hex))
          (t ""))))

(defun hex-to-bin (hex)
  "Convert a hexadecimal string HEX to an octet vector."
  (let* ((len (/ (length hex) 2))
         (bin (make-array len :element-type '(unsigned-byte 8))))
    (dotimes (i len bin)
      (setf (aref bin i) (parse-integer hex :start (* 2 i) :end (+ (* 2 i) 2) :radix 16)))))

(defun utc-to-epoch (timestring)
  "Convert a time string to a UNIX time."
  (let (timestamp)
    (setf timestamp (local-time:parse-timestring timestring))
    (local-time:timestamp-to-unix timestamp)))

(defun epoch-to-utc (seconds)
  "Convert a UNIX time to a UTC time string."
  (let (timestamp)
    (setf timestamp (local-time:unix-to-timestamp seconds))
    (local-time:format-timestring nil timestamp :timezone local-time:+utc-zone+)))
