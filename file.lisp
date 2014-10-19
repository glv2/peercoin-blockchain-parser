#|
Copyright 2014 Guillaume LE VAILLANT

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
|#


(in-package :peercoin-blockchain-parser)


;;; Basic data types

(defun read-bytes (stream bytes)
  "Read a number of BYTES from a STREAM."
  (let ((data (make-array bytes :element-type '(unsigned-byte 8)))
        n)
    (setf n (read-sequence data stream))
    (unless (= n bytes)
      (error "Not enough data"))
    data))

(defun read-unsigned-integer (stream bytes)
  "Read an integer of several BYTES (little-endian) from a STREAM."
  (loop
     :with value = 0
     :for low-bit :from 0 :to (* 8 (1- bytes)) :by 8
     :do (setf (ldb (byte 8 low-bit) value) (read-byte stream))
     :finally (return value)))

(defun read-variable-length-unsigned-integer (stream)
  "Read a variable length integer (little-endian) from a STREAM."
  (let ((d (read-byte stream)))
    (cond ((< d 253) d)
          ((= d 253) (read-unsigned-integer stream 2))
          ((= d 254) (read-unsigned-integer stream 4))
          ((= d 255) (read-unsigned-integer stream 8)))))


;;; Hash

(defun compute-data-hash (stream offset length)
  "Compute the double sha256 hash of LENGTH bytes of data from STREAM after skipping OFFSET bytes from STREAM."
  (let (data)
    (file-position stream (+ (file-position stream) offset))
    (setf data (read-bytes stream length))
    (sha256d data)))


;;; Objects

(defgeneric read-object (object stream)
  (:documentation "Read an OBJECT from a STREAM."))

(defmethod read-object ((object input) stream)
  (with-slots (transaction-hash transaction-index script-length script sequence-number) object
    (setf transaction-hash (read-bytes stream 32)
          transaction-index (read-unsigned-integer stream 4)
          script-length (read-variable-length-unsigned-integer stream)
          script (read-bytes stream script-length)
          sequence-number (read-unsigned-integer stream 4))))

(defmethod read-object ((object output) stream)
  (with-slots (value script-length script) object
    (setf value (read-unsigned-integer stream 8)
          script-length (read-variable-length-unsigned-integer stream)
          script (read-bytes stream script-length))))

(defmethod read-object ((object transaction) stream)
  (with-slots (version timestamp input-count inputs output-count outputs lock-time) object
    (setf version (read-unsigned-integer stream 4)
          timestamp (read-unsigned-integer stream 4)
          input-count (read-variable-length-unsigned-integer stream)
          inputs (make-array input-count))
    (funcall inputs-start)
    (dotimes (i input-count)
      (funcall input-start)
      (setf (aref inputs i) (make-instance 'input))
      (read-object (aref inputs i) stream)
      (funcall input-end (aref inputs i)))
    (funcall inputs-end)
    (setf output-count (read-variable-length-unsigned-integer stream)
          outputs (make-array output-count))
    (funcall outputs-start)
    (dotimes (i output-count)
      (funcall output-start)
      (setf (aref outputs i) (make-instance 'output))
      (read-object (aref outputs i) stream)
      (setf (index (aref outputs i)) i)
      (funcall output-end (aref outputs i)))
    (funcall outputs-end)
    (setf lock-time (read-unsigned-integer stream 4))))

(defmethod read-object ((object blk) stream)
  (with-slots (header-length version previous-hash merkle-root timestamp bits nonce transaction-count transactions)
      object
    (setf header-length (read-unsigned-integer stream 4)
          version (read-unsigned-integer stream 4)
          previous-hash (read-bytes stream 32)
          merkle-root (read-bytes stream 32)
          timestamp (read-unsigned-integer stream 4)
          bits (read-unsigned-integer stream 4)
          nonce (read-unsigned-integer stream 4)
          transaction-count (read-variable-length-unsigned-integer stream)
          transactions (make-array transaction-count))
    (funcall transactions-start)
    (let (start-position end-position transaction-hash)
      (dotimes (i transaction-count)
        (funcall transaction-start)
        (setf (aref transactions i) (make-instance 'transaction))
        (setf start-position (file-position stream))
        (read-object (aref transactions i) stream)
        (setf end-position (file-position stream))
        (file-position stream start-position)
        (setf transaction-hash (compute-data-hash stream 0 (- end-position start-position)))
        (setf (hash (aref transactions i)) transaction-hash)
        (file-position stream end-position)
        (funcall transaction-end (aref transactions i))))
    (funcall transactions-end)))


;;; Parser

(defconstant +peercoin-magic-id+ #xe5e9e8e6)

(defun find-magic-id (stream)
  "Find the magic ID by searching backwards."
  (do ((position (file-position stream)))
      ((minusp position))
    (file-position stream position)
    (if (= (read-unsigned-integer stream 4) +peercoin-magic-id+)
        (return position)
        (decf position))))

(defun parse-blockchain (pathname)
  "Parse the blockchain contained in PATHNAME."
  (with-open-file (stream pathname
                          :direction :input
                          :element-type '(unsigned-byte 8))
    (let (last-block block-hashes)
      (funcall chain-start)
      (file-position stream (- (file-length stream) 4))
      (do ((position (find-magic-id stream)
                     (find-magic-id stream))
           blk
           blk-hash)
          ((null position))
        (incf position 4) ; After magic ID
        (file-position stream position)
        (setf blk-hash (compute-data-hash stream 4 80))
        (when (or (null last-block)
                  (equalp (previous-hash last-block) blk-hash))
          (push blk-hash block-hashes)
          (setf blk (make-instance 'blk))
          (file-position stream position)
          (funcall block-start)
          (handler-case (read-object blk stream)
            (t (e) (return e)))
          (setf (hash blk) blk-hash)
          (funcall block-end blk)
          (setf last-block blk))
        (when (minusp (- position 5))
          (return))
        (file-position stream (- position 5))) ; Before magic ID
      (funcall chain-end block-hashes))))
