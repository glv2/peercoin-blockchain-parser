#|
Copyright 2014-2015 Guillaume LE VAILLANT

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


;;; Basic data types

(defun file-read-bytes (stream bytes)
  "Read a number of BYTES from a STREAM."
  (let ((data (make-array bytes :element-type '(unsigned-byte 8)))
        n)
    (setf n (read-sequence data stream))
    (unless (= n bytes)
      (error "Not enough data"))
    data))

(defun file-read-unsigned-integer (stream bytes)
  "Read an integer of several BYTES (little-endian) from a STREAM."
  (loop
     :with value = 0
     :for low-bit :from 0 :to (* 8 (1- bytes)) :by 8
     :do (setf (ldb (byte 8 low-bit) value) (read-byte stream))
     :finally (return value)))

(defun file-read-variable-length-unsigned-integer (stream)
  "Read a variable length integer (little-endian) from a STREAM."
  (let ((d (read-byte stream)))
    (cond ((< d 253) d)
          ((= d 253) (file-read-unsigned-integer stream 2))
          ((= d 254) (file-read-unsigned-integer stream 4))
          ((= d 255) (file-read-unsigned-integer stream 8)))))


;;; Hash

(defun file-compute-data-hash (stream offset length)
  "Compute the double sha256 hash of LENGTH bytes of data from STREAM after skipping OFFSET bytes from STREAM."
  (let (data)
    (file-position stream (+ (file-position stream) offset))
    (setf data (file-read-bytes stream length))
    (sha256d data)))


;;; Objects

(defgeneric file-read-object (object stream)
  (:documentation "Read an OBJECT from a STREAM."))

(defmethod file-read-object ((object input) stream)
  (with-slots (transaction-hash transaction-index script-length script sequence-number) object
    (setf transaction-hash (file-read-bytes stream 32)
          transaction-index (file-read-unsigned-integer stream 4)
          script-length (file-read-variable-length-unsigned-integer stream)
          script (file-read-bytes stream script-length)
          sequence-number (file-read-unsigned-integer stream 4))))

(defmethod file-read-object ((object output) stream)
  (with-slots (value script-length script) object
    (setf value (file-read-unsigned-integer stream 8)
          script-length (file-read-variable-length-unsigned-integer stream)
          script (file-read-bytes stream script-length))))

(defmethod file-read-object ((object transaction) stream)
  (with-slots (version timestamp input-count inputs output-count outputs lock-time) object
    (setf version (file-read-unsigned-integer stream 4)
          timestamp (file-read-unsigned-integer stream 4))

    ;; Read inputs
    (setf input-count (file-read-variable-length-unsigned-integer stream)
          inputs (make-array input-count))
    (dotimes (i input-count)
      (setf (aref inputs i) (make-instance 'input))
      (file-read-object (aref inputs i) stream))

    ;; Read outputs
    (setf output-count (file-read-variable-length-unsigned-integer stream)
          outputs (make-array output-count))
    (dotimes (i output-count)
      (setf (aref outputs i) (make-instance 'output))
      (file-read-object (aref outputs i) stream)
      (setf (index (aref outputs i)) i))

    (setf lock-time (file-read-unsigned-integer stream 4))))

(defmethod file-read-object ((object blk) stream)
  (with-slots (header-length version previous-hash merkle-root timestamp bits nonce transaction-count transactions signature-length signature)
      object
    (setf header-length (file-read-unsigned-integer stream 4)
          version (file-read-unsigned-integer stream 4)
          previous-hash (file-read-bytes stream 32)
          merkle-root (file-read-bytes stream 32)
          timestamp (file-read-unsigned-integer stream 4)
          bits (file-read-unsigned-integer stream 4)
          nonce (file-read-unsigned-integer stream 4))

    ;; Read transactions
    (setf transaction-count (file-read-variable-length-unsigned-integer stream)
          transactions (make-array transaction-count))
    (let (start-position end-position transaction-hash)
      (dotimes (i transaction-count)
        (setf (aref transactions i) (make-instance 'transaction))
        (setf start-position (file-position stream))
        (file-read-object (aref transactions i) stream)
        (setf end-position (file-position stream))
        (file-position stream start-position)
        (setf transaction-hash (file-compute-data-hash stream 0 (- end-position start-position)))
        (setf (hash (aref transactions i)) transaction-hash)
        (file-position stream end-position)))

    (setf signature-length (file-read-variable-length-unsigned-integer stream)
          signature (file-read-bytes stream signature-length))))


;;; Parser

(defconstant +peercoin-magic-id+ #xe5e9e8e6)
(defconstant +peercoin-testnet-magic-id+ #xefc0f2cb)

(defun find-magic-id (stream &optional backwards)
  "Find the magic ID."
  (do ((position (file-position stream)))
      ((or (minusp position) (> position (file-length stream))))
    (file-position stream position)
    (if (= (file-read-unsigned-integer stream 4) (if *testnet* +peercoin-testnet-magic-id+ +peercoin-magic-id+))
        (return position)
        (if backwards
            (decf position)
            (incf position)))))

(defun file-get-block (h &optional raw)
  "Get a block in the blockchain given its hash."
  (with-open-file (stream *file-blockchain*
                          :direction :input
                          :element-type '(unsigned-byte 8))
    (do ((position (find-magic-id stream)
                   (find-magic-id stream))
         (hbin (reverse (hex-to-bin h)))
         blk
         blk-hash)
        ((null position))
      (incf position 4) ; After magic ID
      (file-position stream position)
      (setf blk-hash (file-compute-data-hash stream 4 80))
      (when (equalp blk-hash hbin)
        (setf blk (make-instance 'blk))
        (file-position stream position)
        (handler-case (file-read-object blk stream)
          (t (e) (return e)))
        (setf (hash blk) blk-hash)
        (if raw
            (let ((position-end (file-position stream))
                  data)
              (file-position stream (- position 4)) ; Before magic ID
              (setf data (file-read-bytes stream (- position-end position)))
              ;;(file-position stream position-end)
              (return data))
            (return blk))))))

(defun file-parse-blockchain (start-callback block-callback end-callback &optional backwards)
  "Call START-CALLBACK, parse the blockchain and pass every block to the BLOCK-CALLBACK function, and pass a list containing the hashes of the blocks to END-CALLBACK."
  (with-open-file (stream *file-blockchain*
                          :direction :input
                          :element-type '(unsigned-byte 8))
    (let (last-block block-hashes)
      (when start-callback
        (funcall start-callback))
      (when backwards
        (file-position stream (- (file-length stream) 4)))
      (do ((position (find-magic-id stream backwards)
                     (find-magic-id stream backwards))
           blk
           blk-hash)
          ((null position))
        (incf position 4) ; After magic ID
        (file-position stream position)
        (setf blk-hash (file-compute-data-hash stream 4 80))
        (when (or (null last-block)
                  (equalp (previous-hash last-block) blk-hash))
          (push blk-hash block-hashes)
          (setf blk (make-instance 'blk))
          (file-position stream position)
          (handler-case (file-read-object blk stream)
            (t (e) (return e)))
          (setf (hash blk) blk-hash)
          (when block-callback
            (funcall block-callback blk))
          (setf last-block blk))
        (if backwards
            (progn
              (when (minusp (- position 5))
                (return))
              (file-position stream (- position 5))) ; Before magic ID
            (when (> (+ (file-position stream) 84) (file-length stream))
              (return))))
      (when end-callback
        (funcall end-callback block-hashes)))))
