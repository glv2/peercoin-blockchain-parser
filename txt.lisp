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


(defun txt-make-file-from-blockchain (txt-file)
  "Export the data from the blockchain to a TXT-FILE."
  (with-open-file (txt txt-file :direction :output :if-exists :supersede)
    (let* ((block-id 0)
           (transaction-id 0)
           (input-id 0)
           (output-id 0)

           (block-callback
            (lambda (blk)
              "Print block, transaction, inputs and outputs."
              (format txt "block: ~a~%  previous block: ~a~%  timestamp: ~d~%  bits: ~d~%  nonce: ~d~%  transactions: ~d~%" (pretty-print-hash (hash blk)) (pretty-print-hash (previous-hash blk)) (timestamp blk) (bits blk) (nonce blk) (transaction-count blk))

              (dotimes (i (transaction-count blk))
                (let ((transaction (aref (transactions blk) i)))
                  (format txt "  transaction: ~a~%    timestamp: ~d~%    inputs: ~d~%    outputs: ~d~%" (pretty-print-hash (hash transaction)) (timestamp transaction) (input-count transaction) (output-count transaction))

                  (dotimes (j (input-count transaction))
                    (let ((input (aref (inputs transaction) j)))
                      (format txt "    input~%      transaction: ~a~%      index: ~d~%" (pretty-print-hash (transaction-hash input)) (transaction-index input))
                      (incf input-id)))

                  (dotimes (j (output-count transaction))
                    (let ((output (aref (outputs transaction) j)))
                      (format txt "    output~%      index: ~d~%      value: ~d~%      address: ~a~%" (index output) (value output) (pretty-print-address (get-output-address (script output))))
                      (incf output-id)))

                  (incf transaction-id)))
              (incf block-id)
              (format txt "~%~%"))))

      (file-parse-blockchain nil block-callback nil t))))
