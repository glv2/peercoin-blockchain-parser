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


(defun sql-create-tables (sql)
  "Create tables."
  (format sql "DROP TABLE IF EXISTS blocks;~%")
  (format sql "DROP TABLE IF EXISTS transactions;~%")
  (format sql "DROP TABLE IF EXISTS inputs;~%")
  (format sql "DROP TABLE IF EXISTS outputs;~%~%")
  (format sql "CREATE TABLE blocks (id BIGINT PRIMARY KEY, height BIGINT, hash CHAR(64), timestamp BIGINT, bits BIGINT, nonce BIGINT);~%")
  (format sql "CREATE TABLE transactions (id BIGINT PRIMARY KEY, block_id BIGINT, hash CHAR(64), timestamp BIGINT);~%")
  (format sql "CREATE TABLE inputs (id BIGINT PRIMARY KEY, transaction_id BIGINT, transaction_hash CHAR(64), transaction_index BIGINT);~%")
  (format sql "CREATE TABLE outputs (id BIGINT PRIMARY KEY, transaction_id BIGINT, index BIGINT, value BIGINT, address CHAR(36));~%~%")
  (format sql "CREATE INDEX addr_index ON outputs (address);~%~%~%"))

(defun sql-update-block-heights (sql block-hashes)
  "Update block numbers."
  (let ((block-height 0))
    (dolist (hash block-hashes)
      (format sql "UPDATE blocks SET height=~d WHERE hash='~a';~%" block-height (pretty-print-hash hash))
      (incf block-height))))

(defun sql-make-script-from-blockchain (sql-file)
  "Export the data from the blockchain to a sql script named SQL-FILE."
  (with-open-file (sql sql-file :direction :output :if-exists :supersede)
    (let* ((block-id 0)
           (transaction-id 0)
           (input-id 0)
           (output-id 0)

           (start-callback
            (lambda ()
              (sql-create-tables sql)))

           (block-callback
            (lambda (blk)
              "Insert block, transaction, inputs and outputs."
              (format sql "INSERT INTO blocks (id, hash, timestamp, bits, nonce) VALUES (~d, '~a', ~d, ~d, ~d);~%" block-id (pretty-print-hash (hash blk)) (timestamp blk) (bits blk) (nonce blk))

              (dotimes (i (transaction-count blk))
                (let ((transaction (aref (transactions blk) i)))
                  (format sql "INSERT INTO transactions (id, block_id, hash, timestamp) VALUES (~d, ~d, '~a', ~d);~%" transaction-id block-id (pretty-print-hash (hash transaction)) (timestamp transaction))

                  (dotimes (j (input-count transaction))
                    (let ((input (aref (inputs transaction) j)))
                      (format sql "INSERT INTO inputs (id, transaction_id, transaction_hash, transaction_index) VALUES (~d, ~d, '~a', ~d);~%" input-id transaction-id (pretty-print-hash (transaction-hash input)) (transaction-index input))
                      (incf input-id)))

                  (dotimes (j (output-count transaction))
                    (let ((output (aref (outputs transaction) j)))
                      (format sql "INSERT INTO outputs (id, transaction_id, index, value, address) VALUES (~d, ~d, ~d, ~d, '~a');~%" output-id transaction-id (index output) (value output) (pretty-print-address (get-output-address (script output))))
                      (incf output-id)))

                  (incf transaction-id)))
              (incf block-id)
              (format sql "~%~%")))

           (end-callback
            (lambda (block-hashes)
              (sql-update-block-heights sql block-hashes))))

      (file-parse-blockchain start-callback block-callback end-callback))))
