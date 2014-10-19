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


(defun pgsql-get-block-count ()
  (dbi:with-connection (database :postgres
                                 :database-name *pgsql-database*
                                 :username *pgsql-username*
                                 :password *pgsql-password*)
    (let (query result)
      (setf query (dbi:prepare database "SELECT max(height) FROM blocks;"))
      (setf result (dbi:execute query))
      (getf (dbi:fetch result) :|max|))))

(defun pgsql-get-max-id (table)
  (dbi:with-connection (database :postgres
                                 :database-name *pgsql-database*
                                 :username *pgsql-username*
                                 :password *pgsql-password*)
    (let (query result)
      (setf query (dbi:prepare database (format nil "SELECT max(id) FROM ~a;" table)))
      (setf result (dbi:execute query))
      (getf (dbi:fetch result) :|max|))))

(defun pgsql-update-database-from-rpc ()
  (declare (optimize (debug 3)))
  (dbi:with-connection (database :postgres
                                 :database-name *pgsql-database*
                                 :username *pgsql-username*
                                 :password *pgsql-password*)
    (let ((max-block (rpc-get-block-count))
          (n (pgsql-get-block-count))
          (block-id (1+ (pgsql-get-max-id "blocks")))
          (transaction-id (1+ (pgsql-get-max-id "transactions")))
          (input-id (1+ (pgsql-get-max-id "inputs")))
          (output-id (1+ (pgsql-get-max-id "outputs"))))
      (do (blk
           (query1 (dbi:prepare database "INSERT INTO blocks (id, height, hash, timestamp, bits, nonce) VALUES (?, ?, ?, ?, ?, ?)"))
           (query2 (dbi:prepare database "INSERT INTO transactions (id, block_id, hash, timestamp) VALUES (?, ?, ?, ?)"))
           (query3 (dbi:prepare database "INSERT INTO inputs (id, transaction_id, transaction_hash, transaction_index) VALUES (?, ?, ?, ?)"))
           (query4 (dbi:prepare database "INSERT INTO outputs (id, transaction_id, index, value, address) VALUES (?, ?, ?, ?, ?)")))
          ((>= n max-block) n)
        (incf n)
        (setf blk (rpc-get-block-by-number n))
        (dbi:execute query1 block-id n (pretty-print-hash (hash blk)) (timestamp blk) (bits blk) (nonce blk))
        (dotimes (i (transaction-count blk))
          (let ((transaction (aref (transactions blk) i)))
            (dbi:execute query2 transaction-id block-id (pretty-print-hash (hash transaction)) (timestamp transaction))
            (dotimes (j (input-count transaction))
              (let ((input (aref (inputs transaction) j)))
                (dbi:execute query3 input-id transaction-id (pretty-print-hash (transaction-hash input)) (transaction-index input))
                (incf input-id)))
            (dotimes (j (output-count transaction))
              (let ((output (aref (outputs transaction) j)))
                (dbi:execute query4 output-id transaction-id (index output) (value output) (pretty-print-address (get-output-address (script output))))
                (incf output-id)))
            (incf transaction-id)))
        (incf block-id)))))
