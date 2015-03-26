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


(defun rdbms-initialize-database ()
  "(Re)initialize the tables of the database."
  (dbi:with-connection (database *rdbms-driver*
                                 :database-name *rdbms-database*
                                 :username *rdbms-username*
                                 :password *rdbms-password*)
    (dbi:do-sql database "DROP TABLE IF EXISTS blocks")
    (dbi:do-sql database "DROP TABLE IF EXISTS transactions")
    (dbi:do-sql database "DROP TABLE IF EXISTS inputs")
    (dbi:do-sql database "DROP TABLE IF EXISTS outputs")
    (dbi:do-sql database "CREATE TABLE blocks (id BIGINT PRIMARY KEY, height BIGINT, hash CHAR(64), timestamp BIGINT, bits BIGINT, nonce BIGINT)")
    (dbi:do-sql database "CREATE TABLE transactions (id BIGINT PRIMARY KEY, block_id BIGINT, hash CHAR(64), timestamp BIGINT)")
    (dbi:do-sql database "CREATE TABLE inputs (id BIGINT PRIMARY KEY, transaction_id BIGINT, transaction_hash CHAR(64), transaction_index BIGINT, script VARCHAR(1024))")
    (dbi:do-sql database "CREATE TABLE outputs (id BIGINT PRIMARY KEY, transaction_id BIGINT, index BIGINT, value BIGINT, address VARCHAR(36), script VARCHAR(1024))")
    (dbi:do-sql database "CREATE INDEX blocks_hash_idx ON blocks (hash)")
    (dbi:do-sql database "CREATE INDEX transactions_hash_idx ON transactions (hash)")
    (dbi:do-sql database "CREATE INDEX inputs_txid_idx ON inputs (transaction_id)")
    (dbi:do-sql database "CREATE INDEX inputs_txhash_idx ON inputs (transaction_hash)")
    (dbi:do-sql database "CREATE INDEX outputs_txid_idx ON outputs (transaction_id)")
    (dbi:do-sql database "CREATE INDEX outputs_addr_idx ON outputs (address)")))

(defun rdbms-get-block-count ()
  "Get the highest block number in the database."
  (dbi:with-connection (database *rdbms-driver*
                                 :database-name *rdbms-database*
                                 :username *rdbms-username*
                                 :password *rdbms-password*)
    (let (query result)
      (setf query (dbi:prepare database "SELECT max(height) FROM blocks"))
      (setf result (dbi:execute query))
      (setf result (getf (dbi:fetch result) :|max|))
      (unless (integerp result)
        (setf result -1))
      result)))

(defun rdbms-get-max-id (table)
  "Get the highest id in a TABLE."
  (dbi:with-connection (database *rdbms-driver*
                                 :database-name *rdbms-database*
                                 :username *rdbms-username*
                                 :password *rdbms-password*)
    (let (query result)
      (setf query (dbi:prepare database (format nil "SELECT max(id) FROM ~a" table)))
      (setf result (dbi:execute query))
      (setf result (getf (dbi:fetch result) :|max|))
      (unless (integerp result)
        (setf result -1))
      result)))

(defun rdbms-update-database-from-rpc ()
  "Update the database using the Peercoin daemon."
  (let ((max-block (rpc-get-block-count))
        (n (rdbms-get-block-count))
        (block-id (1+ (rdbms-get-max-id "blocks")))
        (transaction-id (1+ (rdbms-get-max-id "transactions")))
        (input-id (1+ (rdbms-get-max-id "inputs")))
        (output-id (1+ (rdbms-get-max-id "outputs"))))
    (when (= n -1) ; Database empty
      (incf n)) ; The Peercoin daemon can't give all info on genesis block, so skip it

    (dbi:with-connection (database *rdbms-driver*
                                   :database-name *rdbms-database*
                                   :username *rdbms-username*
                                   :password *rdbms-password*)
      (do (blk
           (query1 (dbi:prepare database "INSERT INTO blocks (id, height, hash, timestamp, bits, nonce) VALUES (?, ?, ?, ?, ?, ?)"))
           (query2 (dbi:prepare database "INSERT INTO transactions (id, block_id, hash, timestamp) VALUES (?, ?, ?, ?)"))
           (query3 (dbi:prepare database "INSERT INTO inputs (id, transaction_id, transaction_hash, transaction_index, script) VALUES (?, ?, ?, ?, ?)"))
           (query4 (dbi:prepare database "INSERT INTO outputs (id, transaction_id, index, value, address, script) VALUES (?, ?, ?, ?, ?, ?)")))
          ((>= n max-block) n)
        (incf n)
        (setf blk (rpc-get-block-by-number n))
        (dbi:execute query1 block-id n (pretty-print-hash (hash blk)) (timestamp blk) (bits blk) (nonce blk))

        (dotimes (i (transaction-count blk))
          (let ((transaction (aref (transactions blk) i)))
            (dbi:execute query2 transaction-id block-id (pretty-print-hash (hash transaction)) (timestamp transaction))

            (dotimes (j (input-count transaction))
              (let ((input (aref (inputs transaction) j)))
                (dbi:execute query3 input-id transaction-id (pretty-print-hash (transaction-hash input)) (transaction-index input) (bin-to-hex (script input)))
                (incf input-id)))

            (dotimes (j (output-count transaction))
              (let ((output (aref (outputs transaction) j)))
                (dbi:execute query4 output-id transaction-id (index output) (value output) (pretty-print-address (get-output-address (script output))) (bin-to-hex (script output)))
                (incf output-id)))

            (incf transaction-id)))
        (incf block-id)))))

(defun rdbms-update-database-from-blockchain ()
  "Update the database using the blockchain."
  (let ((n (rdbms-get-block-count))
        (block-id (1+ (rdbms-get-max-id "blocks")))
        (transaction-id (1+ (rdbms-get-max-id "transactions")))
        (input-id (1+ (rdbms-get-max-id "inputs")))
        (output-id (1+ (rdbms-get-max-id "outputs")))
        last-hash)
    (dbi:with-connection (database *rdbms-driver*
                                   :database-name *rdbms-database*
                                   :username *rdbms-username*
                                   :password *rdbms-password*)
      (unless (minusp n) ; Unless database empty
        (let* ((query (dbi:prepare database "SELECT hash FROM blocks WHERE height=?"))
               (result (dbi:execute query n)))
          (setf last-hash (getf (dbi:fetch result) :|hash|))))

      (let* ((query1 (dbi:prepare database "INSERT INTO blocks (id, hash, timestamp, bits, nonce) VALUES (?, ?, ?, ?, ?)"))
             (query2 (dbi:prepare database "INSERT INTO transactions (id, block_id, hash, timestamp) VALUES (?, ?, ?, ?)"))
             (query3 (dbi:prepare database "INSERT INTO inputs (id, transaction_id, transaction_hash, transaction_index, script) VALUES (?, ?, ?, ?, ?)"))
             (query4 (dbi:prepare database "INSERT INTO outputs (id, transaction_id, index, value, address, script) VALUES (?, ?, ?, ?, ?, ?)"))
             (query5 (dbi:prepare database "UPDATE blocks SET height=? WHERE hash=?"))

             (block-callback (lambda (blk)
                               (when (equal (pretty-print-hash (hash blk)) last-hash)
                                 (return-from rdbms-update-database-from-blockchain))

                               (dbi:execute query1 block-id (pretty-print-hash (hash blk)) (timestamp blk) (bits blk) (nonce blk))

                               (dotimes (i (transaction-count blk))
                                 (let ((transaction (aref (transactions blk) i)))
                                   (dbi:execute query2 transaction-id block-id (pretty-print-hash (hash transaction)) (timestamp transaction))

                                   (dotimes (j (input-count transaction))
                                     (let ((input (aref (inputs transaction) j)))
                                       (dbi:execute query3 input-id transaction-id (pretty-print-hash (transaction-hash input)) (transaction-index input) (bin-to-hex (script input)))
                                       (incf input-id)))

                                   (dotimes (j (output-count transaction))
                                     (let ((output (aref (outputs transaction) j)))
                                       (dbi:execute query4 output-id transaction-id (index output) (value output) (pretty-print-address (get-output-address (script output))) (bin-to-hex (script output)))
                                       (incf output-id)))

                                   (incf transaction-id)))
                               (incf block-id)))

             (end-callback (lambda (block-hashes)
                             (let ((block-height (1+ n)))
                               (dolist (hash block-hashes)
                                 (dbi:execute query5 block-height (pretty-print-hash hash))
                                 (incf block-height))))))

        (file-parse-blockchain nil block-callback end-callback t)))))

(defun rdbms-get-balance (address)
  "Get the current balance of an ADDRESS."
  (dbi:with-connection (database *rdbms-driver*
                                 :database-name *rdbms-database*
                                 :username *rdbms-username*
                                 :password *rdbms-password*)
    (let ((query1 (dbi:prepare database "SELECT sum(value) FROM outputs WHERE address = ?"))
          (query2 (dbi:prepare database "SELECT sum(o.value) FROM inputs i, outputs o, transactions t WHERE o.address = ? AND i.transaction_hash = t.hash AND o.transaction_id = t.id AND i.transaction_index = o.index"))
          (balance 0)
          result
          value)
      ;; Get the outputs
      (setf result (dbi:execute query1 address))
      (setf value (getf (dbi:fetch result) :|sum|))
      (when (integerp value)
        (incf balance value))

      ;; Get the inputs (spent outputs)
      (setf result (dbi:execute query2 address))
      (setf value (getf (dbi:fetch result) :|sum|))
      (when (integerp value)
        (decf balance value))

      (/ balance 1000000.0d0))))

(defun rdbms-get-rich-addresses (&optional (n 10))
  "Get the list of the N richest addresses."
  (dbi:with-connection (database *rdbms-driver*
                                 :database-name *rdbms-database*
                                 :username *rdbms-username*
                                 :password *rdbms-password*)
    (let* ((query (dbi:prepare database "SELECT result.address, sum(result.value) FROM (SELECT o.address, t.hash, o.index, o.value FROM transactions t, outputs o WHERE o.address <> 'NIL' AND o.transaction_id = t.id EXCEPT SELECT o.address, t.hash, o.index, o.value FROM transactions t, inputs i, outputs o WHERE o.address <> 'NIL' AND o.transaction_id = t.id AND i.transaction_hash = t.hash AND i.transaction_index = o.index) AS result GROUP BY result.address ORDER BY sum(result.value)"))
           (result (dbi:execute query))
           balances)
      (loop
         for row = (dbi:fetch result)
         while row
         do (push (list (getf row :|address|) (/ (getf row :|sum|) 1000000.0d0)) balances))

      (subseq balances 0 (min n (length balances))))))

(defun rdbms-get-history (address)
  "Get the transaction history of an ADDRESS."
  (dbi:with-connection (database *rdbms-driver*
                                 :database-name *rdbms-database*
                                 :username *rdbms-username*
                                 :password *rdbms-password*)
    (let ((query1 (dbi:prepare database "SELECT t.timestamp, t.hash, sum(value) FROM outputs o, transactions t WHERE o.address = ? AND o.transaction_id = t.id GROUP BY t.timestamp, t.hash ORDER BY t.timestamp"))
          (query2 (dbi:prepare database "SELECT t2.timestamp, t2.hash, sum(o.value) FROM inputs i, outputs o, transactions t1, transactions t2 WHERE o.address = ? and i.transaction_hash = t1.hash AND o.transaction_id = t1.id AND i.transaction_index = o.index AND i.transaction_id = t2.id GROUP BY t2.hash, t2.timestamp ORDER BY t2.timestamp;"))
          result
          transactions)
      ;; Get the outputs
      (setf result (dbi:execute query1 address))
      (loop
         for row = (dbi:fetch result)
         while row
         do (push (list (getf row :|timestamp|) (getf row :|hash|) (getf row :|sum|)) transactions))

      ;; Get the inputs (spent outputs)
      (setf result (dbi:execute query2 address))
      (loop
         for row = (dbi:fetch result)
         while row
         do (push (list (getf row :|timestamp|) (getf row :|hash|) (- (getf row :|sum|))) transactions))

      ;; Merge the input and output of minting transactions in one transaction
      (setf transactions (sort transactions #'(lambda (x y) (string> (second x) (second y)))))
      (let (last-tx txs)
        (dolist (tx transactions)
          (if (null last-tx)
              (setf last-tx tx)
              (if (and (= (first last-tx) (first tx))
                       (equal (second last-tx) (second tx)))
                  (progn
                    (push (list (first tx) (second tx) (+ (third last-tx) (third tx))) txs)
                    (setf last-tx nil))
                  (progn
                    (push last-tx txs)
                    (setf last-tx tx)))))
        (when last-tx
          (push last-tx txs))
        (setf transactions txs))

      (setf transactions (sort transactions #'(lambda (x y) (< (first x) (first y)))))
      (mapcar #'(lambda (x) (list (epoch-to-utc (first x)) (second x) (/ (third x) 1000000.0d0))) transactions))))

(defun rdbms-get-unspent-transactions (address)
  "Get the list of unspent transactions of an ADDRESS."
  (dbi:with-connection (database *rdbms-driver*
                                 :database-name *rdbms-database*
                                 :username *rdbms-username*
                                 :password *rdbms-password*)
    (let ((query (dbi:prepare database "SELECT t.timestamp, t.hash, o.index, o.value FROM transactions t, outputs o WHERE o.address = ? AND o.transaction_id = t.id EXCEPT SELECT t.timestamp, t.hash, o.index, o.value FROM transactions t, inputs i, outputs o WHERE o.address = ? AND o.transaction_id = t.id AND i.transaction_hash = t.hash AND i.transaction_index = o.index"))
          result
          transactions)
      ;; Get the unspent outputs
      (setf result (dbi:execute query address address))
      (loop
         for row = (dbi:fetch result)
         while row
         do (push (list (getf row :|timestamp|) (getf row :|hash|) (getf row :|index|) (getf row :|value|)) transactions))

      (setf transactions (sort transactions #'(lambda (x y) (< (first x) (first y)))))
      (mapcar #'(lambda (x) (list (epoch-to-utc (first x)) (second x) (third x) (/ (fourth x) 1000000.0d0))) transactions))))

(defun rdbms-get-balances-at-block (n)
  "Get the list of the addresses with a positive balance at block N."
  (dbi:with-connection (database *rdbms-driver*
                                 :database-name *rdbms-database*
                                 :username *rdbms-username*
                                 :password *rdbms-password*)
    (let* ((query (dbi:prepare database "SELECT result.address, sum(result.value) FROM (SELECT o.address, t.hash, o.index, o.value FROM transactions t, outputs o, blocks b WHERE o.address <> 'NIL' AND o.transaction_id = t.id AND t.block_id = b.id AND b.height <= ? EXCEPT SELECT o.address, t.hash, o.index, o.value FROM transactions t, inputs i, outputs o, blocks b, transactions ti WHERE o.address <> 'NIL' AND o.transaction_id = t.id AND i.transaction_id = ti.id AND ti.block_id = b.id AND b.height <= ? AND i.transaction_hash = t.hash AND i.transaction_index = o.index) AS result GROUP BY result.address ORDER BY result.address DESC"))
           (result (dbi:execute query n n))
           balances)
      (loop
         for row = (dbi:fetch result)
         while row
         do (when (plusp (getf row :|sum|))
              (push (list (getf row :|address|) (/ (getf row :|sum|) 1000000.0d0)) balances))
         finally (return balances)))))
