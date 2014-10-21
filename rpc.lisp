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


(defun rpc (command &optional parameters)
  "Send a COMMAND with optional PARAMETERS to the Peercoin daemon."
  (let (answer auth json params)
    (setf auth (list *rpc-username* *rpc-password*))
    (setf params (cl-json:encode-json-to-string parameters))
    (setf json (format nil "{\"jsonrpc\":\"1.0\", \"id\":\"1\", \"method\":\"~a\", \"params\":~a}" command params))
    (setf answer (drakma:http-request *rpc-server-url*
                                      :basic-authorization auth
                                      :content-type "text/plain"
                                      :content json))
    (setf answer (flexi-streams:octets-to-string answer))
    (setf answer (cl-json:decode-json-from-string answer))
    (if (cdr (assoc :error answer))
        (cdr (assoc :error answer))
        (cdr (assoc :result answer)))))

(defun rpc-get-transaction (h)
  "Get a transaction using RPC given its hash."
  (let (result tx)
    (setf result (rpc "getrawtransaction" (list h 1)))
    (setf tx (make-instance 'transaction))
    (with-slots (hash version timestamp input-count inputs output-count outputs lock-time)
        tx
      (setf hash (reverse (hex-to-bin h))
            version (cdr (assoc :version result))
            timestamp (cdr (assoc :time result))
            lock-time (cdr (assoc :locktime result)))

      ;; Read inputs
      (setf input-count (length (cdr (assoc :vin result)))
            inputs (make-array input-count))
      (let ((i 0))
        (dolist (in (cdr (assoc :vin result)))
          (setf (aref inputs i) (make-instance 'input))
          (with-slots (transaction-hash transaction-index script-length script sequence-number)
              (aref inputs i)
            (setf transaction-hash (reverse (hex-to-bin (cdr (assoc :txid in))))
                  transaction-index (cdr (assoc :vout in))
                  script-length (/ (length (cdr (assoc :hex (cdr (assoc :script-sig in))))) 2)
                  script (hex-to-bin (cdr (assoc :hex (cdr (assoc :script-sig in)))))
                  sequence-number (cdr (assoc :sequence in)))
            (when (null transaction-index)
              (setf transaction-index 0)))
          (incf i)))

      ;; Read outputs
      (setf output-count (length (cdr (assoc :vout result)))
            outputs (make-array output-count))
      (let ((i 0))
        (dolist (out (cdr (assoc :vout result)))
          (setf (aref outputs i) (make-instance 'output))
          (with-slots (index value script-length script)
              (aref outputs i)
            (setf index (cdr (assoc :n out))
                  value (floor (* 1000000 (cdr (assoc :value out))))
                  script-length (/ (length (cdr (assoc :hex (cdr (assoc :script-pub-key out))))) 2)
                  script (hex-to-bin (cdr (assoc :hex (cdr (assoc :script-pub-key out)))))))
          (incf i))))
    tx))

(defun rpc-get-block (h)
  "Get a block using RPC given its hash."
  (let (result blk)
    (setf result (rpc "getblock" (list h)))
    (setf blk (make-instance 'blk))
    (with-slots (hash header-length version previous-hash merkle-root timestamp bits nonce transaction-count transactions)
        blk
      (setf hash (reverse (hex-to-bin h))
            header-length (cdr (assoc :size result))
            version (cdr (assoc :version result))
            previous-hash (reverse (hex-to-bin (cdr (assoc :previousblockhash result))))
            merkle-root (reverse (hex-to-bin (cdr (assoc :merkleroot result))))
            timestamp (utc-to-epoch (concatenate 'string
                                                 (subseq (cdr (assoc :time result)) 0 10)
                                                 "T"
                                                 (subseq (cdr (assoc :time result)) 11 19)
                                                 "Z"))
            bits (parse-integer (cdr (assoc :bits result)) :radix 16)
            nonce (cdr (assoc :nonce result)))

      ;; Read transactions
      (setf transaction-count (length (cdr (assoc :tx result)))
            transactions (make-array transaction-count))
      (let ((i 0))
        (dolist (txhash (cdr (assoc :tx result)))
          (setf (aref transactions i) (rpc-get-transaction txhash))
          (incf i))))
    blk))

(defun rpc-get-block-by-number (n)
  "Get a block using RPC given its number."
  (let (h)
    (setf h (rpc "getblockhash" (list n)))
    (rpc-get-block h)))

(defun rpc-get-block-count ()
  "Get the number of blocks available in the Peercoin daemon."
  (rpc "getblockcount"))
