(push "../" asdf:*central-registry*)
(require 'peercoin-blockchain-parser)

(in-package peercoin-blockchain-parser)
(setf *file-blockchain* "blk0.dat")
(sql-make-script-from-blockchain "blk0.sql")
(txt-make-file-from-blockchain "blk0.txt")

(in-package cl-user)
(quit)
