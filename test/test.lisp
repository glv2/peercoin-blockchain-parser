(require :peercoin-blockchain-parser)

(peercoin-blockchain-parser:blockchain-to-sql "blk0.dat" "blk0.sql")
(peercoin-blockchain-parser:blockchain-to-txt "blk0.dat" "blk0.txt")
(quit)
