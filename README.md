Peercoin blockchain parser
===

## Details

This program parses the blockchain contained in a file and exports some of its data to another format.

Written in Common Lisp.

## Dependencies

* A Common Lisp implementation. Tested with:
  * [sbcl](http://www.sbcl.org/)
  * [clozurecl](http://ccl.clozure.com/)
  * [ecl](http://ecls.sourceforge.net/)
* [Ironclad](http://method-combination.net/lisp/ironclad/).

## Installation

* Install Ironclad, the easiest way is probably using [Quicklisp](http://www.quicklisp.org/beta/).
  * ```(ql:quickload :ironclad)```
* Copy the source code of the peercoin blockchain parser where you want it to be.
* Tell your Common Lisp implementation where to find the sources:
  * ```(push "directory-where-the-sources-are/" asdf:*central-registry*)```
  * If you don't want to type this line every time, you can add it to the initialization file (e.g.: .sbclrc, .ccl-init.lisp, .eclrc).

## Start

First, load the parser package:

    (require :peercoin-blockchain-parser)

To export the data from the blockchain to a SQL script:

    (peercoin-blockchain-parser:bockchain-to-sql "blk0001.dat" "peercoin-blockchain.sql")

To export the data from the blockchain to a text file:

    (peercoin-blockchain-parser:bockchain-to-txt "blk0001.dat" "peercoin-blockchain.txt")
