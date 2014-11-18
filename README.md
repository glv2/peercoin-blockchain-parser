Peercoin blockchain parser
==========================

## Description

This program can parse the blockchain contained in a file and export some of its data to a text file, a SQL script or a database.
It can also create a database using the RPC of a Peercoin daemon as source of data instead of a blockchain file.

It is written in Common Lisp.

## Dependencies

* A Common Lisp implementation. Tested with:
  * [sbcl](http://www.sbcl.org)
  * [clozurecl](http://ccl.clozure.com)
  * [ecl](http://ecls.sourceforge.net)
* [cl-json](http://cliki.net/cl-json)
* [cl-dbi](http://cliki.net/cl-dbi)
* [drakma](http://cliki.net/drakma)
* [flexi-streams](http://cliki.net/flexi-streams)
* [ironclad](http://cliki.net/Ironclad)
* [local-time](http://cliki.net/local-time)

## Installation

* Install [quicklisp](http://www.quicklisp.org/beta/) to manage the packages.
* Copy the source code of the peercoin blockchain parser where you want it to be.
* Tell your Common Lisp implementation where to find the sources:
  * ```(push "directory-where-the-sources-are/" asdf:*central-registry*)```
  * If you don't want to type this line every time, you can add it to the initialization file (e.g.: .sbclrc, .ccl-init.lisp, .eclrc).

## Configuration

Edit the configuration file (config.lisp) to indicate where to find the blockchain file, the Peercoin daemon and the database server.

## Usage

First, load the parser package:

    (require 'peercoin-blockchain-parser)
    (in-package peercoin-blockchain-parser)

To export the data from the blockchain to a SQL script:

    (sql-make-script-from-blockchain "peercoin-blockchain.sql")

To export the data from the blockchain to a text file:

    (txt-make-file-from-blockchain "peercoin-blockchain.txt")

To create a database from scratch using a running Peercoin daemon:

    (rdbms-initialize-database)
    (rdbms-update-database-from-rpc)

To update a database using a running Peercoin daemon:

    (rdbms-update-database-from-rpc)

To update a database using the blockchain:

    (rdbms-update-database-from-blockchain)

To get the balance of an address:

    (rdbms-get-balance "PWFNV1Cvq7nQBRyRueuYzwmDNXUGpgNkBC")

To get the transaction history of an address:

    (rdbms-get-history "PWFNV1Cvq7nQBRyRueuYzwmDNXUGpgNkBC")

To get the list of the 20 richest addresses:

    (rdbms-get-rich-addresses 20)

To get the list of the unspent transactions of an address:

    (rdbms-get-unspent-transactions "PWFNV1Cvq7nQBRyRueuYzwmDNXUGpgNkBC")

## Donations

If you find this program useful and want to make a donation, you can send coins to the following Peercoin address: **PWFNV1Cvq7nQBRyRueuYzwmDNXUGpgNkBC**.
