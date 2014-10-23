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


(defparameter *file-blockchain* (merge-pathnames ".ppcoin/blk0001.dat" (user-homedir-pathname)))

(defparameter *rpc-server-url* "http://127.0.0.1:9902/")
(defparameter *rpc-username* "username")
(defparameter *rpc-password* "password")

(defparameter *rdbms-driver* :postgres)
(defparameter *rdbms-database* "peercoin")
(defparameter *rdbms-username* "username")
(defparameter *rdbms-password* "password")
