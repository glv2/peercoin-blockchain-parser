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


(defmacro make-class (name slots)
  `(defclass ,name ()
     ,(mapcar #'(lambda (x) `(,x :accessor , x)) slots)))

(make-class input (transaction-hash transaction-index script-length script sequence-number))

(make-class output (index value script-length script))

(make-class transaction (hash version timestamp input-count inputs output-count outputs lock-time))

(make-class blk (hash header-length version previous-hash merkle-root timestamp bits nonce transaction-count transactions))
