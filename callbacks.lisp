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


;;; Default callbacks (do nothing)

(defparameter chain-start (lambda ()))
(defparameter block-start (lambda ()))
(defparameter transactions-start (lambda ()))
(defparameter transaction-start (lambda ()))
(defparameter inputs-start (lambda ()))
(defparameter input-start (lambda ()))
(defparameter input-end (lambda (input) (declare (ignore input))))
(defparameter inputs-end (lambda ()))
(defparameter outputs-start (lambda ()))
(defparameter output-start (lambda ()))
(defparameter output-end (lambda (output) (declare (ignore output))))
(defparameter outputs-end (lambda ()))
(defparameter transaction-end (lambda (transaction) (declare (ignore transaction))))
(defparameter transactions-end (lambda ()))
(defparameter block-end (lambda (blk) (declare (ignore blk))))
(defparameter chain-end (lambda (block-hashes) (declare (ignore block-hashes))))
