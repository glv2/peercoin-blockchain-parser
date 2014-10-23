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


(defun sha256 (data)
  "Compute the sha256 hash of the byte sequence DATA."
  (ironclad:digest-sequence :sha256 data))

(defun sha256d (data)
  "Compute the double sha256 hash of the byte sequence DATA."
  (sha256 (sha256 data)))

(defun ripemd160 (data)
  "Compute the ripemd160 hash of the byte sequence DATA."
  (ironclad:digest-sequence :ripemd-160 data))
