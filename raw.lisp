#|
Copyright 2015 Guillaume LE VAILLANT

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


(defun raw-extract-blocks-from-blockchain (hashes dat-file)
  "Extract the raw data of the blocks with the specified HASHES from the blockchain and write them to DAT-FILE."
  (with-open-file (dat dat-file
                       :element-type '(unsigned-byte 8)
                       :direction :output
                       :if-exists :supersede)
    (dolist (hash hashes)
      (let ((data (file-get-block hash t)))
        (write-sequence data dat)))))
