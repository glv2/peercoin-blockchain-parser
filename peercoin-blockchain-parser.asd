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


(defsystem :peercoin-blockchain-parser
    :name "peercoin-blockchain-parser"
    :description "Peercoin blockchain parser"
    :version "0.2"
    :author "Guillaume LE VAILLANT"
    :license "LGPL-3"
    :depends-on (:cl-json :dbi :drakma :flexi-streams :ironclad :local-time)
    :components ((:file "config" :depends-on ("package"))
                 (:file "file" :depends-on ("config" "hash" "package" "types"))
                 (:file "hash" :depends-on ("package"))
                 (:file "package")
                 (:file "rdbms" :depends-on ("config" "file" "package" "rpc" "script" "types" "utils"))
                 (:file "rpc" :depends-on ("config" "hash" "package" "types"))
                 (:file "script" :depends-on ("hash" "package"))
                 (:file "sql" :depends-on ("file" "package" "script" "types" "utils"))
                 (:file "txt" :depends-on ("file" "package" "script" "types" "utils"))
                 (:file "types" :depends-on ("package"))
                 (:file "utils" :depends-on ("hash" "package"))))
