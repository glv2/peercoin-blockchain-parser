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


(defsystem :peercoin-blockchain-parser
    :name "peercoin-blockchain-parser"
    :description "Peercoin blockchain parser"
    :version "0.2"
    :author "Guillaume LE VAILLANT"
    :license "GPL-3"
    :depends-on (:cl-json :dbi :drakma :flexi-streams :ironclad :local-time)
    :components ((:file "callbacks" :depends-on ("package"))
                 (:file "config" :depends-on ("package"))
                 (:file "file" :depends-on ("callbacks" "hash" "package" "types"))
                 (:file "hash" :depends-on ("package"))
                 (:file "package")
                 (:file "rdbms" :depends-on ("callbacks" "config" "package" "rpc" "script" "types" "utils"))
                 (:file "rpc" :depends-on ("callbacks" "config" "hash" "package" "types"))
                 (:file "script" :depends-on ("hash" "package"))
                 (:file "sql" :depends-on ("callbacks" "file" "package" "script" "types" "utils"))
                 (:file "txt" :depends-on ("callbacks" "file" "package" "script" "types" "utils"))
                 (:file "types" :depends-on ("package"))
                 (:file "utils" :depends-on ("hash" "package"))))
