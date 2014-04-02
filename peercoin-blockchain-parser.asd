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


(defpackage :peercoin-blockchain-parser-system
  (:use :cl :asdf))


(in-package :peercoin-blockchain-parser-system)


(defsystem :peercoin-blockchain-parser
    :name "peercoin-blockchain-parser"
    :description "Peercoin blockchain parser"
    :version "0.1"
    :author "Guillaume LE VAILLANT"
    :license "GPL-3"
    :depends-on (:ironclad)
    :components ((:file "package")
                 (:file "parser" :depends-on ("package"))
                 (:file "script" :depends-on ("package"))
                 (:file "sql" :depends-on ("package" "parser" "script"))
                 (:file "txt" :depends-on ("package" "parser" "script"))))
