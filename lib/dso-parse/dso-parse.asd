;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp -*-

#|
Copyright (C) 2007, 2008  David Owen <dsowen@fugue88.ws>

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU Lesser Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Lesser Public License for more details.

You should have received a copy of the GNU Lesser Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
|#

(asdf:defsystem #:dso-parse
  :depends-on (#:cl-ppcre #:dso-util)
  :components ((:file "package")
	       (:file "util")
	       (:file "matching")
	       (:file "parsing"))
  :serial t)
