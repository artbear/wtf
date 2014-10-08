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

(defpackage #:dso-parse
  (:documentation
   "Defines macros for matching input against rules and building trees
(see the IF-MATCHES-* macros, but especially IF-MATCHES), and for
defining parsers (DEFPARSER) and grammars (DEFGRAMMAR).")
  (:use #:cl #:cl-ppcre #:dso-util)
  (:export #:if-matches-parser
	   #:if-matches-char
	   #:if-matches-string
	   #:if-matches-regex
	   #:if-matches-choice
	   #:if-matches-sequence
	   #:if-matches-count
	   #:if-matches-required
	   #:if-matches-forbidden
	   #:if-matches-only
	   #:if-matches
	   #:defparser
	   #:defgrammar))
