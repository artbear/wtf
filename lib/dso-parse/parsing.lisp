;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp -*-

#|
Copyright (C) 2007, 2008, 2009  David Owen <dsowen@fugue88.ws>

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

(in-package #:dso-parse)



(defmacro defparser (name rule &key cclass filter)
  "Defines a function named NAME that parses its input according to
RULE (see IF-MATCHES).  On success, returns the values T, the
unconsumed input, and the tree (labeled with NAME, unless CCLASS is
true).  On failure, returns NIL.

The tree may optionally be filtered by FILTER before being labeled."
  (let ((filtered (if filter `(funcall ,filter match) `match)))
    `(defun ,name (input)
       (if-matches input ,rule (next match)
         (values t next ,(if cclass `,filtered `(cons ',name ,filtered)))))))

(defmacro defgrammar (() &body definitions)
  "Defines a grammar, being a collection of parsers.  The syntax is:

\(DEFGRAMMAR (options*) definitions*)
definitions: (name rule &key filter cclass)"
  (flet ((x (definition) `(defparser ,@definition)))
    `(progn
       ,@(mapcar #'x definitions)
       '(,@(mapcar 'first definitions)))))
