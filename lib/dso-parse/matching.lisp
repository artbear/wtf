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

(in-package #:dso-parse)



(defmacro if-matches-parser (input parser (next match)
			     &body (&optional (then t) else))
  "Matches if the function named by PARSER accepts the input.  Such a
parser must return T, the remaining unmatched input, and the accepted
input as values (in that order), or NIL if matching fails.

NOTE: A parser that accepts a portion of input but does not wish to
emit it may do so.  For example, a parser may return the values T,
\"blah....\", NIL."
  (declare (symbol parser next match))
  (with-gensyms (ok)
    `(multiple-value-bind (,ok ,next ,match) (,parser ,input)
      (if ,ok ,then ,else))))

(defmacro if-matches-char (input char (next match)
			   &body (&optional (then t) else))
  "Matches if the first character of INPUT is CHAR."
  (declare (symbol next match)
	   (character char))
  `(if (and (string/= "" ,input) (char= ,char (char ,input 0)))
    (let ((,next (substring ,input :start 1))
	  (,match ,char))
      ,then)
    ,else))

(defmacro if-matches-string (input string (next match)
			     &body (&optional (then t) else))
  "Matches if INPUT is prefixed by STRING."
  (declare (symbol next match)
	   (string string))
  (let ((len (length string)))
    `(if (and (>= (length ,input) ,len)
	  (string= ,string (substring ,input :length ,len)))
      (let ((,next (substring ,input :start ,len))
	    (,match ,string))
	,then)
      ,else)))

(defmacro if-matches-regex (input regex (next match)
			    &body (&optional (then t) else))
  "Matches if REGEX matches the beginning of INPUT (REGEX is modified
to anchor to the beginning automatically)."
  (declare (string regex)
	   (symbol next match))
  (let ((regex `(:sequence (:flags) :start-anchor
		 (:register (:regex ,regex)))))
    (with-gensyms (start end)
      `(multiple-value-bind (,start ,end) (scan ',regex ,input)
	(if ,start
	    (let ((,next (substring ,input :start ,end))
		  (,match (substring ,input :end ,end)))
	      ,then)
	    ,else)))))

(defmacro if-matches-choice (input alts (next match)
			     &body (&optional (then t) else))
  (declare (symbol next match)
	   (list alts))
  "Matches if any of ALTS match INPUT.  The first rule in ALTS that
matches is used to consume input and build the parse tree."
  (if alts
      `(if-matches ,input ,(first alts) (,next ,match)
	,then
	(if-matches-choice ,input ,(rest alts) (,next ,match)
	  ,then
	  ,else))
      else))

(defmacro if-matches-sequence (input seq (next match)
			       &body (&optional (then t) else))
  "Matches if all of the rules in SEQ match INPUT in sequence (ie, the
second rule matches what is left of the input after the first rule has
matched, and so on)."
  (destructuring-bind (head . tail) seq
    (if tail
	(with-gensyms (head-next head-match tail-match)
	  `(if-matches ,input ,head (,head-next ,head-match)
	    (if-matches-sequence ,head-next ,tail (,next ,tail-match)
	      (let ((,match (if ,head-match (cons ,head-match ,tail-match) ,tail-match)))
		,then)
	      ,else)
	    ,else))
	(with-gensyms (tail-match)
	  `(if-matches ,input ,head (,next ,tail-match)
	    (let ((,match (if ,tail-match (list ,tail-match) nil)))
	      ,then)
	    ,else)))))

(defmacro if-matches-eqcount (input count rule (next match)
			      &body (&optional (then t) else))
  "Matches if RULE matches INPUT exactly COUNT times in sequence.

WARNING: ELSE code executes in an anonymous block!"
  (declare ((or null (integer 0)) count)
	   (symbol next match))
  (with-gensyms (r i next2 match2)
    `(block ,r
      (let ((,next ,input)
	    (,match nil))
	(dotimes (,i ,(if count count 0))
	  (if-matches ,next ,rule (,next2 ,match2)
	    (setf ,next ,next2
		  ,match (if ,match2 (cons ,match2 ,match) ,match))
	    (return-from ,r ,else)))
	(setf ,match (nreverse ,match))
	,then))))

(defmacro if-matches-maxcount (input count rule (next match)
			       &body (&optional (then t) else))
  "Always matches INPUT, but attempts to match RULE up to COUNT times
in sequence (ie, RULE is matched against INPUT from 0 to COUNT times).
If COUNT is NIL, RULE is matched as often as it will."
  (declare ((or null (integer 0)) count)
	   (symbol next match)
	   (ignore else))
  (with-gensyms (i next2 match2)
    `(let ((,next ,input)
	   (,match nil))
      (let ((,i 0))
	(while (and ,(if count `(< ,i ,count) t)
		    (if-matches ,next ,rule (,next2 ,match2)
		      (progn
			(psetf ,next ,next2
			       ,match (if ,match2 (cons ,match2 ,match) ,match)
			       ,i (1+ ,i))
			t)))))
      (setf ,match (nreverse ,match))
      ,then)))

(defmacro if-matches-count (input mincount maxcount rule (next match)
			    &body (&optional (then t) else))
  "Matches RULE against INPUT from MINCOUNT to MAXCOUNT times
sequentially.  If MINCOUNT is NIL, it is treated as 0.  If MAXCOUNT is
NIL, RULE is matched as often as it will (after the MINCOUNT is
satisfied)."
  (when (and maxcount mincount) (setf maxcount (- maxcount mincount)))
  (with-gensyms (next2 match2)
    `(if-matches-eqcount ,input ,mincount ,rule (,next2 ,match2)
      (if-matches-maxcount ,next2 ,maxcount ,rule (,next ,match)
	(let ((,match (nconc ,match2 ,match)))
	  ,then)
	,else)
      ,else)))

(defmacro if-matches-required (input rule (next match)
			       &body (&optional (then t) else))
  "Matches if RULE matches against INPUT, but neither consumes input
nor contributes to the parse tree."
  `(if-matches ,input ,rule (,(gensym) ,(gensym))
    (let ((,next ,input)
	  (,match nil))
      ,then)
    ,else))

(defmacro if-matches-forbidden (input rule (next match)
				&body (&optional (then t) else))
  "Matches if RULE doesn't match against INPUT, but neither consumes
input nor contributes to the parse tree."
  `(if-matches ,input ,rule (,(gensym) ,(gensym))
    ,else
    (let ((,next ,input)
	  (,match nil))
      ,then)))

(defmacro if-matches-only (input rule (next match)
			   &body (&optional (then t) else))
  "Matches RULE against INPUT and consumes input, but doesn't
contribute to the parse tree."
  `(if-matches ,input ,rule (,next ,match)
    (let ((,match nil))
      ,then)
    ,else))

(defmacro if-matches (input rule (next match) &body (&optional (then t) else))
  "Matches RULE against INPUT.  If the rule is matched, the THEN form
is executed with NEXT bound to the remainder of the input after
consumption (if any), and MATCH to the parse tree.

Rules may be defined in a short-hand using this macro, and nested:

* Characters and strings match themselves.

* A regular expression is denoted as (^ regex).

* A sequence is a list of rules (rule1 rule2) that doesn't begin with
  a reserved mark.

* An ordered choice is (/ rule1 rule2).

* A Kleene-closure is (* rule).  + and ? may be used as well, and all
  act as in regular expressions.

* Range matching is ({} min max rule).  If MIN is NIL, it is treated
  as 0.  If MAX is NIL, no limit is placed on matches.

* Requiring a match without consuming or tree-building is done as (&
  rule).  Forbidding a match likewise as (! rule).

* Matching and consuming without tree-building is (= rule).

If RULE is a symbol, that symbol will be used as the name of a parsing
function.  The function will be given the input as its only argument,
and is expected to return (VALUES T NEXT MATCH) (where NEXT refers to
the remaining, unsonsumed input, and MATCH is the generated tree), or
NIL otherwise."
  (etypecase rule
    (symbol `(if-matches-parser ,input ,rule (,next ,match) ,then ,else))
    (character `(if-matches-char ,input ,rule (,next ,match) ,then ,else))
    (string `(if-matches-string ,input ,rule (,next ,match) ,then ,else))
    (list
     (destructuring-bind (head . tail) rule
       (if (symbolp head)
           (symcase head
                    (^ (destructuring-bind (regex) tail
                         `(if-matches-regex ,input ,regex (,next ,match)
                                            ,then ,else)))
                    (/ `(if-matches-choice ,input ,tail (,next ,match)
                                           ,then
                                           ,else))
                    ({} (destructuring-bind (mincount maxcount subrule) tail
                          `(if-matches-count ,input ,mincount ,maxcount ,subrule
                                             (,next ,match) ,then ,else)))
                    (* (destructuring-bind (sub) tail
                         `(if-matches-count ,input 0 nil ,sub (,next ,match)
                                            ,then ,else)))
                    (? (destructuring-bind (sub) tail
                         `(if-matches-count ,input 0 1 ,sub (,next ,match)
                                            ,then ,else)))
                    (+ (destructuring-bind (sub) tail
                         `(if-matches-count ,input 1 nil ,sub (,next ,match)
                                            ,then ,else)))
                    (& (destructuring-bind (sub) tail
                         `(if-matches-required ,input ,sub (,next ,match)
                                               ,then ,else)))
                    (! (destructuring-bind (sub) tail
                         `(if-matches-forbidden ,input ,sub (,next ,match)
                                                ,then ,else)))
                    (= (destructuring-bind (sub) tail
                         `(if-matches-only ,input ,sub (,next ,match)
                                           ,then
                                           ,else)))
                    (t `(if-matches-sequence ,input ,rule (,next ,match)
                                             ,then ,else)))
           `(if-matches-sequence ,input ,rule (,next ,match) ,then ,else))))))
