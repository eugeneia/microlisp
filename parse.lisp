;;;; microlisp.parse:  Parse microlisp source files.

(defpackage microlisp.parse
  (:use :cl)
  (:export :parse))

(in-package :microlisp.parse)


;;; The next two constants look like you could change them to modify
;;; parser behaviour but you can't. Never. They just designate the first
;;; and last character of a top-level s-expression.

(defconstant +list-open+ #\(
  "List opening character.")

(defconstant +list-close+ #\)
  "List closing character.")


;;; These you can totally adjust if you'd like.

(defconstant +comment-open+ #\;
  "Comment opening character.")

(defconstant +comment-close+ #\Newline
  "Comment closing character.")

(defparameter *whitespace* '(#\Space #\Tab #\Return #\Newline)
  "Characters considered whitespace.")


;;; Utilities

(defun peek-char* (stream &optional peek-mode)
  "(peek-char PEEK-MODE STREAM nil)"
  (peek-char peek-mode stream nil))

(defun whitespace-p (character)
  "Predicate to test if CHARACTER is in *whitespace*."
  (member character *whitespace*))

(defun comment-open-p (character)
  "Predicate to test if CHARACTER is +comment-open+."
  (and character (char= +comment-open+ character)))

(defun skip-whitespace (input)
  "Skips whitespace in input."
  (loop while (whitespace-p (peek-char* input))
     do (read-char input nil)))

(defun skip-comment (input)
  "Skips comment in INPUT."
  (when (comment-open-p (peek-char* input))
    (peek-char* input +comment-close+)))

(defun skip-to-expression (input)
  "Skips whitespace and comments in INPUT, return nil when end of file is
reached."
  (skip-whitespace input)
  (skip-comment input)
  (let ((next-char (peek-char* input)))
    (if (or (comment-open-p next-char)
            (whitespace-p next-char))
        (skip-to-expression input)
        (when next-char t))))


;;; Parsing functions

(defun parse-sexp (input)
  "Parses an s-expression from stream INPUT."
  (unless (char= +list-open+ (read-char input))
    (error "Invalid input."))
  (read-delimited-list +list-close+ input))

(defun parse (input)
  "Parses an abstract source tree from stream INPUT."
  (let (ast)
    (loop do
         (if (skip-to-expression input)
             (push (parse-sexp input) ast)
             (return (reverse ast))))))
