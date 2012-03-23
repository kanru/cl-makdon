;;;; makdon.lisp --- CL-MAKDON

;;; Copyright (C) 2012  Kan-Ru Chen

;;; Author(s): Kan-Ru Chen <kanru@kanru.info>

;;; Permission is hereby granted, free of charge, to any person obtaining a
;;; copy of this software and associated documentation files (the "Software"),
;;; to deal in the Software without restriction, including without limitation
;;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;;; and/or sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following conditions:

;;; The above copyright notice and this permission notice shall be included in
;;; all copies or substantial portions of the Software.

;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.

;;;; Commentary:

;;;

;;;; Code:

(in-package #:makdon)

(declaim (optimize debug))

;;; enum library

(defclass enum ()
  ((items :initarg :items
          :initform nil
          :accessor items)))

(defun make-enum (&optional list)
  (make-instance 'enum :items list))

(defun enum-push (enum item)
  (push item (items enum))
  item)

(defun enum-peek (enum)
  (car (items enum)))

(defun enum-peek2 (enum)
  (cadr (items enum)))

(defun enum-junk (enum)
  (pop (items enum)))

(defun enum-map (fun enum)
  (make-enum (mapcar fun (items enum))))

(defmethod print-object ((object enum) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (print (items object) stream)))

;;; Line

(defclass line ()
  ((indent :initarg :indent
           :initform 0
           :accessor indent)
   (quotes :initarg :quotes
           :initform 0
           :accessor quotes)
   (text :initarg :text
         :initform ""
         :accessor text)
   (blankp :initarg :blankp
           :initform t
           :accessor blankp)))

(defun make-line (&key indent quotes text blankp)
  (make-instance 'line :indent indent
                       :quotes quotes
                       :text text
                       :blankp blankp))

(defmethod print-object ((object line) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "indent: ~a quotes: ~a text: ~s blankp: ~a"
            (indent object)
            (quotes object)
            (text object)
            (blankp object))))

;;; Utility

(defun indent-level (string)
  (loop for c across string
        while (char= c #\Space)
        count t))

(defun quote-level (string)
  (loop for char across string
        while (member char '(#\Space #\>))
        count (char= char #\>)))

(defun strip (string)
  (string-left-trim '(#\Tab #\Space #\Linefeed #\Return #\>) string))

(defun input->lines (input-stream)
  (loop for line = (read-line input-stream nil)
        while line
        for indent = (indent-level line)
        for quotes = (quote-level line)
        for text = (strip line)
        for blankp = (string= "" text)
        collect (make-line :blankp blankp
                           :quotes quotes
                           :text text
                           :indent indent)))

(defun all (char string)
  (and (not (string= string ""))
       (every (curry #'char= char) string)))

;;; Pass 1

;;; First we have to do Setext-style header to Atx-style header
;;; transform because the header mark has higher precedence

(defun merge-head-line (line1 line2)
  (make-line :indent 0
             :quotes 0
             :text (format nil "~:[#~;##~]~a~a"
                           (char= #\- (char (text line2) 0))
                           (make-string (indent line1)
                                        :initial-element #\Space)
                           (text line1))
             :blankp nil))

(defun setext->atx (list &optional acc)
  (let ((line1 (car list))
        (line2 (cadr list)))
    (if (and line2
             (zerop (indent line2))
             (or (all #\= (text line2))
                 (all #\- (text line2))))
        (let ((acc2 (cons (merge-head-line line1 line2) acc)))
          (setext->atx (cddr list) acc2))
        (if line1
            (setext->atx (cdr list) (cons (car list) acc))
            (reverse acc)))))

;;; Join two lines if no blank lines in between

(defun list-start-p (line)
  (or (member (char (text line) 0) '(#\* #\+ #\-))
      (ordered-list-p (text line))))

(defun ordered-list-p (string)
  (let ((end (position #\. string)))
    (every (lambda (char)
             (member char '(#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\0)))
           (subseq string 0 end))))

(defun join-able-p (line1 line2)
  (and (not (blankp line1))
       (not (blankp line2))
       (= (quotes line1) (quotes line2))
       (not (list-start-p line1))))

(defun join (line1 line2)
  (make-line :indent (indent line1)
             :quotes (quotes line1)
             :text (format nil "~a ~a" (text line1)
                           (strip-quote (text line2)))
             :blankp nil))

(defun unwrap-lines (list &optional acc)
  (let ((line1 (car list))
        (line2 (cadr list)))
    (if (and line2
             (join-able-p line1 line2))
        (let ((line12 (join line1 line2)))
          (unwrap-lines (cons line12 (cddr list)) acc))
        (if line1
            (unwrap-lines (cdr list) (cons line1 acc))
            (reverse acc)))))

;;; makdon.lisp ends here

;;; Local Variables:
;;; mode: lisp
;;; End:
