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
   (text :initarg :text
         :initform ""
         :accessor text)
   (blankp :initarg :blankp
           :initform t
           :accessor blankp)))

(defun make-line (&key indent text blankp)
  (make-instance 'line :indent indent
                       :text text
                       :blankp blankp))

(defmethod print-object ((object line) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "indent: ~a text: ~s blankp: ~a"
            (indent object)
            (text object)
            (blankp object))))

;;; Utility

(defun calc-indent (string)
  (loop for c across string
        while (char= c #\Space)
        count t))

(defun strip (string)
  (string-trim '(#\Tab #\Space #\Linefeed #\Return) string))

(defun input->enum (input-stream)
  (make-enum
   (loop for line = (read-line input-stream nil)
         while line
         for indent = (calc-indent line)
         for text = (strip line)
         for blankp = (string= "" text)
         collect (make-line :blankp blankp
                            :text text
                            :indent indent))))

(defun all (char string)
  (and (not (string= string ""))
       (every (curry #'char= char) string)))

;;; Pass 1

;;; First we have to do Setext-style header to Atx-style header
;;; transform because the header mark has higher precedence

(defun setext->atx (enum &optional (acc (make-enum)))
  (if (enum-peek enum)
      (let ((line2 (enum-peek2 enum)))
        (if (and line2
                 (zerop (indent line2))
                 (or (all #\= (text line2))
                     (all #\- (text line2))))
            (let ((line (enum-peek enum)))
              (enum-junk enum)
              (enum-junk enum)
              (enum-push acc
                         (make-line :indent 0
                                    :text (format nil "~:[#~;##~]~a~a"
                                                  (char= #\= (char (text line2) 0))
                                                  (make-string (indent line)
                                                               :initial-element #\Space)
                                                  (text line))
                                    :blankp nil))
              (setext->atx enum acc))
            (progn
              (enum-push acc (enum-junk enum))
              (setext->atx enum acc))))
      acc))

;;; makdon.lisp ends here

;;; Local Variables:
;;; mode: lisp
;;; End:
