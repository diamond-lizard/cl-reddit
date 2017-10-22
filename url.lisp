;; Copyright (c) 2013, Jason R. Person
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are met:
;;
;; 1. Redistributions of source code must retain the above copyright notice, this
;;    list of conditions and the following disclaimer.
;; 2. Redistributions in binary form must reproduce the above copyright notice,
;;    this list of conditions and the following disclaimer in the documentation
;;    and/or other materials provided with the distribution.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;; DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
;; ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
;; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
;; ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;
;; The views and conclusions contained in the software and documentation are those
;; of the authors and should not be interpreted as representing official policies,
;; either expressed or implied, of the FreeBSD Project.
;;----------------------------------------------------------
;;
;; Function existence compatiblity check result: INCOMPATIBLE
;; Elisp has no in-package function
;;----------------------------------------------------------
(in-package #:cl-reddit)

;; Function existence compatibility check result: COMPATIBLE
;; Elisp has:
;;   defmacro, let, gensym, with-output-to-string, loop,
;;   case, format, char-code, and write-char
;;
;; Function call syntax compatibility check result: MOSTLY COMPATIBLE
;; Three minor concerns here:
;;
;; 1 - The *escape* paramater, since eLisp has no defparamter
;;     (see globals.lisp).
;;
;; 2 - The "(stream)" argument to with-output-to-string has no eLisp
;; equivalent (and also the "stream" argument to format is affected).
;; I expect both "(stream)" and "stream" can be taken out without
;; consequence.
;;
;; 3 - "%~x" in the format spec needs to be converted to "%%%x"
;;----------------------------------------------------------
;;;; URL helper functions ;;;;
(defmacro encode-param (x)
  "Encodes get params"
  (let ((c (gensym)))
    `(with-output-to-string (stream)
       (loop for ,c across ,x collect
             (case ,c
               ((,@*escape*) (format stream "%~x" (char-code ,c)))
               (otherwise (write-char ,c stream)))))))

;; Function existence compatibility check result: INCOMPATIBLE
;; eLisp has:
;;   defun, format, loop, stringp, and with-output-to-string
;; encode-param is a macro in url.lisp
;; eLisp does not have:
;;   string-left-trim
;;
;; Function call syntax compatibility check result: MOSTLY COMPATIBLE
;; "~a" in the format spec needs to be converted to "%s"
;;----------------------------------------------------------
(defun build-get-params (params)
  "Builds get param list from cons list '((param . value) (param2 . value) ...)"
  (string-left-trim "&"
    (with-output-to-string (stream)
      (loop for (p . v) in params do
            (format stream "&~a=~a" (encode-param p) (if (stringp v) (encode-param v) v))))))

;; Function existence compatibility check result: INCOMPATIBLE
;; eLisp has:
;;   defun
;; eLisp does not have:
;;   yason:parse, drakma:http-request
;;
;; Function call syntax compatibility check result: INCOMPATIBLE
;; *user-agent* is a parameter created by defparameter in globals.lisp
;; and eLisp does not have defparameter
;;----------------------------------------------------------
(defun get-json (url &optional (user nil))
  "Gets json data for url with options cookie-jar."
  (yason:parse
    (if (null user)
      (drakma:http-request url :method :get :user-agent *user-agent* :preserve-uri t :want-stream t)
      (drakma:http-request url :method :get :user-agent *user-agent* :cookie-jar (user-cookie user) :preserve-uri t :want-stream t))))

;; Function existence compatibility check result: INCOMPATIBLE
;; eLisp has:
;;   defun
;; eLisp does not have:
;;   drakma:http-request
;;
;; Function call syntax compatibility check result: INCOMPATIBLE
;; *user-agent* is a parameter created by defparameter in globals.lisp
;; and eLisp does not have defparameter
;;----------------------------------------------------------
(defun post-request2 (url cookie-jar params)
  "Send post request to url with params list."
  (drakma:http-request url :method :post :user-agent *user-agent* :parameters params :cookie-jar cookie-jar :want-stream t))

;; Function existence compatibility check result: INCOMPATIBLE
;; eLisp has:
;;   defun
;; with-user is a macro in util.lisp
;;
;; eLisp does not have:
;;   drakma:http-request
;;
;; Function call syntax compatibility check result: INCOMPATIBLE
;; *user-agent* is a parameter created by defparameter in globals.lisp
;; and eLisp does not have defparameter
;;----------------------------------------------------------
(defun post-request (url user params)
  "Send post request to url with params list."
  (with-user (user)
    (drakma:http-request url :method :post :user-agent *user-agent* :parameters params :cookie-jar (user-cookie user) :want-stream t)))
