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

;;;; Helper functions ;;;;
;;----------------------------------------------------------
;; Function existence compatibility check result: COMPATIBLE
;; Elisp has: defun and make-instance.
;;
;; Function call syntax compatibility check result: INCOMPATIBLE
;; eLisp has no &key keyword, and this needs to change to use
;; cl-parsing-keywords and cl-keys
;;----------------------------------------------------------
(defun make-user (&key username password)
  "Make an instance of user class with username and password"
  (make-instance 'user :username username :password password))

;; Function existence compatibility check result: MOSTLY COMPATIBLE
;; eLisp has: cdr, defun, intern, let, push, and values
;; eLisp does not have string-upcase, but upcase could be used instead
;;
;; Function call syntax compatibility check result: COMPATIBLE
;; all functions are called in ways and with argumets that
;; are completely eLisp compatible
;;----------------------------------------------------------
(defun format-key-args (args)
  "Format a list of key arguments"
  (let ((params))
    (loop for arg in (cdr args) do
          (push arg params)
          (push (values (intern (string-upcase `,arg) "KEYWORD")) params))
    params))

;; Function existence compatibility check result: MOSTLY COMPATIBLE
;; eLisp has: case and defun
;; eLisp does not have string-downcase, but downcase could be used instead
;;
;; Function call syntax compatibility check result: COMPATIBLE
;; all functions are called in ways and with argumets that
;; are completely eLisp compatible
;;----------------------------------------------------------
(defun symbol-string(s)
  "Convert the input symbol to the correct string for api call."
  (case s
    ('up "1")
    ('down "-1")
    ('unvote "0")
    (otherwise (string-downcase (symbol-name s)))))

;;;; Helper macros ;;;;
;;----------------------------------------------------------
;; Function existence compatibility check result: INCOMPATIBLE
;; eLisp has:
;;   defmacro,
;;   gensym,
;;   if,
;;   let
;;   let*,
;;   loop,
;;   make-instance
;;   not,
;;   progn,
;;   setf,
;;   string=,
;;   when,
;; user-modhash and user-cookie are definied in datatypes.lisp
;;
;; eLisp does NOT have:
;;   drakma:cookie-jar-cookies
;;   drakma:cookie-name
;;   drakma:cookie-path
;;   drakma:http-request
;;   flexi-streams:flexi-stream-external-format
;;   yason:parse
;;
;; Function call syntax compatibility check result: INCOMPATIBLE
;; eLisp has no &body keyword, but &rest can be used instead.
;;
;; It is not clear why "(usr)" is used in the defmacro arg list
;; instead of just "usr", or how compatible that is with eLisp.
;;----------------------------------------------------------
(defmacro with-user ((usr) &body body)
  "Does 'body' with logged-in user usr.  Logins in user if not logged-in."
  (let ((json (gensym)) (result (gensym)) (cks (gensym)))
    `(if (null (user-modhash ,usr))
       (progn
         (let* ((,cks (make-instance 'drakma:cookie-jar))
                (,result (drakma:http-request "http://www.reddit.com/api/login.json"
                                              :method :post
                                              :parameters `(("passwd" . ,(user-password ,usr))
                                                            ("user" . ,(user-username ,usr))
                                                            ("api_type" . "json"))
                                              :cookie-jar ,cks
                                              :want-stream t)))
           (setf (flexi-streams:flexi-stream-external-format ,result) :utf-8)
           (let ((,json  (gethash "json" (yason:parse ,result))))
             (when (not (gethash "errors" ,json))
               (loop for ck in (drakma:cookie-jar-cookies ,cks)
                     do (if (string= "reddit_session" (drakma:cookie-name ck))
                          (setf (drakma:cookie-path ck) "/")))
               (setf (user-modhash ,usr) (gethash "modhash" (gethash "data",json)))
               (setf (user-cookie ,usr) ,cks)
               ,@body))))
       ,@body)))

;;----------------------------------------------------------
;; Function existence compatibility check result: COMPATIBLE
;; eLisp has:
;;   append,
;;   butlast,
;;   car,
;;   defmacro,
;;   if,
;;   last,
;;   list,
;;   listp,
;;   progn
;; with-user is defined in util.lisp
;;
;; Function call syntax compatibility check result: COMPATIBLE
;; all functions are called in ways and with argumets that
;; are completely eLisp compatible
;;----------------------------------------------------------
(defmacro if-user-with (user then)
  `(if ,user
     (with-user (,user)
       ,(if (listp (car (last then)))
          `(progn ,(append (butlast then) (list (append (car (last then)) (list user)))))
          `(,@then ,user)))
     (,@then)))

;;----------------------------------------------------------
;; Function existence compatibility check result: COMPATIBLE
;; eLisp has:
;;   defmcro
;;   let
;;   when
;;   gensym
;;   push
;;   if
;; post-request is defined in url.lisp
;; symbol-string is definied in util.lisp
;;
;; Function call syntax compatibility check result: INCOMPATIBLE
;; eLisp has no &key keyword, and this needs to change to use
;; cl-parsing-keywords and cl-keys
;;----------------------------------------------------------
(defmacro api-post-generic (url user &key subreddit action id thing-id text vote spam flair-enabled sr kind title)
  "Defines generic post request"
  (let ((params (gensym)) (result (gensym)))
    `(let ((,params nil))
       ,(when subreddit `(push `("sr_name" . ,subreddit) ,params))
       ,(when sr `(push `("sr" . ,sr) ,params))
       ,(when kind `(push `("kind" . ,kind) ,params))
       ,(when title `(push `("title" . ,title) ,params))
       ,(when action `(push `("action" . ,(symbol-string ,action)) ,params))
       ,(when id `(push `("id" . ,id) ,params))
       ,(when thing-id `(push `("thing_id" . ,thing-id) ,params))
       ,(when text `(push `("text" . ,text) ,params))
       ,(when vote `(push `("dir" . ,(symbol-string ,vote)) ,params))
       ,(when spam `(push `("spam" . ,(if ,spam "1" "0")) ,params))
       ,(when flair-enabled `(push `("flair_enabled" . ,(if ,flair-enabled "1" "0")) ,params))
       (push `("api_type" . "json") ,params)
       (push `("uh" . ,(user-modhash ,user)) ,params)
       (post-request ,url ,user ,params))))

;;----------------------------------------------------------
;; Function existence compatibility check result: INCOMPATIBLE
;; eLisp has:
;;   defmacro
;;   defun
;;   format
;;   intern
;;
;; api-post-generic is defined in util.lisp
;; user is defined in datatypes.lisp
;;
;; eLisp does not have string-downcase, but downcase could be used instead
;;
;; Function call syntax compatibility check result: INCOMPATIBLE
;; eLisp's format needs to be passed the "%s" format specifier
;;  instead of "~a"
;; *reddit* is a paramter created in globals.lisp using defparamter,
;;  which eLisp does not have
;;----------------------------------------------------------
(defmacro def-post-api (api &rest args)
  "Defines an api call."
  `(defun ,(intern (format nil "API-~S" `,api)) (user ,@args)
     (api-post-generic ,(format nil "~a/api/~a.json" *reddit* (string-downcase api)) user ,@(format-key-args args))))

;;----------------------------------------------------------
;; Function existence compatibility check result: INCOMPATIBLE
;; eLisp has:
;;   caar
;;   car
;;   defmacro
;;   let
;;   loop
;;   progn
;;   push
;;   typecase
;;   when
;;
;; eLisp does NOT have string-downcase, but downcase could be used instead
;;
;; flet is an obsolete macro in eLisp, and cl-flet or cl-letf should
;;   be used instead
;;
;; Function call syntax compatibility check result: COMPATIBLE
;; all functions are called in ways and with argumets that
;; are completely eLisp compatible
;;----------------------------------------------------------
(defmacro param-push (&rest commands)
  "Used in cl-reddit.lisp when defining get-* fns.
   Example expansion:
   (param-push hello a \"this'll be in place of a\")
   =>
  (PROGN
    (WHEN A (PUSH `(\"this'll be in place of a\" ,@A) PARAMS))
    (WHEN HELLO (PUSH `(\"hello\" ,@HELLO) PARAMS)))"
  (flet ((process-commands (commands)
           (let (stack)
             (loop for c in commands do
                  (typecase c
                    (symbol (push `(,c ,(string-downcase c)) stack))
                    (string (setf (car stack) `(,(caar stack) ,c)))))
             stack)))
    `(progn
       ,@(loop for c in (process-commands commands)
            collect
              `(when ,(car c)
                 (push `(,',(cadr c) . ,,(car c)) params))))))
