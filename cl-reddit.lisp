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
;;
;; Function existence compatiblity check result: INCOMPATIBLE
;; Elisp has no in-package function
(in-package #:cl-reddit)

;;;; API ;;;;
;; Function existence compatibility check result: COMPATIBLE
;; Elisp has: defun and let.
;; with-user is defined in util.lisp
;;
;; Function call syntax compatibility check result: INCOMPATIBLE
;; eLisp has no &key keyword, and this needs to change to use
;; cl-parsing-keywords and cl-keys
;;----------------------------------------------------------
(defun api-login (&key username password)
  "Login user username with password. Returns a User object with modhash,cookie set."
  (let ((usr (make-user :username username :password password)))
    (with-user (usr) usr)))

;; Function existence compatibility check result: COMPATIBLE
;; def-post-api is defined in util.
;;
;; Function call syntax compatibility check result: INCOMPATIBLE
;; eLisp has no &key keyword, and this needs to change to use
;; cl-parsing-keywords and cl-keys
;;----------------------------------------------------------
; "Sub or unsub from subreddit sr for user usr. Action can be 'sub or 'unsub"
(def-post-api subscribe &key subreddit action)

;; Function existence compatibility check result: COMPATIBLE
;; def-post-api is defined in util.
;;
;; Function call syntax compatibility check result: INCOMPATIBLE
;; eLisp has no &key keyword, and this needs to change to use
;; cl-parsing-keywords and cl-keys
;;----------------------------------------------------------
; "Comments text on id with user."
(def-post-api comment &key thing-id text)

;; Function existence compatibility check result: COMPATIBLE
;; def-post-api is defined in util.
;;
;; Function call syntax compatibility check result: INCOMPATIBLE
;; eLisp has no &key keyword, and this needs to change to use
;; cl-parsing-keywords and cl-keys
;;----------------------------------------------------------
; "Edit user text on id with user."
(def-post-api editusertext &key thing-id text)

;; Function existence compatibility check result: COMPATIBLE
;; def-post-api is defined in util.
;;
;; Function call syntax compatibility check result: INCOMPATIBLE
;; eLisp has no &key keyword, and this needs to change to use
;; cl-parsing-keywords and cl-keys
;;----------------------------------------------------------
; "Vote direction dir for thing with id with user."
(def-post-api vote &key id vote)

;; Function existence compatibility check result: COMPATIBLE
;; def-post-api is defined in util.
;;
;; Function call syntax compatibility check result: INCOMPATIBLE
;; eLisp has no &key keyword, and this needs to change to use
;; cl-parsing-keywords and cl-keys
;;----------------------------------------------------------
; "Save thing with id."
(def-post-api save &key id)

;; Function existence compatibility check result: COMPATIBLE
;; def-post-api is defined in util.
;;
;; Function call syntax compatibility check result: INCOMPATIBLE
;; eLisp has no &key keyword, and this needs to change to use
;; cl-parsing-keywords and cl-keys
;;----------------------------------------------------------
; "Unsave thing with id."
(def-post-api unsave &key id)

;; Function existence compatibility check result: COMPATIBLE
;; def-post-api is defined in util.
;;
;; Function call syntax compatibility check result: INCOMPATIBLE
;; eLisp has no &key keyword, and this needs to change to use
;; cl-parsing-keywords and cl-keys
;;----------------------------------------------------------
; "Report thing with id."
(def-post-api report &key id)

;; Function existence compatibility check result: COMPATIBLE
;; def-post-api is defined in util.
;;
;; Function call syntax compatibility check result: INCOMPATIBLE
;; eLisp has no &key keyword, and this needs to change to use
;; cl-parsing-keywords and cl-keys
;;----------------------------------------------------------
; "Mark thing with id as nsfw."
(def-post-api marknsfw &key id)

;; Function existence compatibility check result: COMPATIBLE
;; def-post-api is defined in util.
;;
;; Function call syntax compatibility check result: INCOMPATIBLE
;; eLisp has no &key keyword, and this needs to change to use
;; cl-parsing-keywords and cl-keys
;;----------------------------------------------------------
; Unmark thing with id
(def-post-api unmarknsfw &key id)

;; Function existence compatibility check result: COMPATIBLE
;; def-post-api is defined in util.
;;
;; Function call syntax compatibility check result: INCOMPATIBLE
;; eLisp has no &key keyword, and this needs to change to use
;; cl-parsing-keywords and cl-keys
;;----------------------------------------------------------
; "Hide thing with id."
(def-post-api hide &key id)

;; Function existence compatibility check result: COMPATIBLE
;; def-post-api is defined in util.
;;
;; Function call syntax compatibility check result: INCOMPATIBLE
;; eLisp has no &key keyword, and this needs to change to use
;; cl-parsing-keywords and cl-keys
;;----------------------------------------------------------
; "Unhide thing with id."
(def-post-api unhide &key id)

;; Function existence compatibility check result: COMPATIBLE
;; def-post-api is defined in util.
;;
;; Function call syntax compatibility check result: INCOMPATIBLE
;; eLisp has no &key keyword, and this needs to change to use
;; cl-parsing-keywords and cl-keys
;;----------------------------------------------------------
; "Delete thing with id."
(def-post-api del &key id)

;; Function existence compatibility check result: COMPATIBLE
;; def-post-api is defined in util.
;;
;; Function call syntax compatibility check result: INCOMPATIBLE
;; eLisp has no &key keyword, and this needs to change to use
;; cl-parsing-keywords and cl-keys
;;----------------------------------------------------------
; "Block thing with id."
(def-post-api block &key id)

;; Function existence compatibility check result: COMPATIBLE
;; def-post-api is defined in util.
;;
;; Function call syntax compatibility check result: INCOMPATIBLE
;; eLisp has no &key keyword, and this needs to change to use
;; cl-parsing-keywords and cl-keys
;;----------------------------------------------------------
; "Read message with id."
(def-post-api read_message &key id)

;; Function existence compatibility check result: COMPATIBLE
;; def-post-api is defined in util.
;;
;; Function call syntax compatibility check result: INCOMPATIBLE
;; eLisp has no &key keyword, and this needs to change to use
;; cl-parsing-keywords and cl-keys
;;----------------------------------------------------------
; "Unread message with id."
(def-post-api unread_message &key id)

;; Function existence compatibility check result: COMPATIBLE
;; def-post-api is defined in util.
;;
;;----------------------------------------------------------
; "Approve thing with id."
(def-post-api approve &key id)

;; Function existence compatibility check result: COMPATIBLE
;; def-post-api is defined in util.
;;
;; Function call syntax compatibility check result: INCOMPATIBLE
;; eLisp has no &key keyword, and this needs to change to use
;; cl-parsing-keywords and cl-keys
;;----------------------------------------------------------
; "Self removal as moderator of thing with id."
(def-post-api leavecontributor &key id)

;; Function existence compatibility check result: COMPATIBLE
;; def-post-api is defined in util.
;;
;; Function call syntax compatibility check result: INCOMPATIBLE
;; eLisp has no &key keyword, and this needs to change to use
;; cl-parsing-keywords and cl-keys
;;----------------------------------------------------------
; "Remove as moderator of subreddit with id."
(def-post-api leavemoderator &key id)

;; Function existence compatibility check result: COMPATIBLE
;; def-post-api is defined in util.
;;
;; Function call syntax compatibility check result: INCOMPATIBLE
;; eLisp has no &key keyword, and this needs to change to use
;; cl-parsing-keywords and cl-keys
;;----------------------------------------------------------
; "Remove thing with id. Is-spam t if spam, nil if not."
(def-post-api remove &key id spam)

;; Function existence compatibility check result: COMPATIBLE
;; def-post-api is defined in util.
;;
;; Function call syntax compatibility check result: INCOMPATIBLE
;; eLisp has no &key keyword, and this needs to change to use
;; cl-parsing-keywords and cl-keys
;;----------------------------------------------------------
; "Enable/disable flair."
(def-post-api setflairenabled &key flair-enabled)

;; Function existence compatibility check result: COMPATIBLE
;; def-post-api is defined in util.
;;
;; Function call syntax compatibility check result: INCOMPATIBLE
;; eLisp has no &key keyword, and this needs to change to use
;; cl-parsing-keywords and cl-keys
;;----------------------------------------------------------
"Submit link to subreddit sr with text and title, only works if user has enough karma to avoid captcha."
(def-post-api submit &key sr kind text title)

;; Function existence compatibility check result: COMPATIBLE
;; Elisp has: defun, let, and format.
;; with-user is defined in util.lisp
;; get-json is defined in url.lisp
;;
;; Function call syntax compatibility check result: INCOMPATIBLE
;; eLisp format function does not understand "~a".  Instead
;; we should probably use "%s"
;;----------------------------------------------------------
(defun api-me (user)
  "Get info for user usr.  Returns user data."
  (let ((url (format nil "~a/api/me.json" *reddit*)))
    (with-user (user) (get-json url user))))

;; Function existence compatibility check result: COMPATIBLE
;; Elisp has: defun, let, and format.
;; NOTE: cl-defun should be used instead of defun, to set defaults values for optional arguments
;; if-user-with is defined in util.lisp
;; parse-json is defined in datatypes.lisp
;; get-json in url.lisp
;;
;; Function call syntax compatibility check result: INCOMPATIBLE
;; eLisp format function does not understand "~a".  Instead
;; we should probably use "%s"
;;----------------------------------------------------------
(defun get-user (r-user &optional (user nil))
  "Get /user/<r-user>.json.  Optional user user."
  (let ((url (format nil "~a/user/~a.json" *reddit* r-user)))
    (if-user-with user
      (parse-json (get-json url)) )))

;; Function existence compatibility check result: COMPATIBLE
;; Elisp has: defun, let, and format.
;; NOTE: cl-defun should be used instead of defun, to set defaults values for optional arguments
;; if-user-with is defined in util.lisp
;; parse-json is defined in datatypes.lisp
;; get-json in url.lisp
;;
;; Function call syntax compatibility check result: INCOMPATIBLE
;; eLisp format function does not understand "~a".  Instead
;; we should probably use "%s"
;;----------------------------------------------------------
(defun get-about-user (about-user &optional (user nil))
  "Get /user/<about-user>/about.json.  Optional user user."
  (let ((url (format nil "~a/user/~a/about.json" *reddit* about-user)))
    (if-user-with user
       (parse-json (get-json url)))))

;; Function existence compatibility check result: COMPATIBLE
;; Elisp has: defun, let, and format.
;; with-user and symbol-string are defined in util.lisp
;; parse-json is defined in datatypes.lisp
;; get-json in url.lisp
;;----------------------------------------------------------
(defun get-message (user where)
  "Gets messages from inbox for user user.
   where can be one of 'inbox 'unread 'sent
   "
  (let ((url (format nil "~a/message/~a.json" *reddit* (symbol-string where))))
    (with-user (user)
      (parse-json (get-json url user)))))

;; Function existence compatibility check result: COMPATIBLE
;; Elisp has: defun, let, and format.
;; build-get-params is defined in util.lisp
;; parse-json is defined in datatypes.lisp
;; get-json in url.lisp
;;----------------------------------------------------------
(defun get-username-available (username)
  "Check if a username is available."
  (let ((url (format nil "~a/api/username_available.json~a"
                     *reddit* (build-get-params `(("u" . ,username))))))
    (parse-json (get-json url))))

;; Function existence compatibility check result: COMPATIBLE
;; Elisp has: defun, let, and format.
;; NOTE: cl-defun should be used instead of defun, to set defaults values for optional arguments
;; if-user-with is defined in util.lisp
;; parse-json and listing-children are defined in datatypes.lisp
;; get-json in url.lisp
;;----------------------------------------------------------
;;Listings
(defun get-reddit (&optional (user nil))
  "Gets json data for reddit home page. Optional user."
  (let ((url (format nil "~a/.json" *reddit*)))
    (listing-children
      (if-user-with user
        (parse-json (get-json url))))))

;; Function existence compatibility check result: COMPATIBLE
;; Elisp has: defun, let, and format.
;; if-user-with is defined in util.lisp
;; parse-json is defined in datatypes.lisp
;; get-json in url.lisp
;;----------------------------------------------------------
(defun %get-subreddit (sub user)
  (let ((url (format nil "~a/r/~a.json" *reddit* sub)))
    (if-user-with user (parse-json (get-json url)))))

;; Function existence compatibility check result: COMPATIBLE
;; Elisp has: defun.
;; NOTE: cl-defun should be used instead of defun, to set defaults values for optional arguments
;; if-user-with is defined in util.lisp
;; listing-children is defined in datatypes.lisp
;; %get-subreddit defined in cl-reddit.lisp
;;----------------------------------------------------------
(defun get-subreddit (sub &optional (user nil))
  "Gets json data for subreddit sub.  Optional user."
  (listing-children (%get-subreddit sub user)))

;; Function existence compatibility check result: COMPATIBLE
;; Elisp has: defun and format.
;; get-subreddit defined in cl-reddit.lisp
;;----------------------------------------------------------
(defun get-subreddit-new (sub &optional user)
  "Gets json data for /r/<sub>/new. Optional user."
  (get-subreddit (format nil "~a/new" sub) user))

;; Function existence compatibility check result: COMPATIBLE
;; Elisp has: defun and format.
;; get-subreddit defined in cl-reddit.lisp
;;----------------------------------------------------------
(defun get-subreddit-top (sub &optional user)
  "Gets json data for top posts in subreddit sub. Optional user usr."
  (get-subreddit (format nil "~a/top" sub) user))

;; Function existence compatibility check result: COMPATIBLE
;; Elisp has: defun and format.
;; %get-subreddit defined in cl-reddit.lisp
;;----------------------------------------------------------
(defun get-subreddit-about (sub &optional user)
  "Gets r/<sub>/about.json. Returns Subreddit object about sub. Optional user usr."
  (%get-subreddit (format nil "~a/about" sub) user))

;; Function existence compatibility check result: COMPATIBLE
;; Elisp has: defun, let, and format.
;; with-user is defined in util.lisp
;; listing-children is defined in datatypes.lisp
;; get-json in url.lisp
;;----------------------------------------------------------
(defun get-subscribed (user)
  "Gets subscribed subreddits"
  (let ((url (format nil "~a/reddits/mine.json" *reddit*)))
    (with-user (user)
      (listing-children
        (parse-json (get-json url user))))))

;; Function existence compatibility check result: COMPATIBLE
;; Elisp has: defun, let, and format.
;; with-user and param-push, and symbol-string are defined in util.lisp
;; listing-children and parse-json are defined in datatypes.lisp
;; get-json in url.lisp
;;----------------------------------------------------------
(defun get-reddits-mine (user &key (where 'subscriber) after before count limit show target)
  "Gets listing of subreddits for user.
   where can be one of 'subscriber 'moderator 'contributorfor."
  (let ((url (format nil "~a/reddits/mine/~a.json" *reddit* (symbol-string where)))
        (params))
    (param-push after before count limit show target)
    (with-user (user)
      (listing-children
        (parse-json (get-json url user))))))

;; Function existence compatibility check result: COMPATIBLE
;; Elisp has: defun, let, and format.
;; with-user, param-push, and symbol-string are defined in util.lisp
;; listing-children and parse-json are defined in datatypes.lisp
;; get-json in url.lisp
;;----------------------------------------------------------
(defun get-reddits-where (user &key (where 'new) after before count limit show target)
  "Gets listing of subreddits for user.
   where can be one of 'new 'popular 'banned"
  (let ((url (format nil "~a/reddits/~a.json" *reddit* (symbol-string where)))
        (params))
    (param-push after before count limit show target)
    (with-user (user)
      (listing-children
        (parse-json (get-json url user))))))

;; Function existence compatibility check result: COMPATIBLE
;; Elisp has: defun, let, push, setf, when, and format.
;; if-user-with, param-push, and symbol-string are defined in util.lisp
;; listing-children and parse-json are defined in datatypes.lisp
;; get-json in url.lisp
;;----------------------------------------------------------
(defun get-search (query &key user after before count limit restrict-sr show sort syntax time target sub)
  "Search for query."
  (let ((params)
        (url (if sub (format nil "~a/r/~a/search.json" *reddit* sub) (format nil "~a/search.json" *reddit*))))
    (param-push after before count limit show time target)
    (when restrict-sr (push `("restrict_sr" . "1") params))
    (when sort (push `("sort" . ,(symbol-string sort)) params))
    (push `("q" . ,query) params)
    (when params (setf url (format nil "~a?~a" url (build-get-params params))))
    (listing-children
      (if-user-with user (parse-json (get-json url))))))

;; Function existence compatibility check result: COMPATIBLE
;; Elisp has: defun, let, butlast, second, and format.
;; if-user-with, param-push, and symbol-string are defined in util.lisp
;; listing-children and parse-json are defined in datatypes.lisp
;; get-json and build-get-params in url.lisp
;;----------------------------------------------------------
(defun get-comments (id user &key article comment context depth limit sort)
  "Gets comments for link id in subreddit sr."
  (let ((params nil))
    (param-push sort limit depth context comment article)
    (let ((url (format nil "~a/comments/~a.json?~a" *reddit* id (build-get-params params))))
      (with-user (user)
        (butlast (listing-children (parse-json (second (get-json url user)))))))))
