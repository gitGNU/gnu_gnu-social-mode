;;; bbdb-gnu-social.el --- Integrate BBDB and gnu-social-mode

;; This file is part of gnu-social-mode.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;; Features that might be required by this library:
;;
;;   BBDB
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This should work in BBDB V.3.x for now...
;; It is in heavy, really heavy, development.
;;
;; As far I tried, I couldn't make it work in the way proposed by BBDB. 
;; I couldn't find any documentation of how to use the MUA API.
;; For now, I will use every possible command despite it is not desirable
;; for BBDB developers(I think :-S ).
;; 
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(require 'bbdb)
(require 'bbdb-com)
(require 'bbdb-mua)
(require 'gnu-social-mode)
(require 'gnu-social-friends)

                                        ; GNU-Social-friends-buffer
;; GNU-Social friends buffer must have a way to introduce people into BBDB.
;; There's need of creating a new field into a record. This field will be called "gnu-social".

;; We'll define a ':' key for introducing a new record into BBDB or updating a record.

(defcustom bbdb/gnu-social-update-records-p
  (lambda ()
    (let ((bbdb-update-records-p 'query ))
      (bbdb-select-message)))
  "How `bbdb-mua-update-records' processes mail addresses in GNU Social and GNU-Social-friends.
Allowed values are:
 nil          Do nothing.
 search       Search for existing records.
 query        Update existing records or query for creating new ones.
 create or t  Update existing records or create new ones.
A function which returns one of the above values."
  :group 'bbdb-mua-gnu-social
  :type '(choice (const :tag "do nothing" nil)
                 (const :tag "search for existing records"
                        (lambda () (let ((bbdb-update-records-p 'search))
                                     (bbdb-select-message))))
                 (const :tag "query annotation of all messages"
                        (lambda () (let ((bbdb-update-records-p 'query))
                                     (bbdb-select-message))))
                 ;; (const :tag "annotate (query) only new messages"
                 ;;        (lambda ()
                 ;;          (let ((bbdb-update-records-p
                 ;;                 (if (bbdb/rmail-new-flag) 'query 'search)))
                 ;;            (bbdb-select-message))))
                 (const :tag "annotate all messages"
                        (lambda () (let ((bbdb-update-records-p 'create))
                                     (bbdb-select-message))))
                 (const :tag "accept messages" bbdb-accept-message)
                 (const :tag "ignore messages" bbdb-ignore-message)
                 (const :tag "select messages" bbdb-select-message)
                 (sexp  :tag "user defined function")))

;; (defun bbdb/gnu-social-header (header)
;;   ""
;; )
                                        ; --------------------
                                        ; Insinuation
                                        ; --------------------

;; It doesn't work :-( Is still under development.
;; 
;; ;;;###autoload
;; (defun bbdb-insinuate-gnu-social ()
;;   "Add every keymap and hooks necesary for using BBDB into `gnu-social-friends-mode'.
;; You shouldn't call this in your init file, instead use `bbdb-initialize'"
;;   (define-key gnu-social-friends-mode-map ":" 'bbdb-mua-display-sender)
;;   (define-key gnu-social-friends-mode-map ";" 'bbdb-mua-edit-notes-sender)
;; )



;; ;; We have to make bbdb-mua recognice gnu-social-friends-mode, if not it will fall-back with error.
;; (defadvice bbdb-mua (before gnu-social-bbdb-mua ())
;;   "This advice add into `bbdb-mua' the necessary for BBDB to recognice gnu-social-friends-mode, and gnu-social-mode."
;;   (if (member major-mode '(gnu-social-mode gnu-social-friends-mode))
;;       'gnu-social)
;;   )

;; Activate gnu-social-bbdb-mua advice
;; (ad-activate 'bbdb-mua)
;; (ad-deactivate 'bbdb-mua)


					; ____________________

(defun bbdb-gnu-social-friends-next-usr ()
  "This function is supposed to be used as a hook to the function `gnu-social-friends-next-user'.
Check if the actual user is in BBDB. If not, add it *without query the user*. 

Remember: 
Adding into the BBDB means: 
1) to create a new BBDB record with the same name of the gnu-social user name(NOT NICK!)
2) Add the nick into a new field called \"gnu-social\"."
  (setq usr (gnu-social-friends-get-current-user))
  ;; Our idea is to show the user if founded...
  ;; Search for the *first mach* in the BBDB:
  (setq record 
	(let ((usr-name (nth 1 usr)))
	  (car (bbdb-search (bbdb-records) usr-name))
	  )
	)
 
  ;; check if exist, if not add it(or query to add it).
  (if record 
      (progn 
	(bbdb-display-records (cons record '()))
	(unless (bbdb-gnu-social-check-record record usr)
	  ;; It has to be updated!
	  (bbdb-gnu-social-query-update-record record usr)
	  (bbdb-display-records (cons record '()))
	  )
	)
    (progn
      ;; No record available... query to add it..
      (bbdb-gnu-social-query-add-record record usr)
      ;; Show new record...
      (setq record 
	    (let ((usr-name (nth 1 usr)))
	      (car (bbdb-search (bbdb-records) usr-name))
	      )
	    )
      (when record
	(bbdb-display-records (cons record '()))
	)
      )
    )
  )

(defun bbdb-gnu-social-query-update-record (record usr)
  "Query the user if she/he wants to update the BBDB record.
If she/he answer \"yes\", update it.
If she/he answer \"no\", do nothing."
  (when (bbdb-gnu-social-prompt-yepnop "Do you want to update this record?(y/n)")
    (bbdb-gnu-social-update-record record usr)
    )				     
)

(defun bbdb-gnu-social-update-record (record usr)
  "Update the record usr with new values:
1) Update the \"gnu-social\" field.
2) No need to update anything else..."
  (bbdb-record-set-note record 'gnu-social (nth 0 usr))
  )

(defun bbdb-gnu-social-prompt-yepnop (prompt)
  "Ask a question to the user for a yes-no answer.
Return t when user answer yes.
Return nil when user answer no."
  (let (
	(yepnop (read-char prompt)))
    (cond
     ((eq ?y yepnop)
      t)
     ((eq ?n yepnop)
      nil)
     (t 
      (message "Please, answer 'y' or 'n'.")
      (bbdb-gnu-social-prompt-yepnop prompt))
     )
    )
  )


(defun bbdb-gnu-social-query-add-record (record usr)
  "Query the user if she/he wants to add this gnu-social user into BBDB.
If she/he answer \"yes\", add it.
If she/he answer \"no\", don't add it of course."
  (when (bbdb-gnu-social-prompt-yepnop "Do you want to add this user and gnu-social nick?(y/n)")
    (bbdb-gnu-social-add-record usr)
    )	     
  )

(defun bbdb-gnu-social-add-record (usr)
  "Add friend/follower into BBDB."  
  (bbdb-create-internal 
   (nth 1 usr) ;;name
   nil ;; affix
   nil ;; aka
   nil ;; organizations
   nil ;; mail
   nil ;; phones
   nil ;; addresses
   (cons 
    (cons 'gnu-social (nth 0 usr))
    '()
    ) ;; notes
   )
  )

(defun bbdb-gnu-social-check-record (record usr)
  "Check if the record has the same value in the \"gnu-social\" field and the name field.
If it is the same return t.
If it is different return nil.
If the \"gnu-social\" field does not exists return nil(it means it has different value).
"
  ;; Get if exists the field 
  (if (and 
       record
       usr)
      (string= 
       (bbdb-record-note record 'gnu-social)
       (car usr)
       )
    nil
    )
  )

(defun bbdb-gnu-social-ask-for-save ()
  "This is intended when the user wants to quit gnu-social.
As soon he/she wants to quit, is necessary to ask if she/he wants to update BBDB database."
  (bbdb-save t t)
  )

(eval-and-compile
  (add-hook 'gnu-social-friends-good-bye-hooks 'bbdb-gnu-social-ask-for-save)
  (add-hook 'gnu-social-friends-next-user-hooks 'bbdb-gnu-social-friends-next-usr)
  (add-hook 'gnu-social-friends-prev-user-hooks 'bbdb-gnu-social-friends-next-usr)
  )

(defun bbdb-gnu-social-next-usr ()
  "Go to next gnu-social user in the gnu-social buffer, find its BBDB record and show it if exists."
  (interactive)
  (save-excursion
    (goto-char (bbdb-gnu-social-find-next-usr-pos))
    ;; Get user nick
    (save-excursion
      (search-forward-regexp "[^[:blank:]]+" nil t)
      (setq usrnick (match-string-no-properties 0))
      )
    ;; Remove the '@'
    (when (string= "@" (substring usrnick 0 1))
      ;;Has a '@', take it out.
      (setq usrnick (substring usrnick 0 1))
      )
    ;; Remove the ','
    (when (string= "," (substring usrnick -1))
      (setq usrnick (substring usrnick 0 -1))
      )  
    
    ;; Find usrnick in the BBDB
    (bbdb-search-notes "gnu-social" usrnick)
    )
  )


(defun bbdb-gnu-social-find-next-usr-1-pos ()
  "Find the next gnu-social nick starting with '@'."
  (with-current-buffer gnu-social-buffer
    (save-excursion
      (search-forward-regexp "@[^[:blank:]]*" nil t)
      (match-beginning 0)
      )
    )
  )

(defun bbdb-gnu-social-find-next-usr-2-pos ()
  "Find the next gnu-social nick as the first element that appear of a status. For example:

_
 rms,  10:26  septiembre 26, 2011:
  hola, esto es un estado // from web [algúnlado] in reply to someone

in this case the return value is 'rms'."
  (with-current-buffer gnu-social-buffer
    (gnu-social-get-next-username-face-pos (point))     
    )
  )

(defun bbdb-gnu-social-find-next-usr-pos ()
  "Return the position of the first gnu-social nick after the current point, no matters if it is a '@user' form or just
the name of the status's remitent."
  (let ((usr1 (bbdb-gnu-social-find-next-usr-1-pos))
	(usr2 (bbdb-gnu-social-find-next-usr-2-pos))
	)
    ;; Look wich one is first, and return that one
    (if (< usr1 usr2)
	usr1
      usr2
      )
    )
  )

(defun bbdb-gnu-social-down-key ()
  "Go to down, and then show the next possible nick BBDB record."
  (interactive)
  (next-line)
  (bbdb-gnu-social-next-usr)
  )

(defun bbdb-gnu-social-up-key ()
  "Go to up and then show the next possible nick BBDB record."
  (interactive)
  (previous-line)
  (bbdb-gnu-social-next-usr)
  )

;; I see that this could be a bit destructive.
;; If down or up key are setted to other functions, this will make gnu-social to ignore them!

(eval-and-compile 
  ;; If you want, at every position, to search for BBDB uncomment this:
  ;;(define-key gnu-social-mode-map [down] 'bbdb-gnu-social-down-key)
  ;;(define-key gnu-social-mode-map [up] 'bbdb-gnu-social-up-key)
  )

;; This is better: at each j and k key(gnu-social-goto-next-status) search for its BBDB record.
(defadvice gnu-social-goto-next-status (after bbdb-gnu-social-next-status)
  "Search for BBDB record of the next nearest nick."
  (save-excursion
    (backward-char)
    (bbdb-gnu-social-next-usr)
    )
  )

(defadvice gnu-social-goto-previous-status (after bbdb-gnu-social-next-status)
  "Search for BBDB record of the next nearest nick."
  (save-excursion
    (backward-char)
    (bbdb-gnu-social-next-usr)
    )
  )

(eval-and-compile
  (ad-activate 'gnu-social-goto-next-status)
  (ad-activate 'gnu-social-goto-previous-status)
  )

(provide 'bbdb-gnu-social)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; bbdb-gnu-social.el ends here
