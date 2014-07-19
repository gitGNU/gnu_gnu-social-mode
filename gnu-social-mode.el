;;; gnu-social-mode.el --- Major mode API client for GNU Social mode

;; This file is part of gnu-social-mode
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; Copyright (C) 2014 Sergio Durigan Junior
;; Copyright (C) 2008-2013 Gabriel Saldana
;; Copyright (C) 2009 Bradley M. Kuhn

;; Author: Gabriel Saldana <gsaldana@gmail.com>
;;         and Sergio Durigan Junior <sergiodj (at) sergiodj (dot) net>
;; Keywords: gnu-social web
;; URL: http://blog.gabrielsaldana.org/gnu-social-mode-for-emacs/
;; Contributors:
;;     Jason McBrayer <jmcbray@carcosa.net> (minor updates for working under Emacs 23)
;;     Alex Schröder <kensanata@gmail.com> (mode map patches)
;;     Christian Cheng (fixed long standing xml parsing bug)
;;     Carlos A. Perilla from denting-mode
;;     Alberto Garcia <agarcia@igalia.com> (integrated patch from twittering-mode for retrieving multiplemethods)
;;     Bradley M. Kuhn <bkuhn@ebb.org> (editing status from edit-buffer rather than minibuffer)
;;     Jason McBrayer <jmcbray@carcosa.net> (replace group tags with hashtags on redents, longlines use)
;;     Sean Neakums (patches of bugs flagged by byte-compiler)
;;     Shyam Karanatt <shyam@swathanthran.in> (several patches and code cleanup, new http backend based on url.el)
;;     Tezcatl Franco <tzk@riseup.net> (ur1.ca support)
;;     Anthony Garcia <lagg@lavabit.com> (fix for icon-mode)
;;     Alexande Oliva <lxoliva@fsfla.org> (fix for icon placement on reverse order dents, bug fixes)
;;     Aidan Gauland <aidalgol@no8wireless.co.nz> (variable scope code cleanup)
;;     Joel J. Adamson <adamsonj@email.unc.edu> Added countdown minibuffer-prompt style
;;     Kevin Granade <kevin.granade@gmail.com> (OAuth support)

;;; Commentary:

;; GNU Social Mode is a major mode to check friends timeline, and update your
;; status on Emacs.

;; gnu-social-mode.el is a major mode for GNU Social.  Based on the twittering mode
;; version 0.6 by Y.  Hayamizu and Tsuyoshi CHO found at
;; <http://hayamin.com/wiliki.cgi?twittering-mode-en&l=en>

;; Requirements
;; if using Emacs22 or previous, you'll need json.el
;; get it from http://edward.oconnor.cx/2006/03/json.el
;; json.el is part of Emacs23
;; To use the OAuth support, you need oauth.el
;; Downloadable from http://github.com/psanford/emacs-oauth/

;; If using Oauth with Emacs earlier than 23.3 you'll also need w3m.

;;; Install:

;; You can use M-x customize-group gnu-social-mode to setup all settings or simply
;; add the following to your .emacs or your prefered customizations file

;; (require 'gnu-social-mode)
;; (setq gnu-social-username "yourusername")

;; If you want to use simple authentication add your password
;; (setq gnu-social-password "yourpassword")

;; It is recommended to create a file ~/.authinfo with your login credentials
;; instead of storing your password in plain text, the file should have the
;; following contents:

;; machine servername login yourusername password yourpassword

;; Replace servername with your server, yourusername and yourpassword
;; with your information. If you setup your authinfo file, you don't
;; need to set gnu-social-password variable anywhere

;; If you want to use OAuth authentication add the following
;; (setq gnu-social-auth-mode "oauth")

;; If you want to post from the minibufer without having gnu-social buffer active, add the following global keybinding.
;; Add this to send status updates
;; (global-set-key "\C-cip" 'gnu-social-update-status-interactive)
;; Add this to send direct messages
;; (global-set-key "\C-cid" 'gnu-social-direct-message-interactive)

;; Start using with M-x gnu-social

;;; Code:

(require 'cl)
(require 'xml)
(require 'parse-time)
(require 'url)
(require 'url-http)
(require 'json)
(require 'image)

(defconst gnu-social-mode-version "1.3.1")

;;url-basepath fix for emacs22
(unless (fboundp 'url-basepath)
  (defalias 'url-basepath 'url-file-directory))

;;workaround for url-unhex-string bug that was fixed in emacs 23.3
(defvar gnu-social-unhex-broken nil
  "Predicate indicating broken-ness of `url-unhex-string'.

If non-nil, indicates that `url-unhex-string' is broken and
must be worked around when using oauth.")

(defgroup gnu-social-mode nil
  "GNU Social Mode for microblogging on Emacs"
  :tag "Microblogging"
;;; TODO: put my link here
;  :link '(url-link http://blog.gabrielsaldana.org/gnu-social-mode-for-emacs/)
  :group 'applications )

(defun gnu-social-mode-version ()
  "Display a message for gnu-social-mode version."
  (interactive)
  (let ((version-string
         (format "gnu-social-mode-v%s" gnu-social-mode-version)))
    (if (interactive-p)
        (message "%s" version-string)
      version-string)))

(defvar gnu-social-mode-map (make-sparse-keymap "GNU-Social"))
(defvar menu-bar-gnu-social-mode-menu nil)
(defvar gnu-social-timer nil "Timer object for timeline refreshing will be stored here.  DO NOT SET VALUE MANUALLY.")

(defvar gnu-social-urlshortening-services-map
  '((tinyurl . "http://tinyurl.com/api-create.php?url=")
    (toly    . "http://to.ly/api.php?longurl=")
    (google . "http://ggl-shortener.appspot.com/?url=")
    (ur1ca . "http://ur1.ca/?longurl=")
    (tighturl . "http://2tu.us/?save=y&url=")
    (isgd . "http://is.gd/create.php?format=simple&url="))
  "Alist of tinyfy services.")

(defvar gnu-social-new-dents-count 0
  "Number of new dents when `gnu-social-new-dents-hook' is run.")

(defvar gnu-social-new-dents-hook nil
  "Hook run when new dents are received.

You can read `gnu-social-new-dents-count' to get the number of new
dents received when this hook is run.")

(defvar gnu-social-display-max-dents nil
  "How many dents to keep on the displayed timeline.

If non-nil, dents over this amount will bre removed.")

;; Menu
(unless menu-bar-gnu-social-mode-menu
  (easy-menu-define
    menu-bar-gnu-social-mode-menu gnu-social-mode-map ""
    '("GNU-Social"
      ["Send an update" gnu-social-update-status-interactive t]
      ["Send a direct message" gnu-social-direct-message-interactive t]
      ["Re-dent someone's update" gnu-social-redent t]
      ["Repeat someone's update" gnu-social-repeat t]
      ["Add as favorite" gnu-social-favorite t]
      ["Follow user" gnu-social-follow]
      ["Unfollow user" gnu-social-unfollow]
      ["--" nil nil]
      ["Friends timeline" gnu-social-friends-timeline t]
      ["Public timeline" gnu-social-public-timeline t]
      ["Replies timeline" gnu-social-replies-timeline t]
      ["User timeline" gnu-social-user-timeline t]
      ["Tag timeline" gnu-social-tag-timeline t]
      ["--" nil nil]
      ;; ["Group timeline" gnu-social-group-timeline t]
      ;; ["Join to this group" gnu-social-group-join t]
      ["Leave this group" gnu-social-group-leave t]
      )))

(defcustom gnu-social-idle-time 20
  "Idle time."
  :type 'integer
  :group 'gnu-social-mode)

(defcustom gnu-social-timer-interval 90
  "Timer interval to refresh the timeline."
  :type 'integer
  :group 'gnu-social-mode)

(defcustom gnu-social-username nil
  "Your GNU Social username.  If nil, you will be prompted."
  :type '(choice (const :tag "Ask" nil) (string))
  :group 'gnu-social-mode)

(defcustom gnu-social-password nil
  "Your GNU Social password.  If nil, you will be prompted."
  :type '(choice (const :tag "Ask" nil) (string))
  :group 'gnu-social-mode)

(defcustom gnu-social-auth-mode "password"
  "Authorization mode used, options are password and oauth."
  :type 'string
  :group 'gnu-social-mode)

(defun gnu-social-enable-oauth ()
  "Enables oauth for gnu-social-mode."
  (interactive)
  (require 'oauth)
                                        ;Test if we're running on an emacs version with broken unhex and apply workaround.
  (unless (eq (url-unhex-string (url-hexify-string "²")) "²")
    (setq gnu-social-unhex-broken t)
    (require 'w3m))
  (setq gnu-social-auth-mode "oauth"))

(defvar gnu-social-mode-oauth-consumer-key nil)

(defvar gnu-social-mode-oauth-consumer-secret nil)

(defcustom gnu-social-server nil
  "GNU Social instance URL."
  :type '(choice (const :tag "Ask" nil) (string))
  :group 'gnu-social-mode)

(defcustom gnu-social-request-url nil
  "GNU Social oauth request_token url."
  :type 'string
  :group 'gnu-social-mode)

(defcustom gnu-social-access-url nil
  "GNU Social oauth access_token url."
  :type 'string
  :group 'gnu-social-mode)

(defcustom gnu-social-authorize-url nil
  "GNU Social authorization url."
  :type 'string
  :group 'gnu-social-mode)

(defcustom gnu-social-server-textlimit 140
  "Number of characters allowed in a status."
  :type 'integer
  :group 'gnu-social-mode)

(defvar oauth-access-token nil)

(defcustom gnu-social-port 80
  "Port on which GNU Social instance listens."
  :type 'integer
  :group 'gnu-social-mode)

(defcustom gnu-social-default-timeline "friends_timeline"
  "Default timeline to retrieve."
  :type 'string
  :options '("friends_timeline" "public_timeline" "replies")
  :group 'gnu-social-mode)

(defcustom gnu-social-statuses-count 20
  "Default number of statuses to retrieve."
  :type 'integer
  :group 'gnu-social-mode)

(defcustom gnu-social-display-success-messages nil
  "Display messages when the timeline is successfully retrieved."
  :type 'boolean
  :group 'gnu-social-mode)

(defcustom gnu-social-oldest-first nil
  "If t, display older messages before newer ones."
  :type 'boolean
  :group 'gnu-social-mode)

(defcustom gnu-social-update-status-edit-confirm-cancellation nil
  "If t, ask user if they are sure when aborting editing of an
gnu-social status update when using an edit-buffer"
  :type 'boolean
  :group 'gnu-social-mode)

(defcustom gnu-social-soft-wrap-status t
  "If non-nil, don't fill status messages in the timeline as
paragraphs. Instead, use visual-line-mode or longlines-mode if
  available to wrap messages.  This may work better for narrow
  timeline windows."
  :type 'boolean
  :group 'gnu-social-mode)

(defcustom gnu-social-update-status-method 'minibuffer
  "Method for performaing status updates.

The available choices are:

  'minibuffer  - edit the status update in the minibuffer.
  'edit-buffer - edit the status update in an independent buffer."
  :type '(choice (const :tag "Edit status in minibuffer" minibuffer)
                 (const :tag "Edit status in independent buffer" edit-buffer))
  :group 'gnu-social-mode)

(defcustom gnu-social-http-get-timeout 10
  "Controls how long to wait for a response from the server."
  :type 'integer
  :group 'gnu-social-mode)

;; Initialize with default timeline
(defvar gnu-social-method gnu-social-default-timeline)
(defvar gnu-social-method-class "statuses")
(defvar gnu-social-remote-server nil)

(defvar gnu-social-scroll-mode nil)
(make-variable-buffer-local 'gnu-social-scroll-mode)

(defvar gnu-social-source "gnu-social-mode")

(defcustom gnu-social-redent-format "♻"
  "The format/symbol to represent redents."
  :type 'string
  :group 'gnu-social-mode)

(defcustom gnu-social-blacklist '()
  "List of regexes used to filter statuses, evaluated after status formatting is applied."
  :type 'string
  :group 'gnu-social-mode)

(defcustom gnu-social-status-format "%i %s,  %@:\n  %h%t // from %f%L%r\n\n"
  "The format used to display the status updates."
  :type 'string
  :group 'gnu-social-mode)
;; %s - screen_name
;; %S - name
;; %i - profile_image
;; %d - description
;; %l - location
;; %L - " [location]"
;; %r - in reply to status
;; %u - url
;; %j - user.id
;; %p - protected?
;; %c - created_at (raw UTC string)
;; %C{time-format-str} - created_at (formatted with time-format-str)
;; %@ - X seconds ago
;; %t - text
;; %' - truncated
;; %h - favorited
;; %f - source
;; %# - id

(defcustom gnu-social-urlshortening-service 'ur1ca
  "The service to use for URL shortening.
Values understood are ur1ca, tighturl, tinyurl, toly, google and isgd."
  :type 'symbol
  :group 'gnu-social-mode)

(defvar gnu-social-buffer "*gnu-social*")
(defun gnu-social-buffer (&optional method)
  "Create a buffer for use by gnu-social-mode.
Initialize the global method with the default, or with METHOD, if present."
  (unless method
    (setq method "friends_timeline"))
  (get-buffer-create gnu-social-buffer))

(defstruct (gnu-social-oauth-data
            (:conc-name gn-oauth-))
  "The oauth configuration associated with a GNU Social account."
  consumer-key ; string
  consumer-secret ; string
  request-url ; string
  access-url ; string
  authorize-url ; string
  access-token ; string
  )

(defstruct (gnu-social-account
            (:conc-name gn-account-))
  "Container for account information."
  server ; string
  port ; integer
  username ; string
  auth-mode ; string, either "password" or "oauth"
  password ; string
  textlimit ; integer
  oauth-data ; gnu-social-account-oauth-data
  last-timeline-retrieved ; string
  )

(defvar gnu-social-accounts nil
  "A list of login credentials for GNU Social instances.")

(defvar gn-current-account nil
  "A pointer to the GNU Social account being processed.")

(defvar gnu-social-http-buffer nil
  "Pointer to the current http response buffer.")

(defvar gnu-social-timeline-data nil)
(defvar gnu-social-timeline-last-update nil)
(defvar gnu-social-highlighted-entries nil
  "List of entry ids selected for highlighting.")

(defcustom gnu-social-enable-highlighting nil
  "If non-nil, set the background of every selected entry to the background
of gnu-social-highlight-face."
  :type 'boolean
  :group 'gnu-social-mode)

(defcustom gnu-social-enable-striping nil
  "If non-nil, set the background of every second entry to the background
of gnu-social-stripe-face."
  :type 'boolean
  :group 'gnu-social-mode)

(defvar gnu-social-username-face 'gnu-social-username-face)
(defvar gnu-social-uri-face 'gnu-social-uri-face)
(defvar gnu-social-reply-face 'gnu-social-reply-face)
(defvar gnu-social-stripe-face 'gnu-social-stripe-face)
(defvar gnu-social-highlight-face 'gnu-social-highlight-face)

(defcustom gnu-social-reply-bg-color "DarkSlateGray"
  "The background color on which replies are displayed."
  :type 'string
  :group 'gnu-social-mode)

(defcustom gnu-social-stripe-bg-color "SlateGray"
  "The background color on which striped entries are displayed."
  :type 'string
  :group 'gnu-social-mode)

(defcustom gnu-social-highlight-bg-color "DarkSlateGray"
  "The background color on which highlighted entries are displayed."
  :type 'string
  :group 'gnu-social-mode)

;;; Proxy
(defvar gnu-social-proxy-use nil)
(defvar gnu-social-proxy-server nil)
(defvar gnu-social-proxy-port 8080)
(defvar gnu-social-proxy-user nil)
(defvar gnu-social-proxy-password nil)

(defun gnu-social-toggle-proxy ()
  "Toggle whether gnu-social-mode uses a proxy."
  (interactive)
  (setq gnu-social-proxy-use
        (not gnu-social-proxy-use))
  (message "%s %s"
           "Use Proxy:"
           (if gnu-social-proxy-use
               "on" "off")))

(defun gnu-social-user-agent-default-function ()
  "GNU Social mode default User-Agent function."
  (concat "Emacs/"
          (int-to-string emacs-major-version) "." (int-to-string
                                                   emacs-minor-version)
          " "
          "GNU-Social-mode/"
          gnu-social-mode-version))

(defvar gnu-social-user-agent-function 'gnu-social-user-agent-default-function)

(defun gnu-social-user-agent ()
  "Return User-Agent header string."
  (funcall gnu-social-user-agent-function))

;;; to show image files

(defvar gnu-social-tmp-dir
  (expand-file-name (concat "gnu-socialmode-images-" (user-login-name))
                    temporary-file-directory))

(defvar gnu-social-icon-mode nil "You MUST NOT CHANGE this variable directory.  You should change through function'gnu-social-icon-mode'.")
(make-variable-buffer-local 'gnu-social-icon-mode)
(defun gnu-social-icon-mode (&optional arg)
  (interactive)
  (setq gnu-social-icon-mode
        (if gnu-social-icon-mode
            (if (null arg)
                nil
              (> (prefix-numeric-value arg) 0))
          (when (or (null arg)
                    (and arg (> (prefix-numeric-value arg) 0)))
            (when (file-writable-p gnu-social-tmp-dir)
              (progn
                (if (not (file-directory-p gnu-social-tmp-dir))
                    (make-directory gnu-social-tmp-dir))
                t)))))
  (gnu-social-current-timeline))

(defun gnu-social-scroll-mode (&optional arg)
  (interactive)
  (setq gnu-social-scroll-mode
        (if (null arg)
            (not gnu-social-scroll-mode)
          (> (prefix-numeric-value arg) 0))))

(defvar gnu-social-image-stack nil)

(defun gnu-social-image-type (file-name)
  (cond
   ((string-match "\\.jpe?g" file-name) 'jpeg)
   ((string-match "\\.png" file-name) 'png)
   ((string-match "\\.gif" file-name) 'gif)
   (t nil)))

(defun gnu-social-setftime (fmt string uni)
  (format-time-string fmt ; like "%Y-%m-%d %H:%M:%S"
                      (apply 'encode-time (parse-time-string string))
                      uni))
(defun gnu-social-local-strftime (fmt string)
  (gnu-social-setftime fmt string nil))
(defun gnu-social-global-strftime (fmt string)
  (gnu-social-setftime fmt string t))

(defvar gnu-social-debug-mode nil)
(defvar gnu-social-debug-buffer "*gnu-social-debug*")
(defun gnu-social-debug-buffer ()
  (get-buffer-create gnu-social-debug-buffer))
(defmacro debug-print (obj)
  (let ((obsym (gensym)))
    `(let ((,obsym ,obj))
       (if gnu-social-debug-mode
           (with-current-buffer (gnu-social-debug-buffer)
             (insert (prin1-to-string ,obsym))
             (newline)
             ,obsym)
         ,obsym))))

(defun gnu-social-debug-mode ()
  (interactive)
  (setq gnu-social-debug-mode
        (not gnu-social-debug-mode))
  (message (if gnu-social-debug-mode "debug mode:on" "debug mode:off")))

(defun gnu-social-delete-notice ()
  (interactive)
  (let ((id (get-text-property (point) 'id))
        (usern (get-text-property (point) 'username)))
    (if (string= usern (gn-account-username gn-current-account))
        (when (y-or-n-p "Delete this notice? ")
          (gnu-social-http-post "statuses/destroy" (number-to-string id))
          (gnu-social-get-timeline))
      (message "Can't delete a notice that isn't yours"))))

(if gnu-social-mode-map
    (let ((km gnu-social-mode-map))
      (define-key km "\C-c\C-f" 'gnu-social-friends-timeline)
      ;;      (define-key km "\C-c\C-i" 'gnu-social-direct-messages-timeline)
      (define-key km "\C-c\C-r" 'gnu-social-replies-timeline)
      (define-key km "\C-c\C-a" 'gnu-social-public-timeline)
      (define-key km "\C-c\C-g" 'gnu-social-group-timeline)
      ;;      (define-ley km "\C-c\C-j" 'gnu-social-group-join)
      ;;      (define-ley km "\C-c\C-l" 'gnu-social-group-leave)
      (define-key km "\C-c\C-t" 'gnu-social-tag-timeline)
      (define-key km "\C-c\C-k" 'gnu-social-stop)
      (define-key km "\C-c\C-u" 'gnu-social-user-timeline)
      (define-key km "\C-c\C-c" 'gnu-social-conversation-timeline)
      (define-key km "\C-c\C-o" 'gnu-social-remote-user-timeline)
      (define-key km "\C-c\C-s" 'gnu-social-update-status-interactive)
      (define-key km "\C-c\C-d" 'gnu-social-direct-message-interactive)
      (define-key km "\C-c\C-m" 'gnu-social-redent)
      (define-key km "\C-c\C-h" 'gnu-social-toggle-highlight)
      (define-key km "r" 'gnu-social-repeat)
      (define-key km "F" 'gnu-social-favorite)
      (define-key km "\C-c\C-e" 'gnu-social-erase-old-statuses)
      (define-key km "\C-m" 'gnu-social-enter)
      (define-key km "R" 'gnu-social-reply-to-user)
      (define-key km "A" 'gnu-social-reply-to-all)
      (define-key km "\t" 'gnu-social-next-link)
      (define-key km [backtab] 'gnu-social-prev-link)
      (define-key km [mouse-1] 'gnu-social-click)
      (define-key km "\C-c\C-v" 'gnu-social-view-user-page)
      (define-key km "q" 'bury-buffer)
      (define-key km "e" 'gnu-social-expand-replace-at-point)
      (define-key km "j" 'gnu-social-goto-next-status)
      (define-key km "k" 'gnu-social-goto-previous-status)
      (define-key km "l" 'forward-char)
      (define-key km "h" 'backward-char)
      (define-key km "0" 'beginning-of-line)
      (define-key km "^" 'beginning-of-line-text)
      (define-key km "$" 'end-of-line)
      (define-key km "n" 'gnu-social-goto-next-status-of-user)
      (define-key km "p" 'gnu-social-goto-previous-status-of-user)
      (define-key km [backspace] 'scroll-down)
      (define-key km " " 'scroll-up)
      (define-key km "G" 'end-of-buffer)
      (define-key km "g" 'gnu-social-current-timeline)
      (define-key km "H" 'beginning-of-buffer)
      (define-key km "i" 'gnu-social-icon-mode)
      (define-key km "s" 'gnu-social-scroll-mode)
      (define-key km "t" 'gnu-social-toggle-proxy)
      (define-key km "\C-k" 'gnu-social-delete-notice)
      (define-key km "\C-c\C-p" 'gnu-social-toggle-proxy)
      nil))

(defvar gnu-social-mode-syntax-table nil "")

(if gnu-social-mode-syntax-table
    ()
  (setq gnu-social-mode-syntax-table (make-syntax-table))
  ;; (modify-syntax-entry ?  "" gnu-social-mode-syntax-table)
  (modify-syntax-entry ?\" "w"  gnu-social-mode-syntax-table))

(defun gnu-social-mode-init-variables ()
  ;; (make-variable-buffer-local 'variable)
  ;; (setq variable nil)
  (make-variable-buffer-local 'gnu-social-active-mode)
  (set-default 'gnu-social-active-mode t)
  (font-lock-mode -1)
  (defface gnu-social-username-face
    `((t nil)) "" :group 'faces)
  (defface gnu-social-reply-face
    `((t nil)) "" :group 'faces)
  (defface gnu-social-stripe-face
    `((t nil)) "" :group 'faces)
  (defface gnu-social-highlight-face
    `((t nil)) "" :group 'faces)
  (defface gnu-social-uri-face
    `((t nil)) "" :group 'faces)
  (defface gnu-social-heart-face
    `((t nil)) "" :group 'faces)

  (add-to-list 'minor-mode-alist '(gnu-social-icon-mode " id-icon"))
  (add-to-list 'minor-mode-alist '(gnu-social-scroll-mode " id-scroll"))

  ;; make face properties nonsticky
  (unless (boundp 'gnu-social-text-property-nonsticky-adjustment)
    (setq gnu-social-text-property-nonsticky-adjustment t)
    (nconc text-property-default-nonsticky
           '((face . t)(mouse-face . t)(uri . t)(source . t)(uri-in-text . t))))

  (gnu-social-create-account))

(defun gnu-social-create-account ()
  "Create an account object based on the various custom variables.
  Insert it into the gnu-social accounts list.
This needs to be called from any globally-accessable entry point."
  (unless (boundp 'gnu-social-account-created)
    (setq gnu-social-account-created t)
    (setq gnu-social-accounts
          (cons (make-gnu-social-account
                 :server gnu-social-server
                 :port gnu-social-port
                 :username gnu-social-username
                 :auth-mode gnu-social-auth-mode
                 :password gnu-social-password
                 :textlimit gnu-social-server-textlimit
                 :oauth-data (if (string= gnu-social-auth-mode "oauth")
				 (progn
				   (when (or (null gnu-social-mode-oauth-consumer-key)
					     (null gnu-social-mode-oauth-consumer-secret)
					     (null gnu-social-request-url)
					     (null gnu-social-access-url)
					     (null gnu-social-authorize-url))
				     (user-error "%s"
						 "You must define the necessary OAuth \
variables if you will use OAuth \
authentication"))
				   (make-gnu-social-oauth-data
				    :consumer-key gnu-social-mode-oauth-consumer-key
				    :consumer-secret gnu-social-mode-oauth-consumer-secret
				    :request-url gnu-social-request-url
				    :access-url gnu-social-access-url
				    :authorize-url gnu-social-authorize-url
				    :access-token nil))
                               nil)
                 :last-timeline-retrieved nil)
                gnu-social-accounts))
    (setq gn-current-account (car gnu-social-accounts))))

(defmacro case-string (str &rest clauses)
  `(cond
    ,@(mapcar
       (lambda (clause)
         (let ((keylist (car clause))
               (body (cdr clause)))
           `(,(if (listp keylist)
                  `(or ,@(mapcar (lambda (key) `(string-equal ,str ,key)) keylist))
                't)
             ,@body)))
       clauses)))

;; If you use Emacs21, decode-char 'ucs will fail unless Mule-UCS is loaded.
;; TODO: Show error messages if Emacs 21 without Mule-UCS
(defmacro gnu-social-ucs-to-char (num)
  (if (functionp 'ucs-to-char)
      `(ucs-to-char ,num)
    `(decode-char 'ucs ,num)))

(defvar gnu-social-mode-string gnu-social-method)

(defun gnu-social-set-mode-string (loading)
  (with-current-buffer (gnu-social-buffer)
    (let ((timeline-url
           (concat (or gnu-social-remote-server
                       (gn-account-server gn-current-account))
                   "/" gnu-social-method)))
      (setq mode-name
            (if loading (concat
                         (if (stringp loading) loading "loading")
                         " " timeline-url "...")
              timeline-url))
      (debug-print mode-name))))

(defvar gnu-social-mode-hook nil
  "GNU-Social-mode hook.")

(defcustom gnu-social-load-hook nil
  "Hook that is run after gnu-social-mode.el has been loaded."
  :group 'gnu-social-mode
  :type 'hook)

(defun gnu-social-kill-buffer-function ()
  (when (eq major-mode 'gnu-social-mode)
    (gnu-social-stop)))

(defun gnu-social-autoload-oauth ()
  "Autoloads oauth.el when needed."
  (autoload 'oauth-authorize-app "oauth")
  (autoload 'oauth-hexify-string "oauth")
  (autoload 'make-oauth-access-token "oauth"))

(defun gnu-social-mode ()
  "Major mode for GNU Social.
  \\{gnu-social-mode-map}"
  (interactive)
  (gnu-social-autoload-oauth)
  (switch-to-buffer (gnu-social-buffer))
  (buffer-disable-undo (gnu-social-buffer))
  (kill-all-local-variables)
  (gnu-social-mode-init-variables)
  (use-local-map gnu-social-mode-map)
  (setq major-mode 'gnu-social-mode)
  (setq mode-name gnu-social-mode-string)
  (setq mode-line-buffer-identification
        `(,(default-value 'mode-line-buffer-identification)
          (:eval (gnu-social-mode-line-buffer-identification))))
  (gnu-social-update-mode-line)
  (set-syntax-table gnu-social-mode-syntax-table)
  (font-lock-mode -1)
  (if gnu-social-soft-wrap-status
      (if (fboundp 'visual-line-mode)
          (visual-line-mode t)
        (if (fboundp 'longlines-mode)
            (longlines-mode t))))
  (gnu-social-retrieve-configuration)
  (add-hook 'kill-buffer-hook 'gnu-social-kill-buffer-function)
  (run-mode-hooks 'gnu-social-mode-hook))

;;;
;;; Basic HTTP functions
;;;

(defun gnu-social-set-proxy (&optional url username passwd server port)
  "Sets the proxy authentication variables as required by url library.
When called with no arguments, it reads `gnu-social-mode' proxy
variables to get the authentication parameters.URL is either a string
or parsed URL.  If URL is non-nil and valid, proxy authentication
values are read from it.  The rest of the arguments can be used to
directly set proxy authentication.  This function essentially adds
authentication parameters from one of the above methods to the double
alist `url-http-proxy-basic-auth-storage' and sets `url-using-proxy'."
  (let* ((href (if (stringp url)
                   (url-generic-parse-url url)
                 url))
         (port (or (and href (url-port href))
                   port gnu-social-proxy-port))
         (port (if (integerp port) (int-to-string port) port))
         (server (or (and href (url-host href))
                     server gnu-social-proxy-server))
         (server (and server
                      (concat server (when port (concat ":" port)))))
         (file (if href (let ((file-url (url-filename href)))
                          (cond
                           ((string= "" file-url) "/")
                           ((string-match "/$" file-url) file-url)
                           (t (url-basepath file-url))))
                 "Proxy"))
         (password (or (and href (url-password href))
                       passwd gnu-social-proxy-password))
         (auth (concat (or (and href (url-user href))
                           username gnu-social-proxy-user)
                       (and password (concat ":" password)))))
    (when (and gnu-social-proxy-use
               (not (string= "" server))
               (not (string= "" auth)))
      (setq url-using-proxy server)
      (let* ((proxy-double-alist
              (or (assoc server
                         url-http-proxy-basic-auth-storage)
                  (car (push (cons server nil)
                             url-http-proxy-basic-auth-storage))))
             (proxy-auth-alist (assoc file proxy-double-alist)))
        (if proxy-auth-alist
            (setcdr proxy-auth-alist (base64-encode-string auth))
          (setcdr proxy-double-alist
                  (cons (cons file
                              (base64-encode-string auth))
                        (cdr-safe proxy-double-alist))))))))

(defun gnu-social-change-user ()
  (interactive)
  "Interactive function to instantly change user authentication.
Directly reads parameters from user.  This function only sets the
gnu-social-mode variables `(gn-account-username gn-current-account)' and
`(gn-account-password gn-current-account)'.
It is the `gnu-social-set-auth' function that eventually sets the
url library variables according to the above variables which does the
authentication.  This will be done automatically in normal use cases
enabling dynamic change of user authentication."
  (interactive)
  (gnu-social-ask-credentials)
  (gnu-social-get-timeline))

(defun gnu-social-ask-credentials ()
  "Asks for your username and password."
  (setf (gn-account-username gn-current-account)
        (read-string (concat "Username [for " (gn-account-server gn-current-account)
                             ":" (int-to-string (gn-account-port gn-current-account)) "]: ")
                     nil nil (gn-account-username gn-current-account))
        (gn-account-password gn-current-account)
        (read-passwd "Password: " nil (gn-account-password gn-current-account))))

(defun gnu-social-set-auth (&optional url username passwd server port)
  "Sets the authentication parameters as required by url library.
If URL is non-nil and valid, it reads user authentication
parameters from url.  If URL is nil, Rest of the arguments can be
used to directly set user authentication.
When called with no arguments, user authentication parameters are
read from gnu-social-mode variables `(gn-account-username gn-current-account)'
`(gn-account-password gn-current-account)' `(gn-account-server gn-current-account)'
 `(gn-account-port gn-current-account)'.
The username and password can also be set on ~/.authinfo,
~/.netrc or ~/.authinfo.gpg files for better security.
In this case `(gn-account-password gn-current-account)' should
not be predefined in any .emacs or init.el files, only
`(gn-account-username gn-current-account)' should be set."
  (unless (gn-account-username gn-current-account)
    (gnu-social-ask-credentials))
  (let* ((href (if (stringp url)
                   (url-generic-parse-url url)
                 url))
         (port (or (and href (url-port href))
                   port (gn-account-port gn-current-account)))
         (port (if (integerp port) (int-to-string port) port))
         (server (or (and href (url-host href))
                     server (gn-account-server gn-current-account)))
         (servername server)
         (server (and server
                      (concat server (when port (concat ":" port)))))
         (file (if href (let ((file-url (url-filename href)))
                          (cond
                           ((string= "" file-url) "/")
                           ((string-match "/$" file-url) file-url)
                           (t (url-basepath file-url))))
                 "GNU Social API"))

         (auth-user (if (functionp 'auth-source-search)
                        (plist-get (car (auth-source-search :host servername :max 1)) :user)
                      (auth-source-user-or-password "login" server "http")))
         (auth-pass (if (functionp 'auth-source-search)
                        (if (functionp (plist-get (car (auth-source-search :host servername :max 1)) :secret))
                            (funcall (plist-get (car (auth-source-search :host servername :max 1)) :secret))
                          (plist-get (car (auth-source-search :host servername :max 1)) :secret))
                      (auth-source-user-or-password "password" server "http")))
         (password (or auth-pass (and href (url-password href))
                       passwd (gn-account-password gn-current-account)))
         (auth (concat (or auth-user (and href (url-user href))
                           username (gn-account-username gn-current-account))
                       (and password (concat ":" password)))))
    (when (and (not (string= "" server))
               (not (string= "" auth)))
      (let* ((server-double-alist
              (or (assoc server
                         url-http-real-basic-auth-storage)
                  (car (push (cons server nil)
                             url-http-real-basic-auth-storage))))
             (api-auth-alist (assoc file server-double-alist)))
        (if api-auth-alist
            (setcdr api-auth-alist (base64-encode-string auth))
          (setcdr server-double-alist
                  (cons (cons file
                              (base64-encode-string auth))
                        (cdr-safe server-double-alist))))))))

(defun gnu-social-initialize-oauth ()
  "Get authentication token unless we have one stashed already.
Shamelessly stolen from yammer.el"
  (let ((filename (concat "~/." (gn-account-server gn-current-account) "-"
                          (gn-account-username gn-current-account) "-oauth-token")))
    (when (file-exists-p filename)
      (save-excursion
        (find-file filename)
        (let ((str (buffer-substring (point-min) (point-max))))
          (if (string-match "\\([^:]*\\):\\(.*\\)"
                            (buffer-substring (point-min) (point-max)))
              (setf (gn-oauth-access-token (gn-account-oauth-data gn-current-account))
                    (make-oauth-access-token
                     :consumer-key (gn-oauth-consumer-key (gn-account-oauth-data gn-current-account))
                     :consumer-secret (gn-oauth-consumer-secret (gn-account-oauth-data gn-current-account))
                     :auth-t (make-oauth-t
                              :token (match-string 1 str)
                              :token-secret (match-string 2 str))))))
        (save-buffer)
        (kill-this-buffer)))
    (unless (gn-oauth-access-token (gn-account-oauth-data gn-current-account))
      (setf (gn-oauth-access-token (gn-account-oauth-data gn-current-account))
            (oauth-authorize-app (gn-oauth-consumer-key (gn-account-oauth-data gn-current-account))
                                 (gn-oauth-consumer-secret (gn-account-oauth-data gn-current-account))
                                 (gn-oauth-request-url (gn-account-oauth-data gn-current-account))
                                 (gn-oauth-access-url (gn-account-oauth-data gn-current-account))
                                 (gn-oauth-authorize-url (gn-account-oauth-data gn-current-account))))
      (save-excursion
        (find-file filename)
        (end-of-buffer)
        (let ((token (oauth-access-token-auth-t (gn-oauth-access-token (gn-account-oauth-data gn-current-account)))))
          (insert (format "%s:%s\n"
                          (oauth-t-token token)
                          (oauth-t-token-secret token))))
        (save-buffer)
        (kill-this-buffer))))
  (gn-oauth-access-token (gn-account-oauth-data gn-current-account)))

(defun gnu-social-http-get
  (server auth-mode method-class method &optional parameters sentinel sentinel-arguments)
  "Basic function which communicates with server.
METHOD-CLASS and METHOD are parameters for getting dents messages and
other information from SERVER as specified in api documentation.
Third optional arguments specify the additional parameters required by
the above METHOD.  It is specified as an alist with parameter name and
its corresponding value SENTINEL represents the callback function to
be called after the http response is completely retrieved.
SENTINEL-ARGUMENTS is the list of arguments (if any) of the SENTINEL
procedure."
  (or sentinel (setq sentinel 'gnu-social-http-get-default-sentinel))
  (let ((url (concat "http://" server "/api/"
                     (when (not (string-equal method-class "none"))
                       (concat method-class "/" ))
                     method ".xml"
                     (when parameters
                       (concat "?"
                               (mapconcat
                                (lambda (param-pair)
                                  (format "%s=%s"
                                          (gnu-social-percent-encode (car param-pair))
                                          (gnu-social-percent-encode (cdr param-pair))))
                                parameters
                                "&")))))
        (url-package-name "emacs-gnu-social-mode")
        (url-package-version gnu-social-mode-version)
        (url-show-status nil))
    (gnu-social-set-proxy)
    (unless (equal auth-mode "none")
      (if (equal auth-mode "oauth")
          (or (gn-oauth-access-token (gn-account-oauth-data gn-current-account))
              (gnu-social-initialize-oauth))
        (gnu-social-set-auth url)))
    (when (get-buffer-process gnu-social-http-buffer)
      (delete-process gnu-social-http-buffer)
      (kill-buffer gnu-social-http-buffer))
    (setq gnu-social-http-buffer
          (gnu-social-url-retrieve url sentinel method-class
                                 method parameters sentinel-arguments auth-mode))
    (set-buffer gnu-social-buffer)
    (gnu-social-set-mode-string t)))

(defun gnu-social-render-pending-dents ()
  (interactive)
  "If at the time an HTTP request for new dents finishes,
gnu-social-buffer is not active, we defer its update, to make sure
we adjust point within the right frame."
  (gnu-social-render-timeline)
  (when (> gnu-social-new-dents-count 0)
    (run-hooks 'gnu-social-new-dents-hook)
    (setq gnu-social-new-dents-count 0))
  (when gnu-social-display-success-messages
    (message "Success: Get")))

(defun gnu-social-http-get-default-sentinel
  (&optional status method-class method parameters success-message)
  (debug-print (window-buffer))
  (let ((error-object (assoc-workaround :error status))
        (active-p (eq (window-buffer) (gnu-social-buffer))))
    (cond (error-object
           (let ((error-data (format "%s" (caddr error-object))))
             (when (cond
                    ((string= error-data "deleted\n") t)
                    ((and (string= error-data "404") method
                          (= 13 (string-match "/" method)))
                     (message "No Such User: %s" (substring method 14))
                     t)
                    ((y-or-n-p
                      (format "GNU-Social-Mode: Network error:%s Retry? "
                              status))
                     (gnu-social-http-get (gn-account-server gn-current-account)
                                        (gn-account-auth-mode gn-current-account)
                                        method-class method parameters)
                     nil))
               ;; when the network process is deleted by another query
               ;; or the user queried is not found , query is _finished_
               ;; unsuccessful and we want to restore gnu-social-method
               ;; to loose track of this unsuccessful attempt
               (setq gnu-social-method (gn-account-last-timeline-retrieved gn-current-account)))))
          ((< (- (point-max) (or (re-search-forward ">\r?\n\r*$" nil t) 0)) 2)
           ;;Checking the whether the message is complete by
           ;;searching for > that closes the last tag, followed by
           ;;CRLF at (point-max)
           (let ((body (gnu-social-get-response-body)))
             (if (not body)
                 (gnu-social-set-mode-string nil)
               (setq gnu-social-new-dents-count
                     (+ gnu-social-new-dents-count
                        (count t (mapcar
                                  #'gnu-social-cache-status-datum
                                  (reverse (gnu-social-xmltree-to-status
                                            body))))))
                                        ; Shorten the timeline if necessary
               (if (and gnu-social-display-max-dents
                        (> (safe-length gnu-social-timeline-data)
                           gnu-social-display-max-dents))
                   (cl-set-nthcdr gnu-social-display-max-dents
                                  gnu-social-timeline-data nil))
               (if active-p
                   (gnu-social-render-pending-dents)
                 (gnu-social-set-mode-string "pending"))))))))

(defun merge-text-attribute (start end new-face attribute)
  "Merge the ATTRIBUTE of NEW-FACE into the text between START and END.
If we just add the new face its attributes somehow get overridden by
the attributes of the underlying face, so instead we just add the attribute
we are interested in."
  (while (not (eq start end))
    (let ((bg (face-attribute new-face attribute))
          (prop (get-text-property start 'face))
          (next-change
           (next-single-property-change start 'face (current-buffer) end)))
      (if prop
          (add-text-properties start next-change
                               (list 'face
                                     (list prop
                                           (list attribute bg))))
        (add-text-properties start next-change
                             (list 'face (list attribute bg))))
      (setq start next-change))))

(defun gnu-social-render-timeline ()
  (with-current-buffer (gnu-social-buffer)
    (set-face-attribute 'gnu-social-username-face nil
                        :underline t)
    (set-face-attribute 'gnu-social-reply-face nil
                        :background gnu-social-reply-bg-color)
    (set-face-attribute 'gnu-social-stripe-face nil
                        :background gnu-social-stripe-bg-color)
    (set-face-attribute 'gnu-social-highlight-face nil
                        :background gnu-social-highlight-bg-color)
    (set-face-attribute 'gnu-social-uri-face nil
                        :underline t)
    (set-face-attribute 'gnu-social-heart-face nil
                        :foreground "firebrick1" :height 2.0)
    (let ((point (point))
          (end (point-max))
          (wrapped (cond (visual-line-mode 'visual-line-mode)
                         (longlines-mode 'longlines-mode)
                         (t nil)))
          (stripe-entry nil))

      (setq buffer-read-only nil)
      (erase-buffer)
      (when wrapped (funcall wrapped -1))
      (mapc (lambda (status)
              (let ((before-status (point-marker))
                    (blacklisted 'nil)
                    (formatted-status (gnu-social-format-status
                                       status gnu-social-status-format)))
                (mapc (lambda (regex)
                        (when (string-match-p regex formatted-status)
                          (setq blacklisted 't)))
                      gnu-social-blacklist)
                (unless blacklisted
                  (when gnu-social-enable-striping
                    (setq stripe-entry (not stripe-entry)))
                  (insert formatted-status)
                  (when (not wrapped)
                    (fill-region-as-paragraph
                     (save-excursion (beginning-of-line -1) (point)) (point)))
                  (insert-and-inherit "\n")
                  ;; Apply highlight overlays to status
                  (when (or (string-equal (gn-account-username gn-current-account)
                                          (assoc-default 'in-reply-to-screen-name status))
                            (string-match
                             (concat "@" (gn-account-username gn-current-account)
                                     "\\([^[:word:]_-]\\|$\\)") (assoc-default 'text status)))
                    (merge-text-attribute before-status (point) 'gnu-social-reply-face :background))
                  (when (and gnu-social-enable-highlighting
                             (memq (assoc-default 'id status) gnu-social-highlighted-entries))
                    (merge-text-attribute before-status (point) 'gnu-social-highlight-face :background))
                  (when stripe-entry
                    (merge-text-attribute before-status (point) 'gnu-social-stripe-face :background))
                  (when gnu-social-oldest-first (goto-char (point-min))))))
            gnu-social-timeline-data)
      (when (and gnu-social-image-stack window-system) (clear-image-cache))
      (when wrapped (funcall wrapped 1))
      (setq buffer-read-only t)
      (debug-print (current-buffer))
      (goto-char (+ point (if gnu-social-scroll-mode (- (point-max) end) 0)))
      (gnu-social-set-mode-string nil)
      (setf (gn-account-last-timeline-retrieved gn-current-account) gnu-social-method)
      (if transient-mark-mode (deactivate-mark)))))

(defun gnu-social-format-status (status format-str)
  (flet ((attr (key)
               (assoc-default key status))
         (profile-image
          ()
          (let ((profile-image-url (attr 'user-profile-image-url)))
            (when (string-match "/\\([^/?]+\\)\\(?:\\?\\|$\\)" profile-image-url)
              (let ((filename (match-string-no-properties 1 profile-image-url))
                    (xfilename (match-string-no-properties 0 profile-image-url)))
                ;; download icons if does not exist
                (unless (file-exists-p (concat gnu-social-tmp-dir filename))
                  (if (file-exists-p (concat gnu-social-tmp-dir xfilename))
                      (setq filename xfilename)
                    (setq filename nil)
                    (add-to-list 'gnu-social-image-stack profile-image-url)))
                (when (and gnu-social-icon-mode filename)
                  (let ((avatar (create-image (concat gnu-social-tmp-dir filename))))
                    ;; Make sure the avatar is 48 pixels (which it should already be!, but hey...)
                    ;; For offenders, the top left slice of 48 by 48 pixels is displayed
                    ;; TODO: perhaps make this configurable?
                    (insert-image avatar nil nil `(0 0 48 48)))
                  nil))))))
    (let ((cursor 0)
          (result ())
          c
          found-at)
      (setq cursor 0)
      (setq result '())
      (while (setq found-at (string-match "%\\(C{\\([^}]+\\)}\\|[A-Za-z#@']\\)" format-str cursor))
        (setq c (string-to-char (match-string-no-properties 1 format-str)))
        (if (> found-at cursor)
            (push (substring format-str cursor found-at) result)
          "|")
        (setq cursor (match-end 1))

        (case c
          ((?s)                         ; %s - screen_name
           (push (attr 'user-screen-name) result))
          ((?S)                         ; %S - name
           (push (attr 'user-name) result))
          ((?i)                         ; %i - profile_image
           (push (profile-image) result))
          ((?d)                         ; %d - description
           (push (attr 'user-description) result))
          ((?l)                         ; %l - location
           (push (attr 'user-location) result))
          ((?L)                         ; %L - " [location]"
           (let ((location (attr 'user-location)))
             (unless (or (null location) (string= "" location))
               (push (concat " [" location "]") result)) ))
          ((?u)                         ; %u - url
           (push (attr 'user-url) result))
          ((?U)                         ; %U - profile url
           (push (cadr (split-string (attr 'user-profile-url) "https*://")) result))
          ((?j)                         ; %j - user.id
           (push (format "%d" (attr 'user-id)) result))
          ((?r)                         ; %r - in_reply_to_status_id
           (let ((reply-id (attr 'in-reply-to-status-id))
                 (reply-name (attr 'in-reply-to-screen-name)))
             (unless (or (null reply-id) (string= "" reply-id)
                         (null reply-name) (string= "" reply-name))
               (let ((in-reply-to-string (format "in reply to %s" reply-name))
                     (url (gnu-social-get-status-url reply-id)))
                 (add-text-properties
                  0 (length in-reply-to-string)
                  `(mouse-face highlight
                               face gnu-social-uri-face
                               uri ,url)
                  in-reply-to-string)
                 (push (concat " " in-reply-to-string) result)))))
          ((?p)                         ; %p - protected?
           (let ((protected (attr 'user-protected)))
             (when (string= "true" protected)
               (push "[x]" result))))
          ((?c)                     ; %c - created_at (raw UTC string)
           (push (attr 'created-at) result))
          ((?C) ; %C{time-format-str} - created_at (formatted with time-format-str)
           (push (gnu-social-local-strftime
                  (or (match-string-no-properties 2 format-str) "%H:%M:%S")
                  (attr 'created-at))
                 result))
          ((?@)                         ; %@ - X seconds ago
           (let ((created-at
                  (apply
                   'encode-time
                   (parse-time-string (attr 'created-at))))
                 (now (current-time)))
             (let ((secs (+ (* (- (car now) (car created-at)) 65536)
                            (- (cadr now) (cadr created-at))))
                   time-string url)
               (setq time-string
                     (cond ((< secs 5) "less than 5 seconds ago")
                           ((< secs 10) "less than 10 seconds ago")
                           ((< secs 20) "less than 20 seconds ago")
                           ((< secs 30) "half a minute ago")
                           ((< secs 60) "less than a minute ago")
                           ((< secs 150) "1 minute ago")
                           ((< secs 2400) (format "%d minutes ago"
                                                  (/ (+ secs 30) 60)))
                           ((< secs 5400) "about 1 hour ago")
                           ((< secs 84600) (format "about %d hours ago"
                                                   (/ (+ secs 1800) 3600)))
                           (t (format-time-string "%I:%M %p %B %d, %Y" created-at))))
               (setq url (gnu-social-get-status-url (attr 'id)))
               ;; make status url clickable
               (add-text-properties
                0 (length time-string)
                `(mouse-face highlight
                             face gnu-social-uri-face
                             uri ,url)
                time-string)
               (push time-string result))))
          ((?t)                         ; %t - text
           (push                   ;(clickable-text)
            (attr 'text)
            result))
          ((?')                         ; %' - truncated
           (let ((truncated (attr 'truncated)))
             (when (string= "true" truncated)
               (push "..." result))))
          ((?f)                         ; %f - source
           (push (attr 'source) result))
          ((?F)                         ; %F - ostatus-aware source
           (push (if (string= (attr 'source) "ostatus")
                     (cadr (split-string (attr 'user-profile-url) "https*://"))
                   (attr 'source)) result))
          ((?#)                         ; %# - id
           (push (format "%d" (attr 'id)) result))
          ((?x)                         ; %x - conversation id (conteXt) - default 0
           (push (attr 'conversation-id) result))
          ((?h)
           (let ((likes (attr 'favorited)))
             (when (string= "true" likes)
               (push (propertize "❤" 'face 'gnu-social-heart-face) result))))
          (t
           (push (char-to-string c) result))))
      (push (substring format-str cursor) result)
      (let ((formatted-status (apply 'concat (nreverse result))))
        (add-text-properties 0 (length formatted-status)
                             `(username, (attr 'user-screen-name)
                                         id, (attr 'id)
                                         text, (attr 'text)
                                         profile-url, (attr 'user-profile-url)
                                         conversation-id, (attr 'conversation-id))
                             formatted-status)
        formatted-status))))

(defun gnu-social-url-retrieve
  (url sentinel method-class method parameters sentinel-arguments &optional auth-mode unhex-workaround)
  "Call url-retrieve or oauth-url-retrieve dsepending on the mode.
Apply url-unhex-string workaround if necessary."
  (if (and (equal auth-mode "oauth")
           (gn-oauth-access-token (gn-account-oauth-data gn-current-account)))
      (if unhex-workaround
          (flet ((oauth-extract-url-params
                  (req)
                  "Modified oauth-extract-url-params using w3m-url-decode-string to work around
bug in url-unhex-string present in emacsen previous to 23.3."
                  (let ((url (oauth-request-url req)))
                    (when (string-match (regexp-quote "?") url)
                      (mapcar (lambda (pair)
                                `(,(car pair) . ,(w3m-url-decode-string (cadr pair))))
                              (url-parse-query-string (substring url (match-end 0))))))))
            (gnu-social-url-retrieve url sentinel method-class method parameters sentinel-arguments auth-mode))
        (oauth-url-retrieve (gn-oauth-access-token (gn-account-oauth-data gn-current-account)) url sentinel
                            (append (list method-class method parameters)
                                    sentinel-arguments)))
    (url-retrieve url sentinel
                  (append (list method-class method parameters)
                          sentinel-arguments))))

(defun gnu-social-http-post
  (method-class method &optional parameters sentinel sentinel-arguments)
  "Send HTTP POST request to gnu-social server.
METHOD-CLASS must be one of GNU Social API method classes(statuses, users or direct_messages).
METHOD must be one of GNU Social API method which belongs to METHOD-CLASS.
PARAMETERS is alist of URI parameters. ex) ((\"mode\" . \"view\") (\"page\" . \"6\")) => <URI>?mode=view&page=6"
  (or sentinel (setq sentinel 'gnu-social-http-post-default-sentinel))
  (let ((url-request-method "POST")
        (url (concat "http://"(gn-account-server gn-current-account) "/api/" method-class "/" method ".xml"
                     (when parameters
                       (concat "?"
                               (mapconcat
                                (lambda (param-pair)
                                  (format "%s=%s"
                                          (gnu-social-percent-encode (car param-pair))
                                          (gnu-social-percent-encode (cdr param-pair))))
                                parameters
                                "&")))))
        (url-package-name "emacs-gnu-socialmode")
        (url-package-version gnu-social-mode-version)
        ;; (if (assoc `media parameters)
        ;; (url-request-extra-headers '(("Content-Type" . "multipart/form-data")))
        (url-request-extra-headers '(("Content-Length" . "0")))
        (url-show-status nil))
    (gnu-social-set-proxy)
    (if (equal (gn-account-auth-mode gn-current-account) "oauth")
        (or (gn-oauth-access-token (gn-account-oauth-data gn-current-account))
            (gnu-social-initialize-oauth))
      (gnu-social-set-auth url))
    (when (get-buffer-process gnu-social-http-buffer)
      (delete-process gnu-social-http-buffer)
      (kill-buffer gnu-social-http-buffer))
    (gnu-social-url-retrieve url sentinel method-class method parameters
                           sentinel-arguments (gn-account-auth-mode gn-current-account) gnu-social-unhex-broken)))

(defun gnu-social-http-post-default-sentinel
  (&optional status method-class method parameters success-message)
  (let ((error-object (assoc-workaround :error status)))
    (cond  ((and
             error-object
             (y-or-n-p (format "Network error:%s %s Retry? "
                               (cadr error-object)
                               (caddr error-object))))
            (gnu-social-http-post method-class method parameters nil success-message))
           (gnu-social-display-success-messages
            (message (or success-message "Success: Post")))))
  (unless (get-buffer-process (current-buffer))
    (kill-buffer (current-buffer))))

(defun gnu-social-get-response-header (&optional buffer)
  "Exract HTTP response header from HTTP response.
BUFFER may be a buffer or the name of an existing buffer.
 If BUFFER is omitted, 'current-buffer' is parsed."
  (or buffer
      (setq buffer (current-buffer)))
  (set-buffer buffer)
  (let ((end (or (and (search-forward-regexp "\r?\n\r?\n" (point-max) t)
                      (match-beginning 0))
                 0)))
    (and (> end 1)
         (buffer-substring (point-min) end))))

(defun gnu-social-get-response-body (&optional buffer)
  "Exract HTTP response body from HTTP response, parse it as XML, and return a XML tree as list.
`buffer' may be a buffer or the name of an existing buffer.
 If `buffer' is omitted, current-buffer is parsed."
  (or buffer
      (setq buffer (current-buffer)))
  (set-buffer buffer)
  (set-buffer-multibyte t)
  (let ((start (save-excursion
                 (goto-char (point-min))
                 (and (re-search-forward "<\\?xml" (point-max) t)
                      (match-beginning 0)))))
    (gnu-social-clean-response-body)
    (and start
         (prog1
             (xml-parse-region start (point-max))
           (if gnu-social-debug-mode
               t
             (kill-buffer buffer))))))

(defun gnu-social-clean-weird-chars (&optional buffer)
  (with-current-buffer gnu-social-http-buffer
    (goto-char (point-min))
    (while (re-search-forward "\

?
[0-9a-z]*\

?
?" nil t)
(replace-match ""))
(buffer-string)))

(defun gnu-social-clean-response-body ()
  "Remove weird strings (e.g., 1afc, a or 0) from the response body.
Known GNU Social issue.  Mostly harmless except if in tags."
  (goto-char (point-min))
  (while (re-search-forward "\r?\n[0-9a-z]+\r?\n" nil t)
    (replace-match "")))

(defun gnu-social-compare-statuses (a b)
  "Compare a pair of statuses.
For use as a predicate for sort."
  (< (assoc-default 'id b) (assoc-default 'id a)))

(defun gnu-social-cache-status-datum (status-datum &optional data-var)
  "Cache status datum into data-var(default `gnu-social-timeline-data')
If STATUS-DATUM is already in DATA-VAR, return nil.  If not, return t."
  (when (null data-var)
    (setf data-var 'gnu-social-timeline-data))
  (let ((id (cdr (assq 'id status-datum))))
    (if (or (null (symbol-value data-var))
            (not (find-if
                  (lambda (item)
                    (eql id (cdr (assq 'id item))))
                  (symbol-value data-var))))
        (progn
          (set data-var (sort (cons status-datum (symbol-value data-var))
                              'gnu-social-compare-statuses))
          t)
      nil)))

(defun gnu-social-status-to-status-datum (status)
  (flet ((assq-get (item seq)
                   (car (cddr (assq item seq)))))
    (let* ((status-data (cddr status))
           id text source created-at truncated favorited
           in-reply-to-status-id
           in-reply-to-screen-name
           (user-data (cddr (assq 'user status-data)))
           user-id user-name
           conversation-id
           user-screen-name
           user-location
           user-description
           user-profile-image-url
           user-profile-url
           user-url
           user-protected
           regex-index)

      (setq id (string-to-number (assq-get 'id status-data)))
      (setq text (gnu-social-decode-html-entities
                  (assq-get 'text status-data)))
      (setq source (gnu-social-decode-html-entities
                    (assq-get 'source status-data)))
      (setq created-at (assq-get 'created_at status-data))
      (setq truncated (assq-get 'truncated status-data))
      (setq favorited (assq-get 'favorited status-data))
      (setq in-reply-to-status-id
            (gnu-social-decode-html-entities
             (assq-get 'in_reply_to_status_id status-data)))
      (setq in-reply-to-screen-name
            (gnu-social-decode-html-entities
             (assq-get 'in_reply_to_screen_name status-data)))
      (setq conversation-id (or (assq-get 'statusnet:conversation_id status-data) "0"))
      (setq user-id (string-to-number (assq-get 'id user-data)))
      (setq user-name (gnu-social-decode-html-entities
                       (assq-get 'name user-data)))
      (setq user-screen-name (gnu-social-decode-html-entities
                              (assq-get 'screen_name user-data)))
      (setq user-location (gnu-social-decode-html-entities
                           (assq-get 'location user-data)))
      (setq user-description (gnu-social-decode-html-entities
                              (assq-get 'description user-data)))
      (setq user-profile-image-url (assq-get 'profile_image_url user-data))
      (setq user-url (assq-get 'url user-data))
      (setq user-protected (assq-get 'protected user-data))
      (setq user-profile-url (assq-get 'statusnet:profile_url user-data))

      ;; make username clickable
      (add-text-properties
       0 (length user-name)
       `(mouse-face highlight
                    uri ,user-profile-url
                    face gnu-social-username-face)
       user-name)

      ;; make screen-name clickable
      (add-text-properties
       0 (length user-screen-name)
       `(mouse-face highlight
                    face gnu-social-username-face
                    uri ,user-profile-url
                    face gnu-social-username-face)
       user-screen-name)

      ;; make URI clickable
      (setq regex-index 0)
      (while regex-index
        (setq regex-index
              (string-match "@\\([_[:word:]0-9]+\\)\\|!\\([_[:word:]0-9\-]+\\)\\|#\\([_[:word:]0-9\-]+\\)\\|\\(ur1\.ca/[a-z0-9]+/?\\|https?://[-_.!~*'()[:word:]0-9\;/?:@&=+$,%#]+\\)"
                            text
                            regex-index))
        (when regex-index
          (let* ((matched-string (match-string-no-properties 0 text))
                 (screen-name (match-string-no-properties 1 text))
                 (group-name (match-string-no-properties 2 text))
                 (tag-name (match-string-no-properties 3 text))
                 (uri (match-string-no-properties 4 text)))
            (add-text-properties
             (if (or screen-name group-name tag-name)
                 (+ 1 (match-beginning 0))
               (match-beginning 0))
             (match-end 0)
             (if (or screen-name group-name tag-name)
                 `(mouse-face
                   highlight
                   face gnu-social-uri-face
                   uri ,(if screen-name
                            (concat "https://" (gn-account-server gn-current-account) "/" screen-name)
                          (if group-name
                              (concat "https://" (gn-account-server gn-current-account) "/group/" group-name)
                            (concat "https://" (gn-account-server gn-current-account) "/tag/" tag-name)))
                   uri-in-text ,(if screen-name
                                    (concat "https://" (gn-account-server gn-current-account) "/" screen-name)
                                  (if group-name
                                      (concat "https://" (gn-account-server gn-current-account) "/group/" group-name)
                                    (concat "https://" (gn-account-server gn-current-account) "/tag/" tag-name)))
                   tag ,tag-name
                   group ,group-name)
               `(mouse-face highlight
                            face gnu-social-uri-face
                            uri ,uri
                            uri-in-text ,uri))
             text))
          (setq regex-index (match-end 0)) ))


      ;; make source pretty and clickable
      (when (string-match "<a href=\"\\(.*\\)\">\\(.*\\)</a>" source)
        (let ((uri (match-string-no-properties 1 source))
              (caption (match-string-no-properties 2 source)))
          (setq source caption)
          (add-text-properties
           0 (length source)
           `(mouse-face highlight
                        face gnu-social-uri-face
                        source ,source)
           source)))

      ;; save last update time
      (setq gnu-social-timeline-last-update created-at)

      (mapcar
       (lambda (sym)
         `(,sym . ,(symbol-value sym)))
       '(id text source created-at truncated favorited
            in-reply-to-status-id
            in-reply-to-screen-name
            conversation-id
            user-id user-name user-screen-name user-location
            user-description
            user-profile-image-url
            user-profile-url
            user-url
            user-protected)))))

(defun gnu-social-xmltree-to-status (xmltree)
  (mapcar #'gnu-social-status-to-status-datum
          ;; quirk to treat difference between xml.el in Emacs21 and Emacs22
          ;; On Emacs22, there may be blank strings
          (let ((ret nil) (statuses (reverse (cddr (car xmltree)))))
            (while statuses
              (when (consp (car statuses))
                (setq ret (cons (car statuses) ret)))
              (setq statuses (cdr statuses)))
            ret)))

(defun gnu-social-percent-encode (str &optional coding-system)
  (if (equal (gn-account-auth-mode gn-current-account) "oauth")
      (oauth-hexify-string str)
    (when (or (null coding-system)
              (not (coding-system-p coding-system)))
      (setq coding-system 'utf-8))
    (mapconcat
     (lambda (c)
       (cond
        ((gnu-social-url-reserved-p c)
         (char-to-string c))
        ((eq c ? ) "+")
        (t (format "%%%x" c))))
     (encode-coding-string str coding-system)
     "")))

(defun gnu-social-url-reserved-p (ch)
  (or (and (<= ?A ch) (<= ch ?z))
      (and (<= ?0 ch) (<= ch ?9))
      (eq ?. ch)
      (eq ?- ch)
      (eq ?_ ch)
      (eq ?~ ch)))

(defun gnu-social-decode-html-entities (encoded-str)
  (if encoded-str
      (let ((cursor 0)
            (found-at nil)
            (result '()))
        (while (setq found-at
                     (string-match "&\\(#\\([0-9]+\\)\\|\\([A-Za-z]+\\)\\);"
                                   encoded-str cursor))
          (when (> found-at cursor)
            (push (substring encoded-str cursor found-at) result))
          (let ((number-entity (match-string-no-properties 2 encoded-str))
                (letter-entity (match-string-no-properties 3 encoded-str)))
            (cond (number-entity
                   (push
                    (char-to-string
                     (gnu-social-ucs-to-char
                      (string-to-number number-entity))) result))
                  (letter-entity
                   (cond ((string= "gt" letter-entity) (push ">" result))
                         ((string= "lt" letter-entity) (push "<" result))
                         (t (push "?" result))))
                  (t (push "?" result)))
            (setq cursor (match-end 0))))
        (push (substring encoded-str cursor) result)
        (apply 'concat (nreverse result)))
    ""))

(defun gnu-social-timer-action (func)
  (let ((buf (get-buffer gnu-social-buffer)))
    (if (null buf)
        (gnu-social-stop)
      (funcall func))))

(defun gnu-social-update-status-if-not-blank (method-class method status &optional parameters reply-to-id)
  (if (string-match "^\\s-*\\(?:@[-_a-z0-9]+\\)?\\s-*$" status)
      nil
    (if (equal method-class "statuses")
        (gnu-social-http-post method-class method
                            `(("status" . ,status)
                              ("source" . ,gnu-social-source)
                              ,@(if (assoc `media parameters)
                                    `(("media" . ,(cdr (assoc `media parameters))))
                                  nil)
                              ,@(if reply-to-id
                                    `(("in_reply_to_status_id"
                                       . ,(number-to-string reply-to-id))))))
      (gnu-social-http-post method-class method
                          `(("text" . ,status)
                            ("user" . ,parameters) ;must change this to parse parameters as list
                            ("source" . ,gnu-social-source))))

    t))

(defvar gnu-social-update-status-edit-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'gnu-social-update-status-from-edit-buffer-send)
    (define-key map (kbd "C-c C-k") 'gnu-social-update-status-from-edit-buffer-cancel)
    map))

(define-derived-mode gnu-social-update-status-edit-mode text-mode "GNU Social Status Edit"
  (use-local-map gnu-social-update-status-edit-map))

(defvar gnu-social-update-status-edit-method-class)
(defvar gnu-social-update-status-edit-method)
(defvar gnu-social-update-status-edit-parameters)
(defvar gnu-social-update-status-edit-reply-to-id)

(defun gnu-social-update-status-edit-in-edit-buffer (init-str msgtype method-class method parameters &optional reply-to-id)
  (let ((buf (get-buffer-create "*gnu-social-status-update-edit*")))
    (pop-to-buffer buf)
    (with-current-buffer buf
      (when (not (equal major-mode 'gnu-social-update-status-edit-mode))
        (progn
          (gnu-social-update-status-edit-mode)
          (when gnu-social-soft-wrap-status
            (when (fboundp 'visual-line-mode)
              (visual-line-mode t)))
          (make-local-variable 'gnu-social-update-status-edit-method-class)
          (make-local-variable 'gnu-social-update-status-edit-method)
          (make-local-variable 'gnu-social-update-status-edit-parameters)
          (make-local-variable 'gnu-social-update-status-edit-reply-to-id)
          (if (> (length parameters) 0)
              (setq mode-line-format
                    (cons (format "%s(%s) (%%i/%s) " msgtype parameters
                                  (gn-account-textlimit gn-current-account))
                          mode-line-format))
            t (setq mode-line-format
                    (cons (format "%s (%%i/%s) " msgtype (gn-account-textlimit gn-current-account))
                          mode-line-format)))))
      (setq gnu-social-update-status-edit-method-class method-class)
      (setq gnu-social-update-status-edit-method method)
      (setq gnu-social-update-status-edit-parameters parameters)
      (setq gnu-social-update-status-edit-reply-to-id reply-to-id)
      (message gnu-social-update-status-edit-method-class)
      (insert init-str)
      (message "Type C-c C-c to post status update (C-c C-k to cancel)."))))

(defcustom gnu-social-minibuffer-length-prompt-style nil
  "The preferred style of counting characters in the minibuffer.
prompt; \"Down\" counts down from (gn-account-textlimit gn-current-account); \"Up\" counts
  up from 0"
  :type '(choice (const :tag "Down" nil)
                 (const :tag "Up" t))
  :group 'gnu-social-mode)

(defun gnu-social-show-minibuffer-length (&optional beg end len)
  "Show the number of characters in minibuffer."
  (when (minibuffer-window-active-p (selected-window))
    (let* ((status-len (- (buffer-size) (minibuffer-prompt-width)))
           (mes (format "%d" (if gnu-social-minibuffer-length-prompt-style
                                 status-len
                               (- (gn-account-textlimit gn-current-account) status-len)))))
      (if (<= 23 emacs-major-version)
          (minibuffer-message mes) ; Emacs23 or later
        (minibuffer-message (concat " (" mes ")"))))))

(defun gnu-social-setup-minibuffer ()
  (gnu-social-show-minibuffer-length)
  (add-hook 'post-command-hook 'gnu-social-show-minibuffer-length t t))

(defun gnu-social-finish-minibuffer ()
  (remove-hook 'post-command-hook 'gnu-social-show-minibuffer-length t))

(defun gnu-social-update-status (update-input-method &optional init-str reply-to-id method-class method parameters)
  (gnu-social-create-account)
  (when (null init-str) (setq init-str ""))
  (let ((msgtype "")
        (status init-str)
        (not-posted-p t)
        (user nil)
        (map minibuffer-local-map)
        (minibuffer-message-timeout nil))
    (define-key map (kbd "<f4>") 'gnu-social-shortenurl-replace-at-point)
    (if (null method-class)
        (progn (setq msgtype "Status")
               (setq method-class "statuses")
               (setq method "update"))
      (progn (setq msgtype "Direct message")
             (setq method-class "direct_messages")
             (setq parameters (read-from-minibuffer "To user: " user nil nil nil nil t))
             (setq method "new")))
    (cond ((eq update-input-method 'minibuffer)
           (add-hook 'minibuffer-setup-hook 'gnu-social-setup-minibuffer t)
           (add-hook 'minibuffer-exit-hook 'gnu-social-finish-minibuffer t)
           (unwind-protect
               (while not-posted-p
                 (setq status (read-from-minibuffer (concat msgtype ": ") status nil nil nil nil t))
                 (while (< (+ (gn-account-textlimit gn-current-account) 1) (length status))
                   (setq status (read-from-minibuffer (format (concat msgtype "(%d): ")
                                                              (- (gn-account-textlimit gn-current-account) (length status)))
                                                      status nil nil nil nil t)))
                 (setq not-posted-p
                       (not (gnu-social-update-status-if-not-blank method-class method status parameters reply-to-id))))
             (remove-hook 'minibuffer-setup-hook 'gnu-social-setup-minibuffer)
             (remove-hook 'minibuffer-exit-hook 'gnu-social-finish-minibuffer)))
          ((eq update-input-method 'edit-buffer)
           (gnu-social-update-status-edit-in-edit-buffer init-str msgtype method-class method parameters reply-to-id))
          (t (error "Unknown update-input-method in gnu-social-update-status: %S" update-input-method)))))

(defun gnu-social-update-status-from-edit-buffer-send ()
  (interactive)
  (with-current-buffer "*gnu-social-status-update-edit*"
    (if longlines-mode
        (longlines-encode-region (point-min) (point-max)))
    (let* ((status (buffer-substring-no-properties (point-min) (point-max)))
           (status-len (length status)))
      (if (< (gn-account-textlimit gn-current-account) status-len)
          (message (format "Beyond %s chars.  Remove %d chars."
                           (gn-account-textlimit gn-current-account)
                           (- status-len (gn-account-textlimit gn-current-account))))
        (if (gnu-social-update-status-if-not-blank gnu-social-update-status-edit-method-class
                                                 gnu-social-update-status-edit-method status
                                                 gnu-social-update-status-edit-parameters
                                                 gnu-social-update-status-edit-reply-to-id)
            (progn
              (erase-buffer)
              (bury-buffer))
          (message "Update failed!"))))))

(defun gnu-social-update-status-from-minibuffer (&optional init-str method-class method parameters reply-to-id)
  (interactive)
  (gnu-social-update-status 'minibuffer init-str method-class method parameters reply-to-id))

(defun gnu-social-update-status-from-edit-buffer (&optional init-str method-class method parameters)
  (interactive)
  (gnu-social-update-status 'edit-buffer init-str method-class method parameters))

(defun gnu-social-update-status-from-edit-buffer-cancel ()
  (interactive)
  (when (or (not gnu-social-update-status-edit-confirm-cancellation)
            (yes-or-no-p
             "Really cancel editing this status message (any changes will be lost)?"))
    (erase-buffer)
    (bury-buffer)))

(defun gnu-social-update-status-from-region (beg end)
  (interactive "r")
  (when (> (- end beg) (gn-account-textlimit gn-current-account))
    (setq end (+ beg (gn-account-textlimit gn-current-account))))
  (when (< (- end beg) (gn-account-textlimit gn-current-account))
    (setq beg (+ end (gn-account-textlimit gn-current-account))))
  (gnu-social-update-status-if-not-blank "statuses" "update" (buffer-substring beg end)))

(defun gnu-social-update-status-with-media (attachment &optional init-str method-class method parameters reply-to-id)
  (interactive "f")
  (gnu-social-update-status 'minibuffer nil reply-to-id nil nil `((media . ,(insert-file-contents-literally attachment)))))

(defun gnu-social-tinyurl-unjson-google (result)
  "Gets only the URL from JSON URL tinyfying service results.

Google's shortening service, goo.gl, returns shortened URLs as a
JSON dictionary. This function retrieves only the URL value from
this dictionary, only if gnu-social-urlshortening-service is 'google."
  (if (eq gnu-social-urlshortening-service 'google)
      (cdr (assoc 'short_url (json-read-from-string result)))
    result))

(defun gnu-social-ur1ca-get (api longurl)
  "Shortens url through ur1.ca free service 'as in freedom'."
  (let* ((apiurl (if (string-match "\\(http://.*\\)\\?\\(.*=\\)" api)
                  (match-string 1 api)))
         (url-request-method "POST")
         (url-request-extra-headers
          '(("Content-Type" . "application/x-www-form-urlencoded")))
         (datavar (match-string 2 api))
         (url-request-data (concat datavar (url-hexify-string longurl)))
         (buffer (url-retrieve-synchronously apiurl)))
    (with-current-buffer buffer
      (goto-char (point-min))
      (prog1
          (if (string-equal gnu-social-urlshortening-service "ur1ca")
              (if (search-forward-regexp "Your .* is: .*>\\(http://ur1.ca/[0-9A-Za-z].*\\)</a>" nil t)
                  (match-string-no-properties 1)
                (error "URL shortening service failed: %s" longurl))
            (if (search-forward-regexp "\\(http://[0-9A-Za-z/].*\\)" nil t)
                (match-string-no-properties 1)
              (error "URL shortening service failed: %s" longurl)))
        (kill-buffer buffer)))))

(defun gnu-social-shortenurl-get (longurl)
  "Shortens url through a url shortening service."
  (let ((api (cdr (assoc gnu-social-urlshortening-service
                         gnu-social-urlshortening-services-map))))
    (unless api
      (error "`gnu-social-urlshortening-service' was invalid.  try one of %s"
             (mapconcat (lambda (x)
                          (symbol-name (car x)))
                        gnu-social-urlshortening-services-map ", ")
             "."))
    (if longurl
        (if (not (eq gnu-social-urlshortening-service 'google))
            (gnu-social-ur1ca-get api longurl)
          (let ((buffer (url-retrieve-synchronously (concat api longurl))))
            (with-current-buffer buffer
              (goto-char (point-min))
              (prog1
                  (gnu-social-tinyurl-unjson-google
                   (if (search-forward-regexp "\n\r?\n\\([^\n\r]*\\)" nil t)
                       (match-string-no-properties 1)
                     (error "URL shortening service failed: %s" longurl)))
                (kill-buffer buffer))))
          nil))))

(defun gnu-social-shortenurl-replace-at-point ()
  "Replace the url at point with a tiny version."
  (interactive)
  (let ((url-bounds (bounds-of-thing-at-point 'url)))
    (when url-bounds
      (let ((url (gnu-social-shortenurl-get (thing-at-point 'url))))
        (when url
          (save-restriction
            (narrow-to-region (car url-bounds) (cdr url-bounds))
            (delete-region (point-min) (point-max))
            (insert url)))))))

(defun gnu-social-expand-replace-at-point ()
  "Replace the url at point with a tiny version."
  (interactive)
  (let ((url-bounds (bounds-of-thing-at-point 'url))
        (original-url (thing-at-point 'url)))
    (when url-bounds
      (message (concat "Expanding url: " original-url))
      (let ((uri (gnu-social-expand-shorturl original-url)))
        (when uri
          (set-buffer (get-buffer gnu-social-buffer))
          (save-restriction
            (setq buffer-read-only nil)
            (narrow-to-region (car url-bounds) (cdr url-bounds))
            (delete-region (point-min) (point-max))
            (add-text-properties 0 (length uri)
                                 `(mouse-face highlight
                                              face gnu-social-uri-face
                                              uri ,uri
                                              uri-in-text ,uri) uri)
            (insert uri)
            (message (concat "Expanded Short URL " original-url "to Long URL: " uri))
            (setq buffer-read-only t)))))))

(defun gnu-social-expand-shorturl (url)
  "Return the redirected URL, or the original url if not found."
  (let ((temp-buf (get-buffer-create "*HTTP headers*")))
    (set-buffer temp-buf)
    (erase-buffer)
    (goto-char 0)
    (let*
        ((url (replace-regexp-in-string "http://" "" url))
         (host (substring url 0 (string-match "/" url)))
         (file (if (string-match "/" url)
                   (substring url (string-match "/" url))
                 "/"))
         (tcp-connection (open-network-stream "GNU Social URLExpand"
                                              temp-buf host 80))
         (request (concat "GET http://" url " HTTP/1.1\r\n"
                          "Host:" host "\r\n"
                          "User-Agent: " (gnu-social-user-agent) "\r\n"
                          "Authorization: None\r\n"
                          "Accept-Charset: utf-8;q=0.7,*;q=0.7\r\n\r\n")))
      (set-marker (process-mark tcp-connection) (point-min))
      (set-process-sentinel tcp-connection 'gnu-social-http-headers-sentinel)
      (process-send-string tcp-connection request)
      (sit-for 2)
      (let ((location (gnu-social-get-location-from-header (concat "http://" host file) tcp-connection)))
        (delete-process tcp-connection)
        (kill-buffer temp-buf)
        location))))

(defun gnu-social-http-headers-sentinel (process string)
  "Process the results from the efine network connection."

  )

(defun gnu-social-get-location-from-header (url process)
  "Parse HTTP header."
  (let ((buffer)
        (headers)
        (location))
    (setq buffer (get-buffer-create "*HTTP headers*"))
    (set-buffer buffer)
    (goto-char 0)
    (setq location
          (if (search-forward-regexp "^Location: \\(http://.*?\\)\r?$" nil t)
              (match-string-no-properties 1)
            url))
    (replace-regexp-in-string "\r" "" location)))

;;;
;;; Commands
;;;

(defun gnu-social-start (&optional action)
  (interactive)
  (when (null action)
    (setq action #'gnu-social-current-timeline))
  (if gnu-social-timer
      nil
    (setq gnu-social-timer
          (run-at-time "0 sec"
                       gnu-social-timer-interval
                       #'gnu-social-timer-action action)))
  (set 'gnu-social-active-mode t)
  (gnu-social-update-mode-line))

(defun gnu-social-stop ()
  "Stop Current network activitiy (if any) and the reload-timer."
  (interactive)
  (when (get-buffer-process gnu-social-http-buffer)
    (delete-process gnu-social-http-buffer)
    (kill-buffer gnu-social-http-buffer))
  (setq gnu-social-method (gn-account-last-timeline-retrieved gn-current-account))
  (gnu-social-set-mode-string nil)
  (and gnu-social-timer
       (cancel-timer gnu-social-timer))
  (setq gnu-social-timer nil)
  (set 'gnu-social-active-mode nil)
  (gnu-social-update-mode-line))

(defun gnu-social-switch-account ()
  "Update the current account and reload the default timeline."
  (interactive)
  (let ((current-account (member* gn-current-account gnu-social-accounts)))
    (setq gn-current-account
          (if (cdr current-account)
              (cadr current-account)
            (car gnu-social-accounts))
          gnu-social-timeline-data nil)
    (gnu-social-current-timeline)))

(defun gnu-social-get-timeline (&optional server parameters)
  (setq gnu-social-remote-server server)
  (unless parameters (setq parameters `(("count" . ,(int-to-string gnu-social-statuses-count)))))
  (when (not (eq (gn-account-last-timeline-retrieved gn-current-account) gnu-social-method))
    (setq gnu-social-timeline-last-update nil
          gnu-social-timeline-data nil))
  (let ((buf (get-buffer gnu-social-buffer)))
    (if (not buf)
        (gnu-social-stop)
      (progn
        (when (not gnu-social-method)
          (setq gnu-social-method "friends_timeline"))
        (gnu-social-http-get (or server (gn-account-server gn-current-account))
                           (if server "none"
                             (gn-account-auth-mode gn-current-account))
                           gnu-social-method-class gnu-social-method parameters))))
  (gnu-social-get-icons))

(defun gnu-social-get-icons ()
  "Retrieve icons if icon-mode is active."
  (if gnu-social-icon-mode
      (if (and gnu-social-image-stack window-system)
          (let ((proc
                 (apply
                  #'start-process
                  "wget-images"
                  nil
                  "wget"
                  (format "--directory-prefix=%s" gnu-social-tmp-dir)
                  "--no-clobber"
                  "--quiet"
                  gnu-social-image-stack)))
            (set-process-sentinel
             proc
             (lambda (proc stat)
               (clear-image-cache)
               ))))))

(defun gnu-social-friends-timeline ()
  (interactive)
  (setq gnu-social-method "friends_timeline")
  (setq gnu-social-method-class "statuses")
  (gnu-social-get-timeline))

(defun gnu-social-replies-timeline ()
  (interactive)
  (setq gnu-social-method "replies")
  (setq gnu-social-method-class "statuses")
  (gnu-social-get-timeline))

;; (defun gnu-social-direct-messages-timeline ()
;;   (interactive)
;;   (setq gnu-social-method "direct_messages")
;;   (setq gnu-social-method-class "none")
;;   (gnu-social-get-timeline))

(defun gnu-social-public-timeline ()
  (interactive)
  (setq gnu-social-method "public_timeline")
  (setq gnu-social-method-class "statuses")
  (gnu-social-get-timeline))

(defun gnu-social-group-timeline (&optional group)
  (interactive)
  (unless group
    (setq group (read-from-minibuffer "Group: " nil nil nil nil nil t)))
  (setq gnu-social-method-class "statusnet/groups")
  (if (string-equal group "")
      (setq gnu-social-method "timeline")
    (setq gnu-social-method (concat "timeline/" group)))
  (gnu-social-get-timeline))

(defun gnu-social-tag-timeline (&optional tag)
  (interactive)
  (unless tag
    (setq tag (read-from-minibuffer "Tag: " nil nil nil nil nil t)))
  (setq gnu-social-method-class "statusnet/tags")
  (if (string-equal tag "")
      (setq gnu-social-method "timeline")
    (setq gnu-social-method (concat "timeline/" tag)))
  (gnu-social-get-timeline))

(defun gnu-social-user-timeline (&optional from-user)
  "Retrieve user timeline given its username.

FROM-USER can be an empty string (\"\") meaning that you want to retrieve your own timeline.
If nil, will ask for username in minibuffer."
  (interactive)
  (unless from-user
    (setq from-user (read-from-minibuffer "User [Empty for mine]: "
                                          nil nil nil nil nil t)))
  (setq gnu-social-method-class "statuses")
  (if (string-equal from-user "")
      (setq gnu-social-method "user_timeline")
    (setq gnu-social-method (concat "user_timeline/" from-user)))
  (gnu-social-get-timeline)
  )

(defun gnu-social-conversation-timeline ()
  (interactive)
  (let ((context-id (get-text-property (point) 'conversation-id)))
    (setq gnu-social-method-class "statusnet")
    (setq gnu-social-method (concat "conversation/" context-id)))
  (gnu-social-get-timeline gnu-social-remote-server))

(defun gnu-social-remote-user-timeline ()
  (interactive)
  (let* ((profile (get-text-property (point) 'profile-url))
         (username (get-text-property (point) 'username))
                                        ;Strip potential trailing slashes and username references from profile url to get the server url
         (server-url (if (string-match (concat "/?\\(" username "\\)?/?$") profile)
                         (replace-match "" nil t profile)
                       profile))
         (server (if (string-match "^https?://" server-url)
                     (replace-match "" nil t server-url)
                   server-url)))
    (setq gnu-social-method-class "statuses")
    (setq gnu-social-method (concat "user_timeline/" username))
    (gnu-social-get-timeline server)))

(defun gnu-social-current-timeline (&optional count)
  "Load newer notices, with an argument load older notices, and with a numeric argument load that number of notices."
  (interactive "P")
  (if (> gnu-social-new-dents-count 0)
      (gnu-social-render-pending-dents)
    (gnu-social-get-timeline
     gnu-social-remote-server
     (if count
         (cons `("count" .
                 ,(int-to-string
                   (if (listp count) gnu-social-statuses-count count)))
               (if (listp count)
                   `(("max_id" .
                      ,(int-to-string
                        (- (assoc-default 'id (car (last gnu-social-timeline-data))) 1))))
                 ()))
       nil))))

(defun gnu-social-update-status-interactive ()
  (interactive)
  (gnu-social-update-status gnu-social-update-status-method))

(defun gnu-social-direct-message-interactive ()
  (interactive)
  (gnu-social-update-status gnu-social-update-status-method nil nil "direct_messages" "new"))

(defun gnu-social-erase-old-statuses ()
  (interactive)
  (setq gnu-social-timeline-data nil)
  (when (not (gn-account-last-timeline-retrieved gn-current-account))
    (setf (gn-account-last-timeline-retrieved gn-current-account) gnu-social-method))
  (gnu-social-http-get (gn-account-server gn-current-account) (gn-account-auth-mode gn-current-account)
                     "statuses" (gn-account-last-timeline-retrieved gn-current-account)))

(defun gnu-social-click ()
  (interactive)
  (let ((uri (get-text-property (point) 'uri)))
    (when uri (browse-url uri))))

(defun gnu-social-enter ()
  (interactive)
  (let ((username (get-text-property (point) 'username))
        (id (get-text-property (point) 'id))
        (uri (get-text-property (point) 'uri))
        (group (get-text-property (point) 'group))
        (tag (get-text-property (point) 'tag)))
    (if group (gnu-social-group-timeline group)
      (if tag (gnu-social-tag-timeline tag)
        (if uri (browse-url uri)
          (if username
              (gnu-social-update-status gnu-social-update-status-method
                                      (concat "@" username " ") id)))))))

(defun gnu-social-next-link nil
  (interactive)
  (goto-char (next-single-property-change (point) 'uri))
  (when (not (get-text-property (point) 'uri))
    (goto-char (next-single-property-change (point) 'uri))))

(defun gnu-social-prev-link nil
  (interactive)
  (goto-char (previous-single-property-change (point) 'uri))
  (when (not (get-text-property (point) 'uri))
    (goto-char (previous-single-property-change (point) 'uri))))

(defun gnu-social-follow (&optional remove)
  (interactive)
  (let ((username (get-text-property (point) 'username))
        (method (if remove "destroy" "create"))
        (message (if remove "unfollowing" "following")))
    (unless username
      (setq username (read-from-minibuffer "user: ")))
    (if (> (length username) 0)
        (when (y-or-n-p (format "%s %s? " message username))
          (gnu-social-http-post (format "friendships/%s" method) username)
          (message (format "Now %s %s" message username)))
      (message "No user selected"))))

(defun gnu-social-unfollow ()
  (interactive)
  (gnu-social-follow t))

(defun gnu-social-group-join (&optional leaving)
  "Simple functions to join/leave a group we are visiting."
  (setq gnu-social-method-class "statusnet/groups")
  (string-match "\\([^\\]*\\)\\(/.*\\)" gnu-social-method)
  (let ((group-method (replace-match
                       (if leaving "leave"
                         "join") nil nil gnu-social-method 1)))
    (gnu-social-http-post gnu-social-method-class group-method nil)))

(defun gnu-social-group-leave ()
  (gnu-social-group-join t))

(defun gnu-social-favorite ()
  (interactive)
  (when (y-or-n-p "Do you want to favor this notice? ")
    (let ((id (get-text-property (point) 'id)))
      (gnu-social-http-post "favorites/create" (number-to-string id))
      (message "Notice saved as favorite"))))

(defun gnu-social-repeat ()
  (interactive)
  (when (y-or-n-p "Do you want to repeat this notice? ")
    (let ((id (get-text-property (point) 'id)))
      (gnu-social-http-post "statuses/retweet" (number-to-string id))
      (message "Notice repeated"))))

(defun gnu-social-view-user-page ()
  (interactive)
  (let ((uri (get-text-property (point) 'uri)))
    (when uri (browse-url uri))))

(defun gnu-social-redent ()
  (interactive)
  (let ((username (get-text-property (point) 'username))
        (id (get-text-property (point) 'id))
        (text (replace-regexp-in-string "!\\(\\b\\)" "#\\1" (get-text-property (point) 'text))))
    (when username
      (gnu-social-update-status gnu-social-update-status-method
                              (concat gnu-social-redent-format " @" username ": " text) id))))

(defun gnu-social-reply-to-user (all)
  "Open a minibuffer initialized to type a reply to the notice at point.
With no argument, populate with the username of the author of the notice.
With an argument, populate with the usernames of the author and any usernames mentioned in the notice."
  (interactive "P")
  (let ((username (get-text-property (point) 'username))
        (notice-text (get-text-property (point) 'text))
        (id (get-text-property (point) 'id))
        (usernames nil)
        (usernames-string ""))
    (when all
      (setq usernames
            (mapcar (lambda (string)
                      (when (and (char-equal (aref string 0) ?@)
                                 (memq-face gnu-social-uri-face
                                            (get-text-property 2 'face string)))
                        (concat string " ")))
                    (split-string notice-text))))
    (when username (setq usernames (cons (concat "@" username " ") usernames)))
    (setq usernames (delete-dups usernames))
    (setq usernames (delete (concat "@" (gn-account-username gn-current-account) " ") usernames))
    (setq usernames-string (apply 'concat usernames))
    (gnu-social-update-status gnu-social-update-status-method usernames-string id)))

(defun gnu-social-reply-to-all ()
  (interactive)
  (gnu-social-reply-to-user t))

(defun gnu-social-get-password ()
  (or (gn-account-password gn-current-account)
      (setf (gn-account-password gn-current-account) (read-passwd "password: "))))

(defun gnu-social-goto-next-status ()
  "Go to next status."
  (interactive)
  (let ((pos))
    (setq pos (gnu-social-get-next-username-face-pos (point)))
    (if pos
        (goto-char pos)
      (progn (goto-char (buffer-end 1)) (message "End of status.")))))

(defun gnu-social-toggle-highlight (&optional arg)
  "Toggle the highlighting of entry at 'point'.
With no arg or prefix, toggle the highlighting of the entry at 'point'.
With arg (or prefix, if interactive), highlight the current entry and
un-highlight all other entries."
  (interactive "P")
  (let ((id (get-text-property (point) 'id)))
    (setq gnu-social-highlighted-entries
          (if arg (list id)
            (if (memq id gnu-social-highlighted-entries)
                (delq id gnu-social-highlighted-entries)
              (cons id gnu-social-highlighted-entries)))))
  (gnu-social-render-timeline))

(defun memq-face (face property)
  "Check whether FACE is present in PROPERTY."
  (if (listp property)
      (memq face property)
    (eq property face)))

(defun gnu-social-get-next-username-face-pos (pos &optional object)
  "Returns the position of the next username after POS, or nil when end of string or buffer is reached."
  (interactive "P")
  (let ((prop))
    (catch 'not-found
      (while (and pos (not (memq-face gnu-social-username-face prop)))
        (setq pos (next-single-property-change pos 'face object))
        (when (eq pos nil) (throw 'not-found nil))
        (setq prop (get-text-property pos 'face object)))
      pos)))

(defun gnu-social-goto-previous-status ()
  "Go to previous status."
  (interactive)
  (let ((pos))
    (setq pos (gnu-social-get-previous-username-face-pos (point)))
    (if pos
        (goto-char pos)
      (message "Start of status."))))

(defun gnu-social-get-previous-username-face-pos (pos &optional object)
  "Returns the position of the previous username before POS, or nil when start of string or buffer is reached."
  (interactive)
  (let ((prop))
    (catch 'not-found
      (while (and pos (not (memq-face gnu-social-username-face prop)))
        (setq pos (previous-single-property-change pos 'face object))
        (when (eq pos nil) (throw 'not-found nil))
        (setq prop (get-text-property pos 'face object)))
      pos)))

(defun gnu-social-goto-next-status-of-user ()
  "Go to next status of user."
  (interactive)
  (let ((user-name (gnu-social-get-username-at-pos (point)))
        (pos (gnu-social-get-next-username-face-pos (point))))
    (while (and (not (eq pos nil))
                (not (equal (gnu-social-get-username-at-pos pos) user-name)))
      (setq pos (gnu-social-get-next-username-face-pos pos)))
    (if pos
        (goto-char pos)
      (if user-name
          (message "End of %s's status." user-name)
        (message "Invalid user-name.")))))

(defun gnu-social-goto-previous-status-of-user ()
  "Go to previous status of user."
  (interactive)
  (let ((user-name (gnu-social-get-username-at-pos (point)))
        (pos (gnu-social-get-previous-username-face-pos (point))))
    (while (and (not (eq pos nil))
                (not (equal (gnu-social-get-username-at-pos pos) user-name)))
      (setq pos (gnu-social-get-previous-username-face-pos pos)))
    (if pos
        (goto-char pos)
      (if user-name
          (message "Start of %s's status." user-name)
        (message "Invalid user-name.")))))

(defun gnu-social-get-username-at-pos (pos)
  (let ((start-pos pos)
        (end-pos))
    (catch 'not-found
      (while (memq-face gnu-social-username-face (get-text-property start-pos 'face))
        (setq start-pos (1- start-pos))
        (when (or (eq start-pos nil) (eq start-pos 0)) (throw 'not-found nil)))
      (setq start-pos (1+ start-pos))
      (setq end-pos (next-single-property-change pos 'face))
      (buffer-substring start-pos end-pos))))

(defun assoc-workaround (tag array)
  "Workaround odd semi-associative array returned by url-http."
  (or (assoc tag array)
      (and (equal tag (car array))
           (cadr array))))

(defun gnu-social-get-status-url (id)
  "Generate status URL."
  (format "https://%s/notice/%s" (gn-account-server gn-current-account) id))

(defun gnu-social-get-context-url (id)
  "Generate status URL."
  (format "https://%s/conversation/%s" (gn-account-server gn-current-account) id))

(defun gnu-social-retrieve-configuration ()
  "Retrieve the configuration for the current gnu-social server."
  (gnu-social-http-get (gn-account-server gn-current-account) (gn-account-auth-mode gn-current-account)
                     "gnu-social" "config" nil 'gnu-social-http-get-config-sentinel))

(defun gnu-social-http-get-config-sentinel
  (&optional status method-class method parameters success-message)
  "Process configuration page retrieved from gnu-social server."
  (let ((error-object (assoc-workaround :error status)))
    (unless error-object
      (let* ((body (gnu-social-get-response-body))
             (site (xml-get-children (car body) 'site))
             (textlimit (xml-get-children (car site) 'textlimit))
             (textlimit-value (caddar textlimit)))
        (when (> (string-to-number textlimit-value) 0)
          (setf (gn-account-textlimit gn-current-account) (string-to-number textlimit-value))))))
  (gnu-social-start))

(defun gnu-social-get-config-url ()
  "Generate configuration URL."
  (format "http://%s/api/gnu-social/config.xml" (gn-account-server gn-current-account)))

;; Icons
;;; ACTIVE/INACTIVE
(defconst gnu-social-active-indicator-image
  (when (image-type-available-p 'xpm)
    '(image :type xpm
            :ascent center
            :data
            "/* XPM */
static char * gnu-social_xpm[] = {
\"16 16 14 1\",
\"  c None\",
\".	c #8F0000\",
\"+	c #AB4040\",
\"@	c #D59F9F\",
\"#	c #E3BFBF\",
\"$	c #CE8F8F\",
\"%	c #C78080\",
\"&	c #FFFFFF\",
\"*	c #B96060\",
\"=	c #DCAFAF\",
\"-	c #C07070\",
\";	c #F1DFDF\",
\">	c #961010\",
\",	c #9D2020\",
\"    .......     \",
\"   .........    \",
\"  ...........   \",
\" ....+@#$+....  \",
\"....%&&&&&*.... \",
\"...+&&&&&&&+... \",
\"...=&&&&&&&$... \",
\"...#&&&&&&&#... \",
\"...=&&&&&&&@... \",
\"...*&&&&&&&-... \",
\"....@&&&&&&=... \",
\" ....-#&#$;&>.. \",
\" ..........,>.. \",
\"  ............. \",
\"    ............\",
\"       .      ..\"};")))

(defconst gnu-social-inactive-indicator-image
  (when (image-type-available-p 'xpm)
    '(image :type xpm
            :ascent center
            :data
            "/* XPM */
static char * gnu_social_off_xpm[] = {
\"16 16 13 1\",
\"  g None\",
\".	g #5B5B5B\",
\"+	g #8D8D8D\",
\"@	g #D6D6D6\",
\"#	g #EFEFEF\",
\"$	g #C9C9C9\",
\"%	g #BEBEBE\",
\"&	g #FFFFFF\",
\"*	g #A5A5A5\",
\"=	g #E3E3E3\",
\"-	g #B2B2B2\",
\";	g #676767\",
\">	g #747474\",
\"    .......     \",
\"   .........    \",
\"  ...........   \",
\" ....+@#$+....  \",
\"....%&&&&&*.... \",
\"...+&&&&&&&+... \",
\"...=&&&&&&&$... \",
\"...#&&&&&&&#... \",
\"...=&&&&&&&@... \",
\"...*&&&&&&&-... \",
\"....@&&&&&&=... \",
\" ....-#&#$&&;.. \",
\" ..........>;.. \",
\"  ............. \",
\"    ............\",
\"       .      ..\"};")))

(let ((props
       (when (display-mouse-p)
         `(local-map
           ,(purecopy (make-mode-line-mouse-map
                       'mouse-2 #'gnu-social-toggle-activate-buffer))
           help-echo "mouse-2 toggles automatic updates"))))
  (defconst gnu-social-modeline-active
    (if gnu-social-active-indicator-image
        (apply 'propertize " "
               `(display ,gnu-social-active-indicator-image ,@props))
      " "))
  (defconst gnu-social-modeline-inactive
    (if gnu-social-inactive-indicator-image
        (apply 'propertize "INACTIVE"
               `(display ,gnu-social-inactive-indicator-image ,@props))
      "INACTIVE")))

(defun gnu-social-toggle-activate-buffer ()
  (interactive)
  (setq gnu-social-active-mode (not gnu-social-active-mode))
  (if (not gnu-social-active-mode)
      (gnu-social-stop)
    (gnu-social-start)))

(defun gnu-social-mode-line-buffer-identification ()
  (if gnu-social-active-mode
      gnu-social-modeline-active
    gnu-social-modeline-inactive))

(defun gnu-social-update-mode-line ()
  "Update mode line."
  (force-mode-line-update))

;;;###autoload
(defun gnu-social ()
  "Start gnu-social-mode."
  (interactive)
  (gnu-social-mode))

(provide 'gnu-social-mode)
(add-hook 'gnu-social-load-hook 'gnu-social-autoload-oauth)
(run-hooks 'gnu-social-load-hook)

;;; gnu-social-mode.el ends here
