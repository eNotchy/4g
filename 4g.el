;;; 4g.el --- Browse imageboards in Org-Mode -*- lexical-binding: t; -*-

;; Version: 0.9.3
;; URL: https://github.com/eNotchy/4g
;; Package-Requires: ((emacs "30.1"))
;; Keywords: comm extensions hypermedia org
;; SPDX-License-Identifier: GPL-3.0
;;
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see
;; <http://www.gnu.org/licenses/>.

;;; Commentary:

;; 4g lets you browse image and discussion boards in Org mode.
;; Currently, only 4chan is supported.

;;; Code:

;;; --- Dependencies -----------------------------------------------------------

(require 'url)
(require 'url-parse)
(require 'json)
(require 'subr-x)
(require 'seq)
(require 'map)
(require 'rx)
(require 'org)
(require 'dom nil 'noerror)

;;; --- Buffer-local Variables -------------------------------------------------

(defvar-local 4g--sitename "4chan"
  "Buffer-local variable for the site of the current buffer.")

(defvar-local 4g--boardname nil
  "Buffer-local variable for the board of the current buffer.")

(defvar-local 4g--threadno nil
  "Buffer-local variable for the thread number of the current buffer.")

(defvar-local 4g--parent-buffer nil
  "Buffer-local variable for the buffer from which the current buffer was linked.")

;;; --- Global Variables -------------------------------------------------------

(defvar 4g--boards nil
  "Cache for parsed data from 4chan's boards.json API endpoint.")

(defvar 4g--catalogs (make-hash-table :test 'equal)
  "Cache for parsed data from 4chan's catalog.json API endpoints.
Uses Board names (strings) as keys, returns a list of threads.")

;;; --- Global Constants -------------------------------------------------------

(defconst 4g-version
  "0.9.3")
  ;; (concat "DEV-" (format-time-string "%s")))

(defconst 4g--media-types
  (map-pairs
   '(".webm" :video
     ".mp4"  :video
     ".png"  :image
     ".jpg"  :image
     ".jpeg" :image
     ".gif"  :image
     ".webp" :image)))

(defconst 4g--4chan-icdn "https://i.4cdn.org")
(defconst 4g--4chan-acdn "https://a.4cdn.org")

;;; --- Helpers ----------------------------------------------------------------

(defun 4g--string->keyword (s)
  (intern (concat ":" s)))

(defun 4g--keyword-name (kw)
  "Return KW's name without the leading colon, or signal if not a keyword."
  (declare (pure t) (side-effect-free t))
  (cl-check-type kw (satisfies keywordp))
  (substring (symbol-name kw) 1))

(defun 4g--sanitize-md5 (base64-hash)
  (base64url-encode-string (base64-decode-string base64-hash)))

(defun 4g--existing-dir-or-nil (dir)
  (and (file-directory-p dir) dir))

(defun 4g--set-local-var (sym val)
  (set (make-local-variable sym) val))

(defun 4g--browse-url (url)
  (if (getenv "TERMUX_VERSION") ;; We're on Termux
      (let ((process-connection-type nil)) ;; use pipe, stay fully async
        (start-process "termux-open-url" nil "termux-open-url" url)
        (message "Opening via termux-open-url: %s" url))
    (browse-url url)))

;;; --- User Options -----------------------------------------------------------

(defgroup 4g nil
  "View 4chan in Org-Mode."
  :group 'applications
  :prefix "4g-")

;;;###autoload
(defcustom 4g-quicknav nil
  "If nil, enable `4g-quicknav-mode' in newly created 4g buffers."
  :type 'boolean
  :safe #'booleanp)

;;;###autoload
(defcustom 4g-doom-workarounds (featurep 'doom)
  "When non-nil, apply workarounds specific to Doom Emacs."
  :type 'boolean
  :group '4g
  :safe #'booleanp)

;;;###autoload
(defcustom 4g-greentext-rendering :verbatim
  "How 4chan greentext (quotes) is rendered in Org.

Values:
- :verbatim — Enclose the text in Org =verbatim= markup.
- :quote    — Wrap the text in a QUOTE block (#+begin_quote ... #+end_quote).
- :as-is    — Insert the text as-is, without additional Org markup."
  :type '(choice
          (const :tag "Org =verbatim=" :verbatim)
          (const :tag "Org QUOTE block" :quote)
          (const :tag "Plain text (as-is)" :as-is))
  :group '4g
  :safe (lambda (v) (memq v '(:verbatim :quote :as-is))))

;;;###autoload
(defcustom 4g-spoiler-images t
  "When non-nil, hide thumbnails and names of images marked as spoilers."
  :type 'boolean
  :group '4g
  :safe #'booleanp)

;;;###autoload
(defcustom 4g-timestamp-format " [%F %T]"
  "Timestamp format for posts.

Either a format string for `format-time-string', or the keyword
`:4chan` to use a 4chan-style timestamp (in 4chan's timezone).

Examples:
- \" [%F %T]\" -> \" [2025-12-24 12:34:56]\" (an inactive Org timestamp)
- \" [%F %T %Z]\" includes the timezone.
- \" %c\" your locale's date and time format."
  :group '4g
  :type '(choice
          (const  :tag "4chan-native" :4chan)
          (string :tag "Custom format string"))
  :safe (lambda (v) (or (eq v :4chan) (stringp v))))

;;;###autoload
(defcustom 4g-lang-guess-function #'4g--guess-code-language
  "Function used to guess the Org src language from a CODE string.

This function is called as:
  (funcall 4g-lang-guess-function CODE)

It must return either a language name string suitable for #+BEGIN_SRC,
e.g. clojure, cpp, emacs-lisp etc. or nil if it cannot determine the language.

When this function returns nil, the block's language defaults to `elisp`."
  :type '(choice
          (const    :tag "Built-in detector (4g--guess-code-language)"
                    4g--guess-code-language)
          (function :tag "Custom function: (CODE) → language-or-nil"))
  :group '4g
  :safe #'functionp)

;;;###autoload
(defcustom 4g-videolink-command nil
  "How to open video links.

Allowed values:

- nil
  Try a few popular video players (e.g. mpv/vlc) and fall back to
  web browser if none are available.

- `browse-url'
  Always open the link in the default web browser.

- A string
  A command name (e.g. \"vlc\") to invoke for playing the link.

- A list of strings
  A command with arguments (e.g. \='(\"mpv\" \"--loop\")).

- A function
  A function called with one argument: the URL string.

When a command (string or list) is used, it is expected to accept the URL
as its final argument."
  :type '(choice
          (const :tag "Auto (players then browser)" nil)
          (const :tag "Web browser (`browse-url')" browse-url)
          (string :tag "Command (single string)")
          (cons :tag "Command with arguments (list of strings)"
                (string :tag "Command")
                (repeat :tag "Args" (string :tag "Arg")))
          (function :tag "Function (called with URL string)"))
  :group '4g)

;;; --- User Directories -------------------------------------------------------

(defcustom 4g-directory
  (file-name-as-directory
   (expand-file-name "4g"
    (cond
     ((bound-and-true-p no-littering-var-directory))
     ((and (fboundp 'xdg-cache-home) (xdg-cache-home)))
     ((featurep 'haiku) "~/config/cache/")
     (t                 "~/.cache/"))))
  "Directory where 4g stores thumbnails and other cache data.
Set to nil to disable thumbnail downloads entirely."
  :type '(choice
          (const     :tag "Disable thumbnail downloads" nil)
          (directory :tag "Cache directory"))
  :group '4g
  :safe #'string-or-null-p
  :set (lambda (sym val)
         (set-default sym (and val (file-name-as-directory val)))))

(defcustom 4g-fallback-download-directory
  (or (getenv "XDG_DOWNLOAD_DIR")
      (and (file-directory-p "~/Downloads/") "~/Downloads/"))
  "Directory for downloading files other than videos and pictures."
  :type '(choice (const :tag "Prompt for every file" nil) (directory))
  :group '4g
  :safe #'string-or-null-p
  :set (lambda (sym val)
         (set-default sym (and val (file-name-as-directory val)))))

(defcustom 4g-image-download-directory
  (or (getenv "XDG_PICTURES_DIR")
      (4g--existing-dir-or-nil "~/Pictures/")
      4g-fallback-download-directory)
  "Directory for saving full-size images."
  :type '(choice (const :tag "Prompt for every file" nil) (directory))
  :group '4g
  :safe #'string-or-null-p
  :set (lambda (sym val)
         (set-default sym (and val (file-name-as-directory val)))))

(defcustom 4g-video-download-directory
  (or (getenv "XDG_VIDEOS_DIR")
      (4g--existing-dir-or-nil "~/Videos/")
      4g-image-download-directory
      4g-fallback-download-directory)
  "Directory for saving videos."
  :type '(choice (const :tag "Prompt for every file" nil) (directory))
  :group '4g
  :safe #'string-or-null-p
  :set (lambda (sym val)
         (set-default sym (and val (file-name-as-directory val)))))

;;; --- Protips ----------------------------------------------------------------

(cl-defun 4g--protipmsg-search-functions (&key relevance)
  (if relevance
      (eq relevance :catalog)
    (if (fboundp 'consult-org-heading)
        (substitute-command-keys
         "Use \\[consult-org-heading] to search through thread titles.")
      (substitute-command-keys
       "Use \\[occur] or \\[org-occur] to search the catalog."))))

(cl-defun 4g--protipmsg-no-dir-set (&key relevance)
  (if relevance
      (and (null 4g-directory) (display-graphic-p))
    (concat
     "[[elisp:(customize-option-other-window '4g-directory)][4g-directory]]"
     " not set. Thumbnails will not be shown.")))

(cl-defun 4g--protipmsg-security-headsup (&key relevance)
  (if relevance
      t
    (concat
     "4chan users can technically include links"
     " with malicious elisp code in their posts."
     "\nDo not execute elisp links before reading"
     " and understanding what they do.")))

;; TODO quicknav and other keymap protips
;; (cl-defun 4g--quicknav-keymap (&key relevance)
;;   (if relevance
;;       (bound-and-true-p 4g-quicknav-mode)
;;     "

;; "))
;; "It also enables access to all functions in ~4g-prefix-map~ via a single keystroke:

;; | Key | Action                   |
;; |-----+--------------------------|
;; | b | Board List               |
;; | c | Catalog                  |
;; | t | Thread                   |
;; | r | Refresh                  |
;; | e | View in External Browser |
;; "
(defconst 4g--protips
  '(4g--protipmsg-no-dir-set
    4g--protipmsg-search-functions
    4g--protipmsg-security-headsup))

;;; --- Protip logic -----------------------------------------------------------

(defun 4g--protip-random (relevance tips)
  "Ask all TIPS for their RELEVANCE and call one of them.
This function is WIP."
  (thread-last tips
    (seq-filter (lambda (tip) (funcall tip :relevance relevance)))
    (seq-random-elt)
    (funcall)))

;;; --- Networking and Cache ---------------------------------------------------

(defun 4g--fetch-json (url)
  "Retrieve URL and parse JSON.
Return objects as PLISTs and arrays as LISTs.  Error on failure."
  (message "Fetching %s..." url)
  (let ((buf (url-retrieve-synchronously url t t 10)))
    (unless buf
      (error "Failed to fetch %s" url))
    (unwind-protect
        (with-current-buffer buf
          ;; Skip HTTP headers
          (goto-char (point-min))
          (re-search-forward "\r?\n\r?\n" nil 'move)
          (json-parse-buffer :object-type  'plist
                             :array-type   'array
                             :null-object  nil
                             :false-object nil))
      (kill-buffer buf))))

(cl-defun 4g--fetch-boards (&key url)
  (when-let*
      ((url     (or url (format "%s/boards.json" 4g--4chan-acdn)))
       (res     (4g--fetch-json url))
       (boards  (map-elt res :boards)))
    (setq 4g--boards boards)))

(defun 4g--download-callback (status dest)
  (if-let (err (plist-get status :error))
      (error "Failed to download %s: %s" dest err)
    (make-directory (file-name-directory dest) t)
    (goto-char (point-min))
    (re-search-forward "\r?\n\r?\n")
    (let ((coding-system-for-write 'no-conversion))
      (write-region (point) (point-max) dest))
    dest))

(defun 4g--download-file-async (url dest)
  (url-retrieve url #'4g--download-callback (list dest) nil t))

(defun 4g--download-file-sync (url dest)
  (let ((buf (url-retrieve-synchronously url t t 30)))
    (unless buf
      (error "Failed to fetch %s" url))
    (unwind-protect
        (with-current-buffer buf
          (if (and (fboundp 'url-http-parse-response)
                   (<= 200 (funcall 'url-http-parse-response) 299))
              (4g--download-callback nil dest)
            (message "failed to download %s" dest)))
      (kill-buffer buf))))

(defun 4g--download-new (url dest)
  "Download URL if DEST doesn't exist.  Return DEST on success, nil on failure."
  (cl-check-type url string)
  (cl-check-type dest string)
  (if (file-exists-p dest)
      :already-exists
    (4g--download-file-sync url dest)))

;;; --- Thumbnail download -----------------------------------------------------

(defun 4g--thumbnail-file (MD5)
  "Return the relative path from `4g-directory' to the thumbnail of MD5."
  (let* ((sanitized (4g--sanitize-md5 MD5))
         ;; Having too many files in a folder can severely impact performance,
         ;; so we split our thumbnail collection up into 64 different folders.
         (prefix    (seq-take sanitized 1))
         (filename  (concat sanitized ".jpg")))
    (file-name-concat "thumbnails" prefix filename)))

(defun 4g--get-thumbnail-url+path (board post)
  (when-let*
      (4g-directory
       (tim (map-elt post :tim))
       (MD5 (map-elt post :md5))
       (thm (4g--thumbnail-file MD5))
       (dir (file-name-concat 4g-directory thm))
       (url (format "%s/%s/%ss.jpg" 4g--4chan-icdn board tim)))
    (cons url dir)))

(defun 4g--redisplay-images ()
  (when (eq major-mode #'org-mode)
    (org-redisplay-inline-images)))

(cl-defun 4g--download-files
    (url->dest &key (retries 5) (retry-delay 3) callback)
  "Download URL->DEST mapping on a background thread.
On failure, retry after RETRY-DELAY seconds (default 3)
up to RETRIES (default 5).
When done, run CALLBACK (if non-nil) on the main thread.
CALLBACK takes no arguments."
  (make-thread
   (lambda ()
     (while (and (setq url->dest (map-remove #'4g--download-new url->dest))
                 (natnump (cl-decf retries)))
       (sleep-for retry-delay))
     ;; Run on main thread
     (when callback
       (run-at-time 0 nil callback)))
   "4g-download-thread"))

;;; --- Opening Files ----------------------------------------------------------

(defun 4g--play-video (url)
  (if (functionp 4g-videolink-command)
      (funcall 4g-videolink-command url)
    (if-let* ((app (or 4g-videolink-command
                       (seq-some
                        #'executable-find
                        ["mpv" "smplayer" "ffplay" "vlc"
                         "totem" "xine" "xdg-open"])))
              (cmd (cond
                    ;; Command
                    ((stringp app)
                     (list app url))
                    ;; Custom command with args
                    ((listp app)
                     (append app (list url)))
                    (t (user-error "Invalid value for 4g-videolink command: %s"
                                   4g-videolink-command)))))
          (make-process :name "videoplayer"
                        :command cmd
                        :noquery t)
      ;; No videoplayer found
      (4g--browse-url url))))

;;; --- Download Links ---------------------------------------------------------

(defun 4g--gen-filename (post)
  (map-let (:filename :ext :md5) post
    (thread-first (string-replace " " "_" filename)
      ;; Filenames can be 255 bytes. Subtract 10 for the extension and hash
      (string-limit 245 nil 'utf-8)
      (decode-coding-string 'utf-8 t)
      (concat "." (string-limit (4g--sanitize-md5 md5) 4))
      (concat ext))))

(defun 4g--decide-download-dir (filename)
  (pcase (map-elt 4g--media-types (url-file-extension filename))
    (:video 4g-video-download-directory)
    (:image 4g-image-download-directory)
    (_      4g-fallback-download-directory)))

(cl-defun 4g--gen-download-link
    (board post &key (site 4g--sitename))
  (map-let (:ext :tim :fsize) post
    (let* ((id   (format "%s/%s/%s%s" site board tim ext))
           (dir  (4g--decide-download-dir ext))
           (dest (when dir
                   (thread-last post
                     (4g--gen-filename)
                     ;; Remove the line below if you don't want board subfolders.
                     (file-name-concat board)
                     (file-name-concat dir)
                     (expand-file-name)
                     (concat "::"))))
           ;; when dest is nil, 4g prompts for the download location
           (link (concat "4g-download:" id dest))
           (size (file-size-human-readable-iec (or fsize 0))))
      (format "[[%s][Download (%s)]]" link size))))

;;; --- UI ---------------------------------------------------------------------

(defun 4g--picker (prompt choices)
  "Show PROMPT (a string) and present CHOICES (an alist) with descriptions."
  (let* ((cands (map-keys choices))
         (ann   (lambda (s)
                  (when-let ((desc (map-elt choices s)))
                    (concat "  " desc))))   ; shown dimly to the right
         (completion-extra-properties `(:annotation-function ,ann)))
    (completing-read prompt cands)))

(defun 4g--prompt-board ()
  (thread-last 4g--boards
    (seq-map (lambda (b)
               (map-let (:board :title) b
                 (cons board title))))
    (4g--picker "board: ")))

(defun 4g--prompt-thread (board)
  (thread-last
    (map-elt 4g--catalogs board)
    (seq-map (lambda (thd)
               (format "%s %s"
                       (map-elt thd :no)
                       (4g--thread-title thd))))
    (completing-read "thread: ")
    ;; Converts the number, ignores everything after it:
    (string-to-number)))

;; The above version works well with `fido-vertical-mode' (included in Emacs 28)
;; If you do not use fido, you might prefer this implementation:
;;
;; (defun 4g--picker (prompt candidates)
;;   (let* ((cands  (map-keys candidates))
;;          (lookup (lambda (x)  (map-elt candidates x)))
;;          (affix  (lambda (xs) (seq-map lookup xs)))
;;          (completion-extra-properties `(:affixation-function ,affix)))
;;     (completing-read prompt cands)))
;;
;; (defun 4g--prompt-board ()
;;   (thread-last 4g--boards
;;     (seq-mapcat
;;      (lambda (b)
;;        (map-let (:board :title) b
;;          (list board
;;                (list (format "%-7s - %s" (format "/%s/" board) title) "" "")))))
;;     (map-pairs)
;;     (4g--picker "board: ")))

;;; --- Org Link Types ---------------------------------------------------------

(defun 4g--goto-post (no)
  (org-link-open-from-string (format "[[%s]]" no))
  (recenter))

(defun 4g-follow-link (url _)
  (seq-let (site board thread post) (string-split url "/")
    (cond
     ((string= site "4chan")
      (cond
       (thread (4g-thread board thread :postno post :parent (current-buffer)))
       (board  (4g-catalog board :parent (current-buffer)))
       (t      (4g-board-list :parent (current-buffer)))))
     ((user-error "Unknown 4g site: %s" url)))))

(defun 4g--follow-open-link (url _)
  (seq-let (_site board tim ext) (string-split url "/")
    (let ((src        (format "%s/%s/%s%s" 4g--4chan-icdn board tim ext))
          (media-type (map-elt 4g--media-types ext)))
      (pcase media-type
        (:video (4g--play-video src))
        ((and :image (guard (display-graphic-p))) (eww-browse-url src))
        (_ (4g--browse-url src))))))

(defun 4g--follow-download-link (link _)
  (seq-let (url &rest destlist) (string-split link "::")
    (seq-let (_site board tim ext) (string-split url "/")
      (if (not (and board tim ext))
          (error "Invalid 4g-download link: %s" link)
        (let* ((filename (concat tim ext))
               (src (format "%s/%s/%s" 4g--4chan-icdn board filename))
               (dest (if destlist
                         (string-join destlist "::")
                       (read-file-name "Save file as: "
                                       (4g--decide-download-dir ext)))))
          (4g--download-file-async src dest))))))

(org-link-set-parameters "4g"          :follow #'4g-follow-link)
(org-link-set-parameters "4g-open"     :follow #'4g--follow-open-link)
(org-link-set-parameters "4g-download" :follow #'4g--follow-download-link)

;;; --- Org-Protocol (browser -> 4g) -------------------------------------------

(defun 4g--parse-4chan-url (url)
  "Parse a 4chan URL and return a plist describing what to open.
Returns a plist like:
  (:kind thread  :board \"g\" :threadno \"123\" :postno \"456\")
or
  (:kind catalog :board \"g\")
or nil if URL is not recognized."
  (cl-check-type url string)
  (cl-assert (string-prefix-p "http" url) t)
  (let* ((u    (url-generic-parse-url url))
         (host (url-host u))
         (path (url-filename u))
         (frag (url-target u))) ;; fragment without '#'
    ;; REVIEW dispatch based on hostname to support other sites
    (cl-assert (equal host "boards.4chan.org") t "Unknown host")
    (seq-let (board kind threadno) (split-string path "/" t)
      (cond
       ;; Not on any board
       ((null board) (list :kind 'site))
       ;; /<board>/thread/<threadno>...
       ((and threadno (string= kind "thread"))
        (list :kind   'thread
              :board  board
              :threadno threadno
              ;; Evaluates to nil if frag is nil:
              :postno (string-remove-prefix "p" frag)))
       ;; /<board>/catalog
       ((string= kind "catalog")
        (list :kind 'catalog :board board))
       ;; /<board>/ (treat as catalog)
       (t
        (list :kind 'catalog :board board))))))

(defun 4g-org-protocol-open (info)
  "Org-protocol handler: open a 4chan URL in 4g.
INFO is a plist passed by org-protocol, e.g. (:url \"...\")."
  (map-let (:url :link :ref) info
    (setq url (or url link ref))
    (cl-assert (stringp url) t "Org-protocol 4g: missing required parameter: url")
    (map-let (:kind :board :threadno :postno) (4g--parse-4chan-url url)
      (pcase kind
        ('site    (4g-board-list))
        ('catalog (4g-catalog board))
        ('thread  (4g-thread board threadno :postno postno))
        (_        (user-error "Org-protocol 4g: unsupported URL: %s" url))))))

;;;###autoload
(defun 4g-org-protocol-setup ()
  "Register the org-protocol handler `org-protocol://4g?url=...` for 4g.
This also ensures an Emacs server is running (needed for emacsclient
to deliver org-protocol URLs)."
  (interactive)
  ;; Ensure emacsclient has a server to talk to.
  (require 'server)
  (unless (and (fboundp 'server-running-p)
               (server-running-p))
    ;; REVIEW I do not use Windows and have not tested this
    (when (eq 'windows-nt system-type)
      (setopt server-use-tcp t))
    (server-start))
  ;; Register the org-protocol handler.
  (require 'org-protocol)
  (when (boundp 'org-protocol-protocol-alist) ;; keeps flycheck happy
    (add-to-list 'org-protocol-protocol-alist
                 '("4g"
                   :protocol "4g"
                   :function 4g-org-protocol-open
                   :kill-client t))))

;;; --- Text Processing (Regex-based) ------------------------------------------

(defun 4g--emojify-country-code (country)
  "Return the flag emoji for COUNTRY.
Example: \"US\" → \"🇺🇸\".
Accepts a two-letter string (case-insensitive).
Returns nil if CODE is not 2 characters long or not between A and Z.
Implementation detail: each A–Z maps to Regional Indicator Symbols
U+1F1E6..U+1F1FF, which, when paired, render as a flag emoji."
  (declare (pure t) (side-effect-free t))
  (cl-check-type country string)
  (let ((code (upcase (string-trim country))))
    (when (= (length code) 2)
        (let* ((a (aref code 0)) (b (aref code 1))
               (base 127462)) ;; U+1F1E6
          (when (and (>= a ?A) (<= a ?Z) (>= b ?A) (<= b ?Z))
              (string (+ base (- a ?A))
                      (+ base (- b ?A))))))))

(cl-defun 4g--find-all-matches (string regexp &key (group 0) (start 0))
  "Find all matches of REGEXP in STRING and return them as a list."
  (let ((position start)
        matches)
    ;; REVIEW add safety check for zero-width matches?
    (while (string-match regexp string position)
      (push (match-string group string) matches)
      (setq position (match-end 0)))
    (nreverse matches))) ;; Reverse the list to preserve the order of matches

(defconst 4g--html-linebreaks
  '(("<br>"  . "\n")
    ("<wbr>" . "")))

(defconst 4g--html-literals
  (map-pairs
   '("&quot;" "\""
     "&#039;" "'"
     "&apos;" "'"
     "&nbsp;" " "
     "&lt;"   "<"
     "&gt;"   ">"
     ;; IMPORTANT: &amp; last to avoid double-unescaping
     "&amp;"  "&"))
  "HTML entities and their replacements, in the order in which they get replaced.
These are all I've seen on 4chan.  Open a PR for any others you come across.")

(defconst 4g--simple-tags
  (map-pairs
   '("[eqn]"  "$$"
     "[/eqn]" "$$"
     "<s>"    "\n:SPOILER:\n"
     "</s>"   "\n:END:\n")))

(defconst 4g--rx-code-with-lang
  (rx "<pre" (+ space) "class=\"prettyprint\">"
      (or (* (not alphabetic)) "rem" "REM")
      (* whitespace) "lang:" (* whitespace) (group (+ alphabetic) "<br>")
      (group (*? printing))
      "</pre>")
  "Match a code block, capturing the body.")

(defconst 4g--rx-code
  (rx "<pre" (+ space) "class=\"prettyprint\">"
      (group (*? printing))
      "</pre>")
  "Match a code block, capturing the body.")

(defconst 4g--rx-quotelink-inpage
  (rx "<a href=\"#p" (group (+ digit)) (*? printing) "</a>")
  "Match an in-page quote link like <a href=\"#p123\">…</a> capturing the id.")

(defconst 4g--rx-other-link
  (rx "<a href=\""
      (group "htt" (+? printing))
      "\""
      (*? "</a>")))

(defconst 4g--rx-quotelink-crossthread
  (rx "<a href=\"/"
      (group (+? (not "/")))      ; board (group 1)
      "/thread/"
      (group (+ digit)) "#p" ; thread number (group 2)
      (group (+ digit))      ; post no (group 3)
      (+? printing)
      "</a>")
  "Match a cross-thread quote link and capture the board, threadno and postno.")

(defconst 4g--rx-greentext
  (rx "<span class=\"quote\">"
      (group (*? print))
      "</span>")
  "Match greentext spans and capture the contents.")

(defconst 4g--rx-adjacent-quote-block
  (rx "#+end_quote"
      (+ (or whitespace "<br>"))
      "#+begin_quote"))

(defun 4g--replace-regexes (text pairs)
  (seq-reduce (lambda (s p) (replace-regexp-in-string (car p) (cdr p) s t))
              pairs
              text))

(defun 4g--replace-literals (text pairs)
  (seq-reduce (lambda (s p) (string-replace (car p) (cdr p) s))
              pairs
              text))

(defun 4g--orgify-com-regex (com)
  "Convert board-style HTML comment COM into Org markup."
  (cl-check-type com string t)
  ;; --- order matters! ---
  (thread-first com
    (4g--replace-regexes
     (map-pairs
      (list
       4g--rx-code-with-lang        "\n#+begin_src \\1\\2<br>#+end_src\n"
       4g--rx-code                  "\n#+begin_src elisp<br>\\1<br>#+end_src\n"
       4g--rx-quotelink-crossthread "[[4g:4chan/\\1/\\2/\\3][>>\\3 (🔗 thread \\2)]]"
       4g--rx-quotelink-inpage      "[[\\1][>>\\1]]"
       4g--rx-other-link            "[[\\1]]"
       4g--rx-greentext             (pcase 4g-greentext-rendering
                                      (:quote "\n#+begin_quote\n\\1\n#+end_quote")
                                      (:as-is "\\1")
                                      (:verbatim "=\\1="))
       ;; TODO factor this out to a different function to prevent unnecessary calls
       4g--rx-adjacent-quote-block  (if (eq :quote 4g-greentext-rendering) "" "\\0"))))
    (4g--replace-literals 4g--simple-tags)
    (4g--replace-literals 4g--html-linebreaks)
    (4g--replace-literals 4g--html-literals)))

;;; --- Text Processing (DOM-based) --------------------------------------------

(defconst 4g--rx-quotelink-crossthread-href
  (rx string-start "/"
      (group (+? (not "/"))) ; board (group 1)
      "/thread/"
      (group (+ digit)) "#p" ; thread number (group 2)
      (group (+ digit))      ; post no (group 3)
      string-end)
  "Match a cross-thread quote link and capture the board, threadno and postno.")

(defconst 4g--rx-quotelink-crossboard-href
  (rx string-start "//boards.4chan.org/"
      (group (+? (not "/"))) ; board (group 1)
      "/thread/"
      (group (+ digit)) "#p" ; thread number (group 2)
      (group (+ digit))      ; post no (group 3)
      string-end)
  "Match a cross-board quote link and capture the board, threadno and postno.")

(defconst 4g--rx-code-lang
  (rx string-start
      (or (* (not alphabetic)) "rem" "REM")
      (* whitespace) "lang:" (* whitespace) (group (+ alphanumeric)))
  "Match a lang declaration in a code block, capturing the language.")

(defconst 4g--lang-patterns
  ;; REVIEW: Using Treesitter would be another option to recognize languages,
  ;;    but that relies on grammars being installed.
  ;; SLOP WARNING: Most of these are LLM-generated and untested.
  (map-pairs
   (list
    "(ns\\s-+\\w"                       :clojure
    "(defn\\s-+\\w"                     :clojure
    "#\\({\\|_\\)"                      :clojure
    "(->>?"                             :clojure
    "(defun\\s-+\\w"                    :emacs-lisp
    "(setq\\s-+\\w"                     :emacs-lisp
    "(use-package\\s-+\\w"              :emacs-lisp
    "(interactive)"                     :emacs-lisp
    "(define\\s-*(\\w"                  :scheme
    "(lambda\\s-*\("                    :scheme
    "\\bletrec\\b\\|\\bcond\\b"         :scheme
    "(defmacro\\s-+\\w"                 :lisp
    "(defparameter\\s-+\\w"             :lisp
    "(setf\\s-+\\w"                     :lisp
    "(let\\*?\\s-*\("                   :lisp
    "(loop\\s+"                         :lisp
    ")))))"                             :emacs-lisp
    "\.unwrap()"                        :rust
    "public\s+class"                    :java
    "\\bstatic\\s+void\\s+main"         :java
    "\\bSystem\\.out\\.println"         :java
    "\\bimport\\s+java\\."              :java
    "^#include"                         :cpp
    "^#define"                          :cpp
    "\\b(int|void)\\s+main\\s-*("       :cpp
    "\\bstd::"                          :cpp
    "using\\s+namespace\\s+std"         :cpp
    "\\btemplate\\s-*<"                 :cpp
    "^#!.*/\\(ba\\|z\\)?sh\\b"          :shell
    "\\bfor\\s+\\w+\\s+in\\b.*;\\s*do"  :shell
    "\\bthen\\b.*\\bfi\\b"              :shell
    "^echo\\s+"                         :shell
    "^sudo\\s+"                         :shell
    "\\s--\\w+"                         :shell
    "\\s--?\\w+s*=.*$"                  :shell
    "\\bfunction\\s+\\w+\\s-*("         :javascript
    ")\\s*=>\\s*("                      :javascript
    "\\bconsole\\.log("                 :javascript
    "^func\s+\\w+"                      :go
    "\\bimport\\s+.+\\s+from\\b"        :javascript
    "\\bexport\\s+default\\b"           :javascript
    "\\bdocument\\."                    :javascript
    "<!DOCTYPE\\s-+html"                :html
    "<\\s-*\\(html\\|head\\|body\\|div\\|span\\|a\\|p\\|h[1-6]\\)\\b" :html
    "</\\s-*\\w+\\s*>"                  :html
    "\\.[-_a-zA-Z0-9]+\\s-*{[^}]*:[^}]*}" :css
    "\\b[a-z-]+\\s-*:\\s*[^;]+;"        :css
    "\\bpackage\\s+\\w+\\b"             :go
    "\\bfmt\\.[A-Za-z]+\\s-*\("         :go
    "\\bgo\\s+\\w+\\b"                  :go
    "\\busing\\s+System\\b"             :csharp
    "\\bnamespace\\s+\\w+"              :csharp
    "\\bpublic\\s+\\(class\\|struct\\)\\s+\\w+" :csharp
    "\\bstatic\\s+void\\s+Main\\s-*\("  :csharp
    "\\bConsole\\.WriteLine\\s-*\("     :csharp
    "\\bList<\\w+>"                     :csharp
    "\\bmodule\\s+\\w+\\s+where\\b"     :haskell
    "\\bimport\\s+\\w+"                 :haskell
    "^[[:space:]]*\\w+\\s*::\\s*[^:]+$" :haskell
    "\\bwhere\\b"                       :haskell
    "\\bpub\\s+fn\\s+\\w+\\s-*"         :zig
    "\\bconst\\s+\\w+\\s*=\\s*"         :zig
    "@import\\s-*(\\s-*\"std\"\\s-*)"   :zig
    "\\b\\(defer\\|errdefer\\|try\\)\\b" :zig
    "\\bfn\\s+\\w+\\s-*\("              :rust
    "\\blet\\s+mut\\b"                  :rust
    "\\bprintln!"                       :rust
    "\\buse\\s+\\w\\w*::"               :rust
    "^struct\\b"                        :c
    "\\bprintf\\s-*\("                  :cpp
    "\\b\\(trait\\|impl\\)\\b"          :rust))
  "Mapping of regexps to lang names for use in `4g--guess-code-language'.")

(defun 4g--lang-from-hashbang (code)
  (when (and (stringp code)
             (string-match (rx string-start
                               "#!/usr/bin/env"
                               (+ whitespace)
                               (group (+ alnum)))
                           (string-trim-left code)))
   (match-string 1 code)))

(defun 4g--guess-code-language (code)
  "Guess the language of CODE.  Return nil if no language was recognized."
  (when-let ((s code)
             (case-fold-search t))
    (or (4g--lang-from-hashbang s)
        (map-some (lambda (regexp lang)
                    (when (string-match-p regexp s)
                      (4g--keyword-name lang)))
                  4g--lang-patterns))))

(defun 4g--com->nodes (html)
  "Convert a 4chan :com HTML fragment to dom nodes."
  (with-temp-buffer  ;; REVIEW: replace with 'with-work-buffer' for Emacs 31+
    ;; Wrap in a harmless container so fragments parse cleanly.
    (insert "<div class=\"__frag\">"
            (or html "")
            "</div>")
    (thread-first
      (libxml-parse-html-region (point-min) (point-max))
      (dom-by-class "__frag")
      (dom-children))))

(defun 4g--orgify-com-dom (html)
  "Convert a 4chan :com HTML fragment to Org using libxml DOM."
  (mapconcat #'4g--node->org (4g--com->nodes html)))

(defun 4g--node->org (node)
  "DOM NODE -> Org string."
  (if (stringp node)
      node
    (let ((tag      (dom-tag node))
          (cls      (dom-attr node 'class))
          (children (mapconcat #'4g--node->org (dom-children node))))
      (cond
       ((eq tag 'br) "\n")

       ;; <wbr> is parsed as a node with children for some reason.
       ((eq tag 'wbr) children)

       ;; >greentext
       ;; TODO: merge adjacent #+quote blocks
       ((and (eq      tag 'span)
             (string= cls "quote"))
        (format (pcase 4g-greentext-rendering
                  (:verbatim "=%s=")
                  (:quote    "#+begin_quote\n%s\n#+end_quote")
                  (:as-is    "%s"))
                (string-trim-right children " ")))

       ;; [code] blocks
       ((and (eq      tag 'pre)
             (string= cls "prettyprint"))
        (let* ((lang (if (string-match 4g--rx-code-lang children)
                         (match-string 1 children)
                       (or (funcall 4g-lang-guess-function children)
                           "elisp")))
               (code (string-trim (replace-regexp-in-string 4g--rx-code-lang "" children))))
          (format "\n#+begin_src %s\n%s\n#+end_src\n" lang code)))

       ;; Quote links: >>123 or >>>/g/123
       ((string= cls "quotelink")
        (let ((href (dom-attr node 'href)))
          (cond
           ;; in-page quotelinks like >>12346
           ((string-prefix-p "#p" href)
            (format "[[%s][%s]]" (string-remove-prefix "#p" href) children))

           ;; links to posts in other threads on the same board
           ((string-match 4g--rx-quotelink-crossthread-href href)
            (format "[[4g:4chan/%s/%s/%s][%s (thread %s)]]"
                    (match-string 1 href)
                    (match-string 2 href)
                    (match-string 3 href)
                    children
                    (match-string 2 href)))

           ;; cross-board links like >>>/g/123456
           ((string-match 4g--rx-quotelink-crossboard-href href)
            (format "[[4g:4chan/%s/%s/%s][%s]]"
                    (match-string 1 href)
                    (match-string 2 href)
                    (match-string 3 href)
                    children))

           ;; catalog links like >>>/g/emacs
           ;; TODO: handle search terms with 'org-occur'
           ((string-match (rx string-start ">>>/"
                              (group (+? (not "/")))
                              "/")
                          children)
            (format "[[4g:4chan/%s][%s]]"
                    (match-string 1 children)
                    children)))))

       ;; Dead links. TODO: Point to archives.
       ((string= cls "deadlink")
        (concat "+" children "+"))

       ;; [spoiler] tags
       ((eq tag 's)
        (format "\n:SPOILER:\n%s\n:END:\n" children))

       ;; TODO: /sci/ and /qst/ stuff

       (t children)))))

;; libxml2 support is required for HTML parsing

(defalias '4g--orgify-com
  (if (fboundp 'libxml-parse-html-region)
      #'4g--orgify-com-dom
    #'4g--orgify-com-regex))

;;; --- Data processing --------------------------------------------------------

(defun 4g--orgify-links (links)
  "Turn LINKS into a string of backlinks representing future replies to a post."
  (mapconcat (lambda (no) (format "[[%d][>>%d]]" no no)) links " "))

(defun 4g--find-backlinks (posts)
  (thread-last posts
    (seq-map (lambda (post)
               (when-let ((com (map-elt post :com))
                          (no  (map-elt post :no)))
                 (thread-last
                   (4g--find-all-matches com 4g--rx-quotelink-inpage :group 1)
                   (seq-map #'string-to-number)
                   (seq-uniq)
                   (seq-map (lambda (linkee) (list linkee no)))))))
    (apply #'map-merge-with 'hash-table #'append)))

(defun 4g--add-backlinks (posts backlinks)
  (seq-map (lambda (post)
             (if-let ((links (map-elt backlinks (map-elt post :no))))
                 (map-insert post :backlinks links)
               post))
           posts))

(defun 4g--expand-cooldown-info (board)
  (thread-last
    (map-elt board :cooldowns)
    (map-apply (lambda (k period)
                 (cons (intern (concat (symbol-name k) "_cooldown"))
                       period)))
    (map-merge 'alist board)
    (map-remove (lambda (k _) (eq k :cooldowns)))))

;;; --- Footer -----------------------------------------------------------------

(defun 4g--footer (site &optional board threadno)
  "Return a string to be inserted at the very bottom of all 4g buffers.
Supplying THREADNO implies that the footer is for a thread,
BOARD implies a catalog.
If only SITE is supplied, return footer for the board list."
  (concat
   "* Generated by 4g version " 4g-version
   "\n:properties:"
   "\n:custom_id: footer"
   "\n:end:"
   (format "\n[[%s][Top]]"
           (cond
            (threadno)
            (board "PROTIP")
            (site "#top")))
   "\n# Local Variables:"
   "\n# org-return-follows-link: t"
   (when (and board (eq :verbatim 4g-greentext-rendering))
     "\n# org-hide-emphasis-markers: t")
   "\n# end: "))

;;; --- Formatting pieces ------------------------------------------------------

(defun 4g--board-info->org (brd)
  (map-let (:board :title :meta_description) brd
    (concat
     (format "[[4g:4chan/%s][/%s/]] - %s\n" board board title)
     (4g--replace-literals meta_description 4g--html-literals)
     (thread-last brd
       (4g--expand-cooldown-info)
       (map-apply
        (lambda (k v)
         (unless (seq-contains-p [:board :title :meta_description :ws_board] k)
           (format
            "\n| %s | %s |"
            (string-replace "_" " " (4g--keyword-name k))
            (pcase v
              (0 "no")
              (1 "yes")
              ((and n (pred numberp) (guard (> n 1000000)))
               (file-size-human-readable-iec n))
              ((and s (pred stringp))
               s)
              ((and s (pred plistp))
               (map-length s))
              ((and s (pred sequencep))
               (length s))
              (_ v))))))
       (seq-filter #'stringp)
       (string-join)))))

(defun 4g--image-link (board post)
  "Return an Org link to the image attached in POST from BOARD."
  (map-let (:tim :filename :ext :md5 :spoiler) post
    (when (and tim filename ext md5)
      (let*
          ((fname    (if (and spoiler 4g-spoiler-images)
                         (concat "SPOILER" ext)
                       (concat filename ext))) ;; REVIEW sanitization required?
           (thumb    (4g--thumbnail-file md5))
           (id       (format "4chan/%s/%s/%s" board tim ext))
           (flink    (format "4g-open:%s" id))
           (filelink (org-link-make-string flink fname))
           (dl-link  (4g--gen-download-link board post)))
        (concat filelink
                "\n" (when 4g-doom-workarounds "# ") dl-link
                (when (and 4g-directory (display-graphic-p)
                           (not (and spoiler 4g-spoiler-images)))
                           ;; TODO show a spoiler.svg instead of nothing
                    (format "\n[[./%s]]" thumb)))))))

(defun 4g--reply->org (board post)
  "Format one POST on BOARD to Org text."
  (map-let (:name :id :time :no :com :country :trip :capcode :board_flag
            :filedeleted :backlinks :now (:ext file-attached)) post
    (string-trim
     (concat
      name
      (when trip       (format "🪪%s" trip))
      (when capcode    (format "🍀%s" capcode))
      (when country    (4g--emojify-country-code country))
      (when board_flag (format "(%s)" board_flag))
      (when id         (format " 🆔 ~%s~ " id))
      (if (eq 4g-timestamp-format :4chan)
          (concat " " now)
        (format-time-string 4g-timestamp-format time))
      (format " No.<<%s>>" no)
      (cond (filedeleted   "\n[ ❌ *FILE DELETED* ]")
            (file-attached (concat (if 4g-doom-workarounds "\n# " "\n")
                                   (or (4g--image-link board post)
                                       "¯\\_(ツ)_/¯"))))
      (when com       (concat "\n" (4g--orgify-com com)))
      (when backlinks (concat "\n\n💬 " (4g--orgify-links backlinks)))))))

(defun 4g--thread-title (op)
  (let* ((sub   (map-elt op :sub ""))
         (title (if (string-blank-p sub)
                    (string-replace "-" " " (map-elt op :semantic_url ""))
                  sub))
         (final (if (string-blank-p title)
                    "(no title)"
                  title)))
    (4g--replace-literals final 4g--html-literals)))

(cl-defun 4g--op->org (thd &key board flip align-to)
  "Format the OP heading for a thread THD.
FLIP turns around the title and the counters,
ALIGN-TO makes the right-hand element align to the specified column.
Shows `replies' and `images' as /italics/ when limits are set.

To create a link to the thread, call it with a BOARD arg."
  (map-let (:no :replies :images :bumplimit :imagelimit :closed :sticky :archived) thd
    (let* ((title (4g--thread-title thd))
           (l-str  (if board
                       (format "[[4g:4chan/%s/%s][%s]]" board no title)
                     title))
           (lock  (if closed "🔒" ""))
           (pin   (if sticky "📌" ""))
           (old   (if archived "📦" ""))
           (r-str (if (eq 1 bumplimit)
                      (format "/%s/" replies)
                    (format "%s" replies)))
           (i-str (if (eq 1 imagelimit)
                      (format "/%s/" images)
                    (format "%s" images)))
           (space (if (numberp align-to)
                      (propertize " " 'display `(space :align-to ,align-to))
                    " "))
           (stats (format "[%4s R %3s I]%s%s%s"
                          r-str i-str pin lock old)))
      (if flip
          (concat l-str space stats)
        (concat stats space l-str)))))

(defun 4g--catalog-thread->org (board thd)
  "Return a string for one thread THD on BOARD.
Includes an OP line as a ** heading and its last_replies as *** headings."
  (let* ((op-line  (4g--op->org thd :board board))
         (op-reply (4g--reply->org board thd))
         (replystr (when-let ((replies (map-elt thd :last_replies))
                              (replyfn (apply-partially #'4g--reply->org board)))
                     (concat "\n*** " (mapconcat replyfn replies "\n\n*** ")))))
    (concat
     "\n** "  op-line
     "\n*** " op-reply
     replystr)))

;;; --- Keybindings, Menu, and QuickNav ----------------------------------------

(defun 4g--recenter-after (fn &rest args)
  (apply fn args)
  (set-window-start (selected-window) (point)))

;; Prefer named commands over lambdas (cleaner for help)
;; TODO: Make them accept numeric prefix arguments
(defun 4g-qnav-prev-heading () (interactive) (4g--recenter-after #'org-previous-visible-heading 1))
(defun 4g-qnav-next-heading () (interactive) (4g--recenter-after #'org-next-visible-heading 1))
(defun 4g-qnav-next-block   () (interactive) (4g--recenter-after #'org-next-block 1))
(defun 4g-qnav-prev-block   () (interactive) (4g--recenter-after #'org-previous-block 1))
(defun 4g-qnav-next-src     () (interactive) (4g--recenter-after #'org-babel-next-src-block 1))
(defun 4g-qnav-prev-src     () (interactive) (4g--recenter-after #'org-babel-previous-src-block 1))
(defun 4g-qnav-help         () (interactive) (describe-keymap '4g-quicknav-map))

;;;###autoload
(defvar-keymap 4g-prefix-map
  :doc "Prefix keymap for 4g commands."
  :name "4g"
  "b" (cons "Board List"               #'4g-board-list)
  "c" (cons "Catalog"                  #'4g-catalog)
  "t" (cons "Thread"                   #'4g-thread)
  "e" (cons "View in External Browser" #'4g-view-in-browser)
  "r" (cons "Refresh"                  #'4g-refresh)
  "q" #'4g-quicknav-mode
  "p" (cons "Go to Parent Buffer"      #'4g-goto-parent))

(defvar-keymap 4g-quicknav-map
  :doc "Quick navigation keys for 4g buffers (buffer-local via `4g-quicknav-mode')."
  :parent 4g-prefix-map
  "o"   #'org-open-at-point
  "l"   #'org-next-link
  ";"   #'recenter
  "h"   #'org-previous-link
  "k"   #'4g-qnav-prev-heading
  "j"   #'4g-qnav-next-heading
  "]"   #'4g-qnav-next-block
  "["   #'4g-qnav-prev-block
  "}"   #'4g-qnav-next-src
  "{"   #'4g-qnav-prev-src
  "w w" #'org-copy-subtree
  "s"   (cons "Search" search-map)
  "n"   #'next-error
  "N"   #'previous-error
  "/"   #'org-occur
  "'"   #'org-mark-ring-goto
  "m"   #'org-mark-ring-push
  "g g" #'beginning-of-buffer
  "G"   #'end-of-buffer
  "?"   #'4g-qnav-help)

(define-minor-mode 4g-quicknav-mode
  "Single-key navigation in 4g buffers.  Press `q` to disable."
  :init-value nil
  :lighter "4q"
  :keymap 4g-quicknav-map
  (when 4g-quicknav-mode
    (message
     (substitute-command-keys
      "4g quicknav enabled. Press \\[4g-qnav-help] for help, \\[4g-quicknav-mode] to disable quicknav." nil t))))

(defun 4g--maybe-enable-quicknav ()
  "Enable quicknav in newly created 4g buffers, unless disabled."
  (when 4g-quicknav
    (4g-quicknav-mode 1)))

(defvar-keymap 4g-mode-map
  :doc "Keymap for `4g-mode'."
  "C-c 4" (cons "4g" 4g-prefix-map)
  "<XF86Back>" (cons "Go to Parent buffer" #'4g-goto-parent)
  "<mouse-8>" (cons "Go to Parent buffer" #'4g-goto-parent)
  "<mouse-9>" #'4g-mouse-cycle
  "<mouse-10>" 4g-prefix-map
  "C-<mouse-8>" #'org-mark-ring-goto
  "C-<mouse-9>" #'org-mark-ring-push
  :menu '("4g"
          ["QuickNav (this buffer)" 4g-quicknav-mode
           :style toggle
           :selected 4g-quicknav-mode
           :help "Navigate 4g buffers with vim-style single-key shortcuts"]
          ["QuickNav (new buffers)" (setopt 4g-quicknav (not 4g-quicknav))
           :style toggle
           :selected 4g-quicknav
           :help "Navigate 4g buffers with vim-style single-key shortcuts"]
          ["Board List"          4g-board-list]
          ["Catalog..."          4g-catalog]
          ["Thread..."           4g-thread]
          "---"
          ["Refresh"             4g-refresh
           :active (or 4g--threadno 4g--boardname)]
          ["Back to catalog"     4g-goto-parent
           :active               4g--threadno]
          ["View in Web Browser" 4g-view-in-browser
           :active (or 4g--threadno 4g--boardname)]
          "---"
          ["Customize 4g..."     (customize-group '4g)]))

(define-minor-mode 4g-mode
  "Minor mode for 4g buffers.
Adds a “4g” menu and local keybindings."
  :lighter " 4g"
  :keymap 4g-mode-map)

;; Creation-only auto-activation:
(add-hook '4g-mode-hook #'4g--maybe-enable-quicknav)
(add-hook '4g-mode-hook #'visual-line-mode)


;;; --- Interactive Commands ---------------------------------------------------

;;;###autoload
(cl-defun 4g-catalog (&optional board &key url parent)
  "Prompt for a 4chan BOARD (like g) and build an Org buffer from its catalog.
PARENT implies that the catalog was linked to from another buffer.
Fetch json data from 4chan or, if supplied, from URL."
  (interactive)
  (let* ((board    (or board (4g--prompt-board)))
         (url      (or url (format "%s/%s/catalog.json" 4g--4chan-acdn board)))
         (pages    (4g--fetch-json url))
         (threads  (seq-mapcat (lambda (page) (map-elt page :threads '())) pages))
         (catafun  (apply-partially #'4g--catalog-thread->org board))
         (catalog  (mapconcat catafun threads))
         (lweb     (format "[[https://boards.4chan.org/%s/catalog][Web]]" board))
         (lrefresh (format "[[4g:4chan/%s][Refresh]]" board))
         (lboards  "[[4g:4chan][Board List]]")
         (controls (string-join (list lrefresh lboards lweb) " "))
         (timefmt  (if (eq 4g-timestamp-format :4chan) " %c" 4g-timestamp-format))
         (timestr  (format-time-string timefmt))
         (footer   (4g--footer "4chan" board))
         (bufname  (format "*4chan /%s/ catalog*" board))
         (out      (get-buffer-create bufname)))
    ;; Used by 4g-thread for completions:
    (map-put! 4g--catalogs board threads)
    (with-current-buffer out
      (erase-buffer)
      (when (and 4g-directory (display-graphic-p))
        ;; Download OP thumbnails
        (4g--download-files
         (seq-keep (apply-partially #'4g--get-thumbnail-url+path board)
                   threads)
         :callback #'4g--redisplay-images)
        (cd 4g-directory))
      (insert
       (format "#+TITLE: Catalog of 4chan /%s/ as of %s" board timestr)
       "\n#+STARTUP: show2levels hidestars inlineimages indent"
       "\n<<PROTIP>>\n" (4g--protip-random :catalog 4g--protips)
       "\n\n" controls " [[#footer][Bottom]]"
       "\n\n* Catalog"
       catalog
       "\n\n" controls
       "\n\n" footer)
      (org-mode)
      (goto-char (point-min)))
    (switch-to-buffer out)
    (setq-local 4g--boardname board
                4g--parent-buffer parent
                org-return-follows-link t)
    (when (eq :verbatim 4g-greentext-rendering)
      (setq-local org-hide-emphasis-markers t))
    (4g-mode 1)
    out))

;;;###autoload
(cl-defun 4g-thread (&optional board no &key url parent postno)
  "Prompt for a 4chan thread's BOARD and NO and build an Org buffer from it.
PARENT implies that the catalog was linked to from another buffer.
Fetch json data from 4chan or, if supplied, from URL."
  (interactive)
  (when-let*
      ((board    (or board (4g--prompt-board)))
       (no       (or no (4g--prompt-thread board)))
       (url      (or url (format "%s/%s/thread/%s.json" 4g--4chan-acdn board no)))
       (web-url  (format "https://boards.4chan.org/%s/thread/%s" board no))
       (posts    (map-elt (4g--fetch-json url) :posts))
       (posts    (4g--add-backlinks posts (4g--find-backlinks posts)))
       (op       (seq-first posts))
       (headline (4g--op->org op :flip t))
       (replyfun (apply-partially #'4g--reply->org board))
       (replies  (mapconcat replyfun posts "\n\n** "))
       (lweb     (format "[[%s][Web]]" web-url))
       (lrefresh (format "[[4g:4chan/%s/%s][Refresh]]" board no))
       (lcatalog (format "[[4g:4chan/%s][Catalog]]" board))
       (lboards  "[[4g:4chan][Board List]]")
       (controls (string-join (list lrefresh lcatalog lboards lweb) " "))
       (title    (format  "4chan /%s/: %s [%s]" board (4g--thread-title op) no))
       (bufname  (concat "*" title "*"))
       (footer   (4g--footer "4chan" board no))
       (out      (get-buffer-create bufname)))
    (with-current-buffer out
      (when (and 4g-directory (display-graphic-p))
        (cd 4g-directory)
        (4g--download-files     ;; creates 4g-directory if non-existent
         (seq-keep (apply-partially #'4g--get-thumbnail-url+path board)
                   posts)
         :callback #'4g--redisplay-images))
      (erase-buffer)
      (insert
       "#+TITLE: " title
       "\n#+STARTUP: inlineimages hidestars"
       "\n\n<<PROTIP>>\n"
       (4g--protip-random :thread 4g--protips)
       "\n\n"   controls " [[#footer][Bottom]]"
       "\n\n* " headline
       "\n** "  replies
       "\n\n"   controls
       "\n\n"   footer)
      (goto-char (point-min))
      (org-mode))
    (switch-to-buffer out)
    (when postno
      (4g--goto-post postno))
    ;; Needed for 4g-refresh and 4g-goto-parent:
    (setq-local 4g--boardname board
                4g--threadno  no
                4g--parent-buffer parent
                org-return-follows-link t)
    (when (eq :verbatim 4g-greentext-rendering)
      (setq-local org-hide-emphasis-markers t))
    (4g-mode 1)
    out))

;;;###autoload
(cl-defun 4g-board-list (&key parent)
  "Fetch 4chan's board list and build an Org buffer from it.
PARENT implies that the catalog was linked to from another buffer."
  (interactive)
  (when-let*
      ((boards  (or 4g--boards (4g--fetch-boards)))
       (wsafep  (lambda (b) (eq 1 (map-elt b :ws_board))))
       (wsafe   (seq-filter wsafep boards))
       (nsfw    (seq-remove wsafep boards))
       (bufname "*4chan Board List*")
       (out     (get-buffer-create bufname)))
    (with-current-buffer out
      (erase-buffer)
      (insert
       "#+TITLE: 4chan Board List"
       "\n#+STARTUP: align content hidestars"
       "\n\n* Safe-for-work boards"
       "\n:properties:"
       "\n:custom_id: top"
       "\n:end:"
       "\n** " (mapconcat #'4g--board-info->org wsafe "\n\n** ")
       "\n* NSFW boards"
       "\n** " (mapconcat #'4g--board-info->org nsfw "\n\n** ")
       "\n\n" (4g--footer "4chan"))
      (org-mode)
      (4g-mode 1)
      (goto-char (point-min)))
    (switch-to-buffer out)
    (setq-local org-return-follows-link t
                4g--parent-buffer parent)
    out))

;; Credit to anonymous /g/ user 107218061 for figuring out how to make this work
(defun 4g-refresh ()
  "Fetch the data associated with the current 4g buffer and regenerate the latter."
  (interactive)
  (let ((current-line (line-number-at-pos))
        (win-start    (window-start)))
    (cond
      (4g--threadno  (4g-thread 4g--boardname 4g--threadno :parent 4g--parent-buffer))
      (4g--boardname (4g-catalog 4g--boardname :parent 4g--parent-buffer))
      (t             (4g-board-list :parent 4g--parent-buffer)))
    (forward-line (1- current-line))
    (set-window-start (selected-window) win-start)))

(defun 4g-view-in-browser ()
  "Open web browser to the page corresponding to the current 4g buffer."
  (interactive)
  (cond
   (4g--threadno  (4g--browse-url
                   (format "https://boards.4chan.org/%s/thread/%s"
                           4g--boardname 4g--threadno)))
   (4g--boardname (4g--browse-url
                   (format "https://boards.4chan.org/%s/"
                           4g--boardname)))
   (t (4g--browse-url "https://www.4chan.org/"))))

(defun 4g-goto-parent ()
  "Go back to the buffer that linked you to the current 4g buffer.
If the parent buffer was not specified or is dead, go up the hierarchy:
- From a thread buffer, go to the board's catalog.
- From a catalog, go to the board list."
  (interactive)
  (cond
    ((and (buffer-live-p 4g--parent-buffer)
          (not (eq 4g--parent-buffer (current-buffer))))
     (switch-to-buffer 4g--parent-buffer))
    (4g--threadno  (4g-catalog 4g--boardname))
    (4g--boardname (4g-board-list))
    (t             (message "Nothing to go back to."))))

(defun 4g-mouse-cycle (&rest _ignored)
  "Move point to the position clicked on with the mouse and cycle headings."
  (org-cycle))

(advice-add #'4g-mouse-cycle :before #'mouse-set-point)

(provide '4g)

;;; 4g.el ends here
