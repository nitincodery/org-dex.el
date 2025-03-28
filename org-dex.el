;;; org-dex.el --- Archive web pages as SingleFiles in Org mode  -*- lexical-binding: t; -*-

;; Author: Nitin Choudhary <nitin@codery.xyz>
;; Version: 1.0.0
;; URL: https://github.com/nitincodery/org-dex.el

;;; Commentary:

;; Org-Dex is an Emacs package designed to integrate the SingleFile CLI tool
;; with Org mode, allowing you to archive web pages as single HTML files
;; directly from your Org buffers. It also provides functionality to fetch
;; titles for raw URLs and convert them to Org-mode links.
;;
;; The package relies on external tools:
;;
;; - SingleFile CLI: A command-line tool for saving web pages as single HTML files.
;;   (https://github.com/gildas-lormeau/single-file-cli)
;;
;; - CRC32: An Emacs library for calculating CRC32 checksums, used internally for
;;   archived pages filename collisions. You can use either:
;;   - https://github.com/nitincodery/crc32.el
;;   - https://codeberg.org/Jaft/Emacs-CRC
;;
;; - Curl and HTMLq: Used for fetching webpage titles.
;;
;; Key Features:
;; - Archive Org-mode links in a region, under a heading, or at point.
;; - Fetch titles for raw URLs and convert them to Org-mode links.
;; - Reset archived links back to their original form, deleting associated files.
;; - Revert links with missing archive files to their original URLs.
;; - Open archived SingleFile HTML files or URLs directly from Org-mode.
;; - Customizable domain mapping for URL replacement.
;; - A transient menu (`org-dex-menu`) for easy access to all commands.
;;
;; Usage:
;; - Open `M-x org-dex-menu` to access all org-dex commands.
;; - Use `org-dex-archive-region` to archive links in a selected region.
;; - Use `org-dex-fetch-region` to fetch titles for raw URLs in a region.
;; - Customize settings via `M-x customize-group RET org-dex RET`.
;;
;; For detailed examples and edge cases, see the function docstrings or the README.

;;; Installation:
;;
;; 1. Install SingleFile CLI:
;;    - Follow the instructions at https://github.com/gildas-lormeau/single-file-cli
;;    - Ensure the `single-file` command is available in your PATH.
;;
;; 2. Install Curl:
;;    - Follow the instructions at https://curl.se/download.html
;;    - Ensure the `curl` command is available in your PATH.
;;
;; 3. Install HTMLq:
;;    - Follow the instructions at https://github.com/mgdm/htmlq
;;    - Ensure the `htmlq` command is available in your PATH.
;;
;; 4. Install CRC32 library:
;;    - Install either `crc32.el` or `Emacs-CRC` using your preferred package manager.
;;
;; 5. Install org-dex:
;;    - Add the following to your Emacs configuration:
;;      (add-to-list 'load-path "/path/to/org-dex")
;;      (require 'org-dex)

;;; Code:

(require 'org)
(require 'org-id)
(require 'crc32)
(require 'org-element)
(require 'transient)

(defgroup org-dex nil
  "SingleFile integration for Org Mode."
  :group 'org)

(defcustom org-dex-org-directory "~/org/internet/org-files"
  "Directory for storing Org Files."
  :type 'directory
  :group 'org-dex)

(defcustom org-dex-sf-directory "~/org/internet/single-files"
  "Directory for storing SingleFile HTML archives."
  :type 'directory
  :group 'org-dex)

(defcustom org-dex-delete-unused-files nil
  "If non-nil, delete unused :sf or :org files when trimming keys not in `org-dex-archive-options-and-order`."
  :type 'boolean
  :group 'org-dex)

(defcustom org-dex-hash-location 'prepend
  "Specify where to place the hash in SingleFile filenames.
Possible values:
- 'append: Add hash to the end of the filename.
- 'prepend: Add hash to the beginning of the filename. (DEFAULT)"
  :type '(choice (const :tag "Append hash to filename" append)
                 (const :tag "Prepend hash to filename" prepend))
  :group 'org-dex)

(defun org-dex--link-type (link)
  "Determine the type of an Org-mode link based on its string content.
Argument LINK is a string representing the link's path.
Returns :url for HTTP/HTTPS links, :sf for .html files, :org for .org files,
or nil if the link type is unrecognized."
  (cond
   ((string-match-p "\\`https?:" link) :url)
   ((string-match-p "\\.html\\'" link) :sf)
   ((string-match-p "\\.org\\'" link) :org)
   (t nil)))

(defcustom org-dex-archive-options-and-order '(:url :sf)
  "List specifying which link types to archive and their order in headings.
Valid options are :url, :sf, and :org. Examples:
- '(:sf :url) = SingleFile file + URL.
- '(:org :url) = Org file + URL.
- '(:sf :org) = SingleFile + Org file, no URL.
- '(:sf :org :url) = SingleFile + Org file + URL.
The order determines the display sequence in the generated heading."
  :type '(repeat (choice (const :url) (const :sf) (const :org)))
  :group 'org-dex)

(defun org-dex--generate-heading (&rest link-pairs)
  "Generate a formatted string of Org-mode links based on user configuration.
LINK-PAIRS is a list of (type path description) lists, where type is :url, :sf, or :org.
Filters and orders links according to `org-dex-archive-options-and-order`, joining them with
`org-dex-link-separator`. Returns the formatted string or an empty string if no valid links."
  (let ((links-map '()))
    (dolist (link link-pairs)
      (let ((type (car link))
            (path (org-dex--remove-file-prefix (cadr link)))
            (desc (caddr link)))
        (when (and type path desc)
          (push (cons type
                      (pcase type
                        (:url (format "[[%s][%s]]" path desc))
                        (:sf (format "[[file:%s][%s]]" path desc))
                        (:org (format "[[file:%s][%s]]" path desc))))
                links-map))))    
    (string-join
     (delq nil
           (mapcar (lambda (type) 
                     (cdr (assq type links-map)))
                   org-dex-archive-options-and-order))
     org-dex-link-separator)))

(defcustom org-dex-sf-command "single-file"
  "Command to run SingleFile CLI."
  :type 'string
  :group 'org-dex)

(defcustom org-dex-shell (cond
                          ((eq system-type 'windows-nt)
                           '("cmd.exe" "/c"))
                          ((eq system-type 'darwin)
                           '("/bin/bash" "-c"))
                          (t
                           '("/bin/sh" "-c")))
  "Shell to use (OS dependent) to run SingleFile."
  :type '(choice
          (const :tag "Windows (cmd.exe)" ("cmd.exe" "/c"))
          (const :tag "Windows (PowerShell)" ("powershell.exe" "-Command"))
          (const :tag "macOS/Linux (bash)" ("/bin/bash" "-c"))
          (const :tag "POSIX shell" ("/bin/sh" "-c"))
          (repeat :tag "Custom" string))
  :group 'org-dex)

(defcustom org-dex-hash-func-choice 'crc32
  "CRC function to use for hashing filenames.
Options:
- 'crc32 from github@nitincodery/crc32.el
- 'crc-32 from codeberg@Jaft/Emacs-CRC"
  :type '(choice
          (const :tag "'crc32 (nitincodery)" crc32)
          (const :tag "'crc-32 (Jaft)" crc-32)
	  (repeat :tag "Custom" string))
  :group 'org-dex
  :set (lambda (symbol value)
         (set-default symbol value)
         (defalias 'org-dex-hash-func value)))

(defun org-dex--collect-links (beg end)
  "Collect all Org-mode links in the region from BEG to END.
BEG and END can be either buffer positions or markers.
Returns a list of plists, each containing:
  :type - The link type (:url, :sf, :org, or nil)
  :link - The link text (e.g., URL or file path)
  :desc - The description (or nil if not provided)
  :start - The starting position or marker
  :end - The ending position or marker"
  (let ((beg-pos (if (markerp beg) (marker-position beg) beg))
        (end-pos (if (markerp end) (marker-position end) end))
	(pattern org-bracket-link-regexp)
        (matches '()))
    (save-excursion
      (goto-char beg-pos)
      (while (re-search-forward pattern end-pos t)
        (let* ((match-start (let ((m (make-marker)))
			      (set-marker m (match-beginning 0))
			      (set-marker-insertion-type m t)
			      m))
	       
	       (match-last (let ((m (make-marker)))
			     (set-marker m (match-end 0))
			     (set-marker-insertion-type m t)
			     m))
	       
	       (link (match-string-no-properties 1))
               (desc (match-string-no-properties 2))
               (type (org-dex--link-type link)))
          (when type
            (push (list :type type
                        :link link
                        :desc desc
                        :start match-start
                        :end match-last)
                  matches)))))
    (nreverse matches)))  ;; Return matches in buffer order

(defun org-dex--group-links (matches)
  "Group consecutive links in MATCHES separated by `org-dex-link-separator`.
MATCHES is a list of link plists from `org-dex--collect-links`.
Classifies groups as matching `org-dex-archive-options-and-order` or not.
Returns a cons cell (grouped-matches . ungrouped-matches), where:
- grouped-matches: List of groups matching the order and types.
- ungrouped-matches: List of groups that do not match."
  (let ((all-groups '())
        (current-group '())
        (separator org-dex-link-separator))
    
    ;; Step 1: Group consecutive links
    (while matches
      (let* ((match (car matches))
             (current-start (plist-get match :start)))
        (if (null current-group)
            (progn
              (push match current-group)
              (setq matches (cdr matches)))
          (let* ((last-match (car current-group))
                 (last-end (plist-get last-match :end))
                 (between (buffer-substring-no-properties last-end current-start)))
	    (if (string= between separator) ; Text matches separator
                (progn
                  (push match current-group)
                  (setq matches (cdr matches)))
              (push (nreverse current-group) all-groups)
              (setq current-group (list match))
              (setq matches (cdr matches)))))))
    (when current-group
      (push (nreverse current-group) all-groups))
    (setq all-groups (nreverse all-groups))

    ;; Step 2: Classify groups
    (let ((grouped-matches '())
          (ungrouped-matches '())
          (order org-dex-archive-options-and-order))
      (dolist (group all-groups)
        (if (and (= (length group) (length order))
                 (equal (mapcar (lambda (x) (plist-get x :type)) group) order))
            (push group grouped-matches)
          (push group ungrouped-matches)))
      (cons (nreverse grouped-matches) (nreverse ungrouped-matches)))))


(defalias 'org-dex-hash-func (symbol-value 'org-dex-hash-func-choice))

(defun org-dex--dashed-string (str)
  "Convert STR to a safe filename by replacing non-alphanumeric characters with spaces and joining with dashes.
Argument STR is the input string to process.
Returns a lowercase dashed string suitable for filenames."
  (let ((clean-str (replace-regexp-in-string "[^[:alnum:]]" " " str)))
    (string-join (split-string (downcase clean-str)) "-")))

(defun org-dex--quote-string (str)
  "Add double quotes around STR.
Argument STR is the string to quote.
Returns the quoted string."
  (concat "\"" str "\""))

(defun org-dex--ensure-directory ()
  "Ensure the directories in `org-dex-sf-directory` and `org-dex-org-directory` exist.
Creates directories if they don’t exist, handling errors gracefully.
Signals an error with details if directory creation fails."
  (condition-case err
      (progn
        (unless (file-exists-p org-dex-sf-directory)
          (make-directory (expand-file-name org-dex-sf-directory) t))
        (unless (file-exists-p org-dex-org-directory)
          (make-directory (expand-file-name org-dex-org-directory) t)))
    (file-error
     (error "Failed to create directory: %s (Reason: %s)"
            (if (file-exists-p org-dex-sf-directory) org-dex-org-directory org-dex-sf-directory)
            (error-message-string err)))))

(defcustom org-dex-sf-cmd-extra-args '("--include-infobar=true")
  "Extra command-line arguments for SingleFile CLI."
  :type '(repeat string)
  :group 'org-dex)

(defcustom org-dex-title-override-type :sf
  "Specify a link type (:url, :sf, or :org) to override its description with the title."
  :type '(choice
          (const :tag "URL" :url)
          (const :tag "SingleFile" :sf)
          (const :tag "Org" :org))
  :group 'org-dex)

(defun org-dex--eval-description (desc)
  "Evaluate DESC, which can be a string or a function.
Argument DESC is the description to evaluate.
If DESC is a function, calls it to get the description; otherwise, returns DESC as is."
  (if (functionp desc)
      (funcall desc)
    desc))

(defcustom org-dex-url-description "#"
  "Description for URL links in org-dex.
Can be a string or a function that returns a string."
  :type '(choice
          (string :tag "Plain text description")
          (function :tag "Function returning a description"))
  :group 'org-dex)

(defcustom org-dex-sf-description "❖"
  "Description for SingleFile links in org-dex.
Can be a string or a function that returns a string."
  :type '(choice
          (string :tag "Plain text description")
          (function :tag "Function returning a description"))
  :group 'org-dex)

(defcustom org-dex-org-description "☉"
  "Description for Org file links in org-dex.
Can be a string or a function that returns a string."
  :type '(choice
          (string :tag "Plain text description")
          (function :tag "Function returning a description"))
  :group 'org-dex)

(defcustom org-dex-link-separator " "
  "Separator used between links in generated headings."
  :type 'string
  :group 'org-dex)

(defun org-dex-reset-region (beg end)
  "Reset all processed links in the region from BEG to END back to their original URL form.
BEG and END are buffer positions or markers defining the region.
Processed links match `org-dex-archive-options-and-order`. Deletes associated :sf and :org files
if they exist. Updates the buffer and logs the reset operation."
  (interactive "r")
  (save-excursion
    (goto-char beg)

    (let* ((beg-val (if (markerp beg) beg
		      (let ((m (make-marker)))
			(set-marker m beg)
			(set-marker-insertion-type m t)
			m)))

           (end-val (if (markerp end) end
		      (let ((m (make-marker)))
			(set-marker m end)
			(set-marker-insertion-type m t)
			m)))

	   (link-matches (org-dex--collect-links beg-val end-val))
	   (_ (org-dex--region-has-links link-matches))
	   (group-matches (car (org-dex--group-links link-matches))))
      
      (dolist (match group-matches)
	(let* ((org-group (seq-find (lambda (x) (eq (plist-get x :type) :org)) match))
	       (sf-group (seq-find (lambda (x) (eq (plist-get x :type) :sf)) match))
	       (url-group (seq-find (lambda (x) (eq (plist-get x :type) :url)) match))
	       	       
	       (start (marker-position (plist-get (car match) :start)))
	                
               (end (marker-position (plist-get (car (last match)) :end)))
                   
	       (original-length 0)
	       (replacement-length 0)
	       (has-file nil)
	       
	       (url (org-dex--replace-domain
		     (or (cdr (org-dex--get-title-and-url-from-org (plist-get org-group :link)))
			 (cdar (org-dex--get-title-and-url-from-sf (plist-get org-group :link)))
			 (plist-get url-group :link))
		     t))
	       
	       (url-title nil)

	       (url-desc (or (car (org-dex--get-title-and-url-from-org (plist-get org-group :link)))
			     (caar (org-dex--get-title-and-url-from-sf (plist-get org-group :link)))
			(plist-get url-group :desc)))
	       
	       (org-path (plist-get org-group :link))
	       (org-desc (plist-get org-group :desc))
	       
	       (sf-path (plist-get sf-group :link))
	       (sf-desc (plist-get sf-group :desc)))

	  (setq url-title (pcase org-dex-title-override-type
			    (:url url-desc)
			    (:sf sf-desc)
			    (:org org-desc)))
	           
          (when url

	    (setq original-length (- end start))
	    (setq replacement-length (length (format "[[%s][%s]]" url url-title)))
	    
            ;; Delete any SingleFile, if exists
	    (when sf-path
	      (setq has-file t)
              (if (file-exists-p (org-dex--sanitize-file-path sf-path))
		  (delete-file (org-dex--sanitize-file-path sf-path))))
              
            ;; Delete any Org file, if exists
	    (when org-path
	      (setq has-file t)
              (if (file-exists-p (org-dex--sanitize-file-path org-path))
		  (delete-file (org-dex--sanitize-file-path org-path))))

	    (when has-file
              ;; Replace the entire match with just the URL link
	      (replace-region-contents start end (lambda () (format "[[%s][%s]]" url url-title)))

	      (org-dex--log-message "SingleFile: RESET done." "red")
              (message "SingleFile: RESET done."))))))))

(defcustom org-dex-domain-mapping
  '(("reddit.com" . "eddrit.com")
    ("old.reddit.com" . "eddrit.com")
    ("medium.com" . "scribe.rip"))
  "Alist mapping original domains to their replacements.
Each element is a cons cell (ORIGINAL . REPLACEMENT) for URL substitution."
  :type '(alist :key-type string :value-type string)
  :group 'org-dex)

(defun org-dex--replace-domain (url &optional reverse)
  "Replace the domain in URL based on `org-dex-domain-mapping'.
Argument URL is the link to process.
If REVERSE is non-nil, maps replacement domains back to originals (e.g., 'eddrit.com' to 'reddit.com').
Otherwise, maps originals to replacements (e.g., 'reddit.com' to 'eddrit.com').
Strips 'www' subdomain if present. Returns the modified URL or the original if no mapping applies."
  (when url
    (let* ((domain (replace-regexp-in-string
		    ;; Regex "^https?://\\([^/]+\\)/.*"
                    (rx (seq bol "http" (opt "s") "://"
                             (group (+ (not (any "/"))))
                             "/" (0+ nonl)))
                    "\\1" url))
           (base-domain (replace-regexp-in-string "^www\\." "" domain))  ;; Strip "www."
           (mapping (if reverse
                        ;; Reverse mapping: swap key-value pairs
                        (mapcar (lambda (pair) (cons (cdr pair) (car pair)))
                                org-dex-domain-mapping)
                      ;; Forward mapping: use as-is
                      org-dex-domain-mapping)))
      (if-let ((replacement (cdr (assoc base-domain mapping))))
          (replace-regexp-in-string (regexp-quote domain) replacement url)
        url))))


(defun org-dex--check-overlap (region buffer-or-file)
  "Check if REGION overlaps, matches, or is a subset of any region in `org-dex--operation-queue'.

REGION is a cons cell (BEG . END), where BEG and END are markers defining the region to check.
BUFFER-OR-FILE is either a buffer object or a file name string identifying the context of the region.

Returns t if REGION exactly matches, is a subset of, or overlaps with any queued region in the same buffer or file; otherwise, returns nil.
An exact match occurs when REGION's start and end positions equal a queued region's positions.
A subset occurs when REGION lies entirely within a queued region's bounds.
An overlap occurs when REGION intersects a queued region's bounds at any point.
The check is performed against all entries in `org-dex--operation-queue', considering only regions associated with the same BUFFER-OR-FILE."
  (let* ((beg (car region))
         (end (cdr region))
         (current-beg-pos (marker-position beg))
         (current-end-pos (marker-position end))
	 ;; Check for overlap, exact match, or subset with queued regions
	 (overlap-entry (seq-find (lambda (queued-entry)
				    (let* ((queued-buffer-or-file (nth 0 queued-entry))
					   (queued-region (nth 1 queued-entry))
					   (queued-beg (car queued-region))
					   (queued-end (cdr queued-region))
					   (queued-beg-pos (marker-position queued-beg))
					   (queued-end-pos (marker-position queued-end))
					   (same-buffer (eq buffer-or-file queued-buffer-or-file)))
				      (and same-buffer
					   (or
					    ;; Exact match
					    (and (= current-beg-pos queued-beg-pos)
						 (= current-end-pos queued-end-pos))
					    ;; Subset
					    (and (>= current-beg-pos queued-beg-pos)
						 (<= current-end-pos queued-end-pos))
					    ;; Any overlap
					    (and (<= current-beg-pos queued-end-pos)
						 (>= current-end-pos queued-beg-pos))))))
				  (queue-all org-dex--operation-queue))))
    (if overlap-entry t nil)))

(defun org-dex--add-operation (region op)
  "Add an operation to org-dex--operation-queue and trigger processing.

REGION is a cons cell (BEG . END), where BEG and END are buffer positions or markers defining the region to operate on. OP is a symbol specifying the operation type, one of :fetch, :update, :archive, or :revert.

Creates an entry for the operation, storing either the buffer's file name (if it has one)
or the buffer object, along with REGION and the operation type OP.
If `org-dex--operation-queue' is nil, initializes it as a new queue using `queue-create'.
Appends the entry to the queue with `queue-append' and triggers processing by calling
`org-dex--process-operation-queue'. Does not check for duplicate entries."
  (let* ((buffer (current-buffer))
         (file-name (buffer-file-name))
         (entry (if file-name
		    ;; If the buffer has a file, store the file name
                    (list file-name region op)
		  ;; Otherwise, store the buffer object
                  (list buffer region op))))
    ;; Initialize the queue if it's nil
    (when (null org-dex--operation-queue)
      (setq org-dex--operation-queue (queue-create)))
    ;; Add the entry to the queue
    (queue-append org-dex--operation-queue entry)))

(defun org-dex--process-operation-queue ()
  "Process the next operation in `org-dex--operation-queue' if conditions allow.
Executes only if the queue is not empty (checked via `queue-empty'), no operation
is currently active (`org-dex--current-operation' is nil), and no title or link
processing is ongoing (`org-dex--title-processing' and `org-dex--links-processing'
are nil). Dequeues the next entry from `org-dex--operation-queue', sets it as the
current operation, and processes it in the associated buffer.

Each entry is a list (BUFFER-OR-FILE REGION OP), where BUFFER-OR-FILE is either a
file name (string) or buffer object, REGION is a cons cell (BEG . END), where BEG
and END are buffer positions or markers defining the region to operate on. OP is
one of :fetch, :update, :archive, or :revert. If BUFFER-OR-FILE is a file name,
attempts to find or open the buffer; otherwise, uses the buffer directly.

Operations are executed as follows:
- :fetch: Fetches titles asynchronously via `org-dex--do-fetch-titles'.
- :update: Updates the buffer synchronously with titles, logs completion, and clears
  the current operation.
- :archive: Archives links asynchronously via `org-dex--do-archive-region'.
- :revert: Reverts missing archives synchronously, clears the current operation.

Recursively calls itself after synchronous operations (:update, :revert) or to
continue processing after starting asynchronous ones (:fetch, :archive). Does
nothing if the buffer is not live or the queue is empty."	 
  (when (and (not (queue-empty org-dex--operation-queue))
             (not org-dex--current-operation)
             (not org-dex--title-processing)
             (not org-dex--links-processing))
    (let ((entry (queue-dequeue org-dex--operation-queue)))
      (setq org-dex--current-operation entry)
      (let* ((buffer-or-file (nth 0 entry))
	     (region (nth 1 entry))
             (beg (car region))
             (end (cdr region))
	     (op (nth 2 entry))
             (buffer (if (stringp buffer-or-file)
			 ;; If it's a file name, find or open the buffer
			 (or (find-buffer-visiting buffer-or-file)
                             (find-file-noselect buffer-or-file))
		       ;; Otherwise, it's a buffer object
		       buffer-or-file)))
	(when (buffer-live-p buffer)  ;; Check if the buffer is still valid
          (with-current-buffer buffer
            ;; Process the region (BEG END) here
	    (cond
	     ((eq op :fetch)
	      (progn
		(org-dex--do-fetch-titles beg end)
		(org-dex--process-operation-queue)))

	       ((eq op :update)
		(progn
		  (org-dex--do-update-buffer)
		  (org-dex--log-message "Titles: UPDATE done." "green")
		  (setq org-dex--current-operation nil)
		  (org-dex--process-operation-queue)))

	       ((eq op :archive)
		(progn
		  (org-dex--do-archive-region beg end)
		  (org-dex--process-operation-queue)))

	       ((eq op :revert)
		(progn
		  (org-dex-revert-region beg end)
		  (setq org-dex--current-operation nil)
		  (org-dex--process-operation-queue))))))))))

(defvar org-dex--links-queue nil
  "Queue of links to be processed by SingleFile.")

(defvar org-dex--links-processing nil
  "Flag indicating if a SingleFile process is currently running.")

(defvar org-dex--links-queue-length 0
  "Count of links to be processed by SingleFile.")

(defun org-dex--process-next-link ()
  "Process the next link in `org-dex--links-queue` using SingleFile.
Handles sequential downloading, logs progress, and triggers the next process.
When the queue is empty, logs completion and processes the buffer queue for reverting."
  (if (and org-dex--links-queue (not org-dex--links-processing))
      
      ;; Process next link if queue has items
      (let* ((link (pop org-dex--links-queue))	     
	     (progress (format "[%d/%d]" (- org-dex--links-queue-length (length org-dex--links-queue)) org-dex--links-queue-length))
             (link-hash (org-dex-hash-func (org-dex--process-link (org-dex--replace-domain (car link)))))
	     (sf-command (list org-dex-sf-command (org-dex--replace-domain (car link))
			       (format "--filename-template=%s.html" (cdr link))
			       (format "--output-directory=%s" (expand-file-name org-dex-sf-directory))))
	     (sf-process (make-process
			  :name (concat link-hash "-org-dex-archive")	
			  :command (append org-dex-shell sf-command org-dex-sf-cmd-extra-args)
			  :buffer "*org-dex-log*"
			  :stderr "*org-dex-err*")))

	(org-dex--log-message (format "%s Archiving Page: %s" progress (car link)) "orange")
	(setq org-dex--links-processing t)

	(process-put sf-process 'link (car link))
      
	(set-process-sentinel
	 sf-process
	 (lambda (proc event)
	   (setq org-dex--links-processing nil)
	   (let ((exit-code (process-exit-status proc))
		 (link (process-get proc 'link)))
	     (if (= exit-code 0)		 
		 (org-dex--log-message "")
	       (progn
		 (org-dex--log-message
		  (format "Failed: %s (exit code %d).\nCheck *org-dex-err* buffer for errors."
			  link exit-code)
		  "red"))))
	   
	   ;; Process next link regardless of success/failure
	   (org-dex--process-next-link))))
 
    ;; Queue is empty, print completion message
    (when (and (not org-dex--links-processing) (= 0 (length org-dex--links-queue)))
      (setq org-dex--current-operation nil)
      (org-dex--log-message "SingleFile: ARCHIVE done." "green")
      (message "SingleFile: All downloads completed!")
      (org-dex--process-operation-queue))))

(defun org-dex-revert-region (beg end)
  "Revert links in the region from BEG to END if associated files are missing.
BEG and END are positions or markers.
Checks groups matching `org-dex-archive-options-and-order` and reverts to the original URL
if any :sf or :org file is missing. Logs the number of reverted links."
  (interactive "r")
  (save-excursion
    (let* ((beg-val (if (markerp beg) beg
		      (let ((m (make-marker)))
			(set-marker m beg)
			(set-marker-insertion-type m t)
			m)))

           (end-val (if (markerp end) end
		      (let ((m (make-marker)))
			(set-marker m end)
			(set-marker-insertion-type m t)
			m)))

           (link-matches (org-dex--collect-links beg-val end-val))
	   (_ (org-dex--region-has-links link-matches))
           (group-matches (car (org-dex--group-links link-matches)))
           (reverted-count 0))
      
      (dolist (match group-matches)
        (let* ((org-group (seq-find (lambda (x) (eq (plist-get x :type) :org)) match))
               (sf-group (seq-find (lambda (x) (eq (plist-get x :type) :sf)) match))
               (url-group (seq-find (lambda (x) (eq (plist-get x :type) :url)) match))

	       (start (marker-position (plist-get (car match) :start)))
                      
               (end (marker-position (plist-get (car (last match)) :end)))
	       
                 (original-length 0)
                 (replacement-length 0)
		 
                 (url (or (cdr (org-dex--get-title-and-url-from-org (plist-get org-group :link)))
			  (cdar (org-dex--get-title-and-url-from-sf (plist-get org-group :link)))
                          (plist-get url-group :link)))
                 (url-title nil)
		 (url-desc (or (car (org-dex--get-title-and-url-from-org (plist-get org-group :link)))
			       (caar (org-dex--get-title-and-url-from-sf (plist-get org-group :link)))
			       (plist-get url-group :desc)))
                 (org-path (plist-get org-group :link))
                 (org-desc (plist-get org-group :desc))
                 (sf-path (plist-get sf-group :link))
                 (sf-desc (plist-get sf-group :desc))
                 (has-missing nil))
	    
            ;; Check for missing files
          (when (and sf-path (not (file-exists-p
				   (org-dex--sanitize-file-path sf-path))))
            (setq has-missing t))
          (when (and org-path (not (file-exists-p
				    (org-dex--sanitize-file-path org-path))))
            (setq has-missing t))

            ;; Set url-title
	    (setq url-title (pcase org-dex-title-override-type
			      (:url url-desc)
			      (:sf sf-desc)
			      (:org org-desc)))

            ;; Revert if URL exists and files are missing
            (when (and url has-missing)
	      (replace-region-contents start end (lambda () (format "[[%s][%s]]" url url-title)))

              (setq reverted-count (1+ reverted-count))
              (org-dex--log-message (format "Reverting: %s" url) "yellow"))))

      (if (> reverted-count 0)
	  (progn
            (org-dex--log-message (format "SingleFile: REVERT done (%s links)." reverted-count) "red")
            (message "SingleFile: Reverted %d links with missing files" reverted-count)
	    (org-dex--process-operation-queue))))))

(defun org-dex--log-message (msg &optional color)
  "Append MSG to the SingleFile log buffer with optional COLOR.
Argument MSG is the message to log.
COLOR, if provided, is a string (e.g., 'red', 'green') applied as a foreground face."
  (let ((log-msg (format "[%s] %s\n" 
			 (format-time-string "%H:%M:%S")
			 msg)))
    
    (with-current-buffer (get-buffer-create "*org-dex-log*")
      (goto-char (point-max))
      (cond
       ((or (string-equal "" msg) (string-equal "\n" msg))
	(insert msg))
       (color
	(insert (propertize log-msg 'face `(:foreground ,color))))
       (t
	(insert log-msg))))))

(defun org-dex--download-links (links)
  "Download URLs in LINKS using SingleFile sequentially.
Argument LINKS is a list of cons cells (url . filename).
Initializes the download queue and starts processing."
  ;; Reset and initialize queue
  (setq org-dex--links-queue links
	org-dex--links-queue-length (length links)
        org-dex--links-processing nil)
  
  ;; Start processing the queue
  (org-dex--process-next-link))

(defun org-dex--insert-date-day ()
  "Return the current date in Org-mode timestamp format.
Format is [YYYY-MM-DD Day], e.g., [2023-10-15 Sun]."
  (concat "[" (format-time-string "%Y-%m-%d %a") "]"))

(defun org-dex-archive-heading ()
  "Move to the parent heading and archive all Org-mode links under it.
Uses `org-dex-archive-region` to process the subtree."
  (interactive)
  (org-up-heading-all 1)
  (let ((start (point))
        (end (save-excursion (org-end-of-subtree t t) (point))))
    (org-dex-archive-region start end)))

(defun org-dex-reset-heading ()
  "Move to the parent heading and reset all Org-mode links under it.
Uses `org-dex-reset-region` to revert links in the subtree to their original URLs."
  (interactive)
  (org-up-heading-all 1)
  (let ((start (point))
        (end (save-excursion (org-end-of-subtree t t) (point))))
    (org-dex-reset-region start end)))

(defun org-dex-revert-heading ()
  "Move to the parent heading and revert missing file links under it.
Uses `org-dex-revert-region` to process the subtree."
  (interactive)
  (org-up-heading-all 1)
  (let ((start (point))
        (end (save-excursion (org-end-of-subtree t t) (point))))
    (org-dex-revert-region start end)))

(defun org-dex--process-link (url)
  "Process URL into a dashed form by removing protocols, 'www' prefix, trailing slashes, and fragments.
Argument URL is the link to process.
Returns a dashed string suitable for filenames, e.g., 'example-com-page'."
  (let ((s url))    
    ;; Remove protocol
    (setq s (replace-regexp-in-string "^https?://?" "" s))    
    ;; Remove www prefix
    (setq s (replace-regexp-in-string "^www\\." "" s))
    ;; Remove trailing slash
    (setq s (replace-regexp-in-string "/$" "" s))
    ;; Remove true fragment identifiers (only at the end)
    (when (string-match "#[^/?]*$" s)
      (setq s (substring s 0 (match-beginning 0))))
    ;; Remove .html extension
    (setq s (replace-regexp-in-string "\\.html$" "" s))
    ;; Finally convert to dashed string format
    (org-dex--dashed-string s)))

(defun org-dex--get-title-and-url-from-sf (file)
  "Extract title and URL from a SingleFile HTML archive FILE.
Argument FILE is the path to the HTML file.
Returns a list of cons cells ((title . url)) from the <title> tag and <a> elements with class 'infobar-link-icon'.
Returns nil if FILE doesn’t exist or isn’t readable."
  (when (and file (file-exists-p (org-dex--sanitize-file-path file)))
    (let ((dom (with-temp-buffer
                 (insert-file-contents (org-dex--sanitize-file-path file))
                 (libxml-parse-html-region (point-min) (point-max))))
          (links '()))
      ;; Get the title from the <title> tag
      (let ((title (dom-text (car (dom-by-tag dom 'title)))))
        (cl-labels ((search-elements
                     (element)
                     (when (and (listp element) (car element))
                       (when (and (eq (car element) 'a)
                                  (let ((class (dom-attr element 'class)))
                                    (and class (string-match "infobar-link-icon" class))))
                         (let ((href (dom-attr element 'href)))
                           (when href
                             (push (cons title href) links))))
                       (mapc #'search-elements (dom-children element)))))
          (search-elements dom)))
      (nreverse links))))

(defun org-dex--get-title-and-url-from-org (file)
  "Extract TITLE and URL from an Org file FILE.
Argument FILE is the path to the Org file.
Returns a cons cell (title . url), with nil for missing values.
Returns nil if FILE doesn’t exist or isn’t readable."
  (when (and file (file-exists-p file))
    (with-current-buffer (find-file-noselect (org-dex--sanitize-file-path file))
      (let ((parsed (org-element-parse-buffer))
            title url)
        (org-element-map parsed 'keyword
          (lambda (keyword)
            (cond
             ((string= "TITLE" (org-element-property :key keyword))
              (setq title (org-element-property :value keyword)))
             ((string= "URL" (org-element-property :key keyword))
              (setq url (org-element-property :value keyword)))))
          nil t) ;; Stop after first match per type (assuming one TITLE and URL)
        (cons title url)))))

(defun org-dex--remove-file-prefix (file)
  "Remove 'file:' prefix from FILE if present.
Argument FILE is the string to process.
Returns the string without the prefix."
  (string-remove-prefix "file:" file))

(defun org-dex-archive-region (beg end)
  "Archive web links in the region between BEG and END using SingleFile.
BEG and END are positions or markers.
Processes Org-mode links, grouping them and archiving according to `org-dex-archive-options-and-order`.
Downloads missing SingleFile archives and creates Org files as needed.
Updates the buffer with new link groups and queues the region for post-processing."
  (interactive "r")
  (let* ((region (cons (let ((m (make-marker))) (set-marker m (- beg 2)) (set-marker-insertion-type m t) m)
                       (let ((m (make-marker))) (set-marker m end) (set-marker-insertion-type m t) m)))
         (buffer-or-file (if (buffer-file-name) (buffer-file-name) (current-buffer))))

    (if (not (org-dex--check-overlap region buffer-or-file))
	(progn
	  (org-dex--add-operation region :archive)
	  (org-dex--add-operation region :revert)
	  (if org-dex-auto-archive-raw-urls
	      (progn
		(org-dex--add-operation region :fetch)
		(org-dex--add-operation region :update)
		(org-dex--add-operation region :archive)
		(org-dex--add-operation region :revert))))
      (message "Uh-oh! Overlapping Region exists in operation queue. Try later or kill all org-dex operations."))
 
    (deactivate-mark)
    (org-dex--process-operation-queue)))

(defun org-dex-fetch-region (beg end)
  "Fetch titles for raw URLs in the region from BEG to END and convert them to Org-mode links.
BEG and END are positions or markers.
Queues the region for processing and starts title fetching."
  (interactive "r")
  (let* ((region (cons (let ((m (make-marker))) (set-marker m (- beg 2)) (set-marker-insertion-type m t) m)
		       (let ((m (make-marker))) (set-marker m end) (set-marker-insertion-type m t) m)))
	 (buffer-or-file (if (buffer-file-name) (buffer-file-name) (current-buffer))))
    
    (if (not (org-dex--check-overlap region buffer-or-file))
	(progn
	  (org-dex--add-operation region :fetch)
	  (org-dex--add-operation region :update))
      (message "Uh-oh! Overlapping Region exists in operation queue. Try later or kill all org-dex operations."))
      
    (deactivate-mark)
    (org-dex--process-operation-queue)))

(defun org-dex--do-archive-region (beg end)
  "Archive region from BEG to END by collecting links and ensuring dependencies.
Generate replacements with titles and URLs, then download content if needed.
Called by `org-dex-archive-region' to process queued archive operations."
  (org-dex--check-dependencies)
  (org-dex--ensure-directory)
  
  (let* ((links '())
         (matches '())
	 (beg-val (if (markerp beg) beg
		    (let ((m (make-marker)))
		      (set-marker m (- beg 2))
		      (set-marker-insertion-type m t)
		      m)))

          (end-val (if (markerp end) end
		     (let ((m (make-marker)))
		       (set-marker m end)
		       (set-marker-insertion-type m t)
		       m)))
	  
         (link-matches (org-dex--collect-links beg-val end-val))
	 (_ (org-dex--region-has-links link-matches))
         (unmatched-groups (cdr (org-dex--group-links link-matches)))
         (order-types org-dex-archive-options-and-order))
    
    ;; First pass: Collect matches from ungrouped leaves
    (save-excursion
      (dolist (group unmatched-groups)
        (let* ((group-types (mapcar (lambda (x) (plist-get x :type)) group))

	       (start (plist-get (car group) :start))
	       (end (plist-get (car (last group)) :end))
	       
               (url-group (seq-find (lambda (x) (eq (plist-get x :type) :url)) group))
               (sf-group (seq-find (lambda (x) (eq (plist-get x :type) :sf)) group))
               (org-group (seq-find (lambda (x) (eq (plist-get x :type) :org)) group))
               (url-list (remove nil (list (org-dex--replace-domain (cdr (org-dex--get-title-and-url-from-org (plist-get org-group :link))))
                                           (org-dex--replace-domain (cdar (org-dex--get-title-and-url-from-sf (plist-get sf-group :link))))
                                           (org-dex--replace-domain (plist-get url-group :link)))))
               (title-list (remove nil (list (car (org-dex--get-title-and-url-from-org (plist-get org-group :link)))
                                             (caar (org-dex--get-title-and-url-from-sf (plist-get sf-group :link)))
                                             (plist-get url-group :desc))))
               (url nil)
               (title nil)
               (replacement-links '()))

          ;; Determine url, title, and descriptions based on group size
          (if (= (length group-types) 1)
              ;; Single-element group
              (let* ((type (car group-types))
                     (type-desc (pcase type
                                  (:url org-dex-url-description)
                                  (:sf org-dex-sf-description)
                                  (:org org-dex-org-description))))
                (setq url (car url-list))
                (setq title (car title-list))

		;; Set description to org-dex-<type>-description dynamically, respecting overrides
		(setq replacement-links (list (list type url (cond
							      ((eq type org-dex-url-override-type) url)
							      ((eq type org-dex-title-override-type) title)
							      (t (org-dex--eval-description type-desc)))))))
	    
            ;; Multiple-element group
            (progn
	      ;; Check if all URLs matches equal or single in case
              (if (or (null (cdr url-list)) (apply #'equal url-list))
                  (progn
                    (setq url (car url-list))
                    (setq title (car title-list))
                    ;; Set each type's description to org-dex-<type>-description
                    (dolist (match group)
                      (let* ((type (plist-get match :type))
                             (type-desc (pcase type
                                          (:url org-dex-url-description)
                                          (:sf org-dex-sf-description)
                                          (:org org-dex-org-description)))
                             (match-link (plist-get match :link)))
                        (push (list type match-link (cond
						     ((eq type org-dex-url-override-type) url)
						     ((eq type org-dex-title-override-type) title)
						     (t (org-dex--eval-description type-desc))))
			      replacement-links))))
		
		;; URLs differ: use only :url element
                (progn
                  (setq group (seq-filter (lambda (x) (eq (plist-get x :type) :url)) group))
                  (setq url (plist-get (car group) :link))
                  (setq title (plist-get (car group) :desc))
                  ;; Set description for :url element
                  (let ((type-desc org-dex-url-description))
                    (setq replacement-links (list (list :url url (cond
								  ((eq :url org-dex-url-override-type) url)
								  ((eq :url org-dex-title-override-type) title)
								  (t (org-dex--eval-description type-desc)))))))))))
	  
          ;; Proceed only if url and title are set
          (when (and url title)
            (let* ((dashed-desc (org-dex--hashed-file-name url title))
                   (sf-file (expand-file-name (concat org-dex-sf-directory "/" dashed-desc ".html")))
                   (org-file (expand-file-name (concat org-dex-org-directory "/" dashed-desc ".org"))))
	      
              ;; Add missing keys
              (dolist (key order-types)
                (unless (seq-find (lambda (x) (eq (car x) key)) replacement-links)
                  (let ((type-desc (pcase key
                                     (:url org-dex-url-description)
                                     (:sf org-dex-sf-description)
                                     (:org org-dex-org-description))))
                    (pcase key
                      (:url (push (list :url url (cond
						  ((eq :url org-dex-url-override-type) url)
						  ((eq :url org-dex-title-override-type) title)
						  (t (org-dex--eval-description type-desc))))
				  replacement-links))

		      ;; Add link to `links` to be consumed by org-dex--download-links			   
		      (:sf (let* ((hash (org-dex-hash-func (org-dex--process-link url)))
				  (existing-file (org-dex--sanitize-file-path
						  (org-dex--find-existing-sf-file hash)))
				  (new-file (org-dex--sanitize-file-path sf-file)))
			     (if existing-file
				 (if (not (equal existing-file new-file))
				     ;; Create a HTML shortcut to existing file
				     (org-dex--create-shortcut existing-file new-file title)
				   ;; Else skip, we already have the file
				 )
			       ;; Otherwise, queue for download
			       (push (cons url dashed-desc) links))
			     (push (list :sf new-file (cond
						       ((eq :sf org-dex-url-override-type) url)
						       ((eq :sf org-dex-title-override-type) title)
						       (t (org-dex--eval-description type-desc))))
				   replacement-links)))

		      
                      (:org (unless (file-exists-p (org-dex--sanitize-file-path org-file))
                              (write-region (concat "#+TITLE: " title "\n"
                                                    "#+URL: " url "\n"
                                                    "#+DATE: " (org-dex--insert-date-day) "\n\n"
                                                    "SingleFile: " (org-dex--generate-heading
                                                                    (list :sf sf-file title))
						    "\n")
                                            nil org-file t))
                            (push (list :org org-file (cond
						       ((eq :org org-dex-url-override-type) url)
						       ((eq :org org-dex-title-override-type) title)
						       (t (org-dex--eval-description type-desc))))
				  replacement-links)))))))

	      (setq replacement-links (nreverse (org-dex--remove-duplicates replacement-links)))
	    
	      ;; Generate replacement
              (let ((replacement (apply #'org-dex--generate-heading
                                        (seq-filter (lambda (pair) (seq-contains-p order-types (car pair)))
                                                    replacement-links))))
                (push (list start end replacement) matches))))))

    ;; Second pass: Perform replacements
    (save-excursion
      (dolist (match (reverse matches))
	(let* ((start-pos (nth 0 match))
	       (end-pos (nth 1 match))
	       (start (marker-position start-pos))
	       (end (marker-position end-pos))
	       (original (buffer-substring-no-properties start end))
	       (replacement (nth 2 match)))
	  (replace-region-contents start end (lambda () replacement)))))
    
    (deactivate-mark)

    (if links
	(org-dex--download-links (nreverse links))
      (setq org-dex--current-operation nil))))

(defun org-dex-archive-point ()
  "Archive the Org-mode link at point using `org-dex-archive-region`.
Detects the link under the cursor and processes it if it’s an Org-mode link.
Displays a message if no link is found."
  (interactive)
  (let ((link (org-element-context)))
    (if (eq (org-element-type link) 'link)
        (let ((start (org-element-property :begin link))
              (end (org-element-property :end link)))
          (org-dex-archive-region start end)))))

(transient-define-prefix org-dex-menu ()
  "A transient menu for org-dex commands to archive and manage web links in Org-mode.
Provides options for region, heading, and point operations, plus utility actions."
  :transient-suffix 'transient--do-stay
  :transient-non-suffix 'transient--do-exit
  ["Org-Dex Menu"
   ["Region Operations"
    ("a" "Archive Links" org-dex-archive-region)
    ("t" "Get Titles" org-dex-fetch-region)
    ("r" "Reset Links" org-dex-reset-region)
    ("m" "Revert Links" org-dex-revert-region)
    ("s" "Open all SingleFiles" org-dex-open-sf-region)
    ("u" "Open all URLs" org-dex-open-url-region)]
   ["Heading Operations"
    ("A" "Archive Links" org-dex-archive-heading)
    ("T" "Get Titles" org-dex-fetch-heading)
    ("R" "Reset Links" org-dex-reset-heading)
    ("M" "Revert Links" org-dex-revert-heading)
    ("S" "Open all SingleFiles" org-dex-open-sf-heading)
    ("U" "Open all URLs" org-dex-open-url-heading)]
   ["Point Operations"
    ("x" "Archive Link" org-dex-archive-point)
    ("z" "Get Title" org-dex-fetch-point)]
   ["Actions"
    ("l" "View Log Buffer" (lambda () (interactive) (switch-to-buffer "*org-dex-log*")))
    ("e" "View Error Buffer" (lambda () (interactive) (switch-to-buffer "*org-dex-err*")))
    ("J" "Reset All States" org-dex-reset-all-vars)
    ("K" "Kill All Operations" org-dex-kill-all-operations)
    ("c" "Customize Org-Dex" (lambda () (interactive) (customize-group 'org-dex)))
    ("q" "Quit" transient-quit-all)]])

(defun org-dex-open-sf-region (beg end)
  "Open all SingleFile links in the region from BEG to END.
BEG and END are positions or markers.
Opens each :sf link in the default browser."
  (interactive "r")
  (save-excursion
    (goto-char beg)
    (let* ((beg-val (if (markerp beg) beg
		       (let ((m (make-marker)))
			 (set-marker m beg)
			 (set-marker-insertion-type m t)
			 m)))

           (end-val (if (markerp end) end
		       (let ((m (make-marker)))
			 (set-marker m end)
			 (set-marker-insertion-type m t)
			 m)))
	    
	   (link-matches (org-dex--collect-links beg-val end-val))
	   (_ (org-dex--region-has-links link-matches)))
      
      (dolist (match link-matches)
	(if (eq (plist-get match :type) :sf)
            (browse-url (org-dex--sanitize-file-path (plist-get match :link))))))))

(defun org-dex-open-sf-heading ()
  "Move to the parent heading and open all SingleFile links under it.
Uses `org-dex-open-sf-region` to process the subtree."
  (interactive)
  (org-up-heading-all 1)
  (let ((start (point))
        (end (save-excursion (org-end-of-subtree t t) (point))))
    (org-dex-open-sf-region start end)))

(defun org-dex-open-url-region (beg end)
  "Open all URL links in the region from BEG to END.
BEG and END are positions or markers.
Opens each :url link in the default browser."
  (interactive "r")
  (save-excursion
    (goto-char beg)
    (let* ((beg-val (if (markerp beg) beg
		      (let ((m (make-marker)))
			(set-marker m beg)
			(set-marker-insertion-type m t)
			m)))

           (end-val (if (markerp end) end
		      (let ((m (make-marker)))
			(set-marker m end)
			(set-marker-insertion-type m t)
			m)))

	   (link-matches (org-dex--collect-links beg-val end-val))
	   (_ (org-dex--region-has-links link-matches)))
      
      (dolist (match link-matches)
	(if (eq (plist-get match :type) :url)
            (browse-url (plist-get match :link)))))))

(defun org-dex-open-url-heading ()
  "Move to the parent heading and open all URL links under it.
Uses `org-dex-open-url-region` to process the subtree."
  (interactive)
  (org-up-heading-all 1)
  (let ((start (point))
        (end (save-excursion (org-end-of-subtree t t) (point))))
    (org-dex-open-url-region start end)))

(defun org-dex--check-dependencies ()
  "Check if required dependencies are available.
Verifies SingleFile CLI, Curl, HTMLq and CRC function availability, signaling errors if missing."
  (unless (executable-find org-dex-sf-command)
    (error "SingleFile CLI (%s) not found. Please install it and ensure it's in your PATH." org-dex-sf-command))
  (unless (executable-find "curl")
    (error "Curl not found. Please install it and ensure it's in your PATH."))
  (unless (executable-find "htmlq")
    (error "HTMLq not found. Please install it and ensure it's in your PATH."))
  (unless (fboundp 'org-dex-hash-func)
    (error "CRC function '%s' is not defined. Please install a CRC32 library (e.g., crc32.el or Emacs-CRC)." org-dex-hash-func)))

(defun org-dex--region-has-links (link-matches)
  "Check if LINK-MATCHES contains any Org-mode links.
Argument LINK-MATCHES is a list from `org-dex--collect-links`.
If empty, throws 'no-link. Returns nil if links exist."
  (catch 'no-link
    (if (null link-matches)
        (progn
          (deactivate-mark)
	  (throw 'no-link "no link found")))))

(defun org-dex--sanitize-file-path (path)
  "Return a sanitized, expanded file path from PATH, removing 'file:' prefix.
Argument PATH is the file path to process.
Returns nil if PATH is nil, otherwise the expanded path."
  (when path
    (expand-file-name (org-dex--remove-file-prefix path))))

(defun org-dex--hashed-file-name (url title)
  "Generate a hashed filename using URL and TITLE.
Uses `org-dex-hash-func` for the hash, combined with a dashed title per `org-dex-hash-location`.
Returns a string suitable for filenames."
  (let ((link-hash (org-dex-hash-func (org-dex--process-link url))))
    (setq title (if (or (not title) (string-empty-p title)) (org-dex--process-link url) title))
    (pcase org-dex-hash-location
      ('append (concat (org-dex--dashed-string title) "-" link-hash))
      ('prepend (concat link-hash "-" (org-dex--dashed-string title))))))

(defcustom org-dex-file-name-func #'org-dex--hashed-file-name
  "Function to generate filenames for archived files.
Takes URL and TITLE as arguments and returns a string."
  :type 'function
  :group 'org-dex)

(defun org-dex--collect-urls (beg end)
  "Collect raw URLs in the region from BEG to END, ignoring those within Org-mode links.
BEG and END are positions or markers.
Returns a list of plists with :link, :start, and :end for each raw URL."
  (let ((beg-pos (if (markerp beg) (marker-position beg) beg))
        (end-pos (if (markerp end) (marker-position end) end))
        (pattern (rx (group (or "http://" "https://")
                            (+ (not (any space ?\n))))))
	(org-link-regions (mapcar (lambda (link)
                                    (cons (plist-get link :start)
                                          (plist-get link :end)))
				  (org-dex--collect-links beg end)))
        (matches '()))
    (save-excursion
      ;; Collect raw URLs, excluding those within Org-link regions
      (goto-char beg-pos)
      (while (re-search-forward pattern end-pos t)
        (let* ((match-start (let ((m (make-marker)))
			      (set-marker m (match-beginning 0))
			      (set-marker-insertion-type m t)
			      m))
	       
               (match-last (let ((m (make-marker)))
			     (set-marker m (match-end 0))
			     (set-marker-insertion-type m t)
			     m))
	       
               (link (match-string-no-properties 1))
               (inside-org-link (seq-some (lambda (region)
                                            (let ((region-start (marker-position (car region)))
                                                  (region-end (marker-position (cdr region)))
                                                  (start-pos (marker-position match-start))
                                                  (end-pos (marker-position match-last)))
					      
                                              (and (>= start-pos region-start)
                                                   (<= end-pos region-end))))
					  
                                          org-link-regions)))
          (unless inside-org-link
            (push (list :link link
                        :start match-start
                        :end match-last)
                  matches)))))
    (nreverse matches)))

(defcustom org-dex-url-override-type nil
  "Specify a link type (:url, :sf, or :org) to override its description with the URL itself.
If nil, no override occurs."
  :type '(choice
          (const :tag "URL" :url)
          (const :tag "SingleFile" :sf)
          (const :tag "Org" :org)
	  (const :tag "None" nil))
  :group 'org-dex)

(defvar org-dex--title-queue nil
  "Queue of URLs to fetch titles for, with their metadata.")

(defvar org-dex--title-results nil
  "List of fetched titles and their metadata.")

(defvar org-dex--title-processing nil
  "Flag indicating if a title fetch process is currently running.")

(defvar org-dex--title-queue-length 0
  "Count of URLs to be processed for titles.")

(defun org-dex--fetch-titles (links)
  "Fetch titles for URLs in LINKS using curl + htmlq or SingleFile.
Argument LINKS is a list of plists from `org-dex--collect-urls`.
Initializes the title queue and starts fetching sequentially."
  ;; Reset and initialize queue
  (setq org-dex--title-queue links
        org-dex--title-results nil
        org-dex--title-queue-length (length links)
        org-dex--title-processing nil)

  ;; Start fetching the titles
  (org-dex--fetch-next-title))

(defcustom org-dex-curl-command '("curl" "-Ls")
  "CURL command with arguments for fetching titles."
  :type '(repeat string)
  :group 'org-dex)

(defcustom org-dex-htmlq-command '("|" "htmlq" "--text" "title")
  "HTMLQ command with arguments for extracting titles."
  :type '(repeat string)
  :group 'org-dex)

(defun org-dex--fetch-next-title ()
  "Fetch the next title from `org-dex--title-queue` if no process is running.
Uses curl + htmlq for most domains, or SingleFile for domains in `org-dex-title-domains`.
Falls back to SingleFile if curl fails to retrieve a title, skipping download if file exists.
Logs progress and stores results in `org-dex--title-results`."
  (if (and org-dex--title-queue (not org-dex--title-processing))
      ;; Process next link if queue has items
      (let* ((entry (pop org-dex--title-queue))
             (link (org-dex--replace-domain (plist-get entry :link)))
             (domain (replace-regexp-in-string
		      ;; Regex "^https?://\\([^/]+\\)/.*"
                      (rx (seq bol "http" (opt "s") "://"
                               (group (+ (not (any "/"))))
                               "/" (0+ nonl)))
                      "\\1" link))
             (base-domain (replace-regexp-in-string "^www\\." "" domain))
             (use-sf (if (member base-domain org-dex-title-domains) t nil))
             (link-hash (org-dex-hash-func (org-dex--process-link (org-dex--replace-domain link))))
             (sf-command (list org-dex-sf-command link
                               (format "--filename-template=%s.html" link-hash)
                               (format "--output-directory=%s" (expand-file-name org-dex-sf-directory))))
             (progress (format "[%d/%d]" (- org-dex--title-queue-length (length org-dex--title-queue)) org-dex--title-queue-length))
             (existing-file (org-dex--find-existing-sf-file link-hash)))

        ;; Log the fetch attempt
        (org-dex--log-message (format "%s Fetching Title: %s" progress link) "purple")
        (setq org-dex--title-processing t)

        ;; Handle existing file or start a process
        (if existing-file
            (let* ((title (caar (org-dex--get-title-and-url-from-sf existing-file)))
                   (updated-entry (plist-put entry :title (if (or (not title) (string-empty-p title)) (org-dex--process-link link) title))))
              (when (and title (not (member updated-entry org-dex--title-results)))
                (push updated-entry org-dex--title-results))
              (setq org-dex--title-processing nil))

          ;; No existing file, start the appropriate process
          (let ((process (make-process
                          :name (concat link-hash "-org-dex-fetch")
                          :command (if use-sf
                                       (append org-dex-shell sf-command org-dex-sf-cmd-extra-args)
                                     (append org-dex-shell org-dex-curl-command
                                             (list (format "%s" link))
                                             org-dex-htmlq-command))
                          :buffer "*last-fetched-title*"
                          :stderr "*fetch-titles-err*")))
            (process-put process 'entry entry)
            (process-put process 'use-sf use-sf)
            (process-put process 'link-hash link-hash)

            (set-process-sentinel
             process
             (lambda (proc event)
               (setq org-dex--title-processing nil)
               (let* ((exit-code (process-exit-status proc))
                      (entry (process-get proc 'entry))
                      (use-sf (process-get proc 'use-sf))
                      (link-hash (process-get proc 'link-hash))
                      (link (org-dex--replace-domain (plist-get entry :link)))
                      (sf-file (expand-file-name (concat org-dex-sf-directory "/" link-hash ".html"))))
                 (if (= exit-code 0)
                     (if use-sf
                         ;; Handle SingleFile success
                         (let* ((title (caar (org-dex--get-title-and-url-from-sf sf-file)))
                                (sf-file-updated (when title (expand-file-name (concat org-dex-sf-directory "/"
                                                                                       (org-dex--hashed-file-name link title)
                                                                                       ".html"))))
                                (updated-entry (plist-put entry :title (if (or (not title) (string-empty-p title)) (org-dex--process-link link) title))))
                           (when (and title (not (file-exists-p sf-file-updated)))
                             (rename-file sf-file sf-file-updated))
                           (when (and title (not (member updated-entry org-dex--title-results)))
                             (push updated-entry org-dex--title-results))
			   (org-dex--fetch-next-title))
                       ;; Handle curl success, fallback to SingleFile if title is empty
                       (with-current-buffer (process-buffer proc)
                         (let* ((output (string-trim (buffer-string)))
                                (title (car (string-lines output t)))
                                (updated-entry (plist-put entry :title (if (or (not title) (string-empty-p title)) (org-dex--process-link link) title))))
                           (kill-buffer)
                           (if (or (not title) (string-empty-p title))
                               ;; Fallback to SingleFile
			       (progn 
				 (org-dex--log-message "Titles: CURL failed, Trying SingleFile Fetch" "yellow")
				 (let ((sf-process (make-process
                                                    :name (concat link-hash "-org-dex-archive")
                                                    :command (append org-dex-shell sf-command org-dex-sf-cmd-extra-args)
                                                    :buffer "*last-fetched-title*"
                                                    :stderr "*fetch-titles-err*")))
                                   (org-dex--log-message (format "%s Fetching SF-Title: %s" progress link) "purple")
                                   (setq org-dex--title-processing t)
                                   (process-put sf-process 'entry entry)
                                   (process-put sf-process 'use-sf t)
                                   (process-put sf-process 'link-hash link-hash)
                                   (set-process-sentinel sf-process (process-sentinel proc))))
                             ;; Curl succeeded with title
                             (progn
                               (when (and title (not (member updated-entry org-dex--title-results)))
                                 (push updated-entry org-dex--title-results))
			       (org-dex--fetch-next-title))))))
                   ;; Handle process failure
                   (progn
                     (org-dex--log-message
                      (format "Failed: %s (exit code %d). Check *fetch-titles-err* buffer for errors."
                              link exit-code)
                      "red")
		     (org-dex--fetch-next-title))))))))
	
	(org-dex--fetch-next-title))
    
    ;; Queue is empty, print completion message
    (when (not org-dex--title-processing)
      (setq org-dex--current-operation nil)
      (org-dex--log-message "Titles: FETCH done." "green")
      (org-dex--process-operation-queue))))

(defun org-dex-fetch-heading ()
  "Move to the parent heading, fetch titles for raw URLs, and convert them to Org-mode links.
Processes the entire subtree using `org-dex-fetch-region`."
  (interactive)
  (org-up-heading-all 1)
  (let ((start (point))
        (end (save-excursion (org-end-of-subtree t t) (point))))
    (org-dex-fetch-region start end)))

(defun org-dex-fetch-point ()
  "Convert a raw URL at point to an Org-mode link with its fetched title.
Searches the current line for a raw URL containing the point.
Displays a message if no raw URL is found."
  (interactive)
  (let* ((pattern (rx (group (or "http://" "https://")
                             (+ (not (any space ?\n))))))
         (url-bounds (save-excursion
                       ;; Move to start of line to ensure we catch the URL
                       (beginning-of-line)
                       (when (re-search-forward pattern (line-end-position) t)
                         (cons (match-beginning 0) (match-end 0)))))
         (beg (car url-bounds))
         (end (cdr url-bounds)))
    (if (and beg end
             (<= beg (point)) (>= end (point)))  ;; Ensure point is within URL
          (org-dex-fetch-region beg end)
      (message "No raw URL found at point."))))

(defun org-dex--do-fetch-titles (beg end)
  "Fetch titles for URLs in region from BEG to END by collecting links.
Process URLs and update titles if found, otherwise clear current operation.
Called by `org-dex-fetch-region' to handle queued fetch operations."
  (let* ((beg-val (if (markerp beg) beg
		    (let ((m (make-marker)))
		      (set-marker m (- beg 2))
		      (set-marker-insertion-type m t)
		      m)))
	 
         (end-val (if (markerp end) end
		    (let ((m (make-marker)))
		      (set-marker m end)
		      (set-marker-insertion-type m t)
		      m)))

         (links (org-dex--collect-urls beg-val end-val)))

    (deactivate-mark)
    
    (if links
	(org-dex--fetch-titles links)
      (setq org-dex--current-operation nil))))

(defun org-dex--do-update-buffer ()
  "Update the buffer with Org-mode links using fetched titles from `org-dex--title-results`.
Replaces raw URLs with formatted Org-mode links containing their titles."
  (save-excursion
    (let ((results (nreverse org-dex--title-results)))
      (dolist (result results)
	(let* ((start-pos (plist-get result :start))
	       (end-pos (plist-get result :end))
	       (start (marker-position start-pos))
	       (end (marker-position end-pos))
	       (link (plist-get result :link))
	       (title (plist-get result :title))
	       (original (buffer-substring-no-properties start end))
	       (replacement (format "[[%s][%s]]" link title)))
	   (replace-region-contents start end (lambda () replacement)))))))

(defcustom org-dex-title-domains '("reddit.com")
  "List of domains for which to use SingleFile to fetch titles instead of curl + htmlq."
  :type '(repeat string)
  :group 'org-dex)

(defun org-dex--remove-duplicates (links)
  "Remove duplicate entries from LINKS, keeping the first occurrence.
Argument LINKS is a list of (type path description) lists.
Returns a new list with duplicates removed based on exact triplet matches."
  (let ((seen (make-hash-table :test 'equal))
        (unique-links '()))
    (dolist (link links)
      (let ((key link))
        (unless (gethash key seen)
          (puthash key t seen)
          (push link unique-links))))
    (nreverse unique-links)))

(defun org-dex--create-shortcut (target shortcut title)
  "Create an HTML file at SHORTCUT that auto-opens the TARGET HTML file with TITLE."
  (write-region (concat "<!DOCTYPE html>\n"
                        "<html>\n"
                        "<head>\n"
			"    <title>" title "</title>\n"
                        "    <meta http-equiv=\"refresh\" content=\"0; url=file:///" (expand-file-name target) "\">\n"
                        "</head>\n"
                        "<body>\n"
                        "    <p><a href=\"file:///" (expand-file-name target) "\">Open original file</a></p>\n"
                        "</body>\n"
                        "</html>\n")
                nil shortcut))

(defun org-dex--find-existing-sf-file (hash)
  "Find an existing SingleFile with the given HASH in `org-dex-sf-directory`.
The hash location (prepend or append) is determined by `org-dex-hash-location`.
Returns the full path of the first matching file or nil if none found."
  (let ((pattern (pcase org-dex-hash-location
                   ('prepend (concat "^" hash "-.*\\.html$"))
                   ('append (concat "^.*-" hash "\\.html$")))))
    (let ((files (directory-files org-dex-sf-directory t pattern)))
      (car files))))

(defun org-dex-kill-all-operations ()
  "Kill all ongoing org-dex operations for title fetching and archiving.
Interactively callable command that terminates all running processes spawned by
org-dex, identified by names matching the pattern '[hash]-org-dex-(archive|fetch)'.
Iterates over all Emacs processes, deletes matching ones, and counts how many were
killed. Calls `org-dex-reset-all-vars' to clear all org-dex state variables,
including `org-dex--operation-queue', queues, and processing flags. Logs and
displays a message indicating the number of processes killed if any, or a notice if
none were found."
  (interactive)
  (let ((killed-count 0))
    ;; Iterate over all Emacs processes
    (dolist (proc (process-list))
      (let ((proc-name (process-name proc)))
        ;; Match processes with hash-like names from org-dex
	(when (string-match-p (rx bol (one-or-more digit) "-org-dex-" (or "archive" "fetch") eol) proc-name)
          (delete-process proc)
          (setq killed-count (1+ killed-count)))))

    (org-dex-reset-all-vars)
    
    (if (> killed-count 0)
        (progn
          (org-dex--log-message (format "Killed %d org-dex processes" killed-count) "red")
          (message "Killed %d org-dex processes" killed-count))
      (message "No org-dex processes found to kill"))))

(defun org-dex-reset-all-vars ()
  "Reset all org-dex state variables to their initial values.
Clears `org-dex--operation-queue' if it exists using `queue-clear', and sets all
other org-dex variables to nil, including queues, processing flags, lengths, results,
and the current operation. This effectively stops and resets all ongoing operations
without killing any running processes. Intended for use when a full reset of the
org-dex system is needed, such as before starting a new batch of operations."
  (interactive)
  (when org-dex--operation-queue
    (queue-clear org-dex--operation-queue))
  
  (setq org-dex--links-queue nil
	org-dex--links-processing nil
	org-dex--links-queue-length nil
	
	org-dex--title-queue nil
	org-dex--title-results nil
	org-dex--title-processing nil
	org-dex--title-queue-length nil
	
	org-dex--current-operation nil)
  (message "All Variable States RESET done."))

(defvar org-dex--current-operation nil
  "Active org-dex operation as (BUFFER-OR-FILE REGION OP).
See `org-dex--operation-queue' for entry format. Nil when idle; ensures single operation execution.")

(defcustom org-dex-auto-archive-raw-urls t
  "If non-nil, automatically archive raw URLs after fetching their titles.
When t. If nil, archiving must be triggered manually. Default is t."
  :type 'boolean
  :group 'org-dex)

(defvar org-dex--operation-queue nil
  "Queue of org-dex operations as (BUFFER-OR-FILE REGION OP) entries.
BUFFER-OR-FILE is a buffer or file, REGION is a cons (BEG . END) of markers or positions, OP is :fetch, :update, :archive, or :revert.
Nil until initialized by `org-dex--add-operation'; cleared by reset or kill functions.")

(provide 'org-dex)
;;; org-dex.el ends here
