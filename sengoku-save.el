;;; sengoku-save.el --- Vim-compatible saves for Sengoku -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: dkc
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: games

;;; Commentary:

;; Encode, validate, save, and load the version 5 JSON format used by the Vim
;; implementation.  Only persistent campaign state is serialized; general-use
;; flags and controller, battle, siege, session, and UI state are reset on load.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'sengoku-core)

(defgroup sengoku nil
  "Sengoku game settings."
  :group 'games)

(defcustom sengoku-save-file
  (locate-user-emacs-file "sengoku-save.json")
  "File used to save and load Sengoku campaigns."
  :type 'file
  :group 'sengoku)

(defconst sengoku-save-legacy-file
  (expand-file-name "~/.vim-sengoku-save.json")
  "Legacy save file used by the Vim implementation.")

(defconst sengoku-save-version 5
  "Vim JSON save schema version supported by this module.")

(define-error 'sengoku-save-error "Sengoku save error")
(define-error 'sengoku-save-format-error
  "Invalid Sengoku save data" 'sengoku-save-error)
(define-error 'sengoku-save-version-error
  "Unsupported Sengoku save version" 'sengoku-save-error)
(define-error 'sengoku-save-io-error
  "Sengoku save file error" 'sengoku-save-error)

(defconst sengoku-save--top-level-keys
  '("ver" "year" "month" "player" "prov" "log" "cstate" "ally"))

(defconst sengoku-save--province-keys
  '("name" "owner" "gr" "gc" "adjn" "adj" "koku" "comm" "sol"
    "loyal" "train" "guns" "cannon" "port" "gold" "rice" "auto"))

(defconst sengoku-save--clan-state-keys '("cul" "items" "rank"))

(defun sengoku-save--format-error (format-string &rest arguments)
  "Signal a save format error described by FORMAT-STRING and ARGUMENTS."
  (signal 'sengoku-save-format-error
          (list (apply #'format format-string arguments))))

(defun sengoku-save--resignal (error-data)
  "Re-signal ERROR-DATA without changing its error symbol or details."
  (signal (car error-data) (cdr error-data)))

(defun sengoku-save--normalize-key (key context)
  "Return object KEY as a string, or signal an error mentioning CONTEXT."
  (cond
   ((stringp key) key)
   ((symbolp key) (symbol-name key))
   (t
    (sengoku-save--format-error
     "%s has a non-string object key: %S" context key))))

(defun sengoku-save--object-entries (object context)
  "Return normalized string-key entries from OBJECT for CONTEXT.
OBJECT may be a hash table or an alist.  Nil is accepted as the alist
representation of an empty JSON object."
  (cond
   ((hash-table-p object)
    (let (entries)
      (maphash
       (lambda (key value)
         (push (cons (sengoku-save--normalize-key key context) value)
               entries))
       object)
      (nreverse entries)))
   ((null object) nil)
   ((and (listp object)
         (proper-list-p object)
         (cl-every
          (lambda (entry)
            (and (consp entry)
                 (or (stringp (car entry)) (symbolp (car entry)))))
          object))
    (mapcar
     (lambda (entry)
       (cons (sengoku-save--normalize-key (car entry) context)
             (cdr entry)))
     object))
   (t
    (sengoku-save--format-error
     "%s must be a JSON object (hash table or alist), found %S"
     context object))))

(defun sengoku-save--exact-object-entries (object expected-keys context)
  "Return OBJECT entries after checking EXPECTED-KEYS exactly for CONTEXT."
  (let ((entries (sengoku-save--object-entries object context))
        (seen (make-hash-table :test #'equal))
        unexpected)
    (dolist (entry entries)
      (let ((key (car entry)))
        (when (gethash key seen)
          (sengoku-save--format-error
           "%s contains duplicate key %S" context key))
        (puthash key t seen)
        (unless (member key expected-keys)
          (push key unexpected))))
    (let (missing)
      (dolist (key expected-keys)
        (unless (gethash key seen)
          (push key missing)))
      (when (or missing unexpected)
        (sengoku-save--format-error
         "%s must contain exactly keys [%s]%s%s"
         context
         (string-join expected-keys ", ")
         (if missing
             (format "; missing [%s]" (string-join (nreverse missing) ", "))
           "")
         (if unexpected
             (format "; unexpected [%s]"
                     (string-join (nreverse unexpected) ", "))
           ""))))
    entries))

(defun sengoku-save--field (entries key)
  "Return the value for string KEY in normalized ENTRIES."
  (cdr (assoc key entries)))

(defun sengoku-save--array-elements (value context)
  "Return JSON array VALUE as a fresh list for CONTEXT.
Vectors and proper lists are accepted so both modern and legacy Emacs JSON
representations can be passed to the public object decoder."
  (cond
   ((vectorp value) (append value nil))
   ((and (listp value) (proper-list-p value)) (copy-sequence value))
   (t
    (sengoku-save--format-error
     "%s must be a JSON array (vector or list), found %S" context value))))

(defun sengoku-save--array-with-count (value expected-count context)
  "Return array VALUE after requiring EXPECTED-COUNT elements for CONTEXT."
  (let ((elements (sengoku-save--array-elements value context)))
    (unless (= (length elements) expected-count)
      (sengoku-save--format-error
       "%s must contain %d elements, found %d"
       context expected-count (length elements)))
    elements))

(defun sengoku-save--string (value context)
  "Return VALUE after requiring a string for CONTEXT."
  (unless (stringp value)
    (sengoku-save--format-error "%s must be a string, found %S" context value))
  value)

(defun sengoku-save--integer (value context &optional minimum maximum)
  "Return integer VALUE within optional MINIMUM and MAXIMUM for CONTEXT."
  (unless (integerp value)
    (sengoku-save--format-error "%s must be an integer, found %S" context value))
  (when (and minimum (< value minimum))
    (sengoku-save--format-error
     "%s must be at least %d, found %S" context minimum value))
  (when (and maximum (> value maximum))
    (sengoku-save--format-error
     "%s must be at most %d, found %S" context maximum value))
  value)

(defun sengoku-save--flag (value context)
  "Return VALUE after requiring Vim's numeric 0/1 representation for CONTEXT."
  (unless (memq value '(0 1))
    (sengoku-save--format-error "%s must be numeric 0 or 1, found %S"
                                context value))
  value)

(defun sengoku-save--string-vector (value context)
  "Return array VALUE as a vector of strings for CONTEXT."
  (let ((elements (sengoku-save--array-elements value context))
        (index 0))
    (dolist (element elements)
      (sengoku-save--string element (format "%s[%d]" context index))
      (setq index (1+ index)))
    (vconcat elements)))

(defun sengoku-save--integer-vector (value context &optional minimum maximum)
  "Return array VALUE as an integer vector for CONTEXT.
When non-nil, MINIMUM and MAXIMUM bound every element."
  (let ((elements (sengoku-save--array-elements value context))
        (index 0))
    (dolist (element elements)
      (sengoku-save--integer element (format "%s[%d]" context index)
                             minimum maximum)
      (setq index (1+ index)))
    (vconcat elements)))

(defun sengoku-save--fresh-game ()
  "Return a fresh deterministic game suitable as a load target.
All persisted randomized province values are replaced by the decoder, so a
zero-valued random source avoids advancing the user's random stream."
  (let ((sengoku-random-function (lambda (_upper-bound) 0))
        (sengoku-debug-gold 0))
    (sengoku-new-game)))

(defun sengoku-save--decode-province (object province-index template clan-count)
  "Decode province OBJECT at PROVINCE-INDEX using TEMPLATE and CLAN-COUNT."
  (let* ((context (format "prov[%d]" province-index))
         (entries (sengoku-save--exact-object-entries
                   object sengoku-save--province-keys context))
         (name (sengoku-save--string
                (sengoku-save--field entries "name")
                (format "%s.name" context)))
         (owner (sengoku-save--integer
                 (sengoku-save--field entries "owner")
                 (format "%s.owner" context) -1 (1- clan-count)))
         (grid-row (sengoku-save--integer
                    (sengoku-save--field entries "gr")
                    (format "%s.gr" context)))
         (grid-column (sengoku-save--integer
                       (sengoku-save--field entries "gc")
                       (format "%s.gc" context)))
         (adjacency-names
          (sengoku-save--string-vector
           (sengoku-save--field entries "adjn")
           (format "%s.adjn" context)))
         (adjacency
          (sengoku-save--integer-vector
           (sengoku-save--field entries "adj")
           (format "%s.adj" context) 0 (1- sengoku-data-province-count)))
         (koku (sengoku-save--integer
                (sengoku-save--field entries "koku")
                (format "%s.koku" context) 0))
         (commerce (sengoku-save--integer
                    (sengoku-save--field entries "comm")
                    (format "%s.comm" context) 0))
         (soldiers (sengoku-save--integer
                    (sengoku-save--field entries "sol")
                    (format "%s.sol" context) 0))
         (loyalty (sengoku-save--integer
                   (sengoku-save--field entries "loyal")
                   (format "%s.loyal" context) 0 100))
         (training (sengoku-save--integer
                    (sengoku-save--field entries "train")
                    (format "%s.train" context) 0 100))
         (guns (sengoku-save--integer
                (sengoku-save--field entries "guns")
                (format "%s.guns" context) 0))
         (cannon (sengoku-save--integer
                  (sengoku-save--field entries "cannon")
                  (format "%s.cannon" context) 0))
         (port (sengoku-save--flag
                (sengoku-save--field entries "port")
                (format "%s.port" context)))
         (gold (sengoku-save--integer
                (sengoku-save--field entries "gold")
                (format "%s.gold" context) 0))
         (rice (sengoku-save--integer
                (sengoku-save--field entries "rice")
                (format "%s.rice" context) 0))
         (auto (sengoku-save--flag
                (sengoku-save--field entries "auto")
                (format "%s.auto" context))))
    (unless (string= name (sengoku-province-name template))
      (sengoku-save--format-error
       "%s.name must be %S to preserve province order, found %S"
       context (sengoku-province-name template) name))
    (unless (= grid-row (sengoku-province-grid-row template))
      (sengoku-save--format-error
       "%s.gr must be %d, found %d"
       context (sengoku-province-grid-row template) grid-row))
    (unless (= grid-column (sengoku-province-grid-column template))
      (sengoku-save--format-error
       "%s.gc must be %d, found %d"
       context (sengoku-province-grid-column template) grid-column))
    (unless (equal adjacency-names
                   (sengoku-province-adjacency-names template))
      (sengoku-save--format-error
       "%s.adjn does not match the canonical ordered adjacency names"
       context))
    (unless (equal adjacency (sengoku-province-adjacency template))
      (sengoku-save--format-error
       "%s.adj does not match the canonical ordered adjacency indices"
       context))
    (make-sengoku-province
     :name name
     :owner owner
     :grid-row grid-row
     :grid-column grid-column
     :adjacency-names adjacency-names
     :adjacency adjacency
     :koku koku
     :commerce commerce
     :soldiers soldiers
     :loyalty loyalty
     :training training
     :guns guns
     :cannon cannon
     :port port
     :gold gold
     :rice rice
     :auto auto)))

(defun sengoku-save--decode-clan-state (object clan-index item-owners)
  "Decode clan-state OBJECT at CLAN-INDEX, updating ITEM-OWNERS validation."
  (let* ((context (format "cstate[%d]" clan-index))
         (entries (sengoku-save--exact-object-entries
                   object sengoku-save--clan-state-keys context))
         (culture (sengoku-save--integer
                   (sengoku-save--field entries "cul")
                   (format "%s.cul" context) 0 100))
         (rank (sengoku-save--integer
                (sengoku-save--field entries "rank")
                (format "%s.rank" context) 0 (1- (length sengoku-data-ranks))))
         (items (sengoku-save--array-elements
                 (sengoku-save--field entries "items")
                 (format "%s.items" context)))
         (item-position 0))
    (dolist (item-index items)
      (sengoku-save--integer
       item-index (format "%s.items[%d]" context item-position)
       0 (1- sengoku-data-item-count))
      (when (>= (aref item-owners item-index) 0)
        (sengoku-save--format-error
         "Item %d is listed more than once (cstate[%d] and cstate[%d])"
         item-index (aref item-owners item-index) clan-index))
      (aset item-owners item-index clan-index)
      (setq item-position (1+ item-position)))
    (make-sengoku-clan-state
     :culture culture
     :items items
     :rank rank)))

(defun sengoku-save--decode-alliances (object clan-count)
  "Decode alliance OBJECT for CLAN-COUNT clans into an equal hash table."
  (let ((entries (sengoku-save--object-entries object "ally"))
        (seen (make-hash-table :test #'equal))
        (alliances (make-hash-table :test #'equal)))
    (dolist (entry entries)
      (let ((key (car entry))
            (value (cdr entry)))
        (when (gethash key seen)
          (sengoku-save--format-error "Ally contains duplicate key %S" key))
        (puthash key t seen)
        (unless (string-match "\\`\\([0-9]+\\)-\\([0-9]+\\)\\'" key)
          (sengoku-save--format-error
           "Ally key %S must have canonical A-B form" key))
        (let ((clan-a (string-to-number (match-string 1 key)))
              (clan-b (string-to-number (match-string 2 key))))
          (unless (and (< clan-a clan-b)
                       (< clan-b clan-count)
                       (string= key (sengoku-alliance-key clan-a clan-b)))
            (sengoku-save--format-error
             "Ally key %S is not canonical for clan indices 0..%d"
             key (1- clan-count))))
        (unless (and (integerp value) (= value 1))
          (sengoku-save--format-error
           "Ally.%s must be numeric 1, found %S" key value))
        (puthash key 1 alliances)))
    alliances))

(defun sengoku-save--decode-log (value)
  "Decode log array VALUE, preserving order and enforcing the Vim save cap."
  (let ((entries (sengoku-save--array-elements value "log"))
        (index 0))
    (when (> (length entries) 30)
      (sengoku-save--format-error
       "log may contain at most 30 entries, found %d" (length entries)))
    (dolist (entry entries)
      (sengoku-save--string entry (format "log[%d]" index))
      (setq index (1+ index)))
    entries))

(defun sengoku-save-decode-object (object)
  "Validate Vim v5 save OBJECT and return a newly initialized game.

OBJECT may use hash tables or alists for JSON objects and vectors or proper
lists for JSON arrays.  The decoder never mutates an existing game.  It starts
from `sengoku-new-game', replaces only persistent fields, restores the static
general rosters, clears all general-use flags, and clears transient game state."
  (let* ((entries (sengoku-save--exact-object-entries
                   object sengoku-save--top-level-keys "save"))
         (version (sengoku-save--field entries "ver")))
    (unless (and (integerp version) (= version sengoku-save-version))
      (signal 'sengoku-save-version-error
              (list (format "expected version %d, found %S"
                            sengoku-save-version version))))
    (let* ((year (sengoku-save--integer
                  (sengoku-save--field entries "year") "year" 1))
           (month (sengoku-save--integer
                   (sengoku-save--field entries "month") "month" 1 12))
           (player (sengoku-save--integer
                    (sengoku-save--field entries "player") "player"
                    -1 (1- sengoku-data-clan-count)))
           (province-objects
            (sengoku-save--array-with-count
             (sengoku-save--field entries "prov")
             sengoku-data-province-count "prov"))
           (log (sengoku-save--decode-log
                 (sengoku-save--field entries "log")))
           (clan-state-objects
            (sengoku-save--array-with-count
             (sengoku-save--field entries "cstate")
             sengoku-data-clan-count "cstate"))
           (game (sengoku-save--fresh-game))
           (templates (sengoku-game-provinces game))
           (provinces (make-vector sengoku-data-province-count nil))
           (clan-states (make-vector sengoku-data-clan-count nil))
           (item-owners (make-vector sengoku-data-item-count -1))
           (alliances
            (sengoku-save--decode-alliances
             (sengoku-save--field entries "ally")
             sengoku-data-clan-count)))
      (dotimes (province-index sengoku-data-province-count)
        (aset provinces province-index
              (sengoku-save--decode-province
               (nth province-index province-objects)
               province-index
               (aref templates province-index)
               sengoku-data-clan-count)))
      (dotimes (clan-index sengoku-data-clan-count)
        (aset clan-states clan-index
              (sengoku-save--decode-clan-state
               (nth clan-index clan-state-objects)
               clan-index item-owners)))
      (setf (sengoku-game-year game) year
            (sengoku-game-month game) month
            (sengoku-game-player game) player
            (sengoku-game-provinces game) provinces
            (sengoku-game-log game) log
            (sengoku-game-clan-states game) clan-states
            (sengoku-game-alliances game) alliances
            (sengoku-game-turn-queue game) nil
            (sengoku-game-pending-battle game) nil)
      (sengoku-reset-generals game)
      (let ((errors (sengoku-validate-game game)))
        (when errors
          (sengoku-save--format-error
           "Decoded game failed validation: %s"
           (mapconcat #'identity errors "; "))))
      game)))

(defun sengoku-save-decode-string (string)
  "Parse and decode Vim v5 JSON STRING into a fresh game."
  (unless (stringp string)
    (sengoku-save--format-error
     "Save JSON must be a string, found %S" string))
  (let ((object
         (condition-case error-data
             ;; Use the Lisp implementation available on every supported
             ;; Emacs 27 build.  The native Jansson entry points are optional
             ;; and can return nil when their runtime DLL is unavailable.
             (let ((json-object-type 'hash-table)
                   (json-array-type 'vector)
                   (json-key-type 'string)
                   (json-null :sengoku-json-null)
                   (json-false :sengoku-json-false))
               (json-read-from-string string))
           (error
            (sengoku-save--format-error
             "Invalid JSON: %s" (error-message-string error-data))))))
    (sengoku-save-decode-object object)))

(defun sengoku-save-decode-file (file)
  "Read and decode Vim v5 save FILE without creating any directories."
  (unless (and (stringp file) (not (string-empty-p file)))
    (signal 'sengoku-save-io-error
            (list (format "invalid save file name: %S" file))))
  (let ((path (expand-file-name file))
        contents)
    (condition-case error-data
        (with-temp-buffer
          (let ((coding-system-for-read 'utf-8-unix))
            (insert-file-contents path))
          (setq contents (buffer-string)))
      (file-error
       (signal 'sengoku-save-io-error
               (list (format "%s: %s" path
                             (error-message-string error-data))))))
    (condition-case error-data
        (sengoku-save-decode-string contents)
      (sengoku-save-error
       (signal (car error-data)
               (list (format "%s: %s" path
                             (error-message-string error-data))))))))

(defun sengoku-save--encode-array (value context)
  "Return sequence VALUE as a fresh JSON vector for CONTEXT."
  (vconcat (sengoku-save--array-elements value context)))

(defun sengoku-save--encode-province (province province-index)
  "Return exact Vim v5 JSON object for PROVINCE at PROVINCE-INDEX."
  (unless (sengoku-province-p province)
    (sengoku-save--format-error
     "Game province[%d] is not a sengoku-province: %S"
     province-index province))
  `((name . ,(sengoku-province-name province))
    (owner . ,(sengoku-province-owner province))
    (gr . ,(sengoku-province-grid-row province))
    (gc . ,(sengoku-province-grid-column province))
    (adjn . ,(sengoku-save--encode-array
              (sengoku-province-adjacency-names province)
              (format "game province[%d].adjn" province-index)))
    (adj . ,(sengoku-save--encode-array
             (sengoku-province-adjacency province)
             (format "game province[%d].adj" province-index)))
    (koku . ,(sengoku-province-koku province))
    (comm . ,(sengoku-province-commerce province))
    (sol . ,(sengoku-province-soldiers province))
    (loyal . ,(sengoku-province-loyalty province))
    (train . ,(sengoku-province-training province))
    (guns . ,(sengoku-province-guns province))
    (cannon . ,(sengoku-province-cannon province))
    (port . ,(sengoku-province-port province))
    (gold . ,(sengoku-province-gold province))
    (rice . ,(sengoku-province-rice province))
    (auto . ,(sengoku-province-auto province))))

(defun sengoku-save--encode-clan-state (state clan-index)
  "Return exact Vim v5 JSON object for clan STATE at CLAN-INDEX."
  (unless (sengoku-clan-state-p state)
    (sengoku-save--format-error
     "Game cstate[%d] is not a sengoku-clan-state: %S" clan-index state))
  `((cul . ,(sengoku-clan-state-culture state))
    (items . ,(sengoku-save--encode-array
               (sengoku-clan-state-items state)
               (format "game cstate[%d].items" clan-index)))
    (rank . ,(sengoku-clan-state-rank state))))

(defun sengoku-save--encode-alliances (alliances)
  "Return a detached exact JSON object copied from ALLIANCES."
  (unless (and (hash-table-p alliances)
               (eq (hash-table-test alliances) 'equal))
    (sengoku-save--format-error
     "Game alliances must be an equal hash table, found %S" alliances))
  (let ((object (make-hash-table :test #'equal)))
    (maphash
     (lambda (key value)
       (unless (stringp key)
         (sengoku-save--format-error
          "Game alliance key must be a canonical string, found %S" key))
       (puthash key value object))
     alliances)
    object))

(defun sengoku-save--last-log-vector (log)
  "Return the last 30 entries of LOG as a fresh JSON vector."
  (let* ((entries (sengoku-save--array-elements log "game log"))
         (drop-count (max 0 (- (length entries) 30))))
    (vconcat (nthcdr drop-count entries))))

(defun sengoku-save-encode-object (game)
  "Return GAME as a validated exact Vim v5 JSON Lisp object.

Fixed objects are represented as symbol-key alists in schema order.  JSON
arrays are vectors, including empty arrays, and `ally' is a string-key equal
hash table.  Only the last 30 log entries are included."
  (unless (sengoku-game-p game)
    (sengoku-save--format-error
     "Expected a sengoku-game, found %S" game))
  (condition-case error-data
      (let* ((provinces (sengoku-game-provinces game))
             (states (sengoku-game-clan-states game))
             (province-objects (make-vector (length provinces) nil))
             (state-objects (make-vector (length states) nil)))
        (dotimes (province-index (length provinces))
          (aset province-objects province-index
                (sengoku-save--encode-province
                 (aref provinces province-index) province-index)))
        (dotimes (clan-index (length states))
          (aset state-objects clan-index
                (sengoku-save--encode-clan-state
                 (aref states clan-index) clan-index)))
        (let ((object
               `((ver . ,sengoku-save-version)
                 (year . ,(sengoku-game-year game))
                 (month . ,(sengoku-game-month game))
                 (player . ,(sengoku-game-player game))
                 (prov . ,province-objects)
                 (log . ,(sengoku-save--last-log-vector
                           (sengoku-game-log game)))
                 (cstate . ,state-objects)
                 (ally . ,(sengoku-save--encode-alliances
                            (sengoku-game-alliances game))))))
          ;; The decoder is the single strict schema validator.  It examines a
          ;; detached object and a fresh game, so this cannot mutate GAME.
          (sengoku-save-decode-object object)
          object))
    (sengoku-save-error
     (sengoku-save--resignal error-data))
    (error
     (sengoku-save--format-error
      "Could not encode game: %s" (error-message-string error-data)))))

(defun sengoku-save-encode-string (game)
  "Return GAME encoded as one Vim-compatible v5 JSON string."
  (let ((object (sengoku-save-encode-object game)))
    (condition-case error-data
        (let ((encoded (json-encode object)))
          (unless (and (stringp encoded) (not (string-empty-p encoded)))
            (sengoku-save--format-error
             "JSON serializer returned no data"))
          encoded)
      (sengoku-save-error
       (sengoku-save--resignal error-data))
      (error
       (sengoku-save--format-error
        "Could not serialize game as JSON: %s"
        (error-message-string error-data))))))

(defun sengoku-save-game (game &optional file)
  "Atomically save GAME to FILE and return its absolute path.
FILE defaults to `sengoku-save-file'.  Validation and JSON encoding happen
before the parent directory is created.  The write uses a UTF-8 temporary
sibling followed by an overwrite rename, with cleanup on every error path."
  (let ((requested-file (or file sengoku-save-file)))
    (unless (and (stringp requested-file)
                 (not (string-empty-p requested-file)))
      (signal 'sengoku-save-io-error
              (list (format "invalid save file name: %S" requested-file))))
    (let* ((json-string (sengoku-save-encode-string game))
           (target (expand-file-name requested-file))
           (directory (file-name-directory target))
           temp-file)
      (condition-case error-data
          (progn
            (make-directory directory t)
            (unwind-protect
                (progn
                  (setq temp-file
                        (make-temp-file
                         (expand-file-name
                          (concat "." (file-name-nondirectory target) ".tmp-")
                          directory)))
                  (let ((coding-system-for-write 'utf-8-unix)
                        (inhibit-message t))
                    (write-region (concat json-string "\n")
                                  nil temp-file nil nil))
                  (rename-file temp-file target t)
                  (setq temp-file nil)
                  target)
              (when (and temp-file (file-exists-p temp-file))
                (ignore-errors (delete-file temp-file)))))
        (file-error
         (signal 'sengoku-save-io-error
                 (list (format "%s: %s" target
                               (error-message-string error-data)))))
        (error
         (signal 'sengoku-save-io-error
                 (list (format "%s: %s" target
                               (error-message-string error-data)))))))))

(defun sengoku-save--loadable-file-p (file)
  "Return non-nil when FILE is a readable regular file."
  (and (file-regular-p file) (file-readable-p file)))

(defun sengoku-save-choose-load-path (&optional configured-file legacy-file)
  "Return the preferred readable save path, or nil.
CONFIGURED-FILE defaults to `sengoku-save-file' and is always preferred.
LEGACY-FILE defaults to `sengoku-save-legacy-file'.  This function never
creates files or directories."
  (let ((configured (expand-file-name (or configured-file sengoku-save-file)))
        (legacy (expand-file-name (or legacy-file sengoku-save-legacy-file))))
    (cond
     ((sengoku-save--loadable-file-p configured) configured)
     ((sengoku-save--loadable-file-p legacy) legacy)
     (t nil))))

(defun sengoku-load-game (&optional configured-file legacy-file)
  "Load the preferred save and return a result plist, or nil if none exists.

The plist contains `:game', absolute `:path', `:source' (`configured' or
`legacy'), and `:legacy-p'.  CONFIGURED-FILE and LEGACY-FILE have the same
meaning as in `sengoku-save-choose-load-path'.  A present but invalid preferred
file signals a `sengoku-save-error' rather than silently falling back."
  (let* ((configured (expand-file-name (or configured-file sengoku-save-file)))
         (legacy (expand-file-name (or legacy-file sengoku-save-legacy-file)))
         (path (sengoku-save-choose-load-path configured legacy)))
    (when path
      (let* ((legacy-p (not (string= path configured)))
             (game (sengoku-save-decode-file path)))
        (list :game game
              :path path
              :source (if legacy-p 'legacy 'configured)
              :legacy-p legacy-p)))))

(defalias 'sengoku-save-load-game #'sengoku-load-game)

(defun sengoku-save-migrate-legacy
    (&optional delete-legacy configured-file legacy-file)
  "Migrate a valid legacy save to the configured path.

When the configured file is absent and the legacy file is selected, load and
validate it, atomically save it to CONFIGURED-FILE, and return a load-result
plist containing `:migrated-from'.  If DELETE-LEGACY is non-nil, delete the
legacy source only after the new save succeeds.  Return nil when no migration
is needed.  Path arguments default as in `sengoku-load-game'."
  (let* ((configured (expand-file-name (or configured-file sengoku-save-file)))
         (legacy (expand-file-name (or legacy-file sengoku-save-legacy-file)))
         (result (sengoku-load-game configured legacy)))
    (when (and result (plist-get result :legacy-p))
      (let* ((game (plist-get result :game))
             (source (plist-get result :path))
             (target (sengoku-save-game game configured)))
        (when delete-legacy
          (condition-case error-data
              (delete-file source)
            (file-error
             (signal 'sengoku-save-io-error
                     (list (format "%s: migrated to %s but could not delete: %s"
                                   source target
                                   (error-message-string error-data)))))))
        (list :game game
              :path target
              :source 'configured
              :legacy-p nil
              :migrated-from source)))))

(provide 'sengoku-save)

;;; sengoku-save.el ends here
