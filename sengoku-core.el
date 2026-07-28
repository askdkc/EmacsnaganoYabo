;;; sengoku-core.el --- Core state for Sengoku -*- lexical-binding: t; -*-

;; Copyright (C) 2026 dkc

;; Author: dkc
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: games

;;; Commentary:

;; Mutable game state and non-UI helpers translated from plugin/sengoku.vim.
;; Static definitions live in `sengoku-data'.  Integer indices and integer
;; flags are retained where they make future Vim save compatibility simpler.

;;; Code:

(require 'cl-lib)
(require 'sengoku-data)

(cl-defstruct sengoku-clan
  "A daimyo clan.
WAR, POLITICS, and CHARISMA are the Vim `war', `pol', and `chr' values."
  name abbreviation daimyo war politics charisma)

(cl-defstruct sengoku-general
  "A daimyo or retainer available to perform clan actions."
  name war politics)

(cl-defstruct sengoku-province
  "Mutable state for one province.
OWNER is a clan index or -1.  ADJACENCY-NAMES preserves source names while
ADJACENCY stores resolved province indices.  PORT and AUTO are 0/1 integers
so serialized state can retain the Vim representation."
  name owner grid-row grid-column adjacency-names adjacency
  koku commerce soldiers loyalty training guns cannon port gold rice auto)

(cl-defstruct sengoku-clan-state
  "Mutable culture, item ownership, and court rank for one clan.
ITEMS is a chronological list of stable item indices."
  culture items rank)

(cl-defstruct sengoku-game
  "Complete non-UI state of a campaign.
ALLIANCES is an equal hash table whose canonical string keys are `A-B' and
whose values are integer 1.  GENERALS and GENERAL-USED are parallel vectors;
the latter contains 0/1 integer flags.  TURN-QUEUE and PENDING-BATTLE are
reserved for turn controllers and asynchronous battle UI modules."
  clans provinces items clan-states alliances generals general-used
  year month player log turn-queue pending-battle)

(cl-defstruct sengoku-session
  "Runtime controller state surrounding a `sengoku-game'.
PHASE is a controller-defined symbol.  TURN-QUEUE is session-local work still
to be presented.  PENDING-BATTLE allows a UI to suspend normal command flow.
UI-STATE is opaque to core and must not contain save-format state."
  game phase current-clan active-province selected-general
  turn-queue pending-battle ui-state quit-reason)

(cl-defstruct sengoku-battle-context
  "Strategic battle request and result shared by automatic and siege combat.
SOLDIERS, GUNS, TRAINING, and CANNONS describe the committed attacking force.
LINES is the chronological battle report.  SHOWN records whether a battle UI
already displayed the report; SIEGE may hold a `sengoku-siege'."
  attacker-clan defender-clan from to soldiers guns training cannons general
  lines result shown siege)

(cl-defstruct sengoku-siege
  "Mutable siege state corresponding to Vim's script-local `s:bt' dictionary.
GRID is a vector of row vectors.  STRUCTURE-HP maps `ROW,COLUMN' strings to
hit points.  PLAYER-SIDE is -2 for no player, 0 for attacker, or 1 for
defender."
  attacker-clan defender-clan from to initial-soldiers day grid structure-hp
  units messages result attacker-general-name defender-general-name
  player-side)

(cl-defstruct sengoku-siege-unit
  "One mutable unit on a siege map.
SIDE is 0 for attackers and 1 for defenders.  ROW and COLUMN are grid
coordinates; CANNON is the number of large guns assigned to the unit."
  side label soldiers guns training war cannon row column)

(defvar sengoku-random-function #'random
  "Function used by `sengoku-random'.
The function receives a positive upper bound and should return a number.
Tests may dynamically bind this variable to provide a deterministic stream.")

(defvar sengoku-debug-gold 0
  "Extra gold added to every province by `sengoku-new-game'.
This corresponds to Vim's `g:sengoku_debug_gold' and defaults to zero.")

(defun sengoku-random (upper-bound)
  "Return a random integer in [0, UPPER-BOUND), or zero for nonpositive bounds.
The modulo preserves the contract even for a simple test double that returns
an unrestricted integer."
  (if (<= upper-bound 0)
      0
    (mod (truncate (funcall sengoku-random-function upper-bound))
         upper-bound)))

(defun sengoku-vim-divide (numerator denominator)
  "Divide NUMERATOR by DENOMINATOR with Vim-compatible truncation toward zero."
  (truncate (/ numerator denominator)))

(defun sengoku--vector-map (function vector)
  "Return a vector made by applying FUNCTION to each element of VECTOR."
  (let ((result (make-vector (length vector) nil)))
    (dotimes (index (length vector))
      (aset result index (funcall function (aref vector index))))
    result))

(defun sengoku--index-by-name (vector name accessor)
  "Find NAME in VECTOR using ACCESSOR, returning its index or -1."
  (if (not (stringp name))
      -1
    (or (cl-position name vector :test #'string= :key accessor) -1)))

(defun sengoku-clan-index (game name)
  "Return the stable clan index named NAME in GAME, or -1."
  (sengoku--index-by-name (sengoku-game-clans game)
                          name #'sengoku-clan-name))

(defun sengoku-province-index (game name)
  "Return the stable province index named NAME in GAME, or -1."
  (sengoku--index-by-name (sengoku-game-provinces game)
                          name #'sengoku-province-name))

(defun sengoku-clan-provinces (game clan-index)
  "Return province indices owned by CLAN-INDEX in GAME source order."
  (let ((provinces (sengoku-game-provinces game))
        result)
    (dotimes (province-index (length provinces))
      (when (= (sengoku-province-owner (aref provinces province-index))
               clan-index)
        (push province-index result)))
    (nreverse result)))

(defun sengoku-clan-alive-p (game clan-index)
  "Return non-nil when CLAN-INDEX owns at least one province in GAME."
  (and (>= clan-index 0)
       (cl-some (lambda (province)
                  (= (sengoku-province-owner province) clan-index))
                (append (sengoku-game-provinces game) nil))))

(defun sengoku-reset-generals (game)
  "Mark every general in GAME unused and return the new flag vector."
  (let* ((generals (sengoku-game-generals game))
         (used (make-vector (length generals) nil)))
    (dotimes (clan-index (length generals))
      (aset used clan-index
            (make-vector (length (aref generals clan-index)) 0)))
    (setf (sengoku-game-general-used game) used)
    used))

(defun sengoku-general-used-p (game clan-index general-index)
  "Return non-nil when GENERAL-INDEX of CLAN-INDEX has acted in GAME this month."
  (not (zerop (aref (aref (sengoku-game-general-used game) clan-index)
                    general-index))))

(defun sengoku-unused-generals (game clan-index)
  "Return unused general indices for CLAN-INDEX in GAME roster order."
  (let ((flags (aref (sengoku-game-general-used game) clan-index))
        result)
    (dotimes (general-index (length flags))
      (when (zerop (aref flags general-index))
        (push general-index result)))
    (nreverse result)))

(defun sengoku-use-general (game clan-index general-index)
  "Mark GENERAL-INDEX of CLAN-INDEX used in GAME and return that general."
  (let ((flags (aref (sengoku-game-general-used game) clan-index))
        (roster (aref (sengoku-game-generals game) clan-index)))
    (aset flags general-index 1)
    (aref roster general-index)))

(defun sengoku--general-ability (general ability)
  "Read ABILITY from GENERAL, accepting Vim and descriptive names."
  (pcase ability
    ((or 'war :war) (sengoku-general-war general))
    ((or 'pol :pol 'politics :politics)
     (sengoku-general-politics general))
    (_ (error "Unknown general ability: %S" ability))))

(defun sengoku-best-general (game clan-index ability)
  "Return the best unused general index in GAME for CLAN-INDEX by ABILITY, or -1.
ABILITY may be `war', `pol', or `politics' (and keyword equivalents).  Ties
retain roster order, matching the Vim implementation."
  (let ((roster (aref (sengoku-game-generals game) clan-index))
        (best-index -1)
        (best-value -1))
    (dolist (general-index (sengoku-unused-generals game clan-index)
                           best-index)
      (let ((value (sengoku--general-ability
                    (aref roster general-index) ability)))
        (when (> value best-value)
          (setq best-value value
                best-index general-index))))))

(defun sengoku-best-war-general (game clan-index)
  "Return CLAN-INDEX's strongest defender in GAME, including used generals.
For a negative clan index, return the neutral 足軽大将 used by Vim."
  (if (< clan-index 0)
      (make-sengoku-general :name "足軽大将" :war 50 :politics 30)
    (let* ((roster (aref (sengoku-game-generals game) clan-index))
           (best (aref roster 0)))
      (dotimes (general-index (length roster))
        (let ((general (aref roster general-index)))
          (when (> (sengoku-general-war general)
                   (sengoku-general-war best))
            (setq best general))))
      best)))

(defun sengoku-alliance-key (clan-a clan-b)
  "Return the canonical Vim-compatible alliance key for CLAN-A and CLAN-B."
  (format "%d-%d" (min clan-a clan-b) (max clan-a clan-b)))

(defun sengoku-allied-p (game clan-a clan-b)
  "Return non-nil when distinct, nonnegative CLAN-A and CLAN-B are allied in GAME."
  (and (>= clan-a 0)
       (>= clan-b 0)
       (/= clan-a clan-b)
       (gethash (sengoku-alliance-key clan-a clan-b)
                (sengoku-game-alliances game))))

(defalias 'sengoku-alliance-p #'sengoku-allied-p)

(defun sengoku-set-alliance (game clan-a clan-b on)
  "Set the GAME alliance between CLAN-A and CLAN-B according to ON.
Invalid self-alliances and negative clan indices are ignored.  Return ON when
an alliance was set, nil when it was removed or ignored."
  (when (and (>= clan-a 0) (>= clan-b 0) (/= clan-a clan-b))
    (let ((key (sengoku-alliance-key clan-a clan-b))
          (alliances (sengoku-game-alliances game)))
      (if on
          (progn
            (puthash key 1 alliances)
            on)
        (remhash key alliances)
        nil))))

(defun sengoku-drop-alliance (game clan-a clan-b)
  "Remove the alliance between CLAN-A and CLAN-B from GAME."
  (sengoku-set-alliance game clan-a clan-b nil))

(defun sengoku-alliance-list (game clan-index)
  "Return living allies of CLAN-INDEX in GAME stable clan order."
  (let ((clans (sengoku-game-clans game))
        result)
    (dotimes (other (length clans))
      (when (and (/= other clan-index)
                 (sengoku-allied-p game clan-index other)
                 (sengoku-clan-alive-p game other))
        (push other result)))
    (nreverse result)))

(defun sengoku-drop-alliances (game clan-index)
  "Remove every GAME alliance involving CLAN-INDEX."
  (dotimes (other (length (sengoku-game-clans game)))
    (sengoku-set-alliance game clan-index other nil))
  nil)

(defun sengoku-culture (game clan-index)
  "Return CLAN-INDEX's culture in GAME, or zero for a negative index."
  (if (>= clan-index 0)
      (sengoku-clan-state-culture
       (aref (sengoku-game-clan-states game) clan-index))
    0))

(defalias 'sengoku-clan-culture #'sengoku-culture)

(defun sengoku-item-owner (game item-index)
  "Return the first clan owning ITEM-INDEX in GAME, or -1."
  (let ((states (sengoku-game-clan-states game))
        (owner -1)
        (clan-index 0))
    (while (and (< clan-index (length states)) (< owner 0))
      (when (memq item-index
                  (sengoku-clan-state-items (aref states clan-index)))
        (setq owner clan-index))
      (setq clan-index (1+ clan-index)))
    owner))

(defun sengoku-gain-item (game clan-index item-index)
  "Give ITEM-INDEX to CLAN-INDEX in GAME and apply its culture bonus.
Items are appended to preserve acquisition order.  This function mirrors Vim
and therefore does not silently remove the item from another clan; callers
should transfer ownership explicitly and use validation to catch duplicates."
  (let* ((state (aref (sengoku-game-clan-states game) clan-index))
         (item (aref (sengoku-game-items game) item-index))
         (bonus (plist-get item :culture)))
    (setf (sengoku-clan-state-items state)
          (append (sengoku-clan-state-items state) (list item-index))
          (sengoku-clan-state-culture state)
          (min 100 (+ (sengoku-clan-state-culture state) bonus)))
    item-index))

(defun sengoku-unowned-items (game)
  "Return unowned item indices in GAME stable item order."
  (let ((items (sengoku-game-items game))
        result)
    (dotimes (item-index (length items))
      (when (< (sengoku-item-owner game item-index) 0)
        (push item-index result)))
    (nreverse result)))

(defun sengoku-neighbor-clans (game clan-index)
  "Return GAME clans bordering CLAN-INDEX, in stable clan order."
  (let* ((clan-count (length (sengoku-game-clans game)))
         (seen (make-vector clan-count nil))
         (provinces (sengoku-game-provinces game)))
    (dolist (province-index (sengoku-clan-provinces game clan-index))
      (let ((province (aref provinces province-index)))
        (dotimes (adjacent-index (length (sengoku-province-adjacency province)))
          (let* ((neighbor-index
                  (aref (sengoku-province-adjacency province) adjacent-index))
                 (owner (and (>= neighbor-index 0)
                             (< neighbor-index (length provinces))
                             (sengoku-province-owner
                              (aref provinces neighbor-index)))))
            (when (and owner (>= owner 0) (/= owner clan-index))
              (aset seen owner t))))))
    (let (result)
      (dotimes (other clan-count)
        (when (aref seen other)
          (push other result)))
      (nreverse result))))

(defun sengoku-richest-province (game clan-index)
  "Return CLAN-INDEX's richest province index in GAME, or -1.
The first province wins ties, matching Vim's strict greater-than comparison."
  (let ((provinces (sengoku-game-provinces game))
        (best-index -1)
        (best-gold -1))
    (dolist (province-index (sengoku-clan-provinces game clan-index)
                            best-index)
      (let ((gold (sengoku-province-gold
                   (aref provinces province-index))))
        (when (> gold best-gold)
          (setq best-gold gold
                best-index province-index))))))

(defun sengoku-log (game message)
  "Append dated MESSAGE to GAME's log, cap it at 300, and return the entry."
  (let* ((entry (format "%d年%2d月 %s"
                        (sengoku-game-year game)
                        (sengoku-game-month game)
                        message))
         (log (append (sengoku-game-log game) (list entry)))
         (excess (- (length log) 300)))
    (when (> excess 0)
      (setq log (nthcdr excess log)))
    (setf (sengoku-game-log game) log)
    entry))

(defun sengoku--make-runtime-clans ()
  "Construct mutable clan records from static definitions."
  (sengoku--vector-map
   (lambda (definition)
     (make-sengoku-clan
      :name (plist-get definition :name)
      :abbreviation (plist-get definition :abbreviation)
      :daimyo (plist-get definition :daimyo)
      :war (plist-get definition :war)
      :politics (plist-get definition :politics)
      :charisma (plist-get definition :charisma)))
   sengoku-data-clans))

(defun sengoku--make-runtime-provinces (clans)
  "Construct randomized province records using CLANS for owner resolution."
  (let ((result (make-vector (length sengoku-data-provinces) nil))
        (debug-gold (if (numberp sengoku-debug-gold)
                        (truncate sengoku-debug-gold)
                      0)))
    (dotimes (province-index (length sengoku-data-provinces))
      (let* ((definition (aref sengoku-data-provinces province-index))
             (special (plist-get definition :special))
             ;; Vim evaluates `get' default arguments even when overridden;
             ;; keep all five random draws in exactly the same order.
             (random-loyalty (+ 55 (sengoku-random 11)))
             (training (+ 40 (sengoku-random 21)))
             (random-guns (* (sengoku-random 3) 10))
             (gold (+ 350 (sengoku-random 101) debug-gold))
             (rice (+ 900 (sengoku-random 201)))
             (owner (sengoku--index-by-name
                     clans (plist-get definition :owner)
                     #'sengoku-clan-name)))
        (aset result province-index
              (make-sengoku-province
               :name (plist-get definition :name)
               :owner owner
               :grid-row (plist-get definition :grid-row)
               :grid-column (plist-get definition :grid-column)
               :adjacency-names
               (copy-sequence (plist-get definition :adjacent))
               :adjacency nil
               :koku (plist-get definition :koku)
               :commerce (plist-get definition :commerce)
               :soldiers (plist-get definition :soldiers)
               :loyalty (if (plist-member special :loyalty)
                            (plist-get special :loyalty)
                          random-loyalty)
               :training training
               :guns (if (plist-member special :guns)
                         (plist-get special :guns)
                       random-guns)
               :cannon 0
               :port (if (plist-member special :port)
                         (plist-get special :port)
                       0)
               :gold gold
               :rice rice
               :auto 0))))
    result))

(defun sengoku--make-runtime-generals (clans)
  "Construct daimyo-first general rosters aligned with CLANS."
  (let ((result (make-vector (length clans) nil)))
    (dotimes (clan-index (length clans))
      (let* ((clan (aref clans clan-index))
             (retainers (aref sengoku-data-retainers clan-index))
             (roster (make-vector (1+ (length retainers)) nil)))
        (aset roster 0
              (make-sengoku-general
               :name (sengoku-clan-daimyo clan)
               :war (sengoku-clan-war clan)
               :politics (sengoku-clan-politics clan)))
        (dotimes (retainer-index (length retainers))
          (let ((definition (aref retainers retainer-index)))
            (aset roster (1+ retainer-index)
                  (make-sengoku-general
                   :name (plist-get definition :name)
                   :war (plist-get definition :war)
                   :politics (plist-get definition :politics)))))
        (aset result clan-index roster)))
    result))

(defun sengoku--make-runtime-clan-states ()
  "Construct initial clan states before tea utensil bonuses."
  (let ((states (make-vector (length sengoku-data-clans) nil)))
    (dotimes (clan-index (length states))
      (aset states clan-index
            (make-sengoku-clan-state
             :culture (aref sengoku-data-culture-base clan-index)
             :items nil
             :rank 0)))
    states))

(defun sengoku--resolve-adjacency (game)
  "Resolve all province adjacency names in GAME to stable integer indices."
  (let ((provinces (sengoku-game-provinces game)))
    (dotimes (province-index (length provinces))
      (let* ((province (aref provinces province-index))
             (names (sengoku-province-adjacency-names province))
             (resolved (make-vector (length names) -1)))
        (dotimes (adjacent-index (length names))
          (aset resolved adjacent-index
                (sengoku-province-index game
                                        (aref names adjacent-index))))
        (setf (sengoku-province-adjacency province) resolved)))
    game))

(defun sengoku-new-game ()
  "Create and return a new campaign equivalent to Vim's `s:InitState'.
The date is March 1560, PLAYER is -1, alliances and log are empty, every
province receives its five ordered random draws, and generals start unused."
  (let* ((clans (sengoku--make-runtime-clans))
         (provinces (sengoku--make-runtime-provinces clans))
         (states (sengoku--make-runtime-clan-states))
         (items (sengoku--vector-map #'copy-tree sengoku-data-items))
         (generals (sengoku--make-runtime-generals clans))
         (game (make-sengoku-game
                :clans clans
                :provinces provinces
                :items items
                :clan-states states
                :alliances (make-hash-table :test #'equal)
                :generals generals
                :general-used nil
                :year 1560
                :month 3
                :player -1
                :log nil
                :turn-queue nil
                :pending-battle nil)))
    (sengoku--resolve-adjacency game)
    (dotimes (item-index (length sengoku-data-initial-item-owners))
      (let ((owner (aref sengoku-data-initial-item-owners item-index)))
        (when (>= owner 0)
          (sengoku-gain-item game owner item-index))))
    (sengoku-reset-generals game)
    game))

(defalias 'sengoku-init-state #'sengoku-new-game)

(defun sengoku-new-session (&optional game)
  "Create a controller session around GAME or a fresh campaign."
  (let ((campaign (or game (sengoku-new-game))))
    (make-sengoku-session
     :game campaign
     :phase 'setup
     :current-clan -1
     :active-province -1
     :selected-general -1
     :turn-queue nil
     :pending-battle nil
     :ui-state nil
     :quit-reason nil)))

(defun sengoku-conquer-apply
    (game attacker-clan target-index soldiers guns training &optional lines)
  "Apply conquest in GAME of TARGET-INDEX by ATTACKER-CLAN and return LINES.
SOLDIERS, GUNS, and TRAINING become the new garrison values.  Elimination
captures tea utensils in ownership order, applies their culture bonuses, and
drops every alliance of the defeated clan.  A non-nil LINES list is extended
destructively, like Vim's mutable list; callers must still use the return
value because nil cannot be modified in place."
  (let* ((provinces (sengoku-game-provinces game))
         (target (aref provinces target-index))
         (defender-clan (sengoku-province-owner target)))
    (setf (sengoku-province-owner target) attacker-clan
          (sengoku-province-soldiers target) soldiers
          (sengoku-province-guns target) guns
          (sengoku-province-loyalty target)
          (max 10 (- (sengoku-province-loyalty target) 20))
          (sengoku-province-training target) training
          (sengoku-province-auto target) 0)
    (when (and (>= defender-clan 0)
               (not (sengoku-clan-alive-p game defender-clan)))
      (let* ((clans (sengoku-game-clans game))
             (states (sengoku-game-clan-states game))
             (defender-state (aref states defender-clan))
             (captured-items (copy-sequence
                              (sengoku-clan-state-items defender-state))))
        (setq lines
              (nconc lines
                     (list
                      (format "◆◆ %s家は滅亡した…… ◆◆"
                              (sengoku-clan-name
                               (aref clans defender-clan))))))
        (when captured-items
          (let ((item-names
                 (mapcar
                  (lambda (item-index)
                    (plist-get (aref (sengoku-game-items game) item-index)
                               :name))
                  captured-items)))
            (dolist (item-index captured-items)
              (sengoku-gain-item game attacker-clan item-index))
            (setf (sengoku-clan-state-items defender-state) nil)
            (setq lines
                  (nconc
                   lines
                   (list
                    (format "%s家は名物「%s」を接収した!"
                            (sengoku-clan-name
                             (aref clans attacker-clan))
                            (mapconcat #'identity item-names "」「")))))))
        (sengoku-drop-alliances game defender-clan)))
    lines))

(defun sengoku--data-clan-index (name)
  "Return NAME's static clan index, or -1."
  (if (not (stringp name))
      -1
    (or (cl-position name sengoku-data-clans
                     :test #'string=
                     :key (lambda (definition)
                            (plist-get definition :name)))
        -1)))

(defun sengoku--data-province-index (name)
  "Return NAME's static province index, or -1."
  (if (not (stringp name))
      -1
    (or (cl-position name sengoku-data-provinces
                     :test #'string=
                     :key (lambda (definition)
                            (plist-get definition :name)))
        -1)))

(defun sengoku-validate-data ()
  "Return errors found in the static tables, or nil when they are valid."
  (let (errors
        (clan-names (make-hash-table :test #'equal))
        (province-names (make-hash-table :test #'equal))
        (item-names (make-hash-table :test #'equal)))
    (cl-labels ((record (format-string &rest arguments)
                  (push (apply #'format format-string arguments) errors)))
      (unless (= (length sengoku-data-clans) sengoku-data-clan-count)
        (record "Expected %d clans, found %d"
                sengoku-data-clan-count (length sengoku-data-clans)))
      (unless (= (length sengoku-data-provinces) sengoku-data-province-count)
        (record "Expected %d provinces, found %d"
                sengoku-data-province-count (length sengoku-data-provinces)))
      (unless (= (length sengoku-data-retainers)
                 sengoku-data-clan-count)
        (record "Retainer table has %d clan rows"
                (length sengoku-data-retainers)))
      (unless (= (length sengoku-data-items) sengoku-data-item-count)
        (record "Expected %d items, found %d"
                sengoku-data-item-count (length sengoku-data-items)))
      (unless (= (length sengoku-data-ranks) 8)
        (record "Expected 8 court ranks, found %d"
                (length sengoku-data-ranks)))
      (unless (= (length sengoku-data-culture-base)
                 sengoku-data-clan-count)
        (record "Culture table has %d entries"
                (length sengoku-data-culture-base)))
      (dotimes (clan-index (length sengoku-data-culture-base))
        (let ((value (aref sengoku-data-culture-base clan-index)))
          (unless (and (integerp value) (<= 0 value 100))
            (record "Clan %d has invalid base culture: %S"
                    clan-index value))))
      (unless (= (length sengoku-data-initial-item-owners)
                 sengoku-data-item-count)
        (record "Initial item owner table has %d entries"
                (length sengoku-data-initial-item-owners)))
      (dotimes (clan-index (length sengoku-data-clans))
        (let* ((definition (aref sengoku-data-clans clan-index))
               (name (plist-get definition :name)))
          (if (gethash name clan-names)
              (record "Duplicate clan name: %s" name)
            (puthash name t clan-names))
          (dolist (ability '(:war :politics :charisma))
            (let ((value (plist-get definition ability)))
              (unless (and (integerp value) (> value 0) (<= value 100))
                (record "Clan %s has invalid %s: %S"
                        name ability value))))
          (when (< clan-index (length sengoku-data-retainers))
            (let ((retainers (aref sengoku-data-retainers clan-index)))
              (unless (= (length retainers)
                         sengoku-data-retainers-per-clan)
                (record "Clan %s has %d retainers"
                        name (length retainers)))
              (dotimes (retainer-index (length retainers))
                (let ((retainer (aref retainers retainer-index)))
                  (dolist (ability '(:war :politics))
                    (let ((value (plist-get retainer ability)))
                      (unless (and (integerp value)
                                   (> value 0) (<= value 100))
                        (record "Retainer %s has invalid %s: %S"
                                (plist-get retainer :name)
                                ability value))))))))))
      (dotimes (province-index (length sengoku-data-provinces))
        (let* ((definition (aref sengoku-data-provinces province-index))
               (name (plist-get definition :name))
               (row (plist-get definition :grid-row))
               (column (plist-get definition :grid-column)))
          (if (gethash name province-names)
              (record "Duplicate province name: %s" name)
            (puthash name t province-names))
          (when (< (sengoku--data-clan-index
                    (plist-get definition :owner)) 0)
            (record "%s has unknown initial owner %S"
                    name (plist-get definition :owner)))
          (unless (and (integerp row) (>= row 0)
                       (< row sengoku-data-map-rows)
                       (integerp column) (>= column 0)
                       (< column sengoku-data-map-columns))
            (record "%s has out-of-bounds map cell %S,%S"
                    name row column))))
      (dotimes (province-index (length sengoku-data-provinces))
        (let* ((definition (aref sengoku-data-provinces province-index))
               (name (plist-get definition :name))
               (adjacent (plist-get definition :adjacent)))
          (dotimes (adjacent-index (length adjacent))
            (when (< (sengoku--data-province-index
                      (aref adjacent adjacent-index)) 0)
              (record "%s has unknown neighbor %s"
                      name (aref adjacent adjacent-index))))))
      (dotimes (item-index (length sengoku-data-items))
        (let* ((item (aref sengoku-data-items item-index))
               (name (plist-get item :name))
               (owner (and (< item-index
                              (length sengoku-data-initial-item-owners))
                           (aref sengoku-data-initial-item-owners
                                 item-index))))
          (if (gethash name item-names)
              (record "Duplicate item name: %s" name)
            (puthash name t item-names))
          (unless (and (integerp (plist-get item :price))
                       (> (plist-get item :price) 0)
                       (integerp (plist-get item :culture))
                       (> (plist-get item :culture) 0))
            (record "Item %s has invalid price or culture" name))
          (unless (and (integerp owner)
                       (or (= owner -1)
                           (and (>= owner 0)
                                (< owner sengoku-data-clan-count))))
            (record "Item %s has invalid initial owner %S" name owner))))
      (let ((keep-row (aref sengoku-data-siege-keep 0))
            (keep-column (aref sengoku-data-siege-keep 1))
            (gate-row (aref sengoku-data-siege-gate 0))
            (gate-column (aref sengoku-data-siege-gate 1)))
        (unless (and (<= 0 keep-row) (< keep-row sengoku-data-siege-rows)
                     (<= 0 keep-column)
                     (< keep-column sengoku-data-siege-columns)
                     (<= 0 gate-row) (< gate-row sengoku-data-siege-rows)
                     (<= 0 gate-column)
                     (< gate-column sengoku-data-siege-columns))
          (record "Siege keep or gate lies outside the map"))))
    (nreverse errors)))

(defun sengoku-validate-map (game)
  "Return adjacency, name-resolution, bounds, and grid errors in GAME."
  (let ((provinces (sengoku-game-provinces game))
        (cells (make-hash-table :test #'equal))
        errors)
    (cl-labels ((record (format-string &rest arguments)
                  (push (apply #'format format-string arguments) errors)))
      (dotimes (province-index (length provinces))
        (let* ((province (aref provinces province-index))
               (row (sengoku-province-grid-row province))
               (column (sengoku-province-grid-column province))
               (cell (format "%S,%S" row column))
               (previous (gethash cell cells :missing)))
          (unless (and (integerp row)
                       (>= row 0) (< row sengoku-data-map-rows)
                       (integerp column)
                       (>= column 0) (< column sengoku-data-map-columns))
            (record "%s has out-of-bounds grid cell %s"
                    (sengoku-province-name province) cell))
          (unless (eq previous :missing)
            (record "Grid collision: %s and %s (%s)"
                    (sengoku-province-name (aref provinces previous))
                    (sengoku-province-name province)
                    cell))
          (puthash cell province-index cells)
          (let ((adjacency (sengoku-province-adjacency province))
                (names (sengoku-province-adjacency-names province)))
            (unless (= (length adjacency) (length names))
              (record "%s has %d names but %d resolved neighbors"
                      (sengoku-province-name province)
                      (length names) (length adjacency)))
            (dotimes (adjacent-index (length adjacency))
              (let ((neighbor-index (aref adjacency adjacent-index)))
                (if (or (not (integerp neighbor-index))
                        (< neighbor-index 0)
                        (>= neighbor-index (length provinces)))
                    (record "%s has an unresolved neighbor at position %d"
                            (sengoku-province-name province)
                            adjacent-index)
                  (let ((neighbor (aref provinces neighbor-index)))
                    (unless (cl-find province-index
                                     (sengoku-province-adjacency neighbor)
                                     :test #'=)
                      (record "Asymmetric adjacency: %s -> %s"
                              (sengoku-province-name province)
                              (sengoku-province-name neighbor)))
                    (when (< adjacent-index (length names))
                      (unless (string=
                               (aref names adjacent-index)
                               (sengoku-province-name neighbor))
                        (record "%s neighbor %s resolved to %s"
                                (sengoku-province-name province)
                                (aref names adjacent-index)
                                (sengoku-province-name neighbor))))))))))))
    (nreverse errors)))

(defun sengoku--nonnegative-integer-p (value)
  "Return non-nil when VALUE is an integer greater than or equal to zero."
  (and (integerp value) (>= value 0)))

(defun sengoku-validate-resources (game)
  "Return mutable-state and resource invariant errors in GAME.
This checks nonnegative resources, bounded percentages, vector alignment,
unique item ownership, canonical alliances, and the 300-entry log cap."
  (let* ((clans (sengoku-game-clans game))
         (provinces (sengoku-game-provinces game))
         (states (sengoku-game-clan-states game))
         (generals (sengoku-game-generals game))
         (used (sengoku-game-general-used game))
         (items (sengoku-game-items game))
         (item-owners (make-vector (length items) -1))
         errors)
    (cl-labels ((record (format-string &rest arguments)
                  (push (apply #'format format-string arguments) errors)))
      (unless (and (integerp (sengoku-game-year game))
                   (> (sengoku-game-year game) 0))
        (record "Invalid year: %S" (sengoku-game-year game)))
      (unless (and (integerp (sengoku-game-month game))
                   (<= 1 (sengoku-game-month game) 12))
        (record "Invalid month: %S" (sengoku-game-month game)))
      (unless (and (integerp (sengoku-game-player game))
                   (<= -1 (sengoku-game-player game))
                   (< (sengoku-game-player game) (length clans)))
        (record "Invalid player index: %S" (sengoku-game-player game)))
      (unless (= (length provinces) sengoku-data-province-count)
        (record "Game has %d provinces" (length provinces)))
      (unless (= (length states) (length clans))
        (record "Game has %d clans but %d clan states"
                (length clans) (length states)))
      (unless (= (length generals) (length clans))
        (record "Game has %d clans but %d general rosters"
                (length clans) (length generals)))
      (unless (= (length used) (length generals))
        (record "General-used vector has %d rows for %d rosters"
                (length used) (length generals)))
      (dotimes (province-index (length provinces))
        (let ((province (aref provinces province-index)))
          (unless (and (integerp (sengoku-province-owner province))
                       (<= -1 (sengoku-province-owner province))
                       (< (sengoku-province-owner province) (length clans)))
            (record "%s has invalid owner %S"
                    (sengoku-province-name province)
                    (sengoku-province-owner province)))
          (dolist (slot `((koku . ,(sengoku-province-koku province))
                          (commerce . ,(sengoku-province-commerce province))
                          (soldiers . ,(sengoku-province-soldiers province))
                          (guns . ,(sengoku-province-guns province))
                          (cannon . ,(sengoku-province-cannon province))
                          (gold . ,(sengoku-province-gold province))
                          (rice . ,(sengoku-province-rice province))))
            (unless (sengoku--nonnegative-integer-p (cdr slot))
              (record "%s has invalid %s: %S"
                      (sengoku-province-name province)
                      (car slot) (cdr slot))))
          (dolist (slot `((loyalty . ,(sengoku-province-loyalty province))
                          (training . ,(sengoku-province-training province))))
            (unless (and (integerp (cdr slot))
                         (<= 0 (cdr slot) 100))
              (record "%s has invalid %s: %S"
                      (sengoku-province-name province)
                      (car slot) (cdr slot))))
          (unless (memq (sengoku-province-port province) '(0 1))
            (record "%s has invalid port flag: %S"
                    (sengoku-province-name province)
                    (sengoku-province-port province)))
          (unless (memq (sengoku-province-auto province) '(0 1))
            (record "%s has invalid auto flag: %S"
                    (sengoku-province-name province)
                    (sengoku-province-auto province)))))
      (dotimes (clan-index (min (length clans) (length states)))
        (let ((state (aref states clan-index)))
          (unless (and (integerp (sengoku-clan-state-culture state))
                       (<= 0 (sengoku-clan-state-culture state) 100))
            (record "Clan %d has invalid culture: %S"
                    clan-index (sengoku-clan-state-culture state)))
          (unless (and (integerp (sengoku-clan-state-rank state))
                       (<= 0 (sengoku-clan-state-rank state))
                       (< (sengoku-clan-state-rank state)
                          (length sengoku-data-ranks)))
            (record "Clan %d has invalid rank: %S"
                    clan-index (sengoku-clan-state-rank state)))
          (if (not (listp (sengoku-clan-state-items state)))
              (record "Clan %d item ownership is not a list" clan-index)
            (dolist (item-index (sengoku-clan-state-items state))
              (if (not (and (integerp item-index)
                            (>= item-index 0)
                            (< item-index (length items))))
                  (record "Clan %d has invalid item index %S"
                          clan-index item-index)
                (if (>= (aref item-owners item-index) 0)
                    (record "Item %d is owned by clans %d and %d"
                            item-index (aref item-owners item-index)
                            clan-index)
                  (aset item-owners item-index clan-index)))))))
      (dotimes (clan-index (min (length generals) (length used)))
        (let ((roster (aref generals clan-index))
              (flags (aref used clan-index)))
          (unless (= (length roster) 8)
            (record "Clan %d has %d generals" clan-index (length roster)))
          (unless (= (length flags) (length roster))
            (record "Clan %d has %d generals but %d used flags"
                    clan-index (length roster) (length flags)))
          (dotimes (general-index (length roster))
            (let ((general (aref roster general-index)))
              (unless (and (integerp (sengoku-general-war general))
                           (<= 1 (sengoku-general-war general) 100)
                           (integerp (sengoku-general-politics general))
                           (<= 1 (sengoku-general-politics general) 100))
                (record "General %s has invalid abilities"
                        (sengoku-general-name general)))))
          (dotimes (general-index (length flags))
            (unless (memq (aref flags general-index) '(0 1))
              (record "Clan %d general-used[%d] is %S"
                      clan-index general-index
                      (aref flags general-index))))))
      (let ((alliances (sengoku-game-alliances game)))
        (if (not (hash-table-p alliances))
            (record "Alliances is not a hash table")
          (maphash
           (lambda (key value)
             (if (not (and (stringp key)
                           (string-match
                            "\\`\\([0-9]+\\)-\\([0-9]+\\)\\'" key)))
                 (record "Invalid alliance key: %S" key)
               (let ((clan-a (string-to-number (match-string 1 key)))
                     (clan-b (string-to-number (match-string 2 key))))
                 (unless (and (< clan-a clan-b)
                              (< clan-b (length clans))
                              (string= key
                                       (sengoku-alliance-key clan-a clan-b)))
                   (record "Noncanonical alliance key: %s" key))))
             (unless (equal value 1)
               (record "Alliance %S has non-Vim value %S" key value)))
           alliances)))
      (unless (and (listp (sengoku-game-log game))
                   (<= (length (sengoku-game-log game)) 300)
                   (cl-every #'stringp (sengoku-game-log game)))
        (record "Log is malformed or exceeds 300 entries")))
    (nreverse errors)))

(defun sengoku-validate-game (game)
  "Return all static, map, and resource errors for GAME."
  (append (sengoku-validate-data)
          (sengoku-validate-map game)
          (sengoku-validate-resources game)))

(provide 'sengoku-core)

;;; sengoku-core.el ends here
