;;; sengoku-combat.el --- Combat engine for Sengoku -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: dkc
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: games

;;; Commentary:

;; Non-UI strategic and tactical combat translated from plugin/sengoku.vim.
;; All arithmetic and mutation order intentionally follow the Vim engine.
;;
;; Strategic API:
;;
;; - `sengoku-battle-prepare' commits an army and returns a
;;   `sengoku-battle-context'.
;; - `sengoku-battle-dispatch' resolves an undefended or automatic battle, or
;;   initializes a siege selected by the caller.
;; - `sengoku-battle-start' combines those two operations.
;; - `sengoku-battle-resolve-undefended' and
;;   `sengoku-battle-resolve-automatic' are the individual resolvers.
;;
;; Siege API:
;;
;; - `sengoku-battle-player-side' derives -2 (all AI), 0 (attacker), or 1
;;   (defender) from the campaign player and battle participants.
;; - `sengoku-battle-start-siege' creates the map and units after validating the
;;   caller-supplied PLAYER-SIDE against that derived value.
;; - `sengoku-siege-advance-to-player' runs AI units and day transitions until
;;   a player unit is ready or the siege ends.
;; - `sengoku-siege-player-turn-state', movement, target-query, attack, fire,
;;   wait, retreat, and surrender functions provide nonblocking UI steps.
;; - `sengoku-siege-run-all-ai' synchronously completes an all-AI siege.
;; - `sengoku-battle-apply-siege-result' applies a completed siege to the
;;   campaign provinces; `sengoku-battle-mark-shown' lets the UI record display.
;;
;; Battle context RESULT is `victory' or `defeat' after strategic application.
;; Siege RESULT is one of `fall', `annihilated', `surrender', `destroyed',
;; `retreat', or `timeout'.  Report lines and siege messages remain strings.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'sengoku-data)
(require 'sengoku-core)

(defvar sengoku--siege-turn-states
  (make-hash-table :test #'eq :weakness 'key)
  "External non-save state for nonblocking siege turns.
Keys are `sengoku-siege' objects.  Values are plists containing the side and
unit cursors, current player unit, and remaining movement points.")

(defun sengoku--combat-divide (numerator denominator)
  "Return Vim-compatible integer division of NUMERATOR by DENOMINATOR."
  (sengoku-vim-divide numerator denominator))

(defun sengoku--combat-province (game province-index)
  "Return PROVINCE-INDEX from GAME, or signal an error when invalid."
  (let ((provinces (sengoku-game-provinces game)))
    (unless (and (integerp province-index)
                 (>= province-index 0)
                 (< province-index (length provinces)))
      (error "Invalid province index: %S" province-index))
    (aref provinces province-index)))

(defun sengoku--combat-clan (game clan-index)
  "Return CLAN-INDEX from GAME, or signal an error when invalid."
  (let ((clans (sengoku-game-clans game)))
    (unless (and (integerp clan-index)
                 (>= clan-index 0)
                 (< clan-index (length clans)))
      (error "Invalid clan index: %S" clan-index))
    (aref clans clan-index)))

(defun sengoku--battle-add-line (context format-string &rest arguments)
  "Append FORMAT-STRING with ARGUMENTS to CONTEXT and return the line."
  (let ((line (apply #'format format-string arguments)))
    (setf (sengoku-battle-context-lines context)
          (nconc (sengoku-battle-context-lines context) (list line)))
    line))

(defun sengoku-battle-won-p (context)
  "Return non-nil when CONTEXT has been applied as an attacker victory."
  (eq (sengoku-battle-context-result context) 'victory))

(defun sengoku-battle-finished-p (context)
  "Return non-nil when CONTEXT has a strategically applied result."
  (memq (sengoku-battle-context-result context) '(victory defeat)))

(defun sengoku-battle-mark-shown (context)
  "Mark CONTEXT's report as displayed by a tactical UI and return CONTEXT.
Headless combat never calls this operation, so its contexts remain unshown."
  (setf (sengoku-battle-context-shown context) t)
  context)

(defun sengoku-battle-player-side (game context)
  "Return GAME's valid tactical player side for CONTEXT.
The result is 0 when the campaign player attacks, 1 when the campaign player
defends, and -2 when no campaign player participates."
  (let ((player (sengoku-game-player game)))
    (cond
     ((< player 0) -2)
     ((= player (sengoku-battle-context-attacker-clan context)) 0)
     ((= player (sengoku-battle-context-defender-clan context)) 1)
     (t -2))))

(defun sengoku--battle-validate-pending-state (game context)
  "Verify that GAME still matches prepared battle CONTEXT.
Campaign controllers must suspend strategic mutation while a prepared battle
is pending.  Rejecting ownership drift prevents survivors or conquest effects
from being applied to unrelated clans."
  (let ((source
         (sengoku--combat-province
          game (sengoku-battle-context-from context)))
        (target
         (sengoku--combat-province
          game (sengoku-battle-context-to context))))
    (unless (= (sengoku-province-owner source)
               (sengoku-battle-context-attacker-clan context))
      (error "Battle source ownership changed while combat was pending"))
    (unless (= (sengoku-province-owner target)
               (sengoku-battle-context-defender-clan context))
      (error "Battle target ownership changed while combat was pending"))
    t))

(defun sengoku-battle-prepare
    (game attacker-clan from-index to-index soldiers &optional general)
  "Commit an attacking force in GAME and return its battle context.
ATTACKER-CLAN attacks from FROM-INDEX into TO-INDEX with SOLDIERS.  GENERAL
is the attacking commander; when nil, the clan's daimyo-first roster entry is
used.  This function performs no resolution or prompting.

As in Vim, guns are committed in proportion to the source garrison, cannon are
limited to one per 1500 committed soldiers, and rice costs SOLDIERS / 20.  All
four resources are deducted in Vim's mutation order before the context is
returned.  SOLDIERS must be positive and no greater than the source garrison."
  (unless (sengoku-game-p game)
    (error "Not a sengoku game: %S" game))
  (let* ((attacker (sengoku--combat-clan game attacker-clan))
         (source (sengoku--combat-province game from-index))
         (target (sengoku--combat-province game to-index))
         (source-soldiers (sengoku-province-soldiers source)))
    (unless (= (sengoku-province-owner source) attacker-clan)
      (error "Attacker does not own source province"))
    (unless (and (integerp soldiers) (> soldiers 0)
                 (<= soldiers source-soldiers))
      (error "Invalid committed soldiers: %S" soldiers))
    (when (= (sengoku-province-owner target) attacker-clan)
      (error "Cannot attack a province owned by the attacker"))
    (let* ((commander
            (or general
                (aref (aref (sengoku-game-generals game) attacker-clan) 0)))
           (defender-clan (sengoku-province-owner target))
           (defender-name
            (if (>= defender-clan 0)
                (sengoku-clan-name
                 (sengoku--combat-clan game defender-clan))
              "無主"))
           (committed-guns
            (if (> source-soldiers 0)
                (sengoku--combat-divide
                 (* (sengoku-province-guns source) soldiers)
                 source-soldiers)
              0))
           (committed-cannons
            (min (sengoku-province-cannon source)
                 (sengoku--combat-divide soldiers 1500)))
           (training (sengoku-province-training source))
           (context
            (make-sengoku-battle-context
             :attacker-clan attacker-clan
             :defender-clan defender-clan
             :from from-index
             :to to-index
             :soldiers soldiers
             :guns committed-guns
             :training training
             :cannons committed-cannons
             :general commander
             :lines
             (list
              (format "◆ %s軍%d(大将:%s)、%sより%s(%s領)へ侵攻!"
                      (sengoku-clan-name attacker)
                      soldiers
                      (sengoku-general-name commander)
                      (sengoku-province-name source)
                      (sengoku-province-name target)
                      defender-name))
             :result nil
             :shown nil
             :siege nil)))
      ;; Preserve the source mutation order exactly.
      (setf (sengoku-province-soldiers source)
            (- (sengoku-province-soldiers source) soldiers))
      (setf (sengoku-province-guns source)
            (- (sengoku-province-guns source) committed-guns))
      (setf (sengoku-province-cannon source)
            (- (sengoku-province-cannon source) committed-cannons))
      (setf (sengoku-province-rice source)
            (max 0
                 (- (sengoku-province-rice source)
                    (sengoku--combat-divide soldiers 20))))
      context)))

(defun sengoku-battle-resolve-undefended (game context)
  "Apply CONTEXT as an undefended conquest in GAME and return CONTEXT.
The target must still have no soldiers.  Defender guns are retained and joined
by the committed attacker guns; defender cannon remain in place and committed
attacker cannon are added after conquest, matching Vim."
  (when (sengoku-battle-finished-p context)
    (error "Battle has already been resolved"))
  (sengoku--battle-validate-pending-state game context)
  (let* ((target-index (sengoku-battle-context-to context))
         (target (sengoku--combat-province game target-index)))
    (unless (<= (sengoku-province-soldiers target) 0)
      (error "Target province is defended"))
    (sengoku--battle-add-line
     context "守兵なし! %sは無血開城した。"
     (sengoku-province-name target))
    (setf (sengoku-battle-context-lines context)
          (sengoku-conquer-apply
           game
           (sengoku-battle-context-attacker-clan context)
           target-index
           (sengoku-battle-context-soldiers context)
           (+ (sengoku-province-guns target)
              (sengoku-battle-context-guns context))
           (sengoku-battle-context-training context)
           (sengoku-battle-context-lines context)))
    (setf (sengoku-province-cannon target)
          (+ (sengoku-province-cannon target)
             (sengoku-battle-context-cannons context))
          (sengoku-battle-context-result context) 'victory)
    context))

(defun sengoku--automatic-army-power (soldiers training war guns cannons)
  "Return power for SOLDIERS, TRAINING, WAR, GUNS, and CANNONS.
Use Vim's left-to-right integer truncation order."
  (+ (sengoku--combat-divide
      (* (sengoku--combat-divide
          (* soldiers (+ 100 training)) 100)
         (+ 80 war))
      100)
     (* guns 30)
     (* cannons 300)))

(defun sengoku-battle-resolve-automatic (game context)
  "Resolve CONTEXT in GAME with Vim's eight-round battle and return it.
The defender receives the 20 percent power bonus.  Damage randomization,
simultaneous loss calculation, attacker quarter-strength retreat threshold,
resource returns, conquest, and report lines preserve Vim's order exactly."
  (when (sengoku-battle-finished-p context)
    (error "Battle has already been resolved"))
  (when (sengoku-battle-context-siege context)
    (error "Context already contains a siege"))
  (sengoku--battle-validate-pending-state game context)
  (let* ((source (sengoku--combat-province
                  game (sengoku-battle-context-from context)))
         (target (sengoku--combat-province
                  game (sengoku-battle-context-to context)))
         (attacker-clan (sengoku-battle-context-attacker-clan context))
         (defender-clan (sengoku-battle-context-defender-clan context))
         (attacker-name
          (sengoku-clan-name (sengoku--combat-clan game attacker-clan)))
         (defender-name
          (if (>= defender-clan 0)
              (sengoku-clan-name
               (sengoku--combat-clan game defender-clan))
            "無主"))
         (attacker-war
          (sengoku-general-war (sengoku-battle-context-general context)))
         (defender-war
          (sengoku-general-war
           (sengoku-best-war-general game defender-clan)))
         (initial-soldiers (sengoku-battle-context-soldiers context))
         (attacker-soldiers initial-soldiers)
         (attacker-guns (sengoku-battle-context-guns context))
         (defender-soldiers (sengoku-province-soldiers target))
         (defender-guns (sengoku-province-guns target))
         (round 0))
    (when (<= defender-soldiers 0)
      (setq context (sengoku-battle-resolve-undefended game context)))
    (unless (sengoku-battle-finished-p context)
      (while (and (< round 8)
                  (> attacker-soldiers
                     (sengoku--combat-divide initial-soldiers 4))
                  (> defender-soldiers 0))
        (setq round (1+ round))
        (let* ((attacker-power
                (sengoku--automatic-army-power
                 attacker-soldiers
                 (sengoku-battle-context-training context)
                 attacker-war attacker-guns
                 (sengoku-battle-context-cannons context)))
               (defender-power
                (sengoku--automatic-army-power
                 defender-soldiers
                 (sengoku-province-training target)
                 defender-war defender-guns
                 (sengoku-province-cannon target)))
               (defender-power
                (sengoku--combat-divide (* defender-power 120) 100))
               (defender-loss
                (sengoku--combat-divide
                 (sengoku--combat-divide
                  (* attacker-power (+ 80 (sengoku-random 41))) 100)
                 15))
               (attacker-loss
                (sengoku--combat-divide
                 (sengoku--combat-divide
                  (* defender-power (+ 80 (sengoku-random 41))) 100)
                 15)))
          (setq defender-soldiers
                (max 0 (- defender-soldiers defender-loss)))
          (setq attacker-soldiers
                (max 0 (- attacker-soldiers attacker-loss)))
          (sengoku--battle-add-line
           context "  %d合目: %s軍%d ⇔ %s軍%d"
           round attacker-name attacker-soldiers
           defender-name defender-soldiers)))
      (if (and (<= defender-soldiers 0) (> attacker-soldiers 0))
          (let ((loss-rate
                 (if (> initial-soldiers 0)
                     (sengoku--combat-divide
                      (* (- initial-soldiers attacker-soldiers) 100)
                      initial-soldiers)
                   0)))
            (sengoku--battle-add-line
             context "★ %s軍勝利! %sを攻略した!"
             attacker-name (sengoku-province-name target))
            (setf (sengoku-battle-context-lines context)
                  (sengoku-conquer-apply
                   game attacker-clan
                   (sengoku-battle-context-to context)
                   attacker-soldiers
                   (+ (sengoku--combat-divide
                       (sengoku-province-guns target) 2)
                      (sengoku--combat-divide
                       (* attacker-guns (- 100 loss-rate)) 100))
                   (sengoku-battle-context-training context)
                   (sengoku-battle-context-lines context)))
            (setf (sengoku-province-cannon target)
                  (+ (sengoku-province-cannon target)
                     (sengoku-battle-context-cannons context))
                  (sengoku-battle-context-result context) 'victory))
        (setf (sengoku-province-soldiers target)
              (max defender-soldiers 100))
        (setf (sengoku-province-soldiers source)
              (+ (sengoku-province-soldiers source) attacker-soldiers))
        (setf (sengoku-province-guns source)
              (+ (sengoku-province-guns source)
                 (if (> attacker-soldiers 0) attacker-guns 0)))
        (setf (sengoku-province-cannon source)
              (+ (sengoku-province-cannon source)
                 (sengoku-battle-context-cannons context)))
        (sengoku--battle-add-line
         context "%s軍は攻めきれず撤退。%sの守りは堅かった。"
         attacker-name (sengoku-province-name target))
        (setf (sengoku-battle-context-result context) 'defeat)))
    context))

(defun sengoku--siege-cell-key (row column)
  "Return the Vim-compatible structure key for ROW and COLUMN."
  (format "%d,%d" row column))

(defun sengoku--make-siege-grid ()
  "Return a fresh plain siege grid."
  (let ((grid (make-vector sengoku-data-siege-rows nil)))
    (dotimes (row sengoku-data-siege-rows)
      (aset grid row (make-vector sengoku-data-siege-columns "・")))
    grid))

(defun sengoku--siege-plant-fortifications (grid structure-hp)
  "Mutate GRID and STRUCTURE-HP with Vim's walls, gate, and keep."
  (let ((top (aref sengoku-data-siege-wall-bounds 0))
        (bottom (aref sengoku-data-siege-wall-bounds 1))
        (left (aref sengoku-data-siege-wall-bounds 2))
        (right (aref sengoku-data-siege-wall-bounds 3)))
    (cl-loop for row from top to bottom do
             (cl-loop for column from left to right do
                      (when (or (= row top) (= row bottom)
                                (= column left) (= column right))
                        (aset (aref grid row) column "壁")
                        (puthash
                         (sengoku--siege-cell-key row column)
                         sengoku-data-siege-wall-hit-points
                         structure-hp)))))
  (let ((gate-row (aref sengoku-data-siege-gate 0))
        (gate-column (aref sengoku-data-siege-gate 1))
        (keep-row (aref sengoku-data-siege-keep 0))
        (keep-column (aref sengoku-data-siege-keep 1)))
    (aset (aref grid gate-row) gate-column "門")
    (puthash (sengoku--siege-cell-key gate-row gate-column)
             sengoku-data-siege-gate-hit-points structure-hp)
    (aset (aref grid keep-row) keep-column "本")))

(defun sengoku--siege-plant-forests (grid)
  "Plant seven random forests in GRID in Vim's random-call order.
If a degenerate injected random function repeatedly returns occupied cells,
fall back to the first open candidate after one full candidate count."
  (let ((planted 0)
        (stalled 0)
        (candidate-count
         (* sengoku-data-siege-rows
            (length sengoku-data-siege-forest-columns))))
    (while (< planted sengoku-data-siege-forest-count)
      (let* ((row (sengoku-random sengoku-data-siege-rows))
             (column
              (aref sengoku-data-siege-forest-columns
                    (sengoku-random
                     (length sengoku-data-siege-forest-columns)))))
        (if (string= (aref (aref grid row) column) "・")
            (progn
              (aset (aref grid row) column "森")
              (setq planted (1+ planted)
                    stalled 0))
          (setq stalled (1+ stalled))
          (when (>= stalled candidate-count)
            (let ((fallback-planted nil))
              (catch 'forest-planted
                (dotimes (fallback-row sengoku-data-siege-rows)
                  (dotimes (column-index
                            (length sengoku-data-siege-forest-columns))
                    (let ((fallback-column
                           (aref sengoku-data-siege-forest-columns
                                 column-index)))
                      (when (string=
                             (aref (aref grid fallback-row) fallback-column)
                             "・")
                        (aset (aref grid fallback-row) fallback-column "森")
                        (setq fallback-planted t)
                        (throw 'forest-planted nil))))))
              (unless fallback-planted
                (error "No open siege forest cell remains"))
              (setq planted (1+ planted)
                    stalled 0))))))))

(defun sengoku--siege-units (game context)
  "Return Vim-compatible split and deployed units for CONTEXT in GAME."
  (let* ((source
          (sengoku--combat-province game
                                    (sengoku-battle-context-from context)))
         (target
          (sengoku--combat-province game
                                    (sengoku-battle-context-to context)))
         (defender-general
          (sengoku-best-war-general
           game (sengoku-battle-context-defender-clan context)))
         (attacker-count
          (min 5
               (max 1
                    (sengoku--combat-divide
                     (sengoku-battle-context-soldiers context) 800))))
         (defender-count
          (min 5
               (max 1
                    (sengoku--combat-divide
                     (sengoku-province-soldiers target) 800))))
         units)
    (dotimes (index attacker-count)
      (let ((spot (aref sengoku-data-siege-attacker-spots index)))
        (push
         (make-sengoku-siege-unit
          :side 0
          :label (aref sengoku-data-siege-attacker-labels index)
          :soldiers
          (+ (sengoku--combat-divide
              (sengoku-battle-context-soldiers context) attacker-count)
             (if (= index 0)
                 (mod (sengoku-battle-context-soldiers context)
                      attacker-count)
               0))
          :guns
          (sengoku--combat-divide
           (sengoku-battle-context-guns context) attacker-count)
          :training (sengoku-province-training source)
          :war
          (sengoku-general-war (sengoku-battle-context-general context))
          :cannon
          (+ (sengoku--combat-divide
              (sengoku-battle-context-cannons context) attacker-count)
             (if (= index 0)
                 (mod (sengoku-battle-context-cannons context)
                      attacker-count)
               0))
          :row (aref spot 0)
          :column (aref spot 1))
         units)))
    (dotimes (index defender-count)
      (let ((spot (aref sengoku-data-siege-defender-spots index)))
        (push
         (make-sengoku-siege-unit
          :side 1
          :label (aref sengoku-data-siege-defender-labels index)
          :soldiers
          (+ (sengoku--combat-divide
              (sengoku-province-soldiers target) defender-count)
             (if (= index 0)
                 (mod (sengoku-province-soldiers target) defender-count)
               0))
          :guns
          (sengoku--combat-divide
           (sengoku-province-guns target) defender-count)
          :training (sengoku-province-training target)
          :war (sengoku-general-war defender-general)
          :cannon
          (+ (sengoku--combat-divide
              (sengoku-province-cannon target) defender-count)
             (if (= index 0)
                 (mod (sengoku-province-cannon target) defender-count)
               0))
          :row (aref spot 0)
          :column (aref spot 1))
         units)))
    (vconcat (nreverse units))))

(defun sengoku--new-siege-turn-state ()
  "Return the initial external turn cursor plist."
  (list :side 0 :unit-index 0 :waiting-player nil
        :active-unit nil :move-points 0))

(defun sengoku--siege-turn-state (siege)
  "Return SIEGE's mutable external turn cursor, creating it if needed."
  (or (gethash siege sengoku--siege-turn-states)
      (let ((state (sengoku--new-siege-turn-state)))
        (puthash siege state sengoku--siege-turn-states)
        state)))

(defun sengoku-battle-start-siege (game context player-side)
  "Initialize CONTEXT's tactical siege in GAME for PLAYER-SIDE.
PLAYER-SIDE is supplied by the caller and must equal
`sengoku-battle-player-side': -2 for all AI, 0 for a human attacker, or 1 for
a human defender.  No unit acts until `sengoku-siege-advance-to-player' or
`sengoku-siege-run-all-ai' is called."
  (unless (memq player-side '(-2 0 1))
    (error "Invalid siege player side: %S" player-side))
  (let ((expected-side (sengoku-battle-player-side game context)))
    (unless (= player-side expected-side)
      (error "Siege player side %S conflicts with campaign participants; expected %S"
             player-side expected-side)))
  (when (sengoku-battle-finished-p context)
    (error "Battle has already been resolved"))
  (when (sengoku-battle-context-siege context)
    (error "Siege has already been initialized"))
  (sengoku--battle-validate-pending-state game context)
  (let* ((target
          (sengoku--combat-province game
                                    (sengoku-battle-context-to context)))
         (defender-clan (sengoku-province-owner target)))
    (when (<= (sengoku-province-soldiers target) 0)
      (error "Cannot initialize a siege without defenders"))
    ;; Keep every downstream unit/general lookup on the same defender snapshot.
    (setf (sengoku-battle-context-defender-clan context) defender-clan)
    (let* ((grid (sengoku--make-siege-grid))
           (structure-hp (make-hash-table :test #'equal))
           (defender-general
            (sengoku-best-war-general game defender-clan))
           siege)
      (sengoku--siege-plant-fortifications grid structure-hp)
      (sengoku--siege-plant-forests grid)
      (setq siege
            (make-sengoku-siege
             :attacker-clan (sengoku-battle-context-attacker-clan context)
             :defender-clan defender-clan
             :from (sengoku-battle-context-from context)
             :to (sengoku-battle-context-to context)
             :initial-soldiers (sengoku-battle-context-soldiers context)
             :day 1
             :grid grid
             :structure-hp structure-hp
             :units (sengoku--siege-units game context)
             :messages nil
             :result nil
             :attacker-general-name
             (sengoku-general-name (sengoku-battle-context-general context))
             :defender-general-name (sengoku-general-name defender-general)
             :player-side player-side))
      (setf (sengoku-battle-context-siege context) siege)
      (puthash siege (sengoku--new-siege-turn-state)
               sengoku--siege-turn-states)
      (sengoku-siege-add-message
       siege "%s城の戦い、火蓋が切られた!"
       (sengoku-province-name target))
      context)))

(defun sengoku-siege-interior-p (row column)
  "Return non-nil when ROW,COLUMN lies in Vim's defended castle interior."
  (and (>= row 3) (<= row 5) (>= column 7) (<= column 9)))

(defun sengoku-siege-alive-units (siege side)
  "Return SIEGE's living units on SIDE in stable deployment order."
  (let ((units (sengoku-siege-units siege))
        result)
    (dotimes (index (length units))
      (let ((unit (aref units index)))
        (when (and (> (sengoku-siege-unit-soldiers unit) 0)
                   (= (sengoku-siege-unit-side unit) side))
          (push unit result))))
    (nreverse result)))

(defun sengoku-siege-unit-at (siege row column)
  "Return SIEGE's living unit at ROW,COLUMN, or nil."
  (let ((units (sengoku-siege-units siege))
        found
        (index 0))
    (while (and (< index (length units)) (not found))
      (let ((unit (aref units index)))
        (when (and (> (sengoku-siege-unit-soldiers unit) 0)
                   (= (sengoku-siege-unit-row unit) row)
                   (= (sengoku-siege-unit-column unit) column))
          (setq found unit)))
      (setq index (1+ index)))
    found))

(defun sengoku-siege-add-message (siege format-string &rest arguments)
  "Append FORMAT-STRING with ARGUMENTS as a dated SIEGE message."
  (let ((message
         (format "%2d日目: %s"
                 (sengoku-siege-day siege)
                 (apply #'format format-string arguments))))
    (setf (sengoku-siege-messages siege)
          (nconc (sengoku-siege-messages siege) (list message)))
    message))

(defun sengoku-siege-passable-p (siege unit row column)
  "Return non-nil when UNIT may enter ROW,COLUMN in SIEGE."
  (and (>= row 0) (< row sengoku-data-siege-rows)
       (>= column 0) (< column sengoku-data-siege-columns)
       (let ((cell (aref (aref (sengoku-siege-grid siege) row) column)))
         (and (not (string= cell "壁"))
              (not (and (string= cell "門")
                        (= (sengoku-siege-unit-side unit) 0)))
              (not (sengoku-siege-unit-at siege row column))))))

(defun sengoku-siege-move-cost (siege row column)
  "Return the movement cost of SIEGE cell ROW,COLUMN."
  (if (string= (aref (aref (sengoku-siege-grid siege) row) column)
               "森")
      2
    1))

(defun sengoku-siege-defense-factor (siege target-unit)
  "Return TARGET-UNIT's incoming-damage percentage in SIEGE."
  (let ((factor 100))
    (when (string=
           (aref (aref (sengoku-siege-grid siege)
                       (sengoku-siege-unit-row target-unit))
                 (sengoku-siege-unit-column target-unit))
           "森")
      (setq factor (- factor 20)))
    (when (and (= (sengoku-siege-unit-side target-unit) 1)
               (sengoku-siege-interior-p
                (sengoku-siege-unit-row target-unit)
                (sengoku-siege-unit-column target-unit)))
      (setq factor (- factor 30)))
    factor))

(defun sengoku-siege-melee (siege unit target-unit)
  "Have UNIT melee TARGET-UNIT in SIEGE, mutating both and adding messages."
  (let* ((damage-base
          (+ (sengoku--combat-divide
              (sengoku--combat-divide
               (* (sengoku--combat-divide
                   (* (sengoku-siege-unit-soldiers unit)
                      (+ 100 (sengoku-siege-unit-training unit)))
                   100)
                  (+ 80 (sengoku-siege-unit-war unit)))
               100)
              10)
             (* (sengoku-siege-unit-guns unit) 2)))
         (damage
          (sengoku--combat-divide
           (* (sengoku--combat-divide
               (* damage-base
                  (sengoku-siege-defense-factor siege target-unit))
               100)
              (+ 80 (sengoku-random 41)))
           100))
         (counter-base
          (+ (sengoku--combat-divide
              (sengoku--combat-divide
               (* (sengoku--combat-divide
                   (* (sengoku-siege-unit-soldiers target-unit)
                      (+ 100 (sengoku-siege-unit-training target-unit)))
                   100)
                  (+ 80 (sengoku-siege-unit-war target-unit)))
               100)
              15)
             (sengoku-siege-unit-guns target-unit)))
         (counter
          (sengoku--combat-divide
           (* counter-base (+ 80 (sengoku-random 41))) 100)))
    (setf (sengoku-siege-unit-soldiers target-unit)
          (max 0 (- (sengoku-siege-unit-soldiers target-unit) damage)))
    (setf (sengoku-siege-unit-soldiers unit)
          (max 0 (- (sengoku-siege-unit-soldiers unit) counter)))
    (sengoku-siege-add-message
     siege "%s隊が%s隊に突撃! (敵-%d/自-%d)"
     (sengoku-siege-unit-label unit)
     (sengoku-siege-unit-label target-unit)
     damage counter)
    (when (<= (sengoku-siege-unit-soldiers target-unit) 0)
      (sengoku-siege-add-message
       siege "%s隊 壊滅!" (sengoku-siege-unit-label target-unit)))
    (when (<= (sengoku-siege-unit-soldiers unit) 0)
      (sengoku-siege-add-message
       siege "%s隊 壊滅!" (sengoku-siege-unit-label unit)))
    (list damage counter)))

(defun sengoku-siege-fire (siege unit target-unit)
  "Have UNIT fire on TARGET-UNIT in SIEGE and return the damage."
  (let* ((damage-base
          (+ (* (sengoku-siege-unit-guns unit) 8)
             (* (sengoku-siege-unit-cannon unit) 25)
             (sengoku--combat-divide
              (sengoku-siege-unit-soldiers unit) 100)))
         (damage
          (sengoku--combat-divide
           (* (sengoku--combat-divide
               (* damage-base
                  (sengoku-siege-defense-factor siege target-unit))
               100)
              (+ 80 (sengoku-random 41)))
           100)))
    (setf (sengoku-siege-unit-soldiers target-unit)
          (max 0 (- (sengoku-siege-unit-soldiers target-unit) damage)))
    (sengoku-siege-add-message
     siege "%s隊が%s隊に%s斉射! (敵-%d)"
     (sengoku-siege-unit-label unit)
     (sengoku-siege-unit-label target-unit)
     (if (> (sengoku-siege-unit-cannon unit) 0)
         "大筒・鉄砲"
       "鉄砲")
     damage)
    (when (<= (sengoku-siege-unit-soldiers target-unit) 0)
      (sengoku-siege-add-message
       siege "%s隊 壊滅!" (sengoku-siege-unit-label target-unit)))
    damage))

(defun sengoku-siege-structure-attack (siege unit row column)
  "Have attacker UNIT assault the structure at ROW,COLUMN in SIEGE.
Return the damage dealt."
  (let* ((key (sengoku--siege-cell-key row column))
         (damage
          (+ (sengoku--combat-divide
              (sengoku-siege-unit-soldiers unit) 40)
             (sengoku--combat-divide
              (sengoku-siege-unit-training unit) 5)
             (* (sengoku-siege-unit-cannon unit) 40)))
         (structure-hp (sengoku-siege-structure-hp siege))
         (remaining (- (gethash key structure-hp) damage))
         (cell (aref (aref (sengoku-siege-grid siege) row) column))
         (kind (if (string= cell "門") "城門" "城壁")))
    (puthash key remaining structure-hp)
    (if (<= remaining 0)
        (progn
          (aset (aref (sengoku-siege-grid siege) row) column "・")
          (sengoku-siege-add-message
           siege "%s隊が%sを打ち破った!"
           (sengoku-siege-unit-label unit) kind))
      (sengoku-siege-add-message
       siege "%s隊が%sに攻めかかる (耐久残%d)"
       (sengoku-siege-unit-label unit) kind remaining))
    damage))

(defun sengoku-siege-adjacent-targets (siege unit)
  "Return UNIT's ordered adjacent targets in SIEGE.
Each result is a plist with :kind `unit' or `structure', :row, :column, and
:display.  Unit targets also contain :unit.  Direction order is up, down,
left, right exactly as in Vim."
  (let (targets)
    (dolist (direction '((-1 0) (1 0) (0 -1) (0 1)))
      (let ((row (+ (sengoku-siege-unit-row unit) (nth 0 direction)))
            (column (+ (sengoku-siege-unit-column unit)
                       (nth 1 direction))))
        (when (and (>= row 0) (< row sengoku-data-siege-rows)
                   (>= column 0) (< column sengoku-data-siege-columns))
          (let ((target-unit (sengoku-siege-unit-at siege row column)))
            (if (and target-unit
                     (/= (sengoku-siege-unit-side target-unit)
                         (sengoku-siege-unit-side unit)))
                (push (list :kind 'unit
                            :unit target-unit
                            :row row :column column
                            :display
                            (format "%s隊 (兵%d)"
                                    (sengoku-siege-unit-label target-unit)
                                    (sengoku-siege-unit-soldiers target-unit)))
                      targets)
              (let ((cell
                     (aref (aref (sengoku-siege-grid siege) row) column)))
                (when (and (= (sengoku-siege-unit-side unit) 0)
                           (or (string= cell "門") (string= cell "壁")))
                  (push (list :kind 'structure
                              :row row :column column
                              :display
                              (format "%s (耐久%d)"
                                      (if (string= cell "門")
                                          "城門"
                                        "城壁")
                                      (gethash
                                       (sengoku--siege-cell-key row column)
                                       (sengoku-siege-structure-hp siege))))
                        targets))))))))
    (nreverse targets)))

(defun sengoku-siege-fire-targets (siege unit)
  "Return SIEGE enemies in UNIT's square range two, in source order."
  (let (targets)
    (dolist (target (sengoku-siege-alive-units
                     siege (- 1 (sengoku-siege-unit-side unit))))
      (when (and (<= (abs (- (sengoku-siege-unit-row target)
                             (sengoku-siege-unit-row unit)))
                      2)
                 (<= (abs (- (sengoku-siege-unit-column target)
                             (sengoku-siege-unit-column unit)))
                      2))
        (push target targets)))
    (nreverse targets)))

(defun sengoku--siege-occupy-keep-p (siege unit)
  "Set SIEGE to `fall' if attacker UNIT occupies the keep, and report success."
  (when (and (= (sengoku-siege-unit-side unit) 0)
             (= (sengoku-siege-unit-row unit)
                (aref sengoku-data-siege-keep 0))
             (= (sengoku-siege-unit-column unit)
                (aref sengoku-data-siege-keep 1)))
    (setf (sengoku-siege-result siege) 'fall)
    t))

(defun sengoku--siege-gate-intact-p (siege)
  "Return non-nil when SIEGE's gate cell is still a gate."
  (string=
   (aref (aref (sengoku-siege-grid siege)
               (aref sengoku-data-siege-gate 0))
         (aref sengoku-data-siege-gate 1))
   "門"))

(defun sengoku--siege-distance (unit row column)
  "Return UNIT's Manhattan distance from ROW,COLUMN."
  (+ (abs (- row (sengoku-siege-unit-row unit)))
     (abs (- column (sengoku-siege-unit-column unit)))))

(defun sengoku--siege-nearest-enemy (unit enemies)
  "Return UNIT's nearest member of ENEMIES, preserving source-order ties."
  (let ((nearest (car enemies)))
    (dolist (enemy enemies)
      (when (< (sengoku--siege-distance
                unit
                (sengoku-siege-unit-row enemy)
                (sengoku-siege-unit-column enemy))
               (sengoku--siege-distance
                unit
                (sengoku-siege-unit-row nearest)
                (sengoku-siege-unit-column nearest)))
        (setq nearest enemy)))
    nearest))

(defun sengoku--siege-ai-unit (siege unit)
  "Run one AI UNIT turn in SIEGE with Vim's priorities and direction order."
  (let ((enemies
         (sengoku-siege-alive-units
          siege (- 1 (sengoku-siege-unit-side unit)))))
    (when enemies
      (let ((fire-targets
             (and (or (> (sengoku-siege-unit-guns unit) 0)
                      (> (sengoku-siege-unit-cannon unit) 0))
                  (sengoku-siege-fire-targets siege unit))))
        (if fire-targets
            (sengoku-siege-fire siege unit (car fire-targets))
          (let (goal hold-position)
            (if (= (sengoku-siege-unit-side unit) 0)
                (setq goal
                      (if (sengoku--siege-gate-intact-p siege)
                          (list (aref sengoku-data-siege-gate 0)
                                (aref sengoku-data-siege-gate 1))
                        (list (aref sengoku-data-siege-keep 0)
                              (aref sengoku-data-siege-keep 1))))
              (let ((threat
                     (cl-some
                      (lambda (enemy)
                        (or (sengoku-siege-interior-p
                             (sengoku-siege-unit-row enemy)
                             (sengoku-siege-unit-column enemy))
                            (<= (sengoku--siege-distance
                                 unit
                                 (sengoku-siege-unit-row enemy)
                                 (sengoku-siege-unit-column enemy))
                                2)))
                      enemies)))
                (if (and (sengoku--siege-gate-intact-p siege)
                         (not threat))
                    (setq hold-position t)
                  (let ((nearest
                         (sengoku--siege-nearest-enemy unit enemies)))
                    (setq goal
                          (list (sengoku-siege-unit-row nearest)
                                (sengoku-siege-unit-column nearest)))))))
            (if hold-position
                (let ((targets (sengoku-siege-adjacent-targets siege unit)))
                  (when (and targets
                             (eq (plist-get (car targets) :kind) 'unit))
                    (sengoku-siege-melee
                     siege unit (plist-get (car targets) :unit))))
              (let ((move-left sengoku-data-siege-move-points))
                (catch 'turn-done
                  (while (> move-left 0)
                    (let* ((targets
                            (sengoku-siege-adjacent-targets siege unit))
                           (enemy-target
                            (cl-find-if
                             (lambda (target)
                               (eq (plist-get target :kind) 'unit))
                             targets)))
                      (when enemy-target
                        (sengoku-siege-melee
                         siege unit (plist-get enemy-target :unit))
                        (throw 'turn-done nil))
                      (when (= (sengoku-siege-unit-side unit) 0)
                        (let ((gate-target
                               (cl-find-if
                                (lambda (target)
                                  (and
                                   (eq (plist-get target :kind) 'structure)
                                   (string=
                                    (aref
                                     (aref (sengoku-siege-grid siege)
                                           (plist-get target :row))
                                     (plist-get target :column))
                                    "門")))
                                targets)))
                          (when gate-target
                            (sengoku-siege-structure-attack
                             siege unit
                             (plist-get gate-target :row)
                             (plist-get gate-target :column))
                            (throw 'turn-done nil))))
                      (let ((current-distance
                             (sengoku--siege-distance
                              unit (nth 0 goal) (nth 1 goal)))
                            moved)
                        (dolist (direction '((0 1) (0 -1) (-1 0) (1 0)))
                          (unless moved
                            (let* ((row
                                    (+ (sengoku-siege-unit-row unit)
                                       (nth 0 direction)))
                                   (column
                                    (+ (sengoku-siege-unit-column unit)
                                       (nth 1 direction)))
                                   (cost
                                    (and
                                     (sengoku-siege-passable-p
                                      siege unit row column)
                                     (sengoku-siege-move-cost
                                      siege row column))))
                              (when (and cost
                                         (< (+ (abs (- (nth 0 goal) row))
                                               (abs (- (nth 1 goal) column)))
                                            current-distance)
                                         (<= cost move-left))
                                (setq move-left (- move-left cost)
                                      moved t)
                                (setf (sengoku-siege-unit-row unit) row
                                      (sengoku-siege-unit-column unit) column)
                                (when (sengoku--siege-occupy-keep-p
                                       siege unit)
                                  (throw 'turn-done nil))))))
                        (unless moved
                          (when (= (sengoku-siege-unit-side unit) 0)
                            (let ((structure-target
                                   (cl-find-if
                                    (lambda (target)
                                      (eq (plist-get target :kind)
                                          'structure))
                                    targets)))
                              (when structure-target
                                (sengoku-siege-structure-attack
                                 siege unit
                                 (plist-get structure-target :row)
                                 (plist-get structure-target :column)))))
                          (throw 'turn-done nil))))))))))))))

(defun sengoku-siege-player-turn-state (context)
  "Return CONTEXT's current nonblocking player turn plist, or nil.
The returned plist has :unit and :move-points.  It is a snapshot; callers must
use the player action functions to mutate combat state."
  (let ((siege (sengoku-battle-context-siege context)))
    (when siege
      (let ((state (sengoku--siege-turn-state siege)))
        (when (plist-get state :waiting-player)
          (list :unit
                (copy-sengoku-siege-unit
                 (plist-get state :active-unit))
                :move-points (plist-get state :move-points)))))))

(defun sengoku--siege-current-player-unit (context)
  "Return CONTEXT's active player unit, or signal an error."
  (let ((siege (sengoku-battle-context-siege context)))
    (unless siege
      (error "Context has no siege"))
    (let ((state (sengoku--siege-turn-state siege)))
      (unless (plist-get state :waiting-player)
        (error "No player unit is awaiting an action"))
      (plist-get state :active-unit))))

(defun sengoku--siege-finish-player-unit (siege)
  "End SIEGE's current player unit turn."
  (let ((state (sengoku--siege-turn-state siege)))
    (setf (plist-get state :waiting-player) nil
          (plist-get state :active-unit) nil
          (plist-get state :move-points) 0)))

(defun sengoku-siege-player-adjacent-targets (context)
  "Return ordered adjacent attack choices for CONTEXT's active player unit."
  (let* ((siege (sengoku-battle-context-siege context))
         (unit (sengoku--siege-current-player-unit context)))
    (sengoku-siege-adjacent-targets siege unit)))

(defun sengoku-siege-player-fire-targets (context)
  "Return ordered fire choices for CONTEXT's active player unit."
  (let* ((siege (sengoku-battle-context-siege context))
         (unit (sengoku--siege-current-player-unit context)))
    (sengoku-siege-fire-targets siege unit)))

(defun sengoku-siege-player-move (context direction)
  "Move CONTEXT's active player unit one step in DIRECTION.
DIRECTION is `left', `right', `up', or `down'.  Return `moved', `blocked',
`insufficient-move', or `completed'.  Forest costs two of the three points.
Only insufficient movement adds a message, matching Vim."
  (let* ((siege (sengoku-battle-context-siege context))
         (unit (sengoku--siege-current-player-unit context))
         (state (sengoku--siege-turn-state siege))
         (delta
          (pcase direction
            ('left '(0 -1))
            ('right '(0 1))
            ('up '(-1 0))
            ('down '(1 0))
            (_ (error "Invalid movement direction: %S" direction))))
         (row (+ (sengoku-siege-unit-row unit) (nth 0 delta)))
         (column (+ (sengoku-siege-unit-column unit) (nth 1 delta))))
    (if (not (sengoku-siege-passable-p siege unit row column))
        'blocked
      (let ((cost (sengoku-siege-move-cost siege row column)))
        (if (> cost (plist-get state :move-points))
            (progn
              (sengoku-siege-add-message siege "移動力が足りない")
              'insufficient-move)
          (setf (plist-get state :move-points)
                (- (plist-get state :move-points) cost)
                (sengoku-siege-unit-row unit) row
                (sengoku-siege-unit-column unit) column)
          (if (sengoku--siege-occupy-keep-p siege unit)
              (progn
                (sengoku--siege-finish-player-unit siege)
                'completed)
            'moved))))))

(defun sengoku--select-player-target (target targets descriptor-p)
  "Resolve TARGET from TARGETS.
When DESCRIPTOR-P is non-nil, TARGETS contains target plists; otherwise it
contains units.  TARGET may be a valid nonnegative index or an element.  Nil
selects a sole choice and returns `target-required' when several choices exist.
Out-of-range indices are rejected by returning nil."
  (cond
   ((null targets) nil)
   ((integerp target)
    (and (>= target 0)
         (< target (length targets))
         (nth target targets)))
   (target
    (if descriptor-p
        (cl-find target targets :test #'equal)
      (cl-find target targets :test #'eq)))
   ((null (cdr targets)) (car targets))
   (t 'target-required)))

(defun sengoku-siege-player-attack (context &optional target)
  "Attack an adjacent TARGET with CONTEXT's active player unit.
TARGET may be a zero-based index into
`sengoku-siege-player-adjacent-targets' or one of its descriptor plists.  It
may be omitted for a sole target.  With several choices, return
`target-required' without ending the turn.  Invalid indices signal an error.
On success return `turn-ended'."
  (let* ((siege (sengoku-battle-context-siege context))
         (unit (sengoku--siege-current-player-unit context))
         (targets (sengoku-siege-adjacent-targets siege unit)))
    (if (null targets)
        (progn
          (sengoku-siege-add-message
           siege "攻撃できる相手が隣接していない")
          'no-target)
      (let ((choice (sengoku--select-player-target target targets t)))
        (cond
         ((eq choice 'target-required) 'target-required)
         ((null choice) (error "Invalid adjacent target: %S" target))
         ((eq (plist-get choice :kind) 'unit)
          (sengoku-siege-melee siege unit (plist-get choice :unit))
          (sengoku--siege-finish-player-unit siege)
          'turn-ended)
         (t
          (sengoku-siege-structure-attack
           siege unit (plist-get choice :row) (plist-get choice :column))
          (sengoku--siege-finish-player-unit siege)
          'turn-ended))))))

(defun sengoku-siege-player-fire (context &optional target)
  "Fire at TARGET with CONTEXT's active player unit.
TARGET may be a zero-based index into `sengoku-siege-player-fire-targets' or a
unit from that list, and may be omitted for a sole target.  Return
`target-required', `no-weapons', `no-target', or `turn-ended'; invalid indices
signal an error."
  (let* ((siege (sengoku-battle-context-siege context))
         (unit (sengoku--siege-current-player-unit context)))
    (if (and (<= (sengoku-siege-unit-guns unit) 0)
             (<= (sengoku-siege-unit-cannon unit) 0))
        (progn
          (sengoku-siege-add-message siege "鉄砲も大筒も持っていない")
          'no-weapons)
      (let ((targets (sengoku-siege-fire-targets siege unit)))
        (if (null targets)
            (progn
              (sengoku-siege-add-message siege "射程(2)内に敵がいない")
              'no-target)
          (let ((choice
                 (sengoku--select-player-target target targets nil)))
            (cond
             ((eq choice 'target-required) 'target-required)
             ((null choice) (error "Invalid fire target: %S" target))
             (t
              (sengoku-siege-fire siege unit choice)
              (sengoku--siege-finish-player-unit siege)
              'turn-ended))))))))

(defun sengoku-siege-player-wait (context)
  "End CONTEXT's active player unit turn without acting."
  (let ((siege (sengoku-battle-context-siege context)))
    (sengoku--siege-current-player-unit context)
    (sengoku--siege-finish-player-unit siege)
    'turn-ended))

(defun sengoku-siege-player-retreat (context)
  "End an attacker-player siege in CONTEXT with result `retreat'.
The caller is responsible for obtaining confirmation before invoking this
non-UI primitive."
  (let ((siege (sengoku-battle-context-siege context)))
    (sengoku--siege-current-player-unit context)
    (unless (= (sengoku-siege-player-side siege) 0)
      (error "Only the player attacker may retreat"))
    (setf (sengoku-siege-result siege) 'retreat)
    (sengoku--siege-finish-player-unit siege)
    'completed))

(defun sengoku-siege-player-surrender (context)
  "End a defender-player siege in CONTEXT with result `surrender'.
The caller is responsible for obtaining confirmation before invoking this
non-UI primitive."
  (let ((siege (sengoku-battle-context-siege context)))
    (sengoku--siege-current-player-unit context)
    (unless (= (sengoku-siege-player-side siege) 1)
      (error "Only the player defender may surrender"))
    (setf (sengoku-siege-result siege) 'surrender)
    (sengoku--siege-finish-player-unit siege)
    'completed))

(defun sengoku--siege-end-side (siege state)
  "Apply end-of-side and end-of-day transitions to SIEGE and STATE."
  ;; Source checks attacker destruction before defender annihilation.
  (when (and (null (sengoku-siege-alive-units siege 0))
             (null (sengoku-siege-result siege)))
    (setf (sengoku-siege-result siege) 'destroyed))
  (when (and (null (sengoku-siege-alive-units siege 1))
             (null (sengoku-siege-result siege)))
    (setf (sengoku-siege-result siege) 'annihilated))
  (when (null (sengoku-siege-result siege))
    (if (= (plist-get state :side) 0)
        (setf (plist-get state :side) 1
              (plist-get state :unit-index) 0)
      (setf (sengoku-siege-day siege) (1+ (sengoku-siege-day siege)))
      (when (> (sengoku-siege-day siege) sengoku-data-siege-max-days)
        (setf (sengoku-siege-result siege) 'timeout))
      ;; AI attackers retreat after a full day below one quarter strength.
      (when (and (/= (sengoku-siege-player-side siege) 0)
                 (null (sengoku-siege-result siege)))
        (let ((attacker-soldiers 0))
          (dolist (unit (sengoku-siege-alive-units siege 0))
            (setq attacker-soldiers
                  (+ attacker-soldiers
                     (sengoku-siege-unit-soldiers unit))))
          (when (< attacker-soldiers
                   (sengoku--combat-divide
                    (sengoku-siege-initial-soldiers siege) 4))
            (setf (sengoku-siege-result siege) 'timeout))))
      (when (null (sengoku-siege-result siege))
        (setf (plist-get state :side) 0
              (plist-get state :unit-index) 0)))))

(defun sengoku-siege-advance-to-player (context)
  "Advance CONTEXT's siege until a player unit is ready or combat completes.
AI actions, side checks, day increments, the 30-day limit, and AI attacker
quarter-strength retreat follow Vim's loop.  Return `player-turn' or
`completed'.  Repeated calls while a player unit is waiting return
`player-turn' without advancing."
  (let ((siege (sengoku-battle-context-siege context)))
    (unless siege
      (error "Context has no siege"))
    (let ((state (sengoku--siege-turn-state siege)))
      (catch 'status
        (when (sengoku-siege-result siege)
          (throw 'status 'completed))
        (when (plist-get state :waiting-player)
          (throw 'status 'player-turn))
        (while (null (sengoku-siege-result siege))
          (let* ((units (sengoku-siege-units siege))
                 (index (plist-get state :unit-index))
                 (side (plist-get state :side)))
            (if (< index (length units))
                (let ((unit (aref units index)))
                  ;; Advance the cursor before a potentially suspended turn.
                  (setf (plist-get state :unit-index) (1+ index))
                  (when (and (= (sengoku-siege-unit-side unit) side)
                             (> (sengoku-siege-unit-soldiers unit) 0))
                    (if (= side (sengoku-siege-player-side siege))
                        (progn
                          (setf (plist-get state :waiting-player) t
                                (plist-get state :active-unit) unit
                                (plist-get state :move-points)
                                sengoku-data-siege-move-points)
                          (throw 'status 'player-turn))
                      (sengoku--siege-ai-unit siege unit))))
              (sengoku--siege-end-side siege state))))
        (setf (plist-get state :waiting-player) nil
              (plist-get state :active-unit) nil
              (plist-get state :move-points) 0)
        'completed))))

(defun sengoku-siege-run-all-ai (context)
  "Synchronously run CONTEXT's all-AI siege and return its result symbol.
The siege must have been initialized with PLAYER-SIDE -2.  This function only
completes tactical state; call `sengoku-battle-apply-siege-result' afterward to
mutate province ownership and resources."
  (let ((siege (sengoku-battle-context-siege context)))
    (unless siege
      (error "Context has no siege"))
    (unless (= (sengoku-siege-player-side siege) -2)
      (error "Siege is not configured for all-AI control"))
    (while (null (sengoku-siege-result siege))
      (sengoku-siege-advance-to-player context))
    (sengoku-siege-result siege)))

(defun sengoku--siege-force-totals (siege side)
  "Return SIDE's living (SOLDIERS GUNS CANNONS) totals in SIEGE."
  (let ((soldiers 0)
        (guns 0)
        (cannons 0))
    (dolist (unit (sengoku-siege-alive-units siege side))
      (setq soldiers (+ soldiers (sengoku-siege-unit-soldiers unit))
            guns (+ guns (sengoku-siege-unit-guns unit))
            cannons (+ cannons (sengoku-siege-unit-cannon unit))))
    (list soldiers guns cannons)))

(defun sengoku-battle-apply-siege-result (game context)
  "Apply CONTEXT's completed siege to GAME and return CONTEXT.
Survivors, guns, cannon, the minimum 100-soldier garrison, conquest resources,
and Japanese report lines match Vim.  Tactical result symbols remain on the
siege; CONTEXT RESULT becomes `victory' or `defeat'.  SHOWN remains nil because
this module performs no display."
  (when (sengoku-battle-finished-p context)
    (error "Battle has already been applied"))
  (sengoku--battle-validate-pending-state game context)
  (let ((siege (sengoku-battle-context-siege context)))
    (unless siege
      (error "Context has no siege"))
    (unless (sengoku-siege-result siege)
      (error "Siege is not complete"))
    (unless (memq (sengoku-siege-result siege)
                  '(fall annihilated surrender destroyed retreat timeout))
      (error "Unknown siege result: %S" (sengoku-siege-result siege)))
    (unless (and (= (sengoku-siege-attacker-clan siege)
                    (sengoku-battle-context-attacker-clan context))
                 (= (sengoku-siege-defender-clan siege)
                    (sengoku-battle-context-defender-clan context))
                 (= (sengoku-siege-from siege)
                    (sengoku-battle-context-from context))
                 (= (sengoku-siege-to siege)
                    (sengoku-battle-context-to context)))
      (error "Siege state does not match its battle context"))
    (let* ((source
            (sengoku--combat-province game
                                      (sengoku-battle-context-from context)))
           (target
            (sengoku--combat-province game
                                      (sengoku-battle-context-to context)))
           (attacker-clan (sengoku-battle-context-attacker-clan context))
           (attacker-name
            (sengoku-clan-name
             (sengoku--combat-clan game attacker-clan)))
           (attacker-totals (sengoku--siege-force-totals siege 0))
           (defender-totals (sengoku--siege-force-totals siege 1))
           (attacker-soldiers (nth 0 attacker-totals))
           (attacker-guns (nth 1 attacker-totals))
           (attacker-cannons (nth 2 attacker-totals))
           (defender-soldiers (nth 0 defender-totals))
           (defender-guns (nth 1 defender-totals))
           (defender-cannons (nth 2 defender-totals))
           (siege-result (sengoku-siege-result siege))
           (win (memq siege-result '(fall annihilated surrender))))
      (if win
          (progn
            (pcase siege-result
              ('fall
               (sengoku--battle-add-line
                context "★ %s軍、本丸を占拠! %s城は落城した!(%d日)"
                attacker-name (sengoku-province-name target)
                (sengoku-siege-day siege)))
              ('surrender
               (sengoku--battle-add-line
                context "★ 守将は城を明け渡した。%s城 開城。"
                (sengoku-province-name target)))
              (_
               (sengoku--battle-add-line
                context "★ 守兵全滅! %s城は落城した!(%d日)"
                (sengoku-province-name target)
                (sengoku-siege-day siege))))
            (setf (sengoku-battle-context-lines context)
                  (sengoku-conquer-apply
                   game attacker-clan
                   (sengoku-battle-context-to context)
                   (max attacker-soldiers 100)
                   (+ attacker-guns
                      (sengoku--combat-divide defender-guns 2))
                   (sengoku-battle-context-training context)
                   (sengoku-battle-context-lines context)))
            (setf (sengoku-province-cannon target)
                  (+ attacker-cannons defender-cannons)
                  (sengoku-battle-context-result context) 'victory))
        (pcase siege-result
          ('destroyed
           (sengoku--battle-add-line
            context "%s軍は全滅…… %s城は守り抜かれた!"
            attacker-name (sengoku-province-name target)))
          ('retreat
           (sengoku--battle-add-line
            context "%s軍は退却した。" attacker-name))
          (_
           (sengoku--battle-add-line
            context "%s軍は攻めきれず兵を退いた。(%d日)"
            attacker-name (sengoku-siege-day siege))))
        (setf (sengoku-province-soldiers source)
              (+ (sengoku-province-soldiers source) attacker-soldiers))
        (setf (sengoku-province-guns source)
              (+ (sengoku-province-guns source) attacker-guns))
        (setf (sengoku-province-cannon source)
              (+ (sengoku-province-cannon source) attacker-cannons))
        (setf (sengoku-province-soldiers target)
              (max defender-soldiers 100))
        (setf (sengoku-province-guns target) defender-guns)
        (setf (sengoku-province-cannon target) defender-cannons)
        (setf (sengoku-battle-context-result context) 'defeat))
      (remhash siege sengoku--siege-turn-states)
      context)))

(defun sengoku-battle-dispatch (game context resolution &optional player-side)
  "Dispatch prepared CONTEXT in GAME according to RESOLUTION.
An undefended target is conquered immediately regardless of RESOLUTION.
Otherwise RESOLUTION must be `automatic' or `siege'.  For `siege', PLAYER-SIDE
must be explicitly supplied as -2, 0, or 1.  The function returns CONTEXT; a
siege remains tactically pending until advanced and applied."
  (when (or (sengoku-battle-finished-p context)
            (sengoku-battle-context-siege context))
    (error "Battle context has already been dispatched"))
  (sengoku--battle-validate-pending-state game context)
  (let ((target
         (sengoku--combat-province game
                                   (sengoku-battle-context-to context))))
    (if (<= (sengoku-province-soldiers target) 0)
        (sengoku-battle-resolve-undefended game context)
      (pcase resolution
        ('automatic (sengoku-battle-resolve-automatic game context))
        ('siege
         (unless (memq player-side '(-2 0 1))
           (error "Siege dispatch requires player side -2, 0, or 1"))
         (sengoku-battle-start-siege game context player-side))
        (_ (error "Unknown battle resolution: %S" resolution))))))

(cl-defun sengoku-battle-start
    (game attacker-clan from-index to-index soldiers resolution
          &key general player-side)
  "Prepare and dispatch a battle in GAME, returning its context.
ATTACKER-CLAN commits SOLDIERS from FROM-INDEX against TO-INDEX.  RESOLUTION is
`automatic' or `siege'.  GENERAL defaults to the clan roster's first entry.
PLAYER-SIDE is mandatory for a defended siege and is ignored for an automatic
or undefended battle.  Selection is validated before resources are committed."
  (unless (memq resolution '(automatic siege))
    (error "Unknown battle resolution: %S" resolution))
  (let ((target (sengoku--combat-province game to-index)))
    (when (and (eq resolution 'siege)
               (> (sengoku-province-soldiers target) 0))
      (unless (memq player-side '(-2 0 1))
        (error "Defended siege requires player side -2, 0, or 1"))
      (let* ((player (sengoku-game-player game))
             (expected-side
              (cond
               ((< player 0) -2)
               ((= player attacker-clan) 0)
               ((= player (sengoku-province-owner target)) 1)
               (t -2))))
        (unless (= player-side expected-side)
          (error
           "Siege player side %S conflicts with campaign participants; expected %S"
           player-side expected-side)))))
  (let ((context
         (sengoku-battle-prepare
          game attacker-clan from-index to-index soldiers general)))
    (sengoku-battle-dispatch
     game context resolution player-side)))

(provide 'sengoku-combat)

;;; sengoku-combat.el ends here
