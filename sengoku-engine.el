;;; sengoku-engine.el --- Strategic engine for Sengoku -*- lexical-binding: t; -*-

;; Copyright (C) 2026 dkc

;; Author: dkc
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: games

;;; Commentary:

;; Strategic commands, AI turns, monthly events, and the nonblocking campaign
;; controller translated from plugin/sengoku.vim.  This module performs no
;; minibuffer or buffer I/O.  UI code supplies already-selected indices and
;; quantities, then calls `sengoku-engine-advance' until it returns a player
;; command, decision, battle choice, tactical unit, or terminal result.
;;
;; Main controller API:
;;
;; - `sengoku-engine-select-player' starts a new campaign for one clan.
;; - `sengoku-engine-begin-turn' snapshots this month's player provinces.
;; - `sengoku-engine-command' applies one strategic player command.
;; - `sengoku-engine-advance' runs automatic work to the next input boundary.
;; - `sengoku-engine-resolve-battle-choice' chooses automatic or tactical
;;   combat for a defended battle involving the player.
;; - `sengoku-engine-resolve-decision' answers merchant and alliance offers.
;; - `sengoku-engine-continue-all-delegated' ends the all-delegated pause.
;;
;; Expected rule failures are returned as plists with `:status' equal to
;; `error'; programmer errors and structurally invalid state still signal.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'sengoku-core)
(require 'sengoku-combat)

(defconst sengoku-engine--nanban-rares
  ["ギヤマンの壺" "地球儀" "南蛮時計" "ビロードのマント" "金平糖"]
  "Names used by the southern-barbarian rare-goods command.")

(defun sengoku-engine--result (status message &rest properties)
  "Return a result plist with STATUS, MESSAGE, and PROPERTIES."
  (append (list :status status :message message) properties))

(defun sengoku-engine--error (message)
  "Return a non-consuming command error for MESSAGE."
  (sengoku-engine--result 'error message :consumed nil))

(defun sengoku-engine--ok (message &rest properties)
  "Return a successful result with MESSAGE and PROPERTIES."
  (apply #'sengoku-engine--result 'ok message properties))

(defun sengoku-engine--state (session)
  "Return SESSION's controller property list."
  (or (sengoku-session-ui-state session) nil))

(defun sengoku-engine--set-state (session key value)
  "Set KEY to VALUE in SESSION's controller property list and return VALUE."
  (let ((state (sengoku-engine--state session)))
    (setf (plist-get state key) value
          (sengoku-session-ui-state session) state)
    value))

(defun sengoku-engine--province (game province-index)
  "Return PROVINCE-INDEX in GAME, signaling when it is invalid."
  (let ((provinces (sengoku-game-provinces game)))
    (unless (and (integerp province-index)
                 (>= province-index 0)
                 (< province-index (length provinces)))
      (error "Invalid province index: %S" province-index))
    (aref provinces province-index)))

(defun sengoku-engine--clan (game clan-index)
  "Return CLAN-INDEX in GAME, signaling when it is invalid."
  (let ((clans (sengoku-game-clans game)))
    (unless (and (integerp clan-index)
                 (>= clan-index 0)
                 (< clan-index (length clans)))
      (error "Invalid clan index: %S" clan-index))
    (aref clans clan-index)))

(defun sengoku-engine--general (game clan-index general-index)
  "Return GENERAL-INDEX of CLAN-INDEX in GAME, or nil when unavailable."
  (let ((roster (and (integerp clan-index)
                     (>= clan-index 0)
                     (< clan-index (length (sengoku-game-generals game)))
                     (aref (sengoku-game-generals game) clan-index))))
    (when (and roster
               (integerp general-index)
               (>= general-index 0)
               (< general-index (length roster))
               (not (sengoku-general-used-p game clan-index general-index)))
      (aref roster general-index))))

(defun sengoku-engine--divide (numerator denominator)
  "Return Vim-compatible integer division of NUMERATOR by DENOMINATOR."
  (sengoku-vim-divide numerator denominator))

(defun sengoku-engine--player-context (session)
  "Return (GAME PLAYER PROVINCE-INDEX PROVINCE) for SESSION's command."
  (unless (eq (sengoku-session-phase session) 'player-command)
    (error "Session is not waiting for a province command"))
  (let* ((game (sengoku-session-game session))
         (player (sengoku-game-player game))
         (province-index (sengoku-session-active-province session))
         (province (sengoku-engine--province game province-index)))
    (unless (and (>= player 0)
                 (= (sengoku-province-owner province) player))
      (error "Active province is no longer owned by the player"))
    (list game player province-index province)))

(defun sengoku-engine-attack-targets (game clan-index province-index)
  "Return attackable neighbors of PROVINCE-INDEX for CLAN-INDEX in GAME."
  (let* ((province (sengoku-engine--province game province-index))
         (provinces (sengoku-game-provinces game))
         result)
    (dotimes (position (length (sengoku-province-adjacency province)))
      (let* ((target-index
              (aref (sengoku-province-adjacency province) position))
             (owner (sengoku-province-owner
                     (aref provinces target-index))))
        (when (and (/= owner clan-index)
                   (not (sengoku-allied-p game clan-index owner)))
          (push target-index result))))
    (nreverse result)))

(defun sengoku-engine-transport-targets (game clan-index province-index)
  "Return friendly neighbors of PROVINCE-INDEX for CLAN-INDEX in GAME."
  (let* ((province (sengoku-engine--province game province-index))
         (provinces (sengoku-game-provinces game))
         result)
    (dotimes (position (length (sengoku-province-adjacency province)))
      (let ((target-index
             (aref (sengoku-province-adjacency province) position)))
        (when (= (sengoku-province-owner (aref provinces target-index))
                 clan-index)
          (push target-index result))))
    (nreverse result)))

(defun sengoku-engine-alliance-candidates (game clan-index)
  "Return living non-allied clans available to CLAN-INDEX in GAME."
  (let (result)
    (dotimes (other (length (sengoku-game-clans game)))
      (when (and (/= other clan-index)
                 (sengoku-clan-alive-p game other)
                 (not (sengoku-allied-p game clan-index other)))
        (push other result)))
    (nreverse result)))

(defun sengoku-engine-release-candidates (game clan-index)
  "Return delegated provinces belonging to CLAN-INDEX in GAME."
  (seq-filter
   (lambda (province-index)
     (not (zerop
           (sengoku-province-auto
            (aref (sengoku-game-provinces game) province-index)))))
   (sengoku-clan-provinces game clan-index)))

(defun sengoku-engine-recruit-maximum (province)
  "Return the maximum soldiers that may be recruited in PROVINCE."
  (min (- (* (sengoku-province-koku province) 20)
          (sengoku-province-soldiers province))
       (min (* (sengoku-province-gold province) 5)
            (* (sengoku-province-rice province) 5))))

(defun sengoku-engine-nanban-prices (game clan-index)
  "Return southern-barbarian prices and gun quantity for CLAN-INDEX in GAME."
  (let* ((culture (sengoku-culture game clan-index))
         (discount (- 100 (sengoku-engine--divide culture 2))))
    (list :culture culture
          :discount (- 100 discount)
          :guns (+ 40 (sengoku-engine--divide culture 5))
          :gun-price (sengoku-engine--divide (* 500 discount) 100)
          :cannon-price (sengoku-engine--divide (* 800 discount) 100)
          :rare-price (sengoku-engine--divide (* 600 discount) 100))))

(defun sengoku-engine--require-general (game clan-index general-index)
  "Return GENERAL-INDEX for CLAN-INDEX in GAME, or a command error result."
  (or (sengoku-engine--general game clan-index general-index)
      (sengoku-engine--error "その武将は今月すでに行動済みです。")))

(defun sengoku-engine--command-develop
    (game player province general-index)
  "Apply development in GAME PROVINCE for PLAYER using GENERAL-INDEX."
  (if (< (sengoku-province-gold province) 200)
      (sengoku-engine--error "金が足りません(200必要)。")
    (let ((general-or-error
           (sengoku-engine--require-general game player general-index)))
      (if (not (sengoku-general-p general-or-error))
          general-or-error
        (let* ((general
                (sengoku-use-general game player general-index))
               (increase
                (+ 12
                   (sengoku-engine--divide
                    (sengoku-general-politics general) 8)
                   (sengoku-random 6))))
          (setf (sengoku-province-gold province)
                (- (sengoku-province-gold province) 200)
                (sengoku-province-koku province)
                (+ (sengoku-province-koku province) increase)
                (sengoku-province-loyalty province)
                (min 100 (+ (sengoku-province-loyalty province) 2)))
          (sengoku-log
           game (format "%s %sが開発 石高+%d (%d)"
                        (sengoku-province-name province)
                        (sengoku-general-name general)
                        increase (sengoku-province-koku province)))
          (sengoku-engine--ok "開発を実行しました。" :consumed t
                              :increase increase))))))

(defun sengoku-engine--command-commerce
    (game player province general-index)
  "Apply commercial investment in GAME PROVINCE for PLAYER using GENERAL-INDEX."
  (if (< (sengoku-province-gold province) 200)
      (sengoku-engine--error "金が足りません(200必要)。")
    (let ((general-or-error
           (sengoku-engine--require-general game player general-index)))
      (if (not (sengoku-general-p general-or-error))
          general-or-error
        (let* ((general
                (sengoku-use-general game player general-index))
               (increase
                (+ 6
                   (sengoku-engine--divide
                    (sengoku-general-politics general) 12)
                   (sengoku-random 4))))
          (setf (sengoku-province-gold province)
                (- (sengoku-province-gold province) 200)
                (sengoku-province-commerce province)
                (+ (sengoku-province-commerce province) increase))
          (sengoku-log
           game (format "%s %sが商業投資 商業+%d (%d)"
                        (sengoku-province-name province)
                        (sengoku-general-name general)
                        increase (sengoku-province-commerce province)))
          (sengoku-engine--ok "商業投資を実行しました。" :consumed t
                              :increase increase))))))

(defun sengoku-engine--command-relief
    (game player province general-index)
  "Apply relief in GAME PROVINCE for PLAYER using GENERAL-INDEX."
  (if (or (< (sengoku-province-gold province) 100)
          (< (sengoku-province-rice province) 100))
      (sengoku-engine--error "金100と米100が必要です。")
    (let ((general-or-error
           (sengoku-engine--require-general game player general-index)))
      (if (not (sengoku-general-p general-or-error))
          general-or-error
        (let* ((general
                (sengoku-use-general game player general-index))
               (increase
                (+ 8 (sengoku-engine--divide
                      (sengoku-general-politics general) 15))))
          (setf (sengoku-province-gold province)
                (- (sengoku-province-gold province) 100)
                (sengoku-province-rice province)
                (- (sengoku-province-rice province) 100)
                (sengoku-province-loyalty province)
                (min 100 (+ (sengoku-province-loyalty province) increase)))
          (sengoku-log
           game (format "%s %sが施し 民忠+%d (%d)"
                        (sengoku-province-name province)
                        (sengoku-general-name general)
                        increase (sengoku-province-loyalty province)))
          (sengoku-engine--ok "施しを実行しました。" :consumed t
                              :increase increase))))))

(defun sengoku-engine--command-recruit
    (game player province general-index quantity)
  "Recruit QUANTITY soldiers in GAME PROVINCE using GENERAL-INDEX for PLAYER."
  (let ((maximum (sengoku-engine-recruit-maximum province)))
    (cond
     ((< maximum 100)
      (sengoku-engine--error
       "これ以上徴兵できません(石高上限か金米不足)。"))
     ((not (and (integerp quantity) (> quantity 0) (<= quantity maximum)))
      (sengoku-engine--error "徴兵数が範囲外です。"))
     (t
      (let ((general-or-error
             (sengoku-engine--require-general game player general-index)))
        (if (not (sengoku-general-p general-or-error))
            general-or-error
          (let* ((general
                  (sengoku-use-general game player general-index))
                 (new-soldiers
                  (+ (sengoku-province-soldiers province) quantity)))
            (setf (sengoku-province-gold province)
                  (- (sengoku-province-gold province)
                     (sengoku-engine--divide quantity 5))
                  (sengoku-province-rice province)
                  (- (sengoku-province-rice province)
                     (sengoku-engine--divide quantity 5))
                  (sengoku-province-soldiers province) new-soldiers
                  (sengoku-province-loyalty province)
                  (max 0
                       (- (sengoku-province-loyalty province)
                          (sengoku-engine--divide quantity 500) 2))
                  (sengoku-province-training province)
                  (sengoku-engine--divide
                   (* (sengoku-province-training province) new-soldiers)
                   (+ new-soldiers
                      (sengoku-engine--divide quantity 2))))
            (sengoku-log
             game (format "%s %sが徴兵+%d (兵%d)"
                          (sengoku-province-name province)
                          (sengoku-general-name general)
                          quantity (sengoku-province-soldiers province)))
            (sengoku-engine--ok "徴兵を実行しました。" :consumed t
                                :quantity quantity))))))))

(defun sengoku-engine--command-train
    (game player province general-index)
  "Train troops in GAME PROVINCE for PLAYER using GENERAL-INDEX."
  (cond
   ((< (sengoku-province-gold province) 100)
    (sengoku-engine--error "金が足りません(100必要)。"))
   ((>= (sengoku-province-training province) 100)
    (sengoku-engine--error "訓練度は既に最大です。"))
   (t
    (let ((general-or-error
           (sengoku-engine--require-general game player general-index)))
      (if (not (sengoku-general-p general-or-error))
          general-or-error
        (let* ((general
                (sengoku-use-general game player general-index))
               (increase
                (+ 5 (sengoku-engine--divide
                      (sengoku-general-war general) 15))))
          (setf (sengoku-province-gold province)
                (- (sengoku-province-gold province) 100)
                (sengoku-province-training province)
                (min 100 (+ (sengoku-province-training province) increase)))
          (sengoku-log
           game (format "%s %sが訓練+%d (%d)"
                        (sengoku-province-name province)
                        (sengoku-general-name general)
                        increase (sengoku-province-training province)))
          (sengoku-engine--ok "訓練を実行しました。" :consumed t
                              :increase increase)))))))

(defun sengoku-engine--command-buy-guns (game player province)
  "Buy ordinary firearms in GAME PROVINCE for PLAYER."
  (if (< (sengoku-province-gold province) 300)
      (sengoku-engine--error "金が足りません(300必要)。")
    (let ((quantity
           (+ 20 (sengoku-engine--divide
                  (sengoku-culture game player) 10))))
      (setf (sengoku-province-gold province)
            (- (sengoku-province-gold province) 300)
            (sengoku-province-guns province)
            (+ (sengoku-province-guns province) quantity))
      (sengoku-log
       game (format "%s 南蛮商人から鉄砲%d購入 (計%d)"
                    (sengoku-province-name province) quantity
                    (sengoku-province-guns province)))
      (sengoku-engine--ok "鉄砲を購入しました。" :consumed t
                          :quantity quantity))))

(defun sengoku-engine--command-transport
    (game player province-index province target-index kind quantity)
  "Transport QUANTITY of KIND in GAME from PROVINCE at PROVINCE-INDEX.
TARGET-INDEX receives the resource for PLAYER."
  (let ((targets (sengoku-engine-transport-targets
                  game player province-index)))
    (cond
     ((null targets)
      (sengoku-engine--error "隣接する自領がありません。"))
     ((not (memq target-index targets))
      (sengoku-engine--error "輸送先は隣接する自領ではありません。"))
     ((not (memq kind '(gold rice soldiers)))
      (sengoku-engine--error "輸送する資源の種類が不正です。"))
     (t
      (let* ((target (sengoku-engine--province game target-index))
             (available
              (pcase kind
                ('gold (sengoku-province-gold province))
                ('rice (sengoku-province-rice province))
                (_ (sengoku-province-soldiers province)))))
        (if (not (and (integerp quantity) (> quantity 0)
                      (<= quantity available)))
            (sengoku-engine--error "輸送量が範囲外です。")
          (pcase kind
            ('gold
             (setf (sengoku-province-gold province)
                   (- (sengoku-province-gold province) quantity)
                   (sengoku-province-gold target)
                   (+ (sengoku-province-gold target) quantity)))
            ('rice
             (setf (sengoku-province-rice province)
                   (- (sengoku-province-rice province) quantity)
                   (sengoku-province-rice target)
                   (+ (sengoku-province-rice target) quantity)))
            ('soldiers
             (setf (sengoku-province-soldiers province)
                   (- (sengoku-province-soldiers province) quantity)
                   (sengoku-province-soldiers target)
                   (+ (sengoku-province-soldiers target) quantity))
             (let ((guns
                    (if (> (+ (sengoku-province-soldiers province) quantity) 0)
                        (sengoku-engine--divide
                         (* (sengoku-province-guns province) quantity)
                         (+ (sengoku-province-soldiers province) quantity))
                      0)))
               (setf (sengoku-province-guns province)
                     (- (sengoku-province-guns province) guns)
                     (sengoku-province-guns target)
                     (+ (sengoku-province-guns target) guns)))))
          (sengoku-log
           game (format "%s→%s %sを%d輸送"
                        (sengoku-province-name province)
                        (sengoku-province-name target)
                        (pcase kind
                          ('gold "金") ('rice "米") (_ "兵"))
                        quantity))
          (sengoku-engine--ok "輸送しました。" :consumed t
                              :quantity quantity)))))))

(defun sengoku-engine--command-propose-alliance
    (game player province target-clan general-index)
  "Propose a GAME alliance from PLAYER to TARGET-CLAN using GENERAL-INDEX.
The gift is paid from PROVINCE."
  (cond
   ((< (sengoku-province-gold province) 500)
    (sengoku-engine--error "贈物の金500がこの国にありません。"))
   ((not (memq target-clan
               (sengoku-engine-alliance-candidates game player)))
    (sengoku-engine--error "同盟を結べる相手ではありません。"))
   (t
    (let ((general-or-error
           (sengoku-engine--require-general game player general-index)))
      (if (not (sengoku-general-p general-or-error))
          general-or-error
        (let* ((general
                (sengoku-use-general game player general-index))
               (state (aref (sengoku-game-clan-states game) player))
               (chance
                (min 90
                     (+ 25
                        (sengoku-engine--divide
                         (sengoku-general-politics general) 3)
                        (sengoku-engine--divide
                         (sengoku-culture game player) 5)
                        (* (length (sengoku-clan-state-items state)) 4)
                        (* (sengoku-clan-state-rank state) 3))))
               (success (< (sengoku-random 100) chance))
               (target-name
                (sengoku-clan-name
                 (sengoku-engine--clan game target-clan))))
          (setf (sengoku-province-gold province)
                (- (sengoku-province-gold province) 500))
          (if success
              (progn
                (sengoku-set-alliance game player target-clan t)
                (sengoku-log
                 game (format "使者%s、%s家との同盟をまとめた!"
                              (sengoku-general-name general) target-name)))
            (sengoku-log
             game (format "使者%s、%s家との交渉は決裂……"
                          (sengoku-general-name general) target-name)))
          (sengoku-engine--ok
           (if success
               (format "%s家は同盟を快諾した!" target-name)
             (format "%s家は申し出を断った……" target-name))
           :consumed t :success success :chance chance)))))))

(defun sengoku-engine--command-break-alliance (game player target-clan)
  "Break PLAYER's alliance with TARGET-CLAN in GAME."
  (if (not (memq target-clan (sengoku-alliance-list game player)))
      (sengoku-engine--error "同盟中の家ではありません。")
    (let ((state (aref (sengoku-game-clan-states game) player))
          (target-name
           (sengoku-clan-name (sengoku-engine--clan game target-clan))))
      (sengoku-set-alliance game player target-clan nil)
      (setf (sengoku-clan-state-culture state)
            (max 0 (- (sengoku-clan-state-culture state) 3)))
      (sengoku-log
       game (format "%s家との同盟を破棄 (信義を失い文化-3)" target-name))
      (sengoku-engine--ok "同盟を破棄しました。" :consumed t))))

(defun sengoku-engine--command-court-rank
    (game player province general-index)
  "Petition in GAME for PLAYER's next rank from PROVINCE using GENERAL-INDEX."
  (let* ((state (aref (sengoku-game-clan-states game) player))
         (rank (sengoku-clan-state-rank state)))
    (cond
     ((>= rank (1- (length sengoku-data-ranks)))
      (sengoku-engine--error
       (format "これ以上の官位はありません(すでに%s)。"
               (aref sengoku-data-ranks rank))))
     ((< (sengoku-province-gold province) 1000)
      (sengoku-engine--error "献金の金1000がこの国にありません。"))
     (t
      (let ((general-or-error
             (sengoku-engine--require-general game player general-index)))
        (if (not (sengoku-general-p general-or-error))
            general-or-error
          (let* ((general
                  (sengoku-use-general game player general-index))
                 (kyoto (sengoku-province-index game "山城"))
                 (chance
                  (max 5
                       (min 95
                            (+ 40
                               (sengoku-engine--divide
                                (sengoku-general-politics general) 3)
                               (sengoku-engine--divide
                                (sengoku-culture game player) 5)
                               (- (* rank 8))
                               (if (= (sengoku-province-owner
                                       (sengoku-engine--province game kyoto))
                                      player)
                                   30
                                 0)))))
                 (success (< (sengoku-random 100) chance)))
            (setf (sengoku-province-gold province)
                  (- (sengoku-province-gold province) 1000))
            (if success
                (progn
                  (setf (sengoku-clan-state-rank state) (1+ rank))
                  (dolist (province-index
                           (sengoku-clan-provinces game player))
                    (let ((owned (sengoku-engine--province game province-index)))
                      (setf (sengoku-province-loyalty owned)
                            (min 100 (+ (sengoku-province-loyalty owned) 8)))))
                  (sengoku-log
                   game
                   (format "使者%s、朝廷より「%s」の官位を賜る! 領民歓喜(民忠+8)"
                           (sengoku-general-name general)
                           (aref sengoku-data-ranks (1+ rank)))))
              (sengoku-log
               game (format "使者%sの献金むなしく、官位は沙汰やみに……"
                            (sengoku-general-name general))))
            (sengoku-engine--ok
             (if success "官位を賜りました。" "官位の沙汰はありませんでした。")
             :consumed t :success success :chance chance))))))))

(defun sengoku-engine--command-court-peace
    (game player province target-clan general-index)
  "Use an imperial order in GAME to ally PLAYER with TARGET-CLAN.
Pay from PROVINCE and send GENERAL-INDEX as the envoy."
  (let* ((state (aref (sengoku-game-clan-states game) player))
         (rank (sengoku-clan-state-rank state)))
    (cond
     ((< rank 1)
      (sengoku-engine--error
       "勅命を奏請するには官位が必要です。まず献金を。"))
     ((< (sengoku-province-gold province) 2000)
      (sengoku-engine--error "奏請の金2000がこの国にありません。"))
     ((not (memq target-clan
                 (sengoku-engine-alliance-candidates game player)))
      (sengoku-engine--error "和睦を結ぶ相手ではありません。"))
     (t
      (let ((general-or-error
             (sengoku-engine--require-general game player general-index)))
        (if (not (sengoku-general-p general-or-error))
            general-or-error
          (let* ((general
                  (sengoku-use-general game player general-index))
                 (target-name
                  (sengoku-clan-name
                   (sengoku-engine--clan game target-clan))))
            (setf (sengoku-province-gold province)
                  (- (sengoku-province-gold province) 2000))
            (sengoku-set-alliance game player target-clan t)
            (sengoku-log
             game (format "帝の勅命により%s家と和睦(同盟)が成った! (使者:%s)"
                          target-name (sengoku-general-name general)))
            (sengoku-engine--ok
             (format "%s家との同盟が成立しました。" target-name)
             :consumed t))))))))

(defun sengoku-engine--command-nanban (game player province kind)
  "Perform southern-barbarian trade of KIND in GAME PROVINCE for PLAYER."
  (if (zerop (sengoku-province-port province))
      (sengoku-engine--error
       "南蛮船の入る港がありません(堺・博多・府内・平戸・坊津)。")
    (let* ((prices (sengoku-engine-nanban-prices game player))
           (price
            (pcase kind
              ('guns (plist-get prices :gun-price))
              ('cannon (plist-get prices :cannon-price))
              ('rare (plist-get prices :rare-price))
              (_ nil))))
      (cond
       ((null price)
        (sengoku-engine--error "南蛮交易の品目が不正です。"))
       ((< (sengoku-province-gold province) price)
        (sengoku-engine--error "金が足りません。"))
       (t
        (setf (sengoku-province-gold province)
              (- (sengoku-province-gold province) price))
        (pcase kind
          ('guns
           (let ((quantity (plist-get prices :guns)))
             (setf (sengoku-province-guns province)
                   (+ (sengoku-province-guns province) quantity))
             (sengoku-log
              game (format "%s 南蛮船より鉄砲%d挺を購入 (計%d)"
                           (sengoku-province-name province) quantity
                           (sengoku-province-guns province)))
             (sengoku-engine--ok "鉄砲を購入しました。" :consumed t
                                 :quantity quantity)))
          ('cannon
           (setf (sengoku-province-cannon province)
                 (1+ (sengoku-province-cannon province)))
           (sengoku-log
            game (format "%s 南蛮船より大筒1門を購入 (計%d門)"
                         (sengoku-province-name province)
                         (sengoku-province-cannon province)))
           (sengoku-engine--ok "大筒を購入しました。" :consumed t))
          ('rare
           (let* ((state (aref (sengoku-game-clan-states game) player))
                  (rare
                   (aref sengoku-engine--nanban-rares (sengoku-random 5))))
             (setf (sengoku-clan-state-culture state)
                   (min 100 (+ (sengoku-clan-state-culture state) 5)))
             (sengoku-log
              game (format "%s 南蛮の珍品「%s」を入手 文化+5 (%d)"
                           (sengoku-province-name province) rare
                           (sengoku-culture game player)))
             (sengoku-engine--ok "南蛮の珍品を入手しました。" :consumed t
                                 :rare rare)))))))))

(defun sengoku-engine--command-tea (game player province)
  "Hold a tea gathering in GAME PROVINCE for PLAYER."
  (let ((state (aref (sengoku-game-clan-states game) player)))
    (cond
     ((null (sengoku-clan-state-items state))
      (sengoku-engine--error
       "茶会には名物茶器が必要です。堺の商人を待ちましょう。"))
     ((< (sengoku-province-gold province) 300)
      (sengoku-engine--error "茶会には金300が必要です。"))
     (t
      (let ((increase
             (+ 2 (sengoku-engine--divide
                   (sengoku-clan-charisma
                    (sengoku-engine--clan game player))
                   30))))
        (setf (sengoku-province-gold province)
              (- (sengoku-province-gold province) 300))
        (dolist (province-index (sengoku-clan-provinces game player))
          (let ((owned (sengoku-engine--province game province-index)))
            (setf (sengoku-province-loyalty owned)
                  (min 100 (+ (sengoku-province-loyalty owned) increase)))))
        (setf (sengoku-clan-state-culture state)
              (min 100 (+ (sengoku-clan-state-culture state) 2)))
        (sengoku-log
         game (format "盛大な茶会を開催 全領土の民忠+%d 文化+2 (%d)"
                      increase (sengoku-culture game player)))
        (sengoku-engine--ok "茶会を開催しました。" :consumed t
                            :increase increase))))))

(defun sengoku-engine--log-battle (game context mode)
  "Append CONTEXT's report to GAME according to MODE and return CONTEXT."
  (dolist (line (sengoku-battle-context-lines context))
    (when (or (eq mode 'all)
              (not (string-match-p "合目" line)))
      (sengoku-log game line)))
  context)

(defun sengoku-engine--set-pending-battle
    (session context origin log-mode resume-phase)
  "Suspend SESSION for CONTEXT with ORIGIN, LOG-MODE, and RESUME-PHASE."
  (let ((game (sengoku-session-game session)))
    (setf (sengoku-session-pending-battle session) context
          (sengoku-game-pending-battle game) context
          (sengoku-session-phase session) 'battle-choice)
    (sengoku-engine--set-state session :battle-origin origin)
    (sengoku-engine--set-state session :battle-log-mode log-mode)
    (sengoku-engine--set-state session :battle-resume-phase resume-phase)
    (sengoku-engine--result
     'battle-choice "戦闘の解決方法を選んでください。"
     :battle context :choices '(automatic siege))))

(defun sengoku-engine--clear-pending-battle (session)
  "Clear SESSION's campaign battle references."
  (let ((game (sengoku-session-game session)))
    (setf (sengoku-session-pending-battle session) nil
          (sengoku-game-pending-battle game) nil)
    nil))

(defun sengoku-engine--command-attack
    (session game player province-index province target-index general-index
             quantity)
  "Start PLAYER's GAME attack from PROVINCE against TARGET-INDEX in SESSION."
  (let ((targets (sengoku-engine-attack-targets
                  game player province-index)))
    (cond
     ((null targets)
      (sengoku-engine--error
       "隣接する敵領がありません(同盟国へは出陣できません)。"))
     ((not (memq target-index targets))
      (sengoku-engine--error "出陣先は攻撃可能な隣接国ではありません。"))
     ((not (and (integerp quantity) (> quantity 0)
                (<= quantity (sengoku-province-soldiers province))))
      (sengoku-engine--error "出陣する兵数が範囲外です。"))
     (t
      (let ((general-or-error
             (sengoku-engine--require-general game player general-index)))
        (if (not (sengoku-general-p general-or-error))
            general-or-error
          (let* ((general
                  (sengoku-use-general game player general-index))
                 (target (sengoku-engine--province game target-index))
                 (context
                  (sengoku-battle-prepare
                   game player province-index target-index quantity general)))
            (if (<= (sengoku-province-soldiers target) 0)
                (progn
                  (sengoku-battle-dispatch game context 'automatic)
                  (sengoku-engine--log-battle game context 'all)
                  (sengoku-engine--ok "出陣を解決しました。" :consumed t
                                      :battle context))
              (sengoku-engine--set-pending-battle
               session context 'player 'all 'player-direct)))))))))

(defun sengoku-engine--ai-province-action
    (session clan-index province-index allow-defense-choice)
  "Let CLAN-INDEX manage PROVINCE-INDEX in SESSION.
When ALLOW-DEFENSE-CHOICE is non-nil, a direct player defender may choose a
manual siege.  Return an action result; a pending battle changes SESSION phase."
  (let* ((game (sengoku-session-game session))
         (province (sengoku-engine--province game province-index))
         (provinces (sengoku-game-provinces game))
         (unused (sengoku-unused-generals game clan-index)))
    (cond
     ((or (/= (sengoku-province-owner province) clan-index)
          (null unused))
      (sengoku-engine--ok "AIは行動しませんでした。" :acted nil))
     (t
      (let ((best-target -1)
            (best-soldiers 999999))
        (dotimes (position (length (sengoku-province-adjacency province)))
          (let* ((target-index
                  (aref (sengoku-province-adjacency province) position))
                 (target (aref provinces target-index))
                 (owner (sengoku-province-owner target)))
            (when (and (/= owner clan-index)
                       (not (sengoku-allied-p game clan-index owner))
                       (< (+ (sengoku-engine--divide
                              (* (sengoku-province-soldiers target) 3) 2)
                             500)
                          (sengoku-province-soldiers province))
                       (< (sengoku-province-soldiers target) best-soldiers))
              (setq best-target target-index
                    best-soldiers (sengoku-province-soldiers target)))))
        (if (and (>= best-target 0)
                 (> (sengoku-province-soldiers province) 2500))
            (let* ((general-index
                    (sengoku-best-general game clan-index 'war))
                   (general
                    (sengoku-use-general game clan-index general-index))
                   (target (aref provinces best-target))
                   (context
                    (sengoku-battle-prepare
                     game clan-index province-index best-target
                     (- (sengoku-province-soldiers province) 1000)
                     general))
                   (player (sengoku-game-player game)))
              (if (and allow-defense-choice
                       (>= player 0)
                       (= (sengoku-province-owner target) player)
                       (zerop (sengoku-province-auto target))
                       (> (sengoku-province-soldiers target) 0))
                  (sengoku-engine--set-pending-battle
                   session context 'ai 'summary 'enemy-turns)
                (sengoku-battle-dispatch game context 'automatic)
                (sengoku-engine--log-battle game context 'summary)
                (sengoku-engine--ok "AIの出陣を解決しました。"
                                    :acted t :battle context)))
          (if (and (not (zerop (sengoku-province-port province)))
                   (>= (sengoku-province-gold province) 1000)
                   (< (sengoku-random 100) 25))
              (progn
                (if (< (sengoku-random 100) 40)
                    (setf (sengoku-province-gold province)
                          (- (sengoku-province-gold province) 800)
                          (sengoku-province-cannon province)
                          (1+ (sengoku-province-cannon province)))
                  (setf (sengoku-province-gold province)
                        (- (sengoku-province-gold province) 500)
                        (sengoku-province-guns province)
                        (+ (sengoku-province-guns province) 40)))
                (sengoku-engine--ok "AIが南蛮交易を行いました。" :acted t))
            (let ((capacity
                   (- (* (sengoku-province-koku province) 20)
                      (sengoku-province-soldiers province))))
              (cond
               ((and (>= (sengoku-province-gold province) 400)
                     (> capacity 1000)
                     (> (* (min (sengoku-province-gold province)
                                (sengoku-province-rice province))
                           5)
                        1000))
                (let* ((general-index
                        (sengoku-best-general game clan-index 'pol))
                       (quantity
                        (min capacity
                             (sengoku-engine--divide
                              (* (min (sengoku-province-gold province)
                                      (sengoku-province-rice province))
                                 5)
                              2)
                             3000)))
                  (sengoku-use-general game clan-index general-index)
                  (setf (sengoku-province-gold province)
                        (- (sengoku-province-gold province)
                           (sengoku-engine--divide quantity 5))
                        (sengoku-province-rice province)
                        (- (sengoku-province-rice province)
                           (sengoku-engine--divide quantity 5))
                        (sengoku-province-soldiers province)
                        (+ (sengoku-province-soldiers province) quantity)
                        (sengoku-province-loyalty province)
                        (max 0
                             (- (sengoku-province-loyalty province)
                                (sengoku-engine--divide quantity 500) 2)))
                  (sengoku-engine--ok "AIが徴兵しました。" :acted t)))
               ((>= (sengoku-province-gold province) 300)
                (let ((roll (sengoku-random 4)))
                  (cond
                   ((= roll 0)
                    (let* ((general-index
                            (sengoku-best-general game clan-index 'pol))
                           (general
                            (sengoku-use-general
                             game clan-index general-index)))
                      (setf (sengoku-province-gold province)
                            (- (sengoku-province-gold province) 200)
                            (sengoku-province-koku province)
                            (+ (sengoku-province-koku province)
                               12
                               (sengoku-engine--divide
                                (sengoku-general-politics general) 8)))
                      (sengoku-engine--ok "AIが開発しました。" :acted t)))
                   ((= roll 1)
                    (let* ((general-index
                            (sengoku-best-general game clan-index 'pol))
                           (general
                            (sengoku-use-general
                             game clan-index general-index)))
                      (setf (sengoku-province-gold province)
                            (- (sengoku-province-gold province) 200)
                            (sengoku-province-commerce province)
                            (+ (sengoku-province-commerce province)
                               6
                               (sengoku-engine--divide
                                (sengoku-general-politics general) 12)))
                      (sengoku-engine--ok "AIが商業投資しました。" :acted t)))
                   ((and (= roll 2)
                         (< (sengoku-province-training province) 95))
                    (let* ((general-index
                            (sengoku-best-general game clan-index 'war))
                           (general
                            (sengoku-use-general
                             game clan-index general-index)))
                      (setf (sengoku-province-gold province)
                            (- (sengoku-province-gold province) 100)
                            (sengoku-province-training province)
                            (min 100
                                 (+ (sengoku-province-training province)
                                    5
                                    (sengoku-engine--divide
                                     (sengoku-general-war general) 15))))
                      (sengoku-engine--ok "AIが訓練しました。" :acted t)))
                   ((and (< (sengoku-province-loyalty province) 50)
                         (> (sengoku-province-rice province) 300))
                    (let ((general-index
                           (sengoku-best-general game clan-index 'pol)))
                      (sengoku-use-general game clan-index general-index)
                      (setf (sengoku-province-gold province)
                            (- (sengoku-province-gold province) 100)
                            (sengoku-province-rice province)
                            (- (sengoku-province-rice province) 100)
                            (sengoku-province-loyalty province)
                            (min 100
                                 (+ (sengoku-province-loyalty province) 10)))
                      (sengoku-engine--ok "AIが施しをしました。" :acted t)))
                   (t
                    (sengoku-engine--ok "AIは行動しませんでした。"
                                        :acted nil)))))
               (t
                (sengoku-engine--ok "AIは行動しませんでした。"
                                    :acted nil)))))))))))

(defun sengoku-engine--command-delegate
    (session game player province-index province)
  "Delegate GAME PROVINCE through SESSION and let PLAYER's AI manage it."
  (setf (sengoku-province-auto province) 1)
  (sengoku-log
   game (format "%s を委任 (以後は家臣が差配)"
                (sengoku-province-name province)))
  (let ((ai-result
         (sengoku-engine--ai-province-action
          session player province-index nil)))
    (sengoku-engine--ok "国を委任しました。" :consumed t
                        :ai-result ai-result)))

(defun sengoku-engine--command-release (game player target-index)
  "Release GAME TARGET-INDEX for PLAYER without consuming an action."
  (let ((candidates (sengoku-engine-release-candidates game player)))
    (cond
     ((null candidates)
      (sengoku-engine--error "委任中の国はありません。"))
     ((not (memq target-index candidates))
      (sengoku-engine--error "その国は委任中ではありません。"))
     (t
      (let ((province (sengoku-engine--province game target-index)))
        (setf (sengoku-province-auto province) 0)
        (sengoku-log game (format "%s の委任を解除"
                                  (sengoku-province-name province)))
        (sengoku-engine--ok "委任を解除しました。" :consumed nil))))))

(defun sengoku-engine-command (session action &rest arguments)
  "Apply one selected strategic ACTION with ARGUMENTS in SESSION.

ACTION is one of `develop', `commerce', `relief', `recruit', `train',
`buy-guns', `attack', `transport', `propose-alliance', `break-alliance',
`court-rank', `court-peace', `nanban', `tea', `delegate', `release', `wait',
or `skip'.  ARGUMENTS is a property list using keys such as `:general',
`:quantity', `:target', and `:kind'.  Successful consuming commands advance
SESSION past its current direct province; callers should then invoke
`sengoku-engine-advance'."
  (pcase-let* ((`(,game ,player ,province-index ,province)
                (sengoku-engine--player-context session))
               (result
                (pcase action
                  ('develop
                   (sengoku-engine--command-develop
                    game player province (plist-get arguments :general)))
                  ('commerce
                   (sengoku-engine--command-commerce
                    game player province (plist-get arguments :general)))
                  ('relief
                   (sengoku-engine--command-relief
                    game player province (plist-get arguments :general)))
                  ('recruit
                   (sengoku-engine--command-recruit
                    game player province (plist-get arguments :general)
                    (plist-get arguments :quantity)))
                  ('train
                   (sengoku-engine--command-train
                    game player province (plist-get arguments :general)))
                  ('buy-guns
                   (sengoku-engine--command-buy-guns game player province))
                  ('attack
                   (sengoku-engine--command-attack
                    session game player province-index province
                    (plist-get arguments :target)
                    (plist-get arguments :general)
                    (plist-get arguments :quantity)))
                  ('transport
                   (sengoku-engine--command-transport
                    game player province-index province
                    (plist-get arguments :target)
                    (plist-get arguments :kind)
                    (plist-get arguments :quantity)))
                  ('propose-alliance
                   (sengoku-engine--command-propose-alliance
                    game player province (plist-get arguments :target)
                    (plist-get arguments :general)))
                  ('break-alliance
                   (sengoku-engine--command-break-alliance
                    game player (plist-get arguments :target)))
                  ('court-rank
                   (sengoku-engine--command-court-rank
                    game player province (plist-get arguments :general)))
                  ('court-peace
                   (sengoku-engine--command-court-peace
                    game player province (plist-get arguments :target)
                    (plist-get arguments :general)))
                  ('nanban
                   (sengoku-engine--command-nanban
                    game player province (plist-get arguments :kind)))
                  ('tea
                   (sengoku-engine--command-tea game player province))
                  ('delegate
                   (sengoku-engine--command-delegate
                    session game player province-index province))
                  ('release
                   (sengoku-engine--command-release
                    game player (plist-get arguments :target)))
                  ('wait
                   (sengoku-engine--ok "待機しました。" :consumed t))
                  ('skip
                   (sengoku-engine--set-state session :skip-direct t)
                   (setf (sengoku-session-turn-queue session) nil
                         (sengoku-game-turn-queue game) nil)
                   (sengoku-engine--ok
                    "残りの直轄国を待機させます。" :consumed t :skip t))
                  (_ (sengoku-engine--error "未知のコマンドです。")))))
    (when (and (plist-get result :consumed)
               (not (memq (sengoku-session-phase session)
                          '(battle-choice siege))))
      (setf (sengoku-session-active-province session) -1
            (sengoku-session-phase session) 'player-direct))
    result))

(defun sengoku-engine-pending-decision (session)
  "Return SESSION's pending merchant or alliance decision, or nil."
  (plist-get (sengoku-engine--state session) :pending-decision))

(defun sengoku-engine--set-decision (session decision next-stage)
  "Suspend SESSION for DECISION and resume month processing at NEXT-STAGE."
  (sengoku-engine--set-state session :pending-decision decision)
  (sengoku-engine--set-state session :decision-next-stage next-stage)
  (setf (sengoku-session-phase session) 'decision)
  (sengoku-engine--result
   'decision (plist-get decision :message) :decision decision))

(defun sengoku-engine-resolve-decision (session accept)
  "Resolve SESSION's pending offer according to ACCEPT and return a result."
  (unless (eq (sengoku-session-phase session) 'decision)
    (error "Session is not waiting for a decision"))
  (let* ((game (sengoku-session-game session))
         (decision (sengoku-engine-pending-decision session))
         (next-stage
          (plist-get (sengoku-engine--state session) :decision-next-stage)))
    (unless decision
      (error "Session has no pending decision"))
    (pcase (plist-get decision :type)
      ('merchant
       (let* ((player (sengoku-game-player game))
              (province
               (sengoku-engine--province game
                                         (plist-get decision :province)))
              (item-index (plist-get decision :item))
              (item (aref (sengoku-game-items game) item-index)))
         (if accept
             (progn
               (setf (sengoku-province-gold province)
                     (- (sengoku-province-gold province)
                        (plist-get item :price)))
               (sengoku-gain-item game player item-index)
               (sengoku-log
                game (format "名物「%s」を購入! 文化+%d (%d)"
                             (plist-get item :name)
                             (plist-get item :culture)
                             (sengoku-culture game player))))
           (sengoku-log
            game (format "名物「%s」の購入を見送った"
                         (plist-get item :name))))))
      ('alliance
       (let* ((player (sengoku-game-player game))
              (clan-index (plist-get decision :clan))
              (clan-name
               (sengoku-clan-name
                (sengoku-engine--clan game clan-index))))
         (if accept
             (progn
               (sengoku-set-alliance game player clan-index t)
               (sengoku-log game (format "%s家と同盟成立!" clan-name)))
           (sengoku-log
            game (format "%s家の同盟の申し出を断った" clan-name)))))
      (_ (error "Unknown pending decision: %S" decision)))
    (sengoku-engine--set-state session :pending-decision nil)
    (sengoku-engine--set-state session :decision-next-stage nil)
    (sengoku-engine--set-state session :month-stage next-stage)
    (setf (sengoku-session-phase session) 'month-end)
    (sengoku-engine--ok
     (if accept "申し出を受けました。" "申し出を断りました。")
     :accepted (and accept t))))

(defun sengoku-engine-resolve-battle-choice (session resolution)
  "Resolve SESSION's pending battle by RESOLUTION (`automatic' or `siege')."
  (unless (eq (sengoku-session-phase session) 'battle-choice)
    (error "Session is not waiting for a battle choice"))
  (unless (memq resolution '(automatic siege))
    (error "Unknown battle resolution: %S" resolution))
  (let* ((game (sengoku-session-game session))
         (state (sengoku-engine--state session))
         (context (sengoku-session-pending-battle session))
         (log-mode (plist-get state :battle-log-mode))
         (resume-phase (plist-get state :battle-resume-phase)))
    (unless context
      (error "Session has no pending battle"))
    (if (eq resolution 'automatic)
        (progn
          (sengoku-battle-dispatch game context 'automatic)
          (sengoku-engine--log-battle game context log-mode)
          (sengoku-engine--clear-pending-battle session)
          (setf (sengoku-session-active-province session) -1
                (sengoku-session-phase session) resume-phase)
          (sengoku-engine--result
           'battle-complete "戦闘を自動解決しました。" :battle context))
      (let ((player-side (sengoku-battle-player-side game context)))
        (sengoku-battle-dispatch game context 'siege player-side)
        (setf (sengoku-session-phase session) 'siege)
        (sengoku-engine--result
         'siege "籠城戦を開始しました。" :battle context)))))

(defun sengoku-engine--finish-siege (session)
  "Apply SESSION's completed siege and resume strategic processing."
  (let* ((game (sengoku-session-game session))
         (state (sengoku-engine--state session))
         (context (sengoku-session-pending-battle session))
         (log-mode (plist-get state :battle-log-mode))
         (resume-phase (plist-get state :battle-resume-phase)))
    (sengoku-battle-apply-siege-result game context)
    (sengoku-engine--log-battle game context log-mode)
    (sengoku-engine--clear-pending-battle session)
    (setf (sengoku-session-active-province session) -1
          (sengoku-session-phase session) resume-phase)
    context))

(defun sengoku-engine-end-state (game)
  "Return GAME's player victory or defeat plist, or nil when play continues."
  (let ((player (sengoku-game-player game)))
    (when (>= player 0)
      (cond
       ((not (sengoku-clan-alive-p game player))
        (list :outcome 'defeat
              :message
              (format "%s家は滅亡しました…… 【ゲームオーバー】"
                      (sengoku-clan-name
                       (sengoku-engine--clan game player)))))
       ((= (length (sengoku-clan-provinces game player))
           (length (sengoku-game-provinces game)))
        (let ((clan (sengoku-engine--clan game player)))
          (list :outcome 'victory
                :message
                (format "全国統一!! %s(%s家)は天下人となった! 【勝利】"
                        (sengoku-clan-daimyo clan)
                        (sengoku-clan-name clan)))))
       (t nil)))))

(defun sengoku-engine--end-session (session end-state)
  "Mark SESSION ended according to END-STATE and return a result."
  (setf (sengoku-session-phase session) 'ended
        (sengoku-session-quit-reason session)
        (plist-get end-state :outcome)
        (sengoku-session-active-province session) -1)
  (sengoku-engine--result
   'game-over (plist-get end-state :message)
   :outcome (plist-get end-state :outcome)))

(defun sengoku-engine--month-economy (game)
  "Apply GAME's province income, upkeep, harvest, disaster, and revolt rules."
  (let ((player (sengoku-game-player game))
        (month (sengoku-game-month game)))
    (dotimes (province-index (length (sengoku-game-provinces game)))
      (let ((province (aref (sengoku-game-provinces game) province-index)))
        (when (>= (sengoku-province-owner province) 0)
          (let ((owner (sengoku-province-owner province)))
            (setf (sengoku-province-gold province)
                  (+ (sengoku-province-gold province)
                     (sengoku-engine--divide
                      (* (sengoku-province-commerce province)
                         (+ 100 (sengoku-culture game owner)))
                      100))
                  (sengoku-province-rice province)
                  (- (sengoku-province-rice province)
                     (sengoku-engine--divide
                      (sengoku-province-soldiers province) 50)))
            (when (< (sengoku-province-rice province) 0)
              (let ((desert (* (- (sengoku-province-rice province)) 2)))
                (setf (sengoku-province-soldiers province)
                      (max 0 (- (sengoku-province-soldiers province) desert))
                      (sengoku-province-rice province) 0
                      (sengoku-province-loyalty province)
                      (max 0 (- (sengoku-province-loyalty province) 3)))
                (when (= owner player)
                  (sengoku-log
                   game (format "%s 兵糧が尽き%d人が逃散……"
                                (sengoku-province-name province) desert)))))
            (when (zerop (% month 3))
              (setf (sengoku-province-training province)
                    (max 20 (1- (sengoku-province-training province)))))
            (when (= month 9)
              (let* ((rate (+ 70 (sengoku-random 61)))
                     (harvest
                      (sengoku-engine--divide
                       (* (sengoku-province-koku province) 3 rate) 100)))
                (setf (sengoku-province-rice province)
                      (+ (sengoku-province-rice province) harvest))
                (when (= owner player)
                  (cond
                   ((>= rate 120)
                    (sengoku-log
                     game (format "%s 豊作! 米+%d"
                                  (sengoku-province-name province) harvest)))
                   ((<= rate 80)
                    (sengoku-log
                     game (format "%s 凶作…… 米+%d"
                                  (sengoku-province-name province) harvest)))))))
            (when (and (memq month '(8 9))
                       (< (sengoku-random 100) 12))
              (let ((damage
                     (sengoku-engine--divide
                      (sengoku-province-koku province) 20)))
                (setf (sengoku-province-koku province)
                      (- (sengoku-province-koku province) damage))
                (when (= owner player)
                  (sengoku-log
                   game (format "%s 台風襲来! 石高-%d"
                                (sengoku-province-name province) damage)))))
            (when (and (< (sengoku-province-loyalty province) 25)
                       (< (sengoku-random 100) 30))
              (let ((loss
                     (sengoku-engine--divide
                      (sengoku-province-soldiers province) 5)))
                (setf (sengoku-province-soldiers province)
                      (- (sengoku-province-soldiers province) loss)
                      (sengoku-province-koku province)
                      (sengoku-engine--divide
                       (* (sengoku-province-koku province) 95) 100)
                      (sengoku-province-loyalty province)
                      (+ (sengoku-province-loyalty province) 10))
                (sengoku-log
                 game (format "%s 一揆勃発! 兵-%d 石高減"
                              (sengoku-province-name province) loss))))))))))

(defun sengoku-engine--merchant-player (session)
  "Run the player-offer half of merchant events for SESSION."
  (let* ((game (sengoku-session-game session))
         (player (sengoku-game-player game))
         (pool (sengoku-unowned-items game)))
    (if (and pool
             (>= player 0)
             (sengoku-clan-alive-p game player)
             (< (sengoku-random 100) 10))
        (let* ((item-index (nth (sengoku-random (length pool)) pool))
               (item (aref (sengoku-game-items game) item-index))
               (province-index (sengoku-richest-province game player)))
          (if (and (>= province-index 0)
                   (>= (sengoku-province-gold
                        (sengoku-engine--province game province-index))
                       (plist-get item :price)))
              (sengoku-engine--set-decision
               session
               (list :type 'merchant
                     :item item-index
                     :province province-index
                     :message
                     (format "堺の商人が名物「%s」を金%dで売りに来ました。"
                             (plist-get item :name)
                             (plist-get item :price)))
               'merchant-ai)
            nil))
      nil)))

(defun sengoku-engine--merchant-ai (game)
  "Run the AI-purchase half of merchant events for GAME."
  (let ((pool (sengoku-unowned-items game)))
    (when (and pool (< (sengoku-random 100) 8))
      (let ((minimum-price
             (apply #'min
                    (mapcar
                     (lambda (item-index)
                       (plist-get (aref (sengoku-game-items game) item-index)
                                  :price))
                     pool)))
            candidates)
        (dotimes (clan-index (length (sengoku-game-clans game)))
          (let ((source (sengoku-richest-province game clan-index)))
            (when (and (/= clan-index (sengoku-game-player game))
                       (sengoku-clan-alive-p game clan-index)
                       (>= source 0)
                       (>= (sengoku-province-gold
                            (sengoku-engine--province game source))
                           minimum-price))
              (push clan-index candidates))))
        (setq candidates (nreverse candidates))
        (when candidates
          (let* ((clan-index
                  (nth (sengoku-random (length candidates)) candidates))
                 (source (sengoku-richest-province game clan-index))
                 (gold (sengoku-province-gold
                        (sengoku-engine--province game source)))
                 (affordable
                  (seq-filter
                   (lambda (item-index)
                     (<= (plist-get
                          (aref (sengoku-game-items game) item-index) :price)
                         gold))
                   pool))
                 (item-index
                  (nth (sengoku-random (length affordable)) affordable))
                 (item (aref (sengoku-game-items game) item-index))
                 (province (sengoku-engine--province game source)))
            (setf (sengoku-province-gold province)
                  (- (sengoku-province-gold province)
                     (plist-get item :price)))
            (sengoku-gain-item game clan-index item-index)
            (sengoku-log
             game (format "%s家が名物「%s」を入手したとの噂"
                          (sengoku-clan-name
                           (sengoku-engine--clan game clan-index))
                          (plist-get item :name)))))))))

(defun sengoku-engine--diplo-form (game)
  "Possibly form one alliance between AI clans in GAME."
  (when (< (sengoku-random 100) 12)
    (let (candidates)
      (dotimes (clan-index (length (sengoku-game-clans game)))
        (when (and (/= clan-index (sengoku-game-player game))
                   (sengoku-clan-alive-p game clan-index))
          (push clan-index candidates)))
      (setq candidates (nreverse candidates))
      (when (>= (length candidates) 2)
        (let* ((clan-a
                (nth (sengoku-random (length candidates)) candidates))
               (neighbors
                (seq-filter
                 (lambda (clan-index)
                   (and (/= clan-index (sengoku-game-player game))
                        (not (sengoku-allied-p game clan-a clan-index))))
                 (sengoku-neighbor-clans game clan-a))))
          (when neighbors
            (let ((clan-b
                   (nth (sengoku-random (length neighbors)) neighbors)))
              (sengoku-set-alliance game clan-a clan-b t)
              (sengoku-log
               game (format "%s家と%s家が同盟を結んだ"
                            (sengoku-clan-name
                             (sengoku-engine--clan game clan-a))
                            (sengoku-clan-name
                             (sengoku-engine--clan game clan-b)))))))))))

(defun sengoku-engine--ai-alliance-keys (game)
  "Return GAME alliance keys not involving the player."
  (let ((player (sengoku-game-player game))
        keys)
    (maphash
     (lambda (key _value)
       (pcase-let ((`(,left ,right)
                    (mapcar #'string-to-number (split-string key "-"))))
         (when (and (/= left player) (/= right player))
           (push key keys))))
     (sengoku-game-alliances game))
    (nreverse keys)))

(defun sengoku-engine--diplo-break (game)
  "Possibly break one alliance between AI clans in GAME."
  (let ((keys (sengoku-engine--ai-alliance-keys game)))
    (when (and keys (< (sengoku-random 100) 3))
      (let* ((key (nth (sengoku-random (length keys)) keys))
             (pair (mapcar #'string-to-number (split-string key "-"))))
        (remhash key (sengoku-game-alliances game))
        (sengoku-log
         game (format "%s家と%s家の同盟が手切れに"
                      (sengoku-clan-name
                       (sengoku-engine--clan game (nth 0 pair)))
                      (sengoku-clan-name
                       (sengoku-engine--clan game (nth 1 pair)))))))))

(defun sengoku-engine--diplo-player (session)
  "Possibly create an AI alliance offer for SESSION's player."
  (let* ((game (sengoku-session-game session))
         (player (sengoku-game-player game)))
    (when (and (>= player 0)
               (sengoku-clan-alive-p game player)
               (< (sengoku-random 100) 4))
      (let ((candidates
             (seq-filter
              (lambda (clan-index)
                (not (sengoku-allied-p game player clan-index)))
              (sengoku-neighbor-clans game player))))
        (when candidates
          (let* ((clan-index
                  (nth (sengoku-random (length candidates)) candidates))
                 (clan (sengoku-engine--clan game clan-index)))
            (sengoku-engine--set-decision
             session
             (list :type 'alliance
                   :clan clan-index
                   :message
                   (format "%s家(%s)より同盟の申し出が届きました。"
                           (sengoku-clan-name clan)
                           (sengoku-clan-daimyo clan)))
             'finish)))))))

(defun sengoku-engine--finish-month (session)
  "Advance SESSION's date, reset generals, and enter the post-month check."
  (let ((game (sengoku-session-game session)))
    (setf (sengoku-game-month game) (1+ (sengoku-game-month game)))
    (when (> (sengoku-game-month game) 12)
      (setf (sengoku-game-month game) 1
            (sengoku-game-year game) (1+ (sengoku-game-year game))))
    (sengoku-reset-generals game)
    (sengoku-engine--set-state session :month-stage nil)
    (setf (sengoku-session-phase session) 'check-after-month)))

(defun sengoku-engine--advance-month (session)
  "Advance SESSION's month-end stages until a decision or completion."
  (let ((game (sengoku-session-game session))
        (done nil)
        result)
    (while (and (not done)
                (eq (sengoku-session-phase session) 'month-end))
      (pcase (plist-get (sengoku-engine--state session) :month-stage)
        ('economy
         (sengoku-engine--month-economy game)
         (sengoku-engine--set-state session :month-stage 'merchant-player))
        ('merchant-player
         (setq result (sengoku-engine--merchant-player session))
         (if result
             (setq done t)
           (sengoku-engine--set-state session :month-stage 'merchant-ai)))
        ('merchant-ai
         (sengoku-engine--merchant-ai game)
         (sengoku-engine--set-state session :month-stage 'diplo-form))
        ('diplo-form
         (sengoku-engine--diplo-form game)
         (sengoku-engine--set-state session :month-stage 'diplo-break))
        ('diplo-break
         (sengoku-engine--diplo-break game)
         (sengoku-engine--set-state session :month-stage 'diplo-player))
        ('diplo-player
         (setq result (sengoku-engine--diplo-player session))
         (if result
             (setq done t)
           (sengoku-engine--set-state session :month-stage 'finish)))
        ('finish
         (sengoku-engine--finish-month session)
         (setq done t))
        (_ (error "Unknown month-end stage: %S"
                  (plist-get (sengoku-engine--state session) :month-stage)))))
    result))

(defun sengoku-engine--initialize-enemy-turns (session)
  "Initialize enemy-turn controller fields in SESSION."
  (sengoku-engine--set-state session :enemy-clan 0)
  (sengoku-engine--set-state session :enemy-active nil)
  (sengoku-engine--set-state session :enemy-queue nil)
  (setf (sengoku-session-phase session) 'enemy-turns))

(defun sengoku-engine--advance-enemies (session)
  "Advance SESSION through enemy actions until battle input or completion."
  (let* ((game (sengoku-session-game session))
         (clan-count (length (sengoku-game-clans game)))
         (player (sengoku-game-player game))
         (stopped nil)
         result)
    (while (and (not stopped)
                (eq (sengoku-session-phase session) 'enemy-turns)
                (< (or (plist-get (sengoku-engine--state session) :enemy-clan) 0)
                   clan-count))
      (let* ((state (sengoku-engine--state session))
             (clan-index (plist-get state :enemy-clan)))
        (if (= clan-index player)
            (progn
              (sengoku-engine--set-state session :enemy-clan (1+ clan-index))
              (sengoku-engine--set-state session :enemy-active nil))
          (unless (plist-get state :enemy-active)
            (sengoku-engine--set-state
             session :enemy-queue
             (copy-sequence (sengoku-clan-provinces game clan-index)))
            (sengoku-engine--set-state session :enemy-active t))
          (let ((queue
                 (plist-get (sengoku-engine--state session) :enemy-queue)))
            (if (or (null queue)
                    (null (sengoku-unused-generals game clan-index)))
                (progn
                  (sengoku-engine--set-state session :enemy-queue nil)
                  (sengoku-engine--set-state session :enemy-active nil)
                  (sengoku-engine--set-state session :enemy-clan
                                             (1+ clan-index))
                  (when (and (>= player 0)
                             (not (sengoku-clan-alive-p game player)))
                    (setf (sengoku-session-phase session) 'check-before-month
                          stopped t)))
              (let ((province-index (car queue)))
                (sengoku-engine--set-state session :enemy-queue (cdr queue))
                (when (= (sengoku-province-owner
                          (sengoku-engine--province game province-index))
                         clan-index)
                  (setq result
                        (sengoku-engine--ai-province-action
                         session clan-index province-index t))
                  (when (eq (sengoku-session-phase session) 'battle-choice)
                    (setq stopped t)))))))))
    (when (and (eq (sengoku-session-phase session) 'enemy-turns)
               (>= (or (plist-get (sengoku-engine--state session) :enemy-clan)
                       0)
                   clan-count))
      (setf (sengoku-session-phase session) 'check-before-month))
    result))

(defun sengoku-engine--initialize-turn (session)
  "Snapshot player provinces and initialize SESSION's turn controller."
  (let* ((game (sengoku-session-game session))
         (player (sengoku-game-player game))
         (owned (if (>= player 0)
                    (copy-sequence (sengoku-clan-provinces game player))
                  nil)))
    ;; Keep one source-order snapshot and classify delegation only when the scan
    ;; reaches each province.  Vim therefore lets a command in an earlier direct
    ;; province release a later province for direct action in the same month.
    (setf (sengoku-session-ui-state session)
          (list :auto-queue nil
                :prompted nil
                :skip-direct nil
                :enemy-clan 0
                :enemy-active nil
                :enemy-queue nil
                :month-stage nil
                :pending-decision nil)
          (sengoku-session-turn-queue session) owned
          (sengoku-game-turn-queue game) (copy-sequence owned)
          (sengoku-session-active-province session) -1
          (sengoku-session-pending-battle session) nil
          (sengoku-game-pending-battle game) nil)
    (if (>= player 0)
        (setf (sengoku-session-phase session) 'player-direct)
      (sengoku-engine--initialize-enemy-turns session))))

(defun sengoku-engine-begin-turn (session)
  "Initialize SESSION's current month and advance to its next input boundary.
This entry point is valid only for a fresh or loaded `setup' session and for
`observer-idle' sessions that deliberately run one AI-only month at a time."
  (unless (memq (sengoku-session-phase session) '(setup observer-idle))
    (error "Cannot begin a turn while session phase is %S"
           (sengoku-session-phase session)))
  (sengoku-engine--initialize-turn session)
  (sengoku-engine-advance session))

(defun sengoku-engine-select-player (session clan-index)
  "Select CLAN-INDEX as SESSION's player, log the start, and begin the game."
  (unless (eq (sengoku-session-phase session) 'setup)
    (error "A player may be selected only during setup"))
  (let* ((game (sengoku-session-game session))
         (clan (sengoku-engine--clan game clan-index)))
    (setf (sengoku-game-player game) clan-index)
    (sengoku-log
     game (format "%s家でゲーム開始! 目指せ天下統一!"
                  (sengoku-clan-name clan)))
    (sengoku-engine-begin-turn session)))

(defun sengoku-engine-continue-all-delegated (session)
  "Advance SESSION past the all-delegated pause and through enemy actions."
  (unless (eq (sengoku-session-phase session) 'all-delegated)
    (error "Session is not at the all-delegated pause"))
  (sengoku-engine--initialize-enemy-turns session)
  (sengoku-engine-advance session))

(defun sengoku-engine-release-delegation (session province-index)
  "Release PROVINCE-INDEX during SESSION's all-delegated pause."
  (unless (eq (sengoku-session-phase session) 'all-delegated)
    (error "Session is not at the all-delegated pause"))
  (let* ((game (sengoku-session-game session))
         (player (sengoku-game-player game)))
    (sengoku-engine--command-release game player province-index)))

(defun sengoku-engine-advance (session)
  "Run SESSION automatically until the next input boundary and return its state.

Possible result statuses include `player-command', `all-delegated',
`battle-choice', `siege-turn', `decision', `observer-month-complete', and
`game-over'."
  (let ((running t)
        result)
    (while running
      (pcase (sengoku-session-phase session)
        ('setup
         (setq result (sengoku-engine--result
                       'setup "大名家を選んでください。")
               running nil))
        ('player-direct
         (let* ((game (sengoku-session-game session))
                (player (sengoku-game-player game))
                (queue (sengoku-session-turn-queue session))
                province-index)
           (while (and queue (null province-index))
             (let* ((candidate (car queue))
                    (province (sengoku-engine--province game candidate)))
               (setq queue (cdr queue))
               (when (= (sengoku-province-owner province) player)
                 ;; Vim tests delegation before its remaining-direct skip flag.
                 ;; Provinces already classified as delegated therefore still
                 ;; receive their AI action after `skip'.
                 (cond
                  ((not (zerop (sengoku-province-auto province)))
                   (sengoku-engine--set-state
                    session :auto-queue
                    (append
                     (plist-get (sengoku-engine--state session) :auto-queue)
                     (list candidate))))
                  ((not (plist-get
                         (sengoku-engine--state session) :skip-direct))
                   (setq province-index candidate))))))
           (setf (sengoku-session-turn-queue session) queue
                 (sengoku-game-turn-queue game) (copy-sequence queue))
           (if province-index
               (progn
                 (sengoku-engine--set-state session :prompted t)
                 (setf (sengoku-session-active-province session) province-index
                       (sengoku-session-phase session) 'player-command)
                 (setq result
                       (sengoku-engine--result
                        'player-command "国の行動を選んでください。"
                        :province province-index)
                       running nil))
             (setf (sengoku-session-phase session) 'player-auto))))
        ('player-command
         (setq result
               (sengoku-engine--result
                'player-command "国の行動を選んでください。"
                :province (sengoku-session-active-province session))
               running nil))
        ('player-auto
         (let* ((game (sengoku-session-game session))
                (player (sengoku-game-player game))
                (queue (plist-get (sengoku-engine--state session) :auto-queue)))
           (while queue
             (let ((province-index (car queue)))
               (setq queue (cdr queue))
               (sengoku-engine--set-state session :auto-queue queue)
               (when (= (sengoku-province-owner
                         (sengoku-engine--province game province-index))
                        player)
                 (sengoku-engine--ai-province-action
                  session player province-index nil))))
           (if (and (not (plist-get
                          (sengoku-engine--state session) :prompted))
                    (not (plist-get
                          (sengoku-engine--state session) :skip-direct)))
               (progn
                 (setf (sengoku-session-phase session) 'all-delegated)
                 (setq result
                       (sengoku-engine--result
                        'all-delegated
                        "全国委任中です。次の月へ進むか委任を解除してください。")
                       running nil))
             (sengoku-engine--initialize-enemy-turns session))))
        ('all-delegated
         (setq result
               (sengoku-engine--result
                'all-delegated
                "全国委任中です。次の月へ進むか委任を解除してください。")
               running nil))
        ('enemy-turns
         (sengoku-engine--advance-enemies session)
         (when (eq (sengoku-session-phase session) 'battle-choice)
           (setq result
                 (sengoku-engine--result
                  'battle-choice "戦闘の解決方法を選んでください。"
                  :battle (sengoku-session-pending-battle session)
                  :choices '(automatic siege))
                 running nil)))
        ('battle-choice
         (setq result
               (sengoku-engine--result
                'battle-choice "戦闘の解決方法を選んでください。"
                :battle (sengoku-session-pending-battle session)
                :choices '(automatic siege))
               running nil))
        ('siege
         (let* ((context (sengoku-session-pending-battle session))
                (status (sengoku-siege-advance-to-player context)))
           (if (eq status 'player-turn)
               (setq result
                     (sengoku-engine--result
                      'siege-turn "籠城戦の部隊行動を選んでください。"
                      :battle context
                      :turn (sengoku-siege-player-turn-state context))
                     running nil)
             (sengoku-engine--finish-siege session))))
        ('check-before-month
         (let ((end-state
                (sengoku-engine-end-state (sengoku-session-game session))))
           (if end-state
               (setq result (sengoku-engine--end-session session end-state)
                     running nil)
             (sengoku-engine--set-state session :month-stage 'economy)
             (setf (sengoku-session-phase session) 'month-end))))
        ('month-end
         (let ((month-result (sengoku-engine--advance-month session)))
           (when month-result
             (setq result month-result
                   running nil))))
        ('decision
         (setq result
               (sengoku-engine--result
                'decision
                (plist-get (sengoku-engine-pending-decision session) :message)
                :decision (sengoku-engine-pending-decision session))
               running nil))
        ('check-after-month
         (let* ((game (sengoku-session-game session))
                (end-state (sengoku-engine-end-state game)))
           (cond
            (end-state
             (setq result (sengoku-engine--end-session session end-state)
                   running nil))
            ((< (sengoku-game-player game) 0)
             (setf (sengoku-session-phase session) 'observer-idle)
             (setq result
                   (sengoku-engine--result
                    'observer-month-complete "AIのみの月が終了しました。")
                   running nil))
            (t (sengoku-engine--initialize-turn session)))))
        ('observer-idle
         (setq result
               (sengoku-engine--result
                'observer-month-complete "AIのみの月が終了しました。")
               running nil))
        ('ended
         (setq result
               (sengoku-engine--result
                'game-over "ゲームは終了しています。"
                :outcome (sengoku-session-quit-reason session))
               running nil))
        (_ (error "Unknown session phase: %S"
                  (sengoku-session-phase session)))))
    result))

(defun sengoku-engine-province-report (game province-index)
  "Return a detached report plist for PROVINCE-INDEX in GAME."
  (let* ((province (sengoku-engine--province game province-index))
         (owner (sengoku-province-owner province)))
    (list :index province-index
          :name (sengoku-province-name province)
          :owner owner
          :clan-name (and (>= owner 0)
                          (sengoku-clan-name
                           (sengoku-engine--clan game owner)))
          :daimyo (and (>= owner 0)
                       (sengoku-clan-daimyo
                        (sengoku-engine--clan game owner)))
          :delegated (not (zerop (sengoku-province-auto province)))
          :koku (sengoku-province-koku province)
          :commerce (sengoku-province-commerce province)
          :loyalty (sengoku-province-loyalty province)
          :port (not (zerop (sengoku-province-port province)))
          :soldiers (sengoku-province-soldiers province)
          :training (sengoku-province-training province)
          :guns (sengoku-province-guns province)
          :cannon (sengoku-province-cannon province)
          :gold (sengoku-province-gold province)
          :rice (sengoku-province-rice province)
          :adjacent (append (sengoku-province-adjacency province) nil))))

(defun sengoku-engine-general-report (game clan-index)
  "Return detached general report plists for CLAN-INDEX in GAME."
  (let ((roster (aref (sengoku-game-generals game) clan-index))
        result)
    (dotimes (general-index (length roster))
      (let ((general (aref roster general-index)))
        (push (list :index general-index
                    :name (sengoku-general-name general)
                    :lord (= general-index 0)
                    :used (sengoku-general-used-p
                           game clan-index general-index)
                    :war (sengoku-general-war general)
                    :politics (sengoku-general-politics general))
              result)))
    (nreverse result)))

(defun sengoku-engine-clan-report (game)
  "Return detached status plists for all living clans in GAME."
  (let (result)
    (dotimes (clan-index (length (sengoku-game-clans game)))
      (when (sengoku-clan-alive-p game clan-index)
        (let* ((clan (sengoku-engine--clan game clan-index))
               (state (aref (sengoku-game-clan-states game) clan-index)))
          (push
           (list :index clan-index
                 :player (= clan-index (sengoku-game-player game))
                 :name (sengoku-clan-name clan)
                 :daimyo (sengoku-clan-daimyo clan)
                 :provinces (length (sengoku-clan-provinces game clan-index))
                 :rank (aref sengoku-data-ranks
                             (sengoku-clan-state-rank state))
                 :culture (sengoku-culture game clan-index)
                 :items (copy-sequence (sengoku-clan-state-items state))
                 :allies (sengoku-alliance-list game clan-index))
           result))))
    (nreverse result)))

(provide 'sengoku-engine)

;;; sengoku-engine.el ends here
