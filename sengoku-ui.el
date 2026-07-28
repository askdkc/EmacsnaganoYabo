;;; sengoku-ui.el --- Emacs interface for Sengoku -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: dkc
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: games

;;; Commentary:

;; Dedicated major mode, strategic and tactical rendering, minibuffer
;; selections, and one-action interactive commands for Sengoku.  The campaign
;; engine remains nonblocking: every key invokes one command, then
;; `sengoku-ui-advance' runs automatic stages to the next player decision.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'sengoku-engine)
(require 'sengoku-save)

(defgroup sengoku-ui nil
  "Display settings for Sengoku."
  :group 'sengoku)

(defcustom sengoku-buffer-name "*戦国風雲録*"
  "Name of the main Sengoku game buffer."
  :type 'string
  :group 'sengoku-ui)

(defcustom sengoku-recommended-width 115
  "Recommended minimum game window width."
  :type 'integer
  :group 'sengoku-ui)

(defcustom sengoku-recommended-height 40
  "Recommended minimum game window height."
  :type 'integer
  :group 'sengoku-ui)

(defface sengoku-title-face
  '((t :inherit font-lock-keyword-face :weight bold :height 1.1))
  "Face for Sengoku screen titles."
  :group 'sengoku-ui)

(defface sengoku-active-face
  '((t :foreground "white" :background "#870000" :weight bold))
  "Face for the active strategic province marker."
  :group 'sengoku-ui)

(defface sengoku-delegated-face
  '((t :inherit font-lock-comment-face :weight bold))
  "Face for a delegated strategic province marker."
  :group 'sengoku-ui)

(defface sengoku-panel-face
  '((t :inherit shadow))
  "Face for strategic status panels."
  :group 'sengoku-ui)

(defface sengoku-attacker-face
  '((t :foreground "#ff5f5f" :weight bold))
  "Face for attacking siege units."
  :group 'sengoku-ui)

(defface sengoku-defender-face
  '((t :foreground "#5fd7ff" :weight bold))
  "Face for defending siege units."
  :group 'sengoku-ui)

(defface sengoku-wall-face
  '((t :foreground "#8a8a8a" :weight bold))
  "Face for siege walls."
  :group 'sengoku-ui)

(defface sengoku-forest-face
  '((t :foreground "#00af00"))
  "Face for siege forests."
  :group 'sengoku-ui)

(defface sengoku-gate-face
  '((t :foreground "#ff8700" :weight bold))
  "Face for the siege gate."
  :group 'sengoku-ui)

(defface sengoku-keep-face
  '((t :foreground "#ffd700" :weight bold))
  "Face for the siege keep."
  :group 'sengoku-ui)

(defface sengoku-clan-0-face '((t :foreground "#ff0000")) "Clan color 0." :group 'sengoku-ui)
(defface sengoku-clan-1-face '((t :foreground "#00ff00")) "Clan color 1." :group 'sengoku-ui)
(defface sengoku-clan-2-face '((t :foreground "#ffff00")) "Clan color 2." :group 'sengoku-ui)
(defface sengoku-clan-3-face '((t :foreground "#00ffff")) "Clan color 3." :group 'sengoku-ui)
(defface sengoku-clan-4-face '((t :foreground "#ff87ff")) "Clan color 4." :group 'sengoku-ui)
(defface sengoku-clan-5-face '((t :foreground "#ff8700")) "Clan color 5." :group 'sengoku-ui)
(defface sengoku-clan-6-face '((t :foreground "#00afff")) "Clan color 6." :group 'sengoku-ui)
(defface sengoku-clan-7-face '((t :foreground "#af87ff")) "Clan color 7." :group 'sengoku-ui)
(defface sengoku-clan-8-face '((t :foreground "#e4e4e4")) "Clan color 8." :group 'sengoku-ui)
(defface sengoku-clan-9-face '((t :foreground "#87ff00")) "Clan color 9." :group 'sengoku-ui)
(defface sengoku-clan-10-face '((t :foreground "#ff5f5f")) "Clan color 10." :group 'sengoku-ui)
(defface sengoku-clan-11-face '((t :foreground "#5fafff")) "Clan color 11." :group 'sengoku-ui)
(defface sengoku-clan-12-face '((t :foreground "#ffff87")) "Clan color 12." :group 'sengoku-ui)
(defface sengoku-clan-13-face '((t :foreground "#875fff")) "Clan color 13." :group 'sengoku-ui)
(defface sengoku-clan-14-face '((t :foreground "#ffaf00")) "Clan color 14." :group 'sengoku-ui)
(defface sengoku-clan-15-face '((t :foreground "#afffff")) "Clan color 15." :group 'sengoku-ui)

(defconst sengoku-ui--clan-faces
  [sengoku-clan-0-face sengoku-clan-1-face sengoku-clan-2-face
   sengoku-clan-3-face sengoku-clan-4-face sengoku-clan-5-face
   sengoku-clan-6-face sengoku-clan-7-face sengoku-clan-8-face
   sengoku-clan-9-face sengoku-clan-10-face sengoku-clan-11-face
   sengoku-clan-12-face sengoku-clan-13-face sengoku-clan-14-face
   sengoku-clan-15-face]
  "Faces reused cyclically for the 32 clans.")

(defvar-local sengoku-session nil
  "The `sengoku-session' displayed in the current game buffer.")

(defvar-local sengoku-ui--suppress-kill-query nil
  "Non-nil while package code intentionally replaces the game buffer.")

(defvar sengoku-ui--resize-hook-installed nil
  "Non-nil after installing `window-size-change-functions' hook.")

(defun sengoku-ui--clan-face (clan-index)
  "Return the display face for CLAN-INDEX."
  (aref sengoku-ui--clan-faces
        (% clan-index (length sengoku-ui--clan-faces))))

(defun sengoku-ui--truncate-width (string width)
  "Return STRING truncated to no more than WIDTH display columns."
  (truncate-string-to-width string (max 0 width) nil nil ""))

(defun sengoku-ui--pad-right (string width)
  "Return STRING truncated and padded to exactly WIDTH display columns."
  (let* ((truncated (sengoku-ui--truncate-width string width))
         (padding (max 0 (- width (string-width truncated)))))
    (concat truncated (make-string padding ?\s))))

(defun sengoku-ui--last (sequence count)
  "Return a fresh list containing the last COUNT elements of SEQUENCE."
  (let* ((items (append sequence nil))
         (drop (max 0 (- (length items) count))))
    (copy-sequence (nthcdr drop items))))

(defun sengoku-ui--message (format-string &rest arguments)
  "Record and display FORMAT-STRING with ARGUMENTS for the current session."
  (let ((text (apply #'format format-string arguments)))
    (when (sengoku-session-p sengoku-session)
      (let ((state (or (sengoku-session-ui-state sengoku-session) nil)))
        (setf (plist-get state :ui-message) text
              (sengoku-session-ui-state sengoku-session) state)))
    (message "%s" text)
    text))

(defun sengoku-ui--current-message ()
  "Return the current session's last UI message, or nil."
  (and (sengoku-session-p sengoku-session)
       (plist-get (sengoku-session-ui-state sengoku-session) :ui-message)))

(defun sengoku-ui--province-cell (game province-index active-index)
  "Return three fixed-width map lines for PROVINCE-INDEX in GAME.
ACTIVE-INDEX identifies the currently commanded province."
  (let* ((province (aref (sengoku-game-provinces game) province-index))
         (inner (- sengoku-data-map-cell-width 2))
         (raw-title (format "%d:%s" (1+ province-index)
                            (sengoku-province-name province)))
         (title (sengoku-ui--truncate-width raw-title inner))
         (line-one
          (concat "+" title
                  (make-string (max 0 (- inner (string-width title))) ?-)
                  "+"))
         (owner (sengoku-province-owner province))
         (owner-text
          (if (>= owner 0)
              (sengoku-clan-abbreviation
               (aref (sengoku-game-clans game) owner))
            "--"))
         (mark
          (cond
           ((= active-index province-index)
            (propertize ">" 'face 'sengoku-active-face))
           ((and (= owner (sengoku-game-player game))
                 (not (zerop (sengoku-province-auto province))))
            (propertize "*" 'face 'sengoku-delegated-face))
           (t " ")))
         (owner-display
          (if (>= owner 0)
              (propertize owner-text 'face (sengoku-ui--clan-face owner))
            owner-text))
         (soldiers
          (format "%d" (sengoku-vim-divide
                         (+ (sengoku-province-soldiers province) 50) 100)))
         (body (concat mark owner-display))
         (body-width (string-width body))
         (soldier-display
          (sengoku-ui--truncate-width soldiers
                                      (max 0 (- inner body-width))))
         (gap (max 0 (- inner body-width
                        (string-width soldier-display))))
         (line-two
          (concat "|" body (make-string gap ?\s) soldier-display "|"))
         (line-three (concat "+" (make-string inner ?-) "+")))
    (list (sengoku-ui--pad-right line-one sengoku-data-map-cell-width)
          (sengoku-ui--pad-right line-two sengoku-data-map-cell-width)
          (sengoku-ui--pad-right line-three sengoku-data-map-cell-width))))

(defun sengoku-ui-strategic-map-lines (game &optional active-index)
  "Return GAME's 21 strategic map lines with optional ACTIVE-INDEX.
Every returned line is exactly 110 display columns."
  (let ((grid (make-hash-table :test #'equal))
        lines)
    (dotimes (province-index (length (sengoku-game-provinces game)))
      (let ((province (aref (sengoku-game-provinces game) province-index)))
        (puthash (cons (sengoku-province-grid-row province)
                       (sengoku-province-grid-column province))
                 province-index grid)))
    (dotimes (row sengoku-data-map-rows)
      (dotimes (subrow 3)
        (let ((line ""))
          (dotimes (column sengoku-data-map-columns)
            (let ((province-index (gethash (cons row column) grid)))
              (setq line
                    (concat
                     line
                     (if (integerp province-index)
                         (nth subrow
                              (sengoku-ui--province-cell
                               game province-index (or active-index -1)))
                       (make-string sengoku-data-map-cell-width ?\s))))))
          (push (sengoku-ui--pad-right
                 line (* sengoku-data-map-cell-width
                         sengoku-data-map-columns))
                lines))))
    (nreverse lines)))

(defun sengoku-ui--power-tags (game)
  "Return GAME's living clan tags sorted by province count descending."
  (let (entries)
    (dotimes (clan-index (length (sengoku-game-clans game)))
      (let ((count (length (sengoku-clan-provinces game clan-index))))
        (when (> count 0)
          (push (list clan-index count) entries))))
    (setq entries
          (cl-stable-sort
           (nreverse entries) #'> :key (lambda (entry) (nth 1 entry))))
    (mapcar
     (lambda (entry)
       (let* ((clan-index (nth 0 entry))
              (tag
               (propertize
                (format "%s%d"
                        (sengoku-clan-abbreviation
                         (aref (sengoku-game-clans game) clan-index))
                        (nth 1 entry))
                'face (sengoku-ui--clan-face clan-index))))
         (if (= clan-index (sengoku-game-player game))
             (concat "[" tag "]")
           tag)))
     entries)))

(defun sengoku-ui--wrap-tags (tags width)
  "Wrap TAGS into lines no wider than WIDTH display columns."
  (let ((current "")
        lines)
    (dolist (tag tags)
      (if (and (not (string-empty-p current))
               (> (string-width (concat current " " tag)) width))
          (progn
            (push current lines)
            (setq current tag))
        (setq current (if (string-empty-p current)
                          tag
                        (concat current " " tag)))))
    (unless (string-empty-p current)
      (push current lines))
    (nreverse lines)))

(defun sengoku-ui--strategic-command-line (session)
  "Return the command-help line appropriate for SESSION."
  (pcase (sengoku-session-phase session)
    ('player-command
     "1開発 2商業 3施し 4徴兵 5訓練 6鉄砲 7出陣 8輸送 9外交 C朝廷 N南蛮 T茶会 B武将 A委任 R解除 i情報 0待機 E残待機 S保存 Q降参")
    ('all-delegated
     "全国委任中… RET/SPC:次の月へ R:委任解除 i:情報 S:保存 Q:降参")
    ('battle-choice "RET/SPC:戦闘の選択を再開")
    ('decision "RET/SPC:申し出への回答を再開")
    ('ended "ゲーム終了 — M-x sengoku で新規開始 / M-x sengoku-load で再開")
    ('setup "大名家を選択してください。")
    (_ "自動進行中…")))

(defun sengoku-ui--insert-lines (lines)
  "Replace the current buffer with LINES while preserving read-only state."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (dolist (line lines)
      (insert line "\n"))
    (goto-char (point-min))))

(defun sengoku-ui-render-strategic (&optional active-index)
  "Render the current session's strategic screen using ACTIVE-INDEX."
  (unless (sengoku-session-p sengoku-session)
    (error "Current buffer has no Sengoku session"))
  (let* ((game (sengoku-session-game sengoku-session))
         (player (sengoku-game-player game))
         (width (* sengoku-data-map-cell-width sengoku-data-map-columns))
         (title
          (propertize
           (format "■ 戦国風雲録  %d年%2d月  〜Emacsで天下統一〜  (全%d国)"
                   (sengoku-game-year game) (sengoku-game-month game)
                   (length (sengoku-game-provinces game)))
           'face 'sengoku-title-face))
         (lines (list title)))
    (when (>= player 0)
      (let* ((state (aref (sengoku-game-clan-states game) player))
             (item-names
              (mapcar
               (lambda (item-index)
                 (plist-get (aref (sengoku-game-items game) item-index) :name))
               (sengoku-clan-state-items state)))
             (ally-names
              (mapcar
               (lambda (clan-index)
                 (sengoku-clan-name
                  (aref (sengoku-game-clans game) clan-index)))
               (sengoku-alliance-list game player)))
             (part-one
              (format "自家: 官位[%s] 文化%d"
                      (aref sengoku-data-ranks
                            (sengoku-clan-state-rank state))
                      (sengoku-culture game player)))
             (part-two
              (format "茶器[%s] 同盟[%s]"
                      (if item-names (string-join item-names "・") "なし")
                      (if ally-names (string-join ally-names "・") "なし"))))
        (if (> (string-width (concat part-one " " part-two)) width)
            (setq lines (append lines (list part-one (concat "      " part-two))))
          (setq lines (append lines (list (concat part-one " " part-two)))))))
    (setq lines (append lines (list "")
                        (sengoku-ui-strategic-map-lines
                         game (or active-index -1))
                        (list
                         "(数字=兵数/百人  >=行動中  *=委任中  隣接は出陣メニュー参照)")))
    (let* ((inner (- width 2))
           (panel-title "-- 勢力 "))
      (setq lines
            (append
             lines
             (list
              (propertize
               (concat "+" panel-title
                       (make-string
                        (max 0 (- inner (string-width panel-title))) ?-)
                       "+")
               'face 'sengoku-panel-face))
             (mapcar
              (lambda (line)
                (concat "|" (sengoku-ui--pad-right
                             (concat " " line) inner) "|"))
              (sengoku-ui--wrap-tags
               (sengoku-ui--power-tags game) (1- inner)))
             (list
              (propertize
               (concat "+" (make-string inner ?-) "+")
               'face 'sengoku-panel-face)
              (make-string width ?-))
             (sengoku-ui--last
              (sengoku-game-log game) sengoku-data-log-display-count)
             (when (sengoku-ui--current-message)
               (list (concat "▶ " (sengoku-ui--current-message))))
             (list (sengoku-ui--strategic-command-line sengoku-session)))))
    (sengoku-ui--insert-lines lines)))

(defun sengoku-ui--siege-cell (siege row column)
  "Return the propertized display cell at ROW, COLUMN in SIEGE."
  (let ((unit (sengoku-siege-unit-at siege row column)))
    (if unit
        (propertize
         (sengoku-siege-unit-label unit)
         'face (if (= (sengoku-siege-unit-side unit) 0)
                   'sengoku-attacker-face
                 'sengoku-defender-face))
      (let ((cell (aref (aref (sengoku-siege-grid siege) row) column)))
        (propertize
         cell 'face
         (pcase cell
           ("森" 'sengoku-forest-face)
           ("壁" 'sengoku-wall-face)
           ("門" 'sengoku-gate-face)
           ("本" 'sengoku-keep-face)
           (_ 'default)))))))

(defun sengoku-ui--siege-active-p (unit active)
  "Return non-nil when UNIT corresponds to detached ACTIVE snapshot."
  (and active
       (= (sengoku-siege-unit-side unit) (sengoku-siege-unit-side active))
       (string= (sengoku-siege-unit-label unit)
                (sengoku-siege-unit-label active))))

(defun sengoku-ui-render-siege ()
  "Render the current session's tactical siege screen."
  (let* ((context (sengoku-session-pending-battle sengoku-session))
         (siege (and context (sengoku-battle-context-siege context))))
    (unless siege
      (error "Current session has no tactical siege"))
    (let* ((game (sengoku-session-game sengoku-session))
           (target (aref (sengoku-game-provinces game)
                         (sengoku-battle-context-to context)))
           (turn (sengoku-siege-player-turn-state context))
           (active (plist-get turn :unit))
           (attacker-units
            (seq-filter
             (lambda (unit) (= (sengoku-siege-unit-side unit) 0))
             (append (sengoku-siege-units siege) nil)))
           (defender-units
            (seq-filter
             (lambda (unit) (= (sengoku-siege-unit-side unit) 1))
             (append (sengoku-siege-units siege) nil)))
           (attacker-soldiers
            (cl-loop for unit in attacker-units
                     when (> (sengoku-siege-unit-soldiers unit) 0)
                     sum (sengoku-siege-unit-soldiers unit)))
           (defender-soldiers
            (cl-loop for unit in defender-units
                     when (> (sengoku-siege-unit-soldiers unit) 0)
                     sum (sengoku-siege-unit-soldiers unit)))
           (attacker-clan
            (aref (sengoku-game-clans game)
                  (sengoku-siege-attacker-clan siege)))
           (defender-index (sengoku-siege-defender-clan siege))
           (defender-name
            (if (>= defender-index 0)
                (sengoku-clan-name
                 (aref (sengoku-game-clans game) defender-index))
              "無主"))
           (lines
            (list
             (propertize
              (format "■ %s城 攻防戦  %d日目/%d日"
                      (sengoku-province-name target)
                      (sengoku-siege-day siege)
                      sengoku-data-siege-max-days)
              'face 'sengoku-title-face)
             (format "攻手:%s軍(大将:%s) 兵%d%s  守手:%s軍(守将:%s) 兵%d%s"
                     (sengoku-clan-name attacker-clan)
                     (sengoku-siege-attacker-general-name siege)
                     attacker-soldiers
                     (if (= (sengoku-siege-player-side siege) 0) "★" "")
                     defender-name
                     (sengoku-siege-defender-general-name siege)
                     defender-soldiers
                     (if (= (sengoku-siege-player-side siege) 1) "★" ""))
             "")))
      (dotimes (row sengoku-data-siege-rows)
        (let ((cells ""))
          (dotimes (column sengoku-data-siege-columns)
            (setq cells (concat cells
                                (sengoku-ui--siege-cell siege row column))))
          (let ((line (sengoku-ui--pad-right (concat "  " cells) 30)))
            (when (and (>= row 1) (<= row 5))
              (let ((index (1- row)))
                (setq line
                      (concat
                       line
                       (if (< index (length attacker-units))
                           (let ((unit (nth index attacker-units)))
                             (sengoku-ui--pad-right
                              (format "攻%s 兵%d 砲%d 筒%d%s"
                                      (sengoku-siege-unit-label unit)
                                      (sengoku-siege-unit-soldiers unit)
                                      (sengoku-siege-unit-guns unit)
                                      (sengoku-siege-unit-cannon unit)
                                      (cond
                                       ((<= (sengoku-siege-unit-soldiers unit) 0)
                                        "【壊滅】")
                                       ((sengoku-ui--siege-active-p unit active) "←")
                                       (t "")))
                              24))
                         (make-string 24 ?\s))
                       (if (< index (length defender-units))
                           (let ((unit (nth index defender-units)))
                             (format "守%s 兵%d 砲%d 筒%d%s"
                                     (sengoku-siege-unit-label unit)
                                     (sengoku-siege-unit-soldiers unit)
                                     (sengoku-siege-unit-guns unit)
                                     (sengoku-siege-unit-cannon unit)
                                     (cond
                                      ((<= (sengoku-siege-unit-soldiers unit) 0)
                                       "【壊滅】")
                                      ((sengoku-ui--siege-active-p unit active) "←")
                                      (t ""))))
                         "")))))
            (setq lines (append lines (list line))))))
      (setq lines
            (append
             lines
             (list ""
                   "・:平地 森:森(守+) 壁:城壁 門:城門 本:本丸 | １-５:攻手 甲-戊:守手")
             (sengoku-ui--last (sengoku-siege-messages siege) 4)
             (when (sengoku-ui--current-message)
               (list (concat "▶ " (sengoku-ui--current-message))))
             (list
              (if active
                  (format "【%s隊】兵%d 砲%d 筒%d 移動残%d | hjkl/矢印:移動 a:攻撃 f:射撃 w/SPC/RET:待機 Q:%s"
                          (sengoku-siege-unit-label active)
                          (sengoku-siege-unit-soldiers active)
                          (sengoku-siege-unit-guns active)
                          (sengoku-siege-unit-cannon active)
                          (plist-get turn :move-points)
                          (if (= (sengoku-siege-player-side siege) 0)
                              "全軍退却" "開城"))
                "籠城戦を自動進行中…"))))
      (sengoku-ui--insert-lines lines))))

(defun sengoku-ui-render ()
  "Render the current game buffer according to its session phase."
  (interactive)
  (unless (derived-mode-p 'sengoku-mode)
    (user-error "現在のバッファは戦国風雲録ではありません"))
  (if (eq (sengoku-session-phase sengoku-session) 'siege)
      (sengoku-ui-render-siege)
    (let ((active
           (pcase (sengoku-session-phase sengoku-session)
             ('player-command (sengoku-session-active-province sengoku-session))
             ('battle-choice
              (let ((battle (sengoku-session-pending-battle sengoku-session)))
                (if battle (sengoku-battle-context-to battle) -1)))
             (_ -1))))
      (sengoku-ui-render-strategic active))))

(defun sengoku-ui--warn-small-window ()
  "Warn when the selected game window is smaller than recommended."
  (when (or (< (window-body-width) sengoku-recommended-width)
            (< (window-body-height) sengoku-recommended-height))
    (message "戦国風雲録: 推奨ウィンドウサイズは%d桁×%d行以上です"
             sengoku-recommended-width sengoku-recommended-height)))

(defun sengoku-ui--window-size-change (frame)
  "Rerender visible Sengoku buffers on FRAME after a size change."
  (dolist (window (window-list frame 'no-minibuffer))
    (with-current-buffer (window-buffer window)
      (when (and (derived-mode-p 'sengoku-mode)
                 (sengoku-session-p sengoku-session))
        (sengoku-ui-render)))))

(defun sengoku-ui--remove-resize-hook ()
  "Remove the global resize hook after the last Sengoku buffer is killed."
  (unless
      (seq-some
       (lambda (buffer)
         (and (not (eq buffer (current-buffer)))
              (buffer-live-p buffer)
              (with-current-buffer buffer
                (derived-mode-p 'sengoku-mode))))
       (buffer-list))
    (remove-hook 'window-size-change-functions
                 #'sengoku-ui--window-size-change)
    (setq sengoku-ui--resize-hook-installed nil)))

(defun sengoku-ui--kill-query ()
  "Ask how to handle a live game before killing its buffer."
  (or sengoku-ui--suppress-kill-query
      (not (and (sengoku-session-p sengoku-session)
                (not (eq (sengoku-session-phase sengoku-session) 'ended))))
      (cond
       ((yes-or-no-p "戦国風雲録をセーブして閉じますか? ")
        (condition-case error-data
            (progn
              (sengoku-save-game (sengoku-session-game sengoku-session))
              t)
          (error
           (message "セーブできません: %s" (error-message-string error-data))
           nil)))
       ((yes-or-no-p "セーブせずに閉じますか? ") t)
       (t nil))))

(defvar sengoku-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "1") #'sengoku-ui-develop)
    (define-key map (kbd "2") #'sengoku-ui-commerce)
    (define-key map (kbd "3") #'sengoku-ui-relief)
    (define-key map (kbd "4") #'sengoku-ui-recruit)
    (define-key map (kbd "5") #'sengoku-ui-train)
    (define-key map (kbd "6") #'sengoku-ui-buy-guns)
    (define-key map (kbd "7") #'sengoku-ui-attack)
    (define-key map (kbd "8") #'sengoku-ui-transport)
    (define-key map (kbd "9") #'sengoku-ui-diplomacy)
    (define-key map (kbd "T") #'sengoku-ui-tea)
    (define-key map (kbd "t") #'sengoku-ui-tea)
    (define-key map (kbd "C") #'sengoku-ui-court)
    (define-key map (kbd "c") #'sengoku-ui-court)
    (define-key map (kbd "N") #'sengoku-ui-nanban)
    (define-key map (kbd "n") #'sengoku-ui-nanban)
    (define-key map (kbd "B") #'sengoku-ui-show-generals)
    (define-key map (kbd "b") #'sengoku-ui-show-generals)
    (define-key map (kbd "A") #'sengoku-ui-delegate)
    (define-key map (kbd "R") #'sengoku-ui-release)
    (define-key map (kbd "i") #'sengoku-ui-show-province)
    (define-key map (kbd "I") #'sengoku-ui-show-province)
    (define-key map (kbd "0") #'sengoku-ui-wait)
    (define-key map (kbd "SPC") #'sengoku-ui-space)
    (define-key map (kbd "RET") #'sengoku-ui-return)
    (define-key map (kbd "E") #'sengoku-ui-skip)
    (define-key map (kbd "S") #'sengoku-ui-save)
    (define-key map (kbd "Q") #'sengoku-ui-quit)
    (define-key map (kbd "h") #'sengoku-ui-siege-left)
    (define-key map (kbd "j") #'sengoku-ui-siege-down)
    (define-key map (kbd "k") #'sengoku-ui-siege-up)
    (define-key map (kbd "l") #'sengoku-ui-siege-right)
    (define-key map (kbd "<left>") #'sengoku-ui-siege-left)
    (define-key map (kbd "<down>") #'sengoku-ui-siege-down)
    (define-key map (kbd "<up>") #'sengoku-ui-siege-up)
    (define-key map (kbd "<right>") #'sengoku-ui-siege-right)
    (define-key map (kbd "a") #'sengoku-ui-siege-attack)
    (define-key map (kbd "f") #'sengoku-ui-siege-fire)
    (define-key map (kbd "w") #'sengoku-ui-siege-wait)
    map)
  "Keymap for `sengoku-mode'.")

(define-derived-mode sengoku-mode special-mode "Sengoku"
  "Major mode for playing Sengoku in an Emacs buffer."
  (setq-local truncate-lines t)
  (setq-local word-wrap nil)
  (setq-local cursor-type 'box)
  (setq-local buffer-read-only t)
  (buffer-face-set 'fixed-pitch)
  (add-hook 'kill-buffer-query-functions #'sengoku-ui--kill-query nil t)
  (add-hook 'kill-buffer-hook #'sengoku-ui--remove-resize-hook nil t)
  (unless sengoku-ui--resize-hook-installed
    (add-hook 'window-size-change-functions #'sengoku-ui--window-size-change)
    (setq sengoku-ui--resize-hook-installed t)))

(defun sengoku-ui-current-session ()
  "Return the live session in the main game buffer, or nil."
  (let ((buffer (get-buffer sengoku-buffer-name)))
    (when (buffer-live-p buffer)
      (buffer-local-value 'sengoku-session buffer))))

(defun sengoku-ui-open-session (session)
  "Display SESSION in the main Sengoku buffer and return that buffer."
  (unless (sengoku-session-p session)
    (error "Not a Sengoku session: %S" session))
  (let ((buffer (get-buffer-create sengoku-buffer-name)))
    (pop-to-buffer buffer)
    (unless (derived-mode-p 'sengoku-mode)
      (sengoku-mode))
    (setq sengoku-session session)
    (sengoku-ui--warn-small-window)
    (sengoku-ui-render)
    buffer))

(defun sengoku-ui--selection (prompt values formatter)
  "Display numbered VALUES with PROMPT and FORMATTER, then read one.
Return nil when VALUES is empty, the user enters nothing, or the user quits."
  (when values
    (let* ((choices
            (cl-loop for value in values
                     for number from 1
                     collect
                     (cons (format "%d. %s" number
                                   (funcall formatter value))
                           value)))
           (completion-table
            (lambda (string predicate action)
              (if (eq action 'metadata)
                  '(metadata
                    (display-sort-function . identity)
                    (cycle-sort-function . identity))
                (complete-with-action action choices string predicate))))
           (answer
            (condition-case nil
                (minibuffer-with-setup-hook #'minibuffer-completion-help
                  (completing-read
                   (format "%s (1-%d): "
                           (string-remove-suffix ": " prompt)
                           (length choices))
                   completion-table nil nil))
              (quit nil))))
      (when answer
        (let* ((exact (assoc answer choices))
               (number-text (string-trim answer))
               (number
                (and (string-match-p "\\`[[:digit:]]+\\'" number-text)
                     (string-to-number number-text))))
          (cond
           ((string-empty-p number-text) nil)
           (exact (cdr exact))
           ((and number (<= 1 number) (<= number (length choices)))
            (cdr (nth (1- number) choices)))
           (t
            (user-error "選択肢は1から%dの番号で選択してください"
                        (length choices)))))))))

(defun sengoku-ui--number (prompt maximum)
  "Read a positive integer for PROMPT no greater than MAXIMUM, or nil."
  (when (> maximum 0)
    (condition-case nil
        (let ((number
               (read-number (format "%s (最大%d, 空欄=中止): "
                                    prompt maximum)
                            0)))
          (and (integerp number) (> number 0) (<= number maximum) number))
      (quit nil))))

(defun sengoku-ui--yes-or-no (prompt &optional strong)
  "Ask PROMPT and return t, nil, or `cancel' on quit.
Use `yes-or-no-p' instead of `y-or-n-p' when STRONG is non-nil."
  (condition-case nil
      (if (funcall (if strong #'yes-or-no-p #'y-or-n-p) prompt) t nil)
    (quit 'cancel)))

(defun sengoku-ui--player-command-p ()
  "Return non-nil when the current session awaits a strategic command."
  (and (sengoku-session-p sengoku-session)
       (eq (sengoku-session-phase sengoku-session) 'player-command)))

(defun sengoku-ui--require-player-command ()
  "Return non-nil at a strategic command prompt, otherwise display a message."
  (or (sengoku-ui--player-command-p)
      (progn
        (sengoku-ui--message "現在は領国コマンドを実行できません。")
        nil)))

(defun sengoku-ui--game ()
  "Return the current buffer's game."
  (sengoku-session-game sengoku-session))

(defun sengoku-ui--active-province ()
  "Return the current buffer's active province record."
  (aref (sengoku-game-provinces (sengoku-ui--game))
        (sengoku-session-active-province sengoku-session)))

(defun sengoku-ui--read-general (prompt ability)
  "Read an unused general using PROMPT and display ABILITY first."
  (let* ((game (sengoku-ui--game))
         (player (sengoku-game-player game))
         (reports
          (seq-filter
           (lambda (report) (not (plist-get report :used)))
           (sengoku-engine-general-report game player))))
    (if reports
        (sengoku-ui--selection
         prompt reports
         (lambda (report)
           (if (eq ability 'war)
               (format "%s (戦%d 政%d)"
                       (plist-get report :name)
                       (plist-get report :war)
                       (plist-get report :politics))
             (format "%s (政%d 戦%d)"
                     (plist-get report :name)
                     (plist-get report :politics)
                     (plist-get report :war)))))
      (sengoku-ui--message "この月に行動できる武将はもういません。")
      nil)))

(defun sengoku-ui--run-command (action &rest arguments)
  "Run strategic ACTION with ARGUMENTS and advance after success."
  (when (sengoku-ui--require-player-command)
    (let ((result (apply #'sengoku-engine-command
                         sengoku-session action arguments)))
      (sengoku-ui--message "%s" (plist-get result :message))
      (if (eq (plist-get result :status) 'error)
          (sengoku-ui-render)
        (sengoku-ui-advance))
      result)))

(defun sengoku-ui-advance ()
  "Advance the current session to its next input boundary and render it."
  (interactive)
  (unless (sengoku-session-p sengoku-session)
    (user-error "ゲームセッションがありません"))
  (let ((continue t)
        result)
    (while continue
      (setq result (sengoku-engine-advance sengoku-session))
      (pcase (plist-get result :status)
        ('battle-choice
         (sengoku-ui-render)
         (let ((answer
                (sengoku-ui--yes-or-no
                 "自ら采配を振るいますか? (y=籠城戦 / n=自動解決) ")))
           (if (eq answer 'cancel)
               (progn
                 (sengoku-ui--message "戦闘の選択を保留しました。")
                 (setq continue nil))
             (setq result
                   (sengoku-engine-resolve-battle-choice
                    sengoku-session (if answer 'siege 'automatic))))))
        ('decision
         (sengoku-ui-render)
         (let* ((decision (plist-get result :decision))
                (answer
                 (sengoku-ui--yes-or-no
                  (concat (plist-get decision :message) " 受けますか? "))))
           (if (eq answer 'cancel)
               (progn
                 (sengoku-ui--message "回答を保留しました。")
                 (setq continue nil))
             (sengoku-engine-resolve-decision sengoku-session answer))))
        ((or 'player-command 'all-delegated 'siege-turn
             'observer-month-complete 'setup 'game-over)
         (sengoku-ui--message "%s" (plist-get result :message))
         (setq continue nil))
        (_
         (sengoku-ui--message "%s" (or (plist-get result :message) "")))))
    (sengoku-ui-render)
    result))

(defun sengoku-ui-develop ()
  "Develop the active province."
  (interactive)
  (when (sengoku-ui--require-player-command)
    (let ((general (sengoku-ui--read-general "開発を任せる武将: " 'politics)))
      (when general
        (sengoku-ui--run-command 'develop :general
                                 (plist-get general :index))))))

(defun sengoku-ui-commerce ()
  "Invest in commerce in the active province."
  (interactive)
  (when (sengoku-ui--require-player-command)
    (let ((general (sengoku-ui--read-general "商業を任せる武将: " 'politics)))
      (when general
        (sengoku-ui--run-command 'commerce :general
                                 (plist-get general :index))))))

(defun sengoku-ui-relief ()
  "Distribute relief in the active province."
  (interactive)
  (when (sengoku-ui--require-player-command)
    (let ((general (sengoku-ui--read-general "施しを任せる武将: " 'politics)))
      (when general
        (sengoku-ui--run-command 'relief :general
                                 (plist-get general :index))))))

(defun sengoku-ui-recruit ()
  "Recruit soldiers in the active province."
  (interactive)
  (when (sengoku-ui--require-player-command)
    (let* ((province (sengoku-ui--active-province))
           (maximum (sengoku-engine-recruit-maximum province)))
      (if (< maximum 100)
          (sengoku-ui--run-command 'recruit :general -1 :quantity 0)
        (let ((general
               (sengoku-ui--read-general "徴兵を任せる武将: " 'politics)))
          (when general
            (let ((quantity (sengoku-ui--number "徴兵数" maximum)))
              (when quantity
                (sengoku-ui--run-command
                 'recruit :general (plist-get general :index)
                 :quantity quantity)))))))))

(defun sengoku-ui-train ()
  "Train troops in the active province."
  (interactive)
  (when (sengoku-ui--require-player-command)
    (let ((general (sengoku-ui--read-general "訓練を任せる武将: " 'war)))
      (when general
        (sengoku-ui--run-command 'train :general
                                 (plist-get general :index))))))

(defun sengoku-ui-buy-guns ()
  "Buy firearms in the active province."
  (interactive)
  (sengoku-ui--run-command 'buy-guns))

(defun sengoku-ui-attack ()
  "Attack an adjacent enemy province."
  (interactive)
  (when (sengoku-ui--require-player-command)
    (let* ((game (sengoku-ui--game))
           (player (sengoku-game-player game))
           (from (sengoku-session-active-province sengoku-session))
           (targets (sengoku-engine-attack-targets game player from))
           (target
            (sengoku-ui--selection
             "出陣先: " targets
             (lambda (province-index)
               (let* ((province (aref (sengoku-game-provinces game)
                                      province-index))
                      (owner (sengoku-province-owner province)))
                 (format "%s (%s領 兵%d)"
                         (sengoku-province-name province)
                         (if (>= owner 0)
                             (sengoku-clan-name
                              (aref (sengoku-game-clans game) owner))
                           "無主")
                         (sengoku-province-soldiers province)))))))
      (if (null targets)
          (sengoku-ui--run-command
           'attack :target -1 :general -1 :quantity 0)
        (when target
          (let ((general
                 (sengoku-ui--read-general "出陣の大将: " 'war)))
            (when general
              (let ((quantity
                     (sengoku-ui--number
                      "出陣する兵数"
                      (sengoku-province-soldiers
                       (sengoku-ui--active-province)))))
                (when quantity
                  (sengoku-ui--run-command
                   'attack :target target :general (plist-get general :index)
                   :quantity quantity))))))))))

(defun sengoku-ui-transport ()
  "Transport resources to an adjacent friendly province."
  (interactive)
  (when (sengoku-ui--require-player-command)
    (let* ((game (sengoku-ui--game))
           (player (sengoku-game-player game))
           (from (sengoku-session-active-province sengoku-session))
           (targets (sengoku-engine-transport-targets game player from))
           (target
            (sengoku-ui--selection
             "輸送先: " targets
             (lambda (province-index)
               (sengoku-province-name
                (aref (sengoku-game-provinces game) province-index))))))
      (if (null targets)
          (sengoku-ui--run-command
           'transport :target -1 :kind 'gold :quantity 0)
        (when target
          (let* ((province (sengoku-ui--active-province))
                 (kind
                  (sengoku-ui--selection
                   "何を送るか: " '(gold rice soldiers)
                   (lambda (value)
                     (pcase value
                       ('gold (format "金 (%d)" (sengoku-province-gold province)))
                       ('rice (format "米 (%d)" (sengoku-province-rice province)))
                       (_ (format "兵 (%d)" (sengoku-province-soldiers province))))))))
            (when kind
              (let* ((maximum
                      (pcase kind
                        ('gold (sengoku-province-gold province))
                        ('rice (sengoku-province-rice province))
                        (_ (sengoku-province-soldiers province))))
                     (quantity (sengoku-ui--number "送る量" maximum)))
                (when quantity
                  (sengoku-ui--run-command
                   'transport :target target :kind kind
                   :quantity quantity))))))))))

(defun sengoku-ui-show-clans ()
  "Show a non-consuming report of all living clans."
  (interactive)
  (let* ((game (sengoku-ui--game))
         (reports (sengoku-engine-clan-report game)))
    (with-help-window "*戦国風雲録 諸家*"
      (princ "== 諸家の状況 ==\n\n")
      (dolist (report reports)
        (let ((item-names
               (mapcar
                (lambda (item-index)
                  (plist-get (aref (sengoku-game-items game) item-index) :name))
                (plist-get report :items)))
              (ally-names
               (mapcar
                (lambda (clan-index)
                  (sengoku-clan-name
                   (aref (sengoku-game-clans game) clan-index)))
                (plist-get report :allies))))
          (princ
           (format "%s%s家 %d国 %s 文化%d 茶器[%s] 同盟[%s]\n"
                   (if (plist-get report :player) "*" " ")
                   (plist-get report :name)
                   (plist-get report :provinces)
                   (plist-get report :rank)
                   (plist-get report :culture)
                   (if item-names (string-join item-names "・") "なし")
                   (if ally-names (string-join ally-names "・") "なし"))))))))

(defun sengoku-ui-diplomacy ()
  "Open the diplomacy menu for the active province."
  (interactive)
  (when (sengoku-ui--require-player-command)
    (let ((choice
           (sengoku-ui--selection
            "外交: " '(propose break report)
            (lambda (value)
              (pcase value
                ('propose "同盟を申し入れる (贈物 金500)")
                ('break "同盟を破棄する")
                (_ "諸家の状況を見る"))))))
      (pcase choice
        ('report (sengoku-ui-show-clans))
        ('propose
         (let* ((game (sengoku-ui--game))
                (player (sengoku-game-player game))
                (targets (sengoku-engine-alliance-candidates game player))
                (target
                 (sengoku-ui--selection
                  "同盟の相手: " targets
                  (lambda (clan-index)
                    (format "%s家 (%d国)"
                            (sengoku-clan-name
                             (aref (sengoku-game-clans game) clan-index))
                            (length (sengoku-clan-provinces
                                     game clan-index)))))))
           (when target
             (let ((general
                    (sengoku-ui--read-general "使者に立てる武将: " 'politics)))
               (when general
                 (sengoku-ui--run-command
                  'propose-alliance :target target
                  :general (plist-get general :index)))))))
        ('break
         (let* ((game (sengoku-ui--game))
                (player (sengoku-game-player game))
                (allies (sengoku-alliance-list game player))
                (target
                 (sengoku-ui--selection
                  "破棄する同盟: " allies
                  (lambda (clan-index)
                    (concat
                     (sengoku-clan-name
                      (aref (sengoku-game-clans game) clan-index))
                     "家")))))
           (when target
             (sengoku-ui--run-command 'break-alliance :target target))))))))

(defun sengoku-ui-court ()
  "Open the imperial-court menu for the active province."
  (interactive)
  (when (sengoku-ui--require-player-command)
    (let ((choice
           (sengoku-ui--selection
            "朝廷: " '(rank peace)
            (lambda (value)
              (if (eq value 'rank)
                  "献金して官位を賜る (金1000)"
                "勅命和睦を奏請する (金2000・要官位)")))))
      (pcase choice
        ('rank
         (let ((general
                (sengoku-ui--read-general "京へ上らせる使者: " 'politics)))
           (when general
             (sengoku-ui--run-command
              'court-rank :general (plist-get general :index)))))
        ('peace
         (let* ((game (sengoku-ui--game))
                (player (sengoku-game-player game))
                (targets (sengoku-engine-alliance-candidates game player))
                (target
                 (sengoku-ui--selection
                  "勅命和睦の相手: " targets
                  (lambda (clan-index)
                    (format "%s家 (%d国)"
                            (sengoku-clan-name
                             (aref (sengoku-game-clans game) clan-index))
                            (length (sengoku-clan-provinces
                                     game clan-index)))))))
           (when target
             (let ((general
                    (sengoku-ui--read-general "京へ上らせる使者: " 'politics)))
               (when general
                 (sengoku-ui--run-command
                  'court-peace :target target
                  :general (plist-get general :index)))))))))))

(defun sengoku-ui-nanban ()
  "Open the southern-barbarian trade menu."
  (interactive)
  (when (sengoku-ui--require-player-command)
    (let* ((game (sengoku-ui--game))
           (player (sengoku-game-player game))
           (prices (sengoku-engine-nanban-prices game player))
           (kind
            (sengoku-ui--selection
             "南蛮商人: " '(guns cannon rare)
             (lambda (value)
               (pcase value
                 ('guns
                  (format "鉄砲%d挺を買う (金%d)"
                          (plist-get prices :guns)
                          (plist-get prices :gun-price)))
                 ('cannon
                  (format "大筒1門を買う (金%d)"
                          (plist-get prices :cannon-price)))
                 (_
                  (format "南蛮の珍品を買う (金%d) 文化+5"
                          (plist-get prices :rare-price))))))))
      (when kind
        (sengoku-ui--run-command 'nanban :kind kind)))))

(defun sengoku-ui-tea ()
  "Hold a tea gathering."
  (interactive)
  (sengoku-ui--run-command 'tea))

(defun sengoku-ui-show-generals ()
  "Show the player's general roster without consuming an action."
  (interactive)
  (let* ((game (sengoku-ui--game))
         (player (sengoku-game-player game))
         (reports (sengoku-engine-general-report game player)))
    (with-help-window "*戦国風雲録 武将*"
      (princ "== 自家の武将 (毎月1人1回行動) ==\n\n")
      (dolist (report reports)
        (princ
         (format " %s %s%s 戦%d 政%d\n"
                 (if (plist-get report :used) "済" "　")
                 (plist-get report :name)
                 (if (plist-get report :lord) "(当主)" "")
                 (plist-get report :war)
                 (plist-get report :politics)))))))

(defun sengoku-ui-delegate ()
  "Delegate the active province to its AI."
  (interactive)
  (sengoku-ui--run-command 'delegate))

(defun sengoku-ui-release ()
  "Release one delegated province."
  (interactive)
  (let* ((game (sengoku-ui--game))
         (player (sengoku-game-player game))
         (targets (sengoku-engine-release-candidates game player))
         (target
          (sengoku-ui--selection
           "委任を解除する国: " targets
           (lambda (province-index)
             (sengoku-province-name
              (aref (sengoku-game-provinces game) province-index))))))
    (cond
     ((null targets)
      (sengoku-ui--message "委任中の国はありません。"))
     ((null target) nil)
     ((eq (sengoku-session-phase sengoku-session) 'all-delegated)
      (let ((result
             (sengoku-engine-release-delegation sengoku-session target)))
        (sengoku-ui--message "%s" (plist-get result :message))
        (sengoku-ui-render)))
     ((sengoku-ui--player-command-p)
      (sengoku-ui--run-command 'release :target target))
     (t (sengoku-ui--message "現在は委任を解除できません。")))))

(defun sengoku-ui-show-province ()
  "Show detailed information for one province without consuming an action."
  (interactive)
  (let* ((game (sengoku-ui--game))
         (indices (number-sequence 0 (1- (length (sengoku-game-provinces game)))))
         (province-index
          (sengoku-ui--selection
           "情報を見る国: " indices
           (lambda (index)
             (let* ((province (aref (sengoku-game-provinces game) index))
                    (owner (sengoku-province-owner province)))
               (format "%s (%s)"
                       (sengoku-province-name province)
                       (if (>= owner 0)
                           (sengoku-clan-name
                            (aref (sengoku-game-clans game) owner))
                         "--")))))))
    (when (integerp province-index)
      (let* ((report (sengoku-engine-province-report game province-index))
             (adjacent
              (mapcar
               (lambda (index)
                 (sengoku-province-name
                  (aref (sengoku-game-provinces game) index)))
               (plist-get report :adjacent))))
        (with-help-window "*戦国風雲録 国情報*"
          (princ
           (format "【%s】 領主:%s(%s)%s\n\n"
                   (plist-get report :name)
                   (or (plist-get report :clan-name) "--")
                   (or (plist-get report :daimyo) "--")
                   (if (plist-get report :delegated) " [委任中]" "")))
          (princ
           (format "石高:%d 商業:%d 民忠:%d%s\n兵数:%d 訓練:%d 鉄砲:%d 大筒:%d\n金:%d 米:%d\n隣接: %s\n"
                   (plist-get report :koku)
                   (plist-get report :commerce)
                   (plist-get report :loyalty)
                   (if (plist-get report :port) " 【南蛮港】" "")
                   (plist-get report :soldiers)
                   (plist-get report :training)
                   (plist-get report :guns)
                   (plist-get report :cannon)
                   (plist-get report :gold)
                   (plist-get report :rice)
                   (string-join adjacent "・"))))))))

(defun sengoku-ui-wait ()
  "Wait in the active province."
  (interactive)
  (sengoku-ui--run-command 'wait))

(defun sengoku-ui-skip ()
  "Skip all remaining direct provinces this month."
  (interactive)
  (sengoku-ui--run-command 'skip))

(defun sengoku-ui-save ()
  "Save the current campaign without consuming an action."
  (interactive)
  (condition-case error-data
      (let ((path (sengoku-save-game (sengoku-ui--game))))
        (sengoku-ui--message "セーブしました: %s" path)
        (sengoku-ui-render))
    (error
     (sengoku-ui--message "セーブできません: %s"
                          (error-message-string error-data)))))

(defun sengoku-ui-return ()
  "Continue a pending prompt, or wait during a siege."
  (interactive)
  (pcase (sengoku-session-phase sengoku-session)
    ('siege (sengoku-ui-siege-wait))
    ('all-delegated
     (sengoku-engine-continue-all-delegated sengoku-session)
     (sengoku-ui-advance))
    ((or 'battle-choice 'decision) (sengoku-ui-advance))
    (_ (sengoku-ui--message "この場面ではRETを使いません。"))))

(defun sengoku-ui-space ()
  "Wait, continue an all-delegated month, or wait during a siege."
  (interactive)
  (pcase (sengoku-session-phase sengoku-session)
    ('player-command (sengoku-ui-wait))
    ('siege (sengoku-ui-siege-wait))
    (_ (sengoku-ui-return))))

(defun sengoku-ui-quit ()
  "Surrender the campaign, retreat, or open the castle during a siege."
  (interactive)
  (if (eq (sengoku-session-phase sengoku-session) 'siege)
      (let* ((context (sengoku-session-pending-battle sengoku-session))
             (siege (sengoku-battle-context-siege context))
             (attacker (= (sengoku-siege-player-side siege) 0))
             (answer
              (sengoku-ui--yes-or-no
               (if attacker
                   "本当に全軍退却しますか? "
                 "本当に開城しますか? ")
               t)))
        (when (eq answer t)
          (if attacker
              (sengoku-siege-player-retreat context)
            (sengoku-siege-player-surrender context))
          (sengoku-ui-advance)))
    (let ((answer
           (sengoku-ui--yes-or-no "本当に降参して終了しますか? " t)))
      (when (eq answer t)
        (setf (sengoku-session-phase sengoku-session) 'ended
              (sengoku-session-quit-reason sengoku-session) 'surrendered)
        (sengoku-ui--message "乱世に幕を下ろしました。")
        (sengoku-ui-render)))))

(defun sengoku-ui--siege-context ()
  "Return the pending tactical context, or display an error and return nil."
  (if (eq (sengoku-session-phase sengoku-session) 'siege)
      (sengoku-session-pending-battle sengoku-session)
    (sengoku-ui--message "現在は籠城戦ではありません。")
    nil))

(defun sengoku-ui--siege-move (direction)
  "Move the active tactical unit one step in DIRECTION."
  (let ((context (sengoku-ui--siege-context)))
    (when context
      (let ((result (sengoku-siege-player-move context direction)))
        (pcase result
          ('blocked (sengoku-ui--message "そこへは移動できません。"))
          ('insufficient-move (sengoku-ui--message "移動力が足りません。"))
          (_ (sengoku-ui--message "部隊を移動しました。")))
        (sengoku-ui-advance)))))

(defun sengoku-ui-siege-left () "Move one tactical step left." (interactive) (sengoku-ui--siege-move 'left))
(defun sengoku-ui-siege-right () "Move one tactical step right." (interactive) (sengoku-ui--siege-move 'right))
(defun sengoku-ui-siege-up () "Move one tactical step up." (interactive) (sengoku-ui--siege-move 'up))
(defun sengoku-ui-siege-down () "Move one tactical step down." (interactive) (sengoku-ui--siege-move 'down))

(defun sengoku-ui--adjacent-target-label (target)
  "Return a completion label for adjacent tactical TARGET."
  (if (eq (plist-get target :kind) 'unit)
      (let ((unit (plist-get target :unit)))
        (format "%s隊 (兵%d)" (sengoku-siege-unit-label unit)
                (sengoku-siege-unit-soldiers unit)))
    (format "%s (耐久%d)"
            (if (eq (plist-get target :terrain) 'gate) "城門" "城壁")
            (plist-get target :hit-points))))

(defun sengoku-ui-siege-attack ()
  "Attack an adjacent siege target."
  (interactive)
  (let ((context (sengoku-ui--siege-context)))
    (when context
      (let* ((targets (sengoku-siege-player-adjacent-targets context))
             (target
              (cond
               ((null targets) nil)
               ((null (cdr targets)) 0)
               (t
                (let ((selected
                       (sengoku-ui--selection
                        "攻撃目標: "
                        (number-sequence 0 (1- (length targets)))
                        (lambda (index)
                          (sengoku-ui--adjacent-target-label
                           (nth index targets))))))
                  selected)))))
        (cond
         ((null targets)
          (sengoku-siege-player-attack context)
          (sengoku-ui--message "攻撃できる相手が隣接していません。")
          (sengoku-ui-render))
         ((integerp target)
          (sengoku-siege-player-attack context target)
          (sengoku-ui--message "攻撃しました。")
          (sengoku-ui-advance)))))))

(defun sengoku-ui-siege-fire ()
  "Fire at a siege target within range two."
  (interactive)
  (let ((context (sengoku-ui--siege-context)))
    (when context
      (let* ((targets (sengoku-siege-player-fire-targets context))
             (target
              (cond
               ((null targets) nil)
               ((null (cdr targets)) 0)
               (t
                (sengoku-ui--selection
                 "射撃目標: "
                 (number-sequence 0 (1- (length targets)))
                 (lambda (index)
                   (let ((unit (nth index targets)))
                     (format "%s隊 (兵%d)"
                             (sengoku-siege-unit-label unit)
                             (sengoku-siege-unit-soldiers unit)))))))))
        (cond
         ((integerp target)
          (sengoku-siege-player-fire context target)
          (sengoku-ui--message "射撃しました。")
          (sengoku-ui-advance))
         (targets nil)
         (t
          (let ((result (sengoku-siege-player-fire context)))
            (sengoku-ui--message
             "%s" (pcase result
                    ('no-weapons "鉄砲も大筒も持っていません。")
                    (_ "射程内に敵がいません。")))
            (sengoku-ui-render))))))))

(defun sengoku-ui-siege-wait ()
  "End the active tactical unit's turn."
  (interactive)
  (let ((context (sengoku-ui--siege-context)))
    (when context
      (sengoku-siege-player-wait context)
      (sengoku-ui--message "部隊を待機させました。")
      (sengoku-ui-advance))))

(provide 'sengoku-ui)

;;; sengoku-ui.el ends here
