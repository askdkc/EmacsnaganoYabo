;;; sengoku.el --- Sengoku simulation game for Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2026 dkc

;; Author: dkc
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: games

;;; Commentary:

;; Public entry points and lifecycle management for the Emacs port of
;; 戦国風雲録.  Use `M-x sengoku' to start a campaign or `M-x sengoku-load'
;; to resume a version 5 save shared with the Vim implementation.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'sengoku-ui)

(defconst sengoku-version "0.1.0"
  "Current Sengoku package version.")

(defconst sengoku-self-test-preview-buffer-name
  "*戦国風雲録 セルフテスト描画*"
  "Buffer name used for the strategic rendering self-test preview.")

(defun sengoku--active-session ()
  "Return the current live campaign session, or nil."
  (let ((session (sengoku-ui-current-session)))
    (and (sengoku-session-p session)
         (not (eq (sengoku-session-phase session) 'ended))
         session)))

(defun sengoku--confirm-replacement ()
  "Return non-nil when a running campaign may be replaced.
Offer to save the current campaign first.  Quitting either confirmation leaves
that campaign untouched."
  (let ((session (sengoku--active-session)))
    (or (null session)
        (condition-case nil
            (cond
             ((yes-or-no-p "進行中のゲームをセーブして置き換えますか? ")
              (condition-case error-data
                  (progn
                    (sengoku-save-game (sengoku-session-game session))
                    t)
                (error
                 (message "セーブできません: %s"
                          (error-message-string error-data))
                 nil)))
             ((yes-or-no-p "セーブせずに進行中のゲームを置き換えますか? ") t)
             (t nil))
          (quit nil)))))

(defun sengoku--read-clan (game)
  "Display and prompt for a player clan in GAME, then return its index."
  (let (choices)
    (dotimes (clan-index (length (sengoku-game-clans game)))
      (let* ((clan (aref (sengoku-game-clans game) clan-index))
             (provinces (sengoku-clan-provinces game clan-index))
             (label
              (format "%d. %s家 — %s (%d国)"
                      (1+ clan-index)
                      (sengoku-clan-name clan)
                      (sengoku-clan-daimyo clan)
                      (length provinces))))
        (push (cons label clan-index) choices)))
    (setq choices (nreverse choices))
    (let* ((completion-table
            (lambda (string predicate action)
              (if (eq action 'metadata)
                  '(metadata
                    (display-sort-function . identity)
                    (cycle-sort-function . identity))
                (complete-with-action action choices string predicate))))
           (answer
            (minibuffer-with-setup-hook #'minibuffer-completion-help
              (completing-read
               (format "大名家を選択 (1-%d): " (length choices))
               completion-table nil nil)))
           (exact (assoc answer choices))
           (number-text (string-trim answer))
           (number (and (string-match-p "\\`[[:digit:]]+\\'" number-text)
                        (string-to-number number-text))))
      (cond
       (exact (cdr exact))
       ((and number (<= 1 number) (<= number (length choices)))
        (1- number))
       (t
        (user-error "大名家は1から%dの番号で選択してください"
                    (length choices)))))))

;;;###autoload
(defun sengoku ()
  "Start a new Sengoku campaign after selecting one of the 32 clans."
  (interactive)
  ;; Read the choice before asking to replace the current campaign so `C-g'
  ;; during completion never disturbs the live game.
  (let* ((game (sengoku-new-game))
         (session (sengoku-new-session game))
         (clan-index (sengoku--read-clan game)))
    (when (sengoku--confirm-replacement)
      (sengoku-engine-select-player session clan-index)
      (sengoku-ui-open-session session))))

;;;###autoload
(defun sengoku-load ()
  "Load the configured Sengoku save, falling back to the legacy Vim save."
  (interactive)
  ;; Decode and validate before asking to replace a running campaign.
  (let ((loaded (sengoku-load-game)))
    (unless loaded
      (user-error "セーブデータがありません: %s"
                  (expand-file-name sengoku-save-file)))
    (when (sengoku--confirm-replacement)
      (let* ((game (plist-get loaded :game))
             (session (sengoku-new-session game))
             (result (sengoku-engine-begin-turn session))
             (source (plist-get loaded :path))
             (message-text
              (if (plist-get loaded :legacy-p)
                  (format "Vim版セーブを読み込みました: %s" source)
                (format "セーブを読み込みました: %s" source))))
        (let ((state (or (sengoku-session-ui-state session) nil)))
          (setf (plist-get state :ui-message) message-text
                (sengoku-session-ui-state session) state))
        (sengoku-ui-open-session session)
        (message "%s — %s" message-text (plist-get result :message))))))

(defun sengoku--write-self-test-preview (text game)
  "Write rendered TEXT for GAME to the self-test preview buffer.
Return the read-only preview buffer."
  (let ((buffer (get-buffer-create sengoku-self-test-preview-buffer-name)))
    (with-current-buffer buffer
      (special-mode)
      (setq-local truncate-lines t)
      (setq-local word-wrap nil)
      (setq-local header-line-format
                  (format "戦国風雲録 セルフテスト描画 — %d年%d月"
                          (sengoku-game-year game)
                          (sengoku-game-month game)))
      (buffer-face-set 'fixed-pitch)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert text)
        (goto-char (point-min))))
    buffer))

(defun sengoku--self-test-result (name failures &optional details)
  "Display self-test NAME with collected FAILURES and successful DETAILS.
Return non-nil when FAILURES is empty."
  (with-help-window "*戦国風雲録 セルフテスト*"
    (princ (format "== %s ==\n\n" name))
    (dolist (detail details)
      (princ (format "OK: %s\n" detail)))
    (when details
      (princ "\n"))
    (if failures
        (progn
          (princ (format "%d件の問題を検出しました。\n\n" (length failures)))
          (dolist (failure failures)
            (princ (format "NG: %s\n" failure))))
      (princ "OK: すべての検査に合格しました。\n")))
  (if failures
      (progn
        (message "%s: %d件の問題" name (length failures))
        nil)
    (message "%s: OK" name)
    t))

;;;###autoload
(defun sengoku-self-test ()
  "Validate game data, map integrity, 36 all-AI months, and rendering."
  (interactive)
  (let* ((data-failures (copy-sequence (or (sengoku-validate-data) nil)))
         (failures data-failures)
         (details nil)
         (preview-buffer nil)
         (seed 2463534242))
    (unless data-failures
      (push (format "データ整合性: %d国 %d家 %d武将 %d茶器"
                    (length sengoku-data-provinces)
                    (length sengoku-data-clans)
                    (* (length sengoku-data-clans) 8)
                    (length sengoku-data-items))
            details))
    (let ((sengoku-random-function
           (lambda (upper-bound)
             (setq seed (logand #xffffffff
                                (+ (* seed 1103515245) 12345)))
             (% seed upper-bound))))
      (condition-case error-data
          (let* ((game (sengoku-new-game))
                 (session (sengoku-new-session game))
                 (map-failures (sengoku-validate-map game))
                 (resource-failures (sengoku-validate-resources game))
                 (map-width-ok t)
                 (simulation-ok t))
            (setq failures
                  (nconc failures
                         (copy-sequence map-failures)
                         (copy-sequence resource-failures)))
            (unless map-failures
              (push "マップ整合性: 隣接対称・グリッド衝突なし・名前解決済み"
                    details))
            (dolist (line (sengoku-ui-strategic-map-lines game))
              (unless (= (string-width line)
                         (* sengoku-data-map-cell-width
                            sengoku-data-map-columns))
                (setq map-width-ok nil)
                (push (format "マップ行の表示幅が%d桁" (string-width line))
                      failures)))
            (when map-width-ok
              (push (format "戦略マップ幅: %d行すべて%d表示桁"
                            (* sengoku-data-map-rows 3)
                            (* sengoku-data-map-cell-width
                               sengoku-data-map-columns))
                    details))
            (dotimes (month 36)
              (let ((result (sengoku-engine-begin-turn session)))
                (unless (eq (plist-get result :status)
                            'observer-month-complete)
                  (setq simulation-ok nil)
                  (push (format "%dヶ月目の状態が%S"
                                (1+ month) (plist-get result :status))
                        failures)))
              (let ((month-failures (sengoku-validate-game game)))
                (when month-failures
                  (setq simulation-ok nil))
                (dolist (failure month-failures)
                  (push (format "%dヶ月目: %s" (1+ month) failure)
                        failures))))
            (when simulation-ok
              (push (format "全%d家AI: 36ヶ月進行 (%d年%d月)"
                            (length (sengoku-game-clans game))
                            (sengoku-game-year game)
                            (sengoku-game-month game))
                    details))
            (condition-case render-error
                (with-temp-buffer
                  (sengoku-mode)
                  (setq-local sengoku-ui--suppress-kill-query t)
                  (setq-local sengoku-session session)
                  (sengoku-ui-render)
                  (let ((text (buffer-string))
                        (line-count (count-lines (point-min) (point-max)))
                        (render-ok t))
                    (setq preview-buffer
                          (sengoku--write-self-test-preview text game))
                    (dolist (required '("戦国風雲録" "勢力" "数字=兵数"))
                      (unless (string-match-p (regexp-quote required) text)
                        (setq render-ok nil)
                        (push (format "描画結果に「%s」がありません" required)
                              failures)))
                    (when (= (buffer-size) 0)
                      (setq render-ok nil)
                      (push "描画結果が空です" failures))
                    (when render-ok
                      (push (format "戦略画面描画: %d行、主要表示を確認"
                                    line-count)
                            details))))
              (error
               (push (format "戦略画面の描画に失敗: %s"
                             (error-message-string render-error))
                     failures))))
        (error
         (push (error-message-string error-data) failures))))
    (let ((result
           (sengoku--self-test-result "戦国風雲録セルフテスト"
                                      (nreverse failures)
                                      (nreverse details))))
      (when (buffer-live-p preview-buffer)
        (display-buffer preview-buffer))
      result)))

;;;###autoload
(defun sengoku-siege-test ()
  "Run six deterministic all-AI siege simulations and validate each result."
  (interactive)
  (let ((failures nil)
        (seed 362436069))
    (let ((sengoku-random-function
           (lambda (upper-bound)
             (setq seed (logand #xffffffff
                                (+ (* seed 1664525) 1013904223)))
             (% seed upper-bound))))
      (dotimes (iteration 6)
        (condition-case error-data
            (let* ((game (sengoku-new-game))
                   (attacker (sengoku-clan-index game "織田"))
                   (from (sengoku-province-index game "尾張"))
                   (target-index (sengoku-province-index game "三河"))
                   (source (aref (sengoku-game-provinces game) from)))
              (setf (sengoku-province-soldiers source) 9000
                    (sengoku-province-guns source) 60
                    (sengoku-province-training source) 70
                    (sengoku-province-cannon source) (mod iteration 3))
              (let ((context
                     (sengoku-battle-start
                      game attacker from target-index
                      (+ 4000 (* iteration 1000)) 'siege
                      :general
                      (aref (aref (sengoku-game-generals game) attacker)
                            (mod iteration 8))
                      :player-side -2)))
                (sengoku-siege-run-all-ai context)
                (sengoku-battle-apply-siege-result game context)
                (unless (sengoku-battle-finished-p context)
                  (push (format "籠城戦%dが未決着" (1+ iteration)) failures))
                (dolist (failure (sengoku-validate-game game))
                  (push (format "籠城戦%d: %s" (1+ iteration) failure)
                        failures))))
          (error
           (push (format "籠城戦%d: %s"
                         (1+ iteration) (error-message-string error-data))
                 failures)))))
    (sengoku--self-test-result "籠城戦セルフテスト" (nreverse failures))))

(provide 'sengoku)

;;; sengoku.el ends here
