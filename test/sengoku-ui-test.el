;;; sengoku-ui-test.el --- UI tests for Sengoku -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'sengoku)
(require 'sengoku-core-test)

(defun sengoku-test--ui-oda-session ()
  "Return a deterministic Oda session at its first command prompt."
  (let* ((game (sengoku-new-game))
         (session (sengoku-new-session game))
         (oda (sengoku-clan-index game "織田")))
    (sengoku-engine-select-player session oda)
    session))

(defun sengoku-test--ui-siege-session ()
  "Return a deterministic session awaiting Oda's first tactical move."
  (let* ((game (sengoku-new-game))
         (oda (sengoku-clan-index game "織田"))
         (owari (sengoku-province-index game "尾張"))
         (mikawa (sengoku-province-index game "三河"))
         (source (aref (sengoku-game-provinces game) owari))
         (session (sengoku-new-session game)))
    (setf (sengoku-game-player game) oda
          (sengoku-province-soldiers source) 9000)
    (let ((context
           (sengoku-battle-start
            game oda owari mikawa 4000 'siege :player-side 0)))
      (should (eq (sengoku-siege-advance-to-player context) 'player-turn))
      (setf (sengoku-session-phase session) 'siege
            (sengoku-session-pending-battle session) context
            (sengoku-game-pending-battle game) context)
      session)))

(defmacro sengoku-test-with-ui-buffer (session &rest body)
  "Evaluate BODY in a temporary Sengoku buffer displaying SESSION."
  (declare (indent 1) (debug (form body)))
  `(with-temp-buffer
     (sengoku-mode)
     (setq-local sengoku-ui--suppress-kill-query t)
     (setq-local sengoku-session ,session)
     ,@body))

(ert-deftest sengoku-ui-every-province-cell-is-ten-columns ()
  (sengoku-test-with-zero-random
    (let ((game (sengoku-new-game)))
      (dotimes (province-index (length (sengoku-game-provinces game)))
        (let ((cell-lines
               (sengoku-ui--province-cell game province-index -1)))
          (should (= (length cell-lines) 3))
          (dolist (line cell-lines)
            (should (= (string-width line)
                       sengoku-data-map-cell-width))))))))

(ert-deftest sengoku-ui-every-strategic-map-line-is-110-columns ()
  (sengoku-test-with-zero-random
    (let* ((game (sengoku-new-game))
           (lines (sengoku-ui-strategic-map-lines game)))
      (should (= (length lines) (* sengoku-data-map-rows 3)))
      (dolist (line lines)
        (should (= (string-width line)
                   (* sengoku-data-map-cell-width
                      sengoku-data-map-columns)))))))

(ert-deftest sengoku-ui-required-key-bindings-exist ()
  (dolist (binding
           '(("1" . sengoku-ui-develop)
             ("2" . sengoku-ui-commerce)
             ("3" . sengoku-ui-relief)
             ("4" . sengoku-ui-recruit)
             ("5" . sengoku-ui-train)
             ("6" . sengoku-ui-buy-guns)
             ("7" . sengoku-ui-attack)
             ("8" . sengoku-ui-transport)
             ("9" . sengoku-ui-diplomacy)
             ("T" . sengoku-ui-tea)
             ("C" . sengoku-ui-court)
             ("N" . sengoku-ui-nanban)
             ("B" . sengoku-ui-show-generals)
             ("A" . sengoku-ui-delegate)
             ("R" . sengoku-ui-release)
             ("i" . sengoku-ui-show-province)
             ("0" . sengoku-ui-wait)
             ("SPC" . sengoku-ui-space)
             ("RET" . sengoku-ui-return)
             ("E" . sengoku-ui-skip)
             ("S" . sengoku-ui-save)
             ("Q" . sengoku-ui-quit)
             ("h" . sengoku-ui-siege-left)
             ("j" . sengoku-ui-siege-down)
             ("k" . sengoku-ui-siege-up)
             ("l" . sengoku-ui-siege-right)
             ("<left>" . sengoku-ui-siege-left)
             ("<down>" . sengoku-ui-siege-down)
             ("<up>" . sengoku-ui-siege-up)
             ("<right>" . sengoku-ui-siege-right)
             ("a" . sengoku-ui-siege-attack)
             ("f" . sengoku-ui-siege-fire)
             ("w" . sengoku-ui-siege-wait)))
    (should (eq (lookup-key sengoku-mode-map (kbd (car binding)))
                (cdr binding)))))

(ert-deftest sengoku-ui-strategic-render-works-in-temporary-buffer ()
  (sengoku-test-with-zero-random
    (let ((session (sengoku-test--ui-oda-session)))
      (sengoku-test-with-ui-buffer session
        (sengoku-ui-render)
        (should (string-match-p "戦国風雲録" (buffer-string)))
        (should (string-match-p "尾張" (buffer-string)))
        (should (eq (get-text-property (point-min) 'face)
                    'sengoku-title-face))))))

(ert-deftest sengoku-public-self-test-renders-after-all-ai-simulation ()
  (let ((original-render (symbol-function 'sengoku-ui-render))
        rendered-phase
        rendered-player
        rendered-date
        captured-failures
        captured-details
        displayed-buffer)
    (when (get-buffer sengoku-self-test-preview-buffer-name)
      (kill-buffer sengoku-self-test-preview-buffer-name))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'sengoku-ui-render)
                     (lambda ()
                       (setq rendered-phase
                             (sengoku-session-phase sengoku-session)
                             rendered-player
                             (sengoku-game-player
                              (sengoku-session-game sengoku-session))
                             rendered-date
                             (cons
                              (sengoku-game-year
                               (sengoku-session-game sengoku-session))
                              (sengoku-game-month
                               (sengoku-session-game sengoku-session))))
                       (funcall original-render)))
                    ((symbol-function 'sengoku--self-test-result)
                     (lambda (_name failures &optional details)
                       (setq captured-failures failures
                             captured-details details)
                       (null failures)))
                    ((symbol-function 'display-buffer)
                     (lambda (buffer-or-name &rest _arguments)
                       (setq displayed-buffer (get-buffer buffer-or-name)))))
            (should (sengoku-self-test)))
          (should-not captured-failures)
          (should (eq rendered-phase 'observer-idle))
          (should (= rendered-player -1))
          (should (equal rendered-date '(1563 . 3)))
          (should
           (seq-some (lambda (detail)
                       (string-match-p "全32家AI: 36ヶ月進行" detail))
                     captured-details))
          (should
           (seq-some (lambda (detail)
                       (string-match-p "戦略画面描画" detail))
                     captured-details))
          (should (buffer-live-p displayed-buffer))
          (should (equal (buffer-name displayed-buffer)
                         sengoku-self-test-preview-buffer-name))
          (with-current-buffer displayed-buffer
            (should (derived-mode-p 'special-mode))
            (should-not (derived-mode-p 'sengoku-mode))
            (should-not sengoku-session)
            (should (string-match-p "戦国風雲録" (buffer-string)))
            (should (string-match-p "勢力" (buffer-string)))
            (should (string-match-p "数字=兵数" (buffer-string)))
            (should (equal header-line-format
                           "戦国風雲録 セルフテスト描画 — 1563年3月"))))
      (when (buffer-live-p displayed-buffer)
        (kill-buffer displayed-buffer)))))

(ert-deftest sengoku-ui-siege-render-works-in-temporary-buffer ()
  (sengoku-test-with-zero-random
    (let ((session (sengoku-test--ui-siege-session)))
      (sengoku-test-with-ui-buffer session
        (sengoku-ui-render)
        (should (string-match-p "三河城 攻防戦" (buffer-string)))
        (should (string-match-p "hjkl/矢印:移動" (buffer-string)))))))

(ert-deftest sengoku-ui-one-movement-key-moves-exactly-one-grid-step ()
  (sengoku-test-with-zero-random
    (let* ((session (sengoku-test--ui-siege-session))
           (context (sengoku-session-pending-battle session))
           (before (plist-get (sengoku-siege-player-turn-state context) :unit))
           (before-row (sengoku-siege-unit-row before))
           (before-column (sengoku-siege-unit-column before)))
      (sengoku-test-with-ui-buffer session
        (sengoku-ui-render)
        (call-interactively (lookup-key sengoku-mode-map (kbd "l")))
        (let ((after
               (plist-get (sengoku-siege-player-turn-state context) :unit)))
          (should (= (sengoku-siege-unit-row after) before-row))
          (should (= (sengoku-siege-unit-column after)
                     (1+ before-column))))))))

(ert-deftest sengoku-ui-tactical-target-menus-route-the-second-choice ()
  (sengoku-test-with-zero-random
    (let* ((session (sengoku-test--ui-siege-session))
           (context (sengoku-session-pending-battle session))
           (unit (plist-get (sengoku-siege-player-turn-state context) :unit))
           selected-attack
           selected-fire)
      (sengoku-test-with-ui-buffer session
        (cl-letf
            (((symbol-function 'sengoku-siege-player-adjacent-targets)
              (lambda (_context)
                (list '(:kind structure :terrain wall :hit-points 200)
                      '(:kind structure :terrain wall :hit-points 200))))
             ((symbol-function 'sengoku-siege-player-fire-targets)
              (lambda (_context) (list unit unit)))
             ((symbol-function 'completing-read)
              (lambda (&rest _arguments) "2"))
             ((symbol-function 'sengoku-siege-player-attack)
              (lambda (_context &optional target)
                (setq selected-attack target)))
             ((symbol-function 'sengoku-siege-player-fire)
              (lambda (_context &optional target)
                (setq selected-fire target)))
             ((symbol-function 'sengoku-ui-advance) #'ignore))
          (sengoku-ui-siege-attack)
          (sengoku-ui-siege-fire)))
      (should (= selected-attack 1))
      (should (= selected-fire 1)))))

(ert-deftest sengoku-ui-completion-quit-cancels-only-selection ()
  (cl-letf (((symbol-function 'completing-read)
             (lambda (&rest _arguments) (signal 'quit nil))))
    (should-not
     (sengoku-ui--selection "選択: " '(a b) #'symbol-name))))

(ert-deftest sengoku-ui-selection-keeps-duplicate-labels-distinct ()
  (let (displayed)
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_prompt completion-table &rest _arguments)
                 (let* ((metadata
                         (completion-metadata "" completion-table nil))
                        (sort-function
                         (completion-metadata-get
                          metadata 'display-sort-function)))
                   (setq displayed
                         (funcall sort-function
                                  (all-completions "" completion-table))))
                 "2")))
      (should
       (eq (sengoku-ui--selection
            "対象: " '(first second) (lambda (_value) "同名候補"))
           'second)))
    (should (equal displayed '("1. 同名候補" "2. 同名候補")))))

(ert-deftest sengoku-ui-number-input-supports-cancel-and-range-checking ()
  (let (default-value prompts)
    (cl-letf (((symbol-function 'read-number)
               (lambda (prompt &optional default)
                 (push prompt prompts)
                 (setq default-value default)
                 default)))
      (should-not (sengoku-ui--number "兵数" 100))
      (should (= default-value 0))
      (should (string-match-p "空欄=中止" (car prompts))))
    (cl-letf (((symbol-function 'read-number)
               (lambda (&rest _arguments)
                 (ert-fail "Maximum zero must not prompt"))))
      (should-not (sengoku-ui--number "兵数" 0)))
    (cl-letf (((symbol-function 'read-number)
               (lambda (&rest _arguments) 101)))
      (should-not (sengoku-ui--number "兵数" 100)))
    (cl-letf (((symbol-function 'read-number)
               (lambda (&rest _arguments) 75)))
      (should (= (sengoku-ui--number "兵数" 100) 75)))))

(ert-deftest sengoku-ui-confirmation-distinguishes-no-and-cancel ()
  (cl-letf (((symbol-function 'y-or-n-p)
             (lambda (&rest _arguments) nil)))
    (should-not (sengoku-ui--yes-or-no "確認: ")))
  (cl-letf (((symbol-function 'y-or-n-p)
             (lambda (&rest _arguments) (signal 'quit nil))))
    (should (eq (sengoku-ui--yes-or-no "確認: ") 'cancel)))
  (let (strong-called)
    (cl-letf (((symbol-function 'yes-or-no-p)
               (lambda (&rest _arguments)
                 (setq strong-called t))))
      (should (sengoku-ui--yes-or-no "重要な確認: " t)))
    (should strong-called)))

(ert-deftest sengoku-ui-quit-routes-strategic-and-siege-confirmations ()
  (sengoku-test-with-zero-random
    (let ((session (sengoku-test--ui-oda-session)))
      (sengoku-test-with-ui-buffer session
        (cl-letf (((symbol-function 'sengoku-ui--yes-or-no)
                   (lambda (&rest _arguments) 'cancel))
                  ((symbol-function 'sengoku-ui-render)
                   (lambda () (ert-fail "Cancelled surrender rendered"))))
          (sengoku-ui-quit))
        (should (eq (sengoku-session-phase session) 'player-command))
        (cl-letf (((symbol-function 'sengoku-ui--yes-or-no)
                   (lambda (&rest _arguments) t))
                  ((symbol-function 'sengoku-ui-render) #'ignore))
          (sengoku-ui-quit))
        (should (eq (sengoku-session-phase session) 'ended))
        (should (eq (sengoku-session-quit-reason session) 'surrendered))))
    (let* ((session (sengoku-test--ui-siege-session))
           (context (sengoku-session-pending-battle session))
           (siege (sengoku-battle-context-siege context))
           retreated
           surrendered)
      (sengoku-test-with-ui-buffer session
        (cl-letf (((symbol-function 'sengoku-ui--yes-or-no)
                   (lambda (&rest _arguments) t))
                  ((symbol-function 'sengoku-siege-player-retreat)
                   (lambda (_context) (setq retreated t)))
                  ((symbol-function 'sengoku-siege-player-surrender)
                   (lambda (_context) (setq surrendered t)))
                  ((symbol-function 'sengoku-ui-advance) #'ignore))
          (sengoku-ui-quit)
          (setf (sengoku-siege-player-side siege) 1)
          (sengoku-ui-quit)))
      (should retreated)
      (should surrendered))))

(ert-deftest sengoku-ui-battle-choice-routes-every-answer ()
  (sengoku-test-with-zero-random
    (dolist (case '((t siege) (nil automatic)))
      (let ((session (sengoku-test--ui-oda-session))
            (advance-count 0)
            resolved)
        (sengoku-test-with-ui-buffer session
          (cl-letf (((symbol-function 'sengoku-engine-advance)
                     (lambda (_session)
                       (prog1
                           (if (zerop advance-count)
                               '(:status battle-choice :message "戦闘")
                             '(:status player-command :message "入力"))
                         (setq advance-count (1+ advance-count)))))
                    ((symbol-function 'sengoku-ui--yes-or-no)
                     (lambda (&rest _arguments) (car case)))
                    ((symbol-function 'sengoku-engine-resolve-battle-choice)
                     (lambda (_session resolution)
                       (setq resolved resolution)
                       '(:status battle-complete)))
                    ((symbol-function 'sengoku-ui-render) #'ignore))
            (sengoku-ui-advance)))
        (should (eq resolved (cadr case)))))
    (let* ((session (sengoku-test--ui-oda-session))
           (pending (list :pending-battle))
           (original-phase 'battle-choice))
      (setf (sengoku-session-phase session) original-phase
            (sengoku-session-pending-battle session) pending)
      (sengoku-test-with-ui-buffer session
        (cl-letf (((symbol-function 'sengoku-engine-advance)
                   (lambda (_session)
                     '(:status battle-choice :message "戦闘")))
                  ((symbol-function 'sengoku-ui--yes-or-no)
                   (lambda (&rest _arguments) 'cancel))
                  ((symbol-function 'sengoku-engine-resolve-battle-choice)
                   (lambda (&rest _arguments)
                     (ert-fail "Cancelled battle choice was resolved")))
                  ((symbol-function 'sengoku-ui-render) #'ignore))
          (sengoku-ui-advance)))
      (should (eq (sengoku-session-phase session) original-phase))
      (should (eq (sengoku-session-pending-battle session) pending)))))

(ert-deftest sengoku-ui-decision-routes-every-answer ()
  (sengoku-test-with-zero-random
    (dolist (answer '(t nil))
      (let ((session (sengoku-test--ui-oda-session))
            (advance-count 0)
            resolved)
        (sengoku-test-with-ui-buffer session
          (cl-letf (((symbol-function 'sengoku-engine-advance)
                     (lambda (_session)
                       (prog1
                           (if (zerop advance-count)
                               '(:status decision
                                 :decision (:message "申し出"))
                             '(:status player-command :message "入力"))
                         (setq advance-count (1+ advance-count)))))
                    ((symbol-function 'sengoku-ui--yes-or-no)
                     (lambda (&rest _arguments) answer))
                    ((symbol-function 'sengoku-engine-resolve-decision)
                     (lambda (_session accepted)
                       (setq resolved (list accepted))))
                    ((symbol-function 'sengoku-ui-render) #'ignore))
            (sengoku-ui-advance)))
        (should (equal resolved (list answer)))))
    (let* ((session (sengoku-test--ui-oda-session))
           (pending '(:type merchant :message "商人")))
      (sengoku-engine--set-decision session pending 'merchant-player)
      (sengoku-test-with-ui-buffer session
        (cl-letf (((symbol-function 'sengoku-engine-advance)
                   (lambda (_session)
                     (list :status 'decision :decision pending)))
                  ((symbol-function 'sengoku-ui--yes-or-no)
                   (lambda (&rest _arguments) 'cancel))
                  ((symbol-function 'sengoku-engine-resolve-decision)
                   (lambda (&rest _arguments)
                     (ert-fail "Cancelled decision was resolved")))
                  ((symbol-function 'sengoku-ui-render) #'ignore))
          (sengoku-ui-advance)))
      (should (eq (sengoku-session-phase session) 'decision))
      (should (eq (sengoku-engine-pending-decision session) pending)))))

(ert-deftest sengoku-ui-general-selection-displays-numbered-candidates ()
  (sengoku-test-with-zero-random
    (let* ((session (sengoku-test--ui-oda-session))
           (game (sengoku-session-game session))
           (player (sengoku-game-player game)))
      (sengoku-use-general game player 0)
      (sengoku-test-with-ui-buffer session
        (let ((original-hooks (copy-sequence minibuffer-setup-hook))
              displayed
              help-shown)
          (cl-letf (((symbol-function 'minibuffer-completion-help)
                     (lambda () (setq help-shown t)))
                    ((symbol-function 'completing-read)
                     (lambda (_prompt completion-table &rest _arguments)
                       (let ((setup-hook
                              (cl-find-if
                               (lambda (hook)
                                 (not (memq hook original-hooks)))
                               minibuffer-setup-hook)))
                         (should setup-hook)
                         (funcall setup-hook))
                       (let* ((metadata
                               (completion-metadata "" completion-table nil))
                              (sort-function
                               (completion-metadata-get
                                metadata 'display-sort-function)))
                         (should (eq sort-function #'identity))
                         (setq displayed
                               (funcall
                                sort-function
                                (all-completions "" completion-table))))
                       "1")))
            (let ((report
                   (sengoku-ui--read-general "開発を任せる武将: "
                                             'politics)))
              (should (= (plist-get report :index) 1))))
          (should help-shown)
          (should (= (length displayed) 7))
          (should (string-prefix-p "1. " (nth 0 displayed)))
          (should (string-prefix-p "2. " (nth 1 displayed)))
          (should (string-match-p "政[0-9]+ 戦[0-9]+" (car displayed))))))))

(ert-deftest sengoku-ui-general-selection-reports-when-none-can-act ()
  (sengoku-test-with-zero-random
    (let* ((session (sengoku-test--ui-oda-session))
           (game (sengoku-session-game session))
           (player (sengoku-game-player game)))
      (dotimes (general-index
                (length (aref (sengoku-game-generals game) player)))
        (sengoku-use-general game player general-index))
      (sengoku-test-with-ui-buffer session
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (&rest _arguments)
                     (ert-fail "No general prompt was expected"))))
          (should-not
           (sengoku-ui--read-general "開発を任せる武将: " 'politics)))
        (should
         (string-match-p
          "行動できる武将はもういません"
          (plist-get (sengoku-session-ui-state session) :ui-message)))))))

(ert-deftest sengoku-ui-all-strategic-selection-paths-route-values ()
  (sengoku-test-with-zero-random
    (let* ((session (sengoku-test--ui-oda-session))
           (game (sengoku-session-game session))
           (player (sengoku-game-player game))
           (owari (sengoku-province-index game "尾張"))
           (mikawa (sengoku-province-index game "三河"))
           (saito (sengoku-clan-index game "斎藤"))
           (diplomacy-choice 'propose)
           (court-choice 'rank)
           (transport-kind 'gold)
           (nanban-kind 'cannon)
           actions
           general-prompts
           number-prompts
           selection-prompts)
      (sengoku-test-with-ui-buffer session
        (cl-letf
            (((symbol-function 'sengoku-ui--selection)
              (lambda (prompt values formatter)
                (push prompt selection-prompts)
                (should values)
                (dolist (value values)
                  (should (stringp (funcall formatter value))))
                (cond
                 ((equal prompt "出陣先: ") (car values))
                 ((equal prompt "輸送先: ") (car values))
                 ((equal prompt "何を送るか: ") transport-kind)
                 ((equal prompt "外交: ") diplomacy-choice)
                 ((equal prompt "同盟の相手: ") (car values))
                 ((equal prompt "破棄する同盟: ") saito)
                 ((equal prompt "朝廷: ") court-choice)
                 ((equal prompt "勅命和睦の相手: ") (car values))
                 ((equal prompt "南蛮商人: ") nanban-kind)
                 ((equal prompt "委任を解除する国: ") (car values))
                 ((equal prompt "情報を見る国: ") nil)
                 (t (ert-fail (format "Unexpected selection prompt: %s"
                                      prompt))))))
             ((symbol-function 'sengoku-ui--read-general)
              (lambda (prompt _ability)
                (push prompt general-prompts)
                '(:index 2)))
             ((symbol-function 'sengoku-ui--number)
              (lambda (prompt _maximum)
                (push prompt number-prompts)
                100))
             ((symbol-function 'sengoku-ui--run-command)
              (lambda (action &rest arguments)
                (push (cons action arguments) actions))))
          (sengoku-ui-develop)
          (sengoku-ui-commerce)
          (sengoku-ui-relief)
          (sengoku-ui-recruit)
          (sengoku-ui-train)
          (sengoku-ui-attack)
          (setf (sengoku-province-owner
                 (aref (sengoku-game-provinces game) mikawa))
                player)
          (sengoku-ui-transport)
          (setq diplomacy-choice 'propose)
          (sengoku-ui-diplomacy)
          (sengoku-set-alliance game player saito t)
          (setq diplomacy-choice 'break)
          (sengoku-ui-diplomacy)
          (setq court-choice 'rank)
          (sengoku-ui-court)
          (setq court-choice 'peace)
          (sengoku-ui-court)
          (sengoku-ui-nanban)
          (setf (sengoku-province-auto
                 (aref (sengoku-game-provinces game) owari))
                1)
          (sengoku-ui-release)
          (sengoku-ui-show-province))
        (dolist (action '(develop commerce relief recruit train attack
                          transport propose-alliance break-alliance
                          court-rank court-peace nanban release))
          (should (assq action actions)))
        (should
         (equal (cdr (assq 'attack actions))
                (list :target mikawa :general 2 :quantity 100)))
        (should
         (equal (cdr (assq 'transport actions))
                (list :target mikawa :kind 'gold :quantity 100)))
        (should (= (length general-prompts) 9))
        (should (= (length number-prompts) 3))
        (dolist (prompt '("出陣先: " "輸送先: " "何を送るか: "
                          "同盟の相手: " "破棄する同盟: "
                          "勅命和睦の相手: " "南蛮商人: "
                          "委任を解除する国: " "情報を見る国: "))
          (should (member prompt selection-prompts)))))))

(ert-deftest sengoku-public-replacement-confirmation-routes-all-choices ()
  (let ((session (sengoku-test--ui-oda-session))
        saved)
    (cl-letf (((symbol-function 'sengoku--active-session)
               (lambda () session))
              ((symbol-function 'yes-or-no-p)
               (lambda (&rest _arguments) t))
              ((symbol-function 'sengoku-save-game)
               (lambda (_game &optional _file) (setq saved t))))
      (should (sengoku--confirm-replacement)))
    (should saved)
    (let ((answers '(nil t)))
      (cl-letf (((symbol-function 'sengoku--active-session)
                 (lambda () session))
                ((symbol-function 'yes-or-no-p)
                 (lambda (&rest _arguments) (pop answers)))
                ((symbol-function 'sengoku-save-game)
                 (lambda (&rest _arguments)
                   (ert-fail "Discard choice must not save"))))
        (should (sengoku--confirm-replacement))))
    (let ((answers '(nil nil)))
      (cl-letf (((symbol-function 'sengoku--active-session)
                 (lambda () session))
                ((symbol-function 'yes-or-no-p)
                 (lambda (&rest _arguments) (pop answers))))
        (should-not (sengoku--confirm-replacement))))
    (cl-letf (((symbol-function 'sengoku--active-session)
               (lambda () session))
              ((symbol-function 'yes-or-no-p)
               (lambda (&rest _arguments) (signal 'quit nil))))
      (should-not (sengoku--confirm-replacement)))))

(ert-deftest sengoku-ui-kill-query-routes-all-choices ()
  (let ((session (sengoku-test--ui-oda-session)))
    (sengoku-test-with-ui-buffer session
      (setq-local sengoku-ui--suppress-kill-query nil)
      (let (saved)
        (cl-letf (((symbol-function 'yes-or-no-p)
                   (lambda (&rest _arguments) t))
                  ((symbol-function 'sengoku-save-game)
                   (lambda (_game &optional _file) (setq saved t))))
          (should (sengoku-ui--kill-query)))
        (should saved))
      (let ((answers '(nil t)))
        (cl-letf (((symbol-function 'yes-or-no-p)
                   (lambda (&rest _arguments) (pop answers)))
                  ((symbol-function 'sengoku-save-game)
                   (lambda (&rest _arguments)
                     (ert-fail "Discard choice must not save"))))
          (should (sengoku-ui--kill-query))))
      (let ((answers '(nil nil)))
        (cl-letf (((symbol-function 'yes-or-no-p)
                   (lambda (&rest _arguments) (pop answers))))
          (should-not (sengoku-ui--kill-query))))
      (cl-letf (((symbol-function 'yes-or-no-p)
                 (lambda (&rest _arguments) (signal 'quit nil))))
        (should
         (condition-case nil
             (progn (sengoku-ui--kill-query) nil)
           (quit t)))))))

(ert-deftest sengoku-public-clan-selection-displays-numbered-candidates ()
  (sengoku-test-with-zero-random
    (let ((game (sengoku-new-game))
          (original-hooks (copy-sequence minibuffer-setup-hook))
          displayed
          help-shown)
      (cl-letf (((symbol-function 'minibuffer-completion-help)
                 (lambda () (setq help-shown t)))
                ((symbol-function 'completing-read)
                 (lambda (_prompt completion-table &rest _arguments)
                   (let ((setup-hook
                          (cl-find-if
                           (lambda (hook) (not (memq hook original-hooks)))
                           minibuffer-setup-hook)))
                     (should setup-hook)
                     (funcall setup-hook))
                   (let* ((metadata
                           (completion-metadata "" completion-table nil))
                          (sort-function
                           (completion-metadata-get
                            metadata 'display-sort-function)))
                     (should (eq sort-function #'identity))
                     (setq displayed
                           (funcall sort-function
                                    (all-completions "" completion-table)))
                     (car displayed)))))
        (should (zerop (sengoku--read-clan game))))
      (should help-shown)
      (should (= (length displayed) 32))
      (should (string-prefix-p "1. " (nth 0 displayed)))
      (should (string-prefix-p "2. " (nth 1 displayed)))
      (should (string-prefix-p "9. " (nth 8 displayed)))
      (should (string-prefix-p "10. " (nth 9 displayed)))
      (should (string-prefix-p "32. " (car (last displayed)))))))

(ert-deftest sengoku-public-clan-selection-accepts-a-number ()
  (sengoku-test-with-zero-random
    (let ((game (sengoku-new-game)))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (&rest _arguments) "14")))
        (should (= (sengoku--read-clan game) 13))))))

(ert-deftest sengoku-public-new-game-opens-selected-session ()
  (sengoku-test-with-zero-random
    (let (opened)
      (cl-letf (((symbol-function 'sengoku--read-clan)
                 (lambda (_game) 13))
                ((symbol-function 'sengoku--confirm-replacement)
                 (lambda () t))
                ((symbol-function 'sengoku-ui-open-session)
                 (lambda (session) (setq opened session))))
        (sengoku))
      (should (sengoku-session-p opened))
      (should (= (sengoku-game-player (sengoku-session-game opened)) 13))
      (should (eq (sengoku-session-phase opened) 'player-command)))))

(ert-deftest sengoku-public-load-opens-a-fresh-controller-session ()
  (sengoku-test-with-zero-random
    (let* ((directory (make-temp-file "sengoku-public-load-" t))
           (save-file (expand-file-name "save.json" directory))
           (legacy-file (expand-file-name "legacy.json" directory))
           (game (sengoku-new-game))
           (oda (sengoku-clan-index game "織田"))
           opened)
      (unwind-protect
          (progn
            (setf (sengoku-game-player game) oda)
            (sengoku-save-game game save-file)
            (let ((sengoku-save-file save-file)
                  (sengoku-save-legacy-file legacy-file))
              (cl-letf (((symbol-function 'sengoku--confirm-replacement)
                         (lambda () t))
                        ((symbol-function 'sengoku-ui-open-session)
                         (lambda (session) (setq opened session))))
                (sengoku-load)))
            (should (sengoku-session-p opened))
            (should (= (sengoku-game-player (sengoku-session-game opened)) oda))
            (should (eq (sengoku-session-phase opened) 'player-command)))
        (delete-directory directory t)))))

(provide 'sengoku-ui-test)

;;; sengoku-ui-test.el ends here
