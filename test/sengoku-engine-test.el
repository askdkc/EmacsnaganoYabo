;;; sengoku-engine-test.el --- Strategic engine tests -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'sengoku-engine)
(require 'sengoku-core-test)

(defun sengoku-test--oda-session ()
  "Return a deterministic session waiting on Oda's first province."
  (let* ((game (sengoku-new-game))
         (session (sengoku-new-session game))
         (oda (sengoku-clan-index game "織田")))
    (sengoku-engine-select-player session oda)
    session))

(ert-deftest sengoku-engine-select-player-opens-first-direct-province ()
  (sengoku-test-with-zero-random
    (let* ((session (sengoku-test--oda-session))
           (game (sengoku-session-game session))
           (owari (sengoku-province-index game "尾張")))
      (should (eq (sengoku-session-phase session) 'player-command))
      (should (= (sengoku-session-active-province session) owari))
      (should (string-match-p "織田家でゲーム開始"
                              (car (sengoku-game-log game)))))))

(ert-deftest sengoku-engine-development-matches-vim-formula ()
  (sengoku-test-with-zero-random
    (let* ((session (sengoku-test--oda-session))
           (game (sengoku-session-game session))
           (oda (sengoku-game-player game))
           (owari (sengoku-session-active-province session))
           (province (aref (sengoku-game-provinces game) owari))
           (general (aref (aref (sengoku-game-generals game) oda) 2))
           (old-gold (sengoku-province-gold province))
           (old-koku (sengoku-province-koku province))
           (expected (+ 12 (sengoku-vim-divide
                            (sengoku-general-politics general) 8)))
           (result (sengoku-engine-command session 'develop :general 2)))
      (should (eq (plist-get result :status) 'ok))
      (should (plist-get result :consumed))
      (should (= (sengoku-province-gold province) (- old-gold 200)))
      (should (= (sengoku-province-koku province) (+ old-koku expected)))
      (should (sengoku-general-used-p game oda 2))
      (should (eq (sengoku-session-phase session) 'player-direct)))))

(ert-deftest sengoku-engine-failed-command-does-not-consume-general-or-province ()
  (sengoku-test-with-zero-random
    (let* ((session (sengoku-test--oda-session))
           (game (sengoku-session-game session))
           (oda (sengoku-game-player game))
           (province (aref (sengoku-game-provinces game)
                           (sengoku-session-active-province session))))
      (setf (sengoku-province-gold province) 0)
      (let ((result (sengoku-engine-command session 'develop :general 2)))
        (should (eq (plist-get result :status) 'error))
        (should-not (plist-get result :consumed))
        (should-not (sengoku-general-used-p game oda 2))
        (should (eq (sengoku-session-phase session) 'player-command))))))

(ert-deftest sengoku-engine-recruitment-preserves-vim-training-order ()
  (sengoku-test-with-zero-random
    (let* ((session (sengoku-test--oda-session))
           (game (sengoku-session-game session))
           (province (aref (sengoku-game-provinces game)
                           (sengoku-session-active-province session)))
           (old-soldiers 2000)
           (old-training 60)
           (quantity 1000)
           (new-soldiers (+ old-soldiers quantity))
           (expected-training
            (sengoku-vim-divide
             (* old-training new-soldiers)
             (+ new-soldiers (sengoku-vim-divide quantity 2)))))
      (setf (sengoku-province-soldiers province) old-soldiers
            (sengoku-province-training province) old-training
            (sengoku-province-gold province) 1000
            (sengoku-province-rice province) 1000)
      (let ((result (sengoku-engine-command
                     session 'recruit :general 0 :quantity quantity)))
        (should (eq (plist-get result :status) 'ok))
        (should (= (sengoku-province-soldiers province) new-soldiers))
        (should (= (sengoku-province-training province) expected-training))
        (should (= (sengoku-province-gold province) 800))
        (should (= (sengoku-province-rice province) 800))))))

(ert-deftest sengoku-engine-soldier-transport-moves-proportional-guns ()
  (sengoku-test-with-zero-random
    (let* ((session (sengoku-test--oda-session))
           (game (sengoku-session-game session))
           (oda (sengoku-game-player game))
           (owari (sengoku-session-active-province session))
           (mikawa (sengoku-province-index game "三河"))
           (source (aref (sengoku-game-provinces game) owari))
           (target (aref (sengoku-game-provinces game) mikawa)))
      (setf (sengoku-province-owner target) oda
            (sengoku-province-soldiers source) 4000
            (sengoku-province-guns source) 100
            (sengoku-province-soldiers target) 1000
            (sengoku-province-guns target) 10)
      (let ((result (sengoku-engine-command
                     session 'transport :target mikawa
                     :kind 'soldiers :quantity 1000)))
        (should (eq (plist-get result :status) 'ok))
        (should (= (sengoku-province-soldiers source) 3000))
        (should (= (sengoku-province-soldiers target) 2000))
        (should (= (sengoku-province-guns source) 75))
        (should (= (sengoku-province-guns target) 35))))))

(ert-deftest sengoku-engine-diplomacy-consumes-gift-and-general ()
  (sengoku-test-with-zero-random
    (let* ((session (sengoku-test--oda-session))
           (game (sengoku-session-game session))
           (oda (sengoku-game-player game))
           (matsudaira (sengoku-clan-index game "松平"))
           (province (aref (sengoku-game-provinces game)
                           (sengoku-session-active-province session))))
      (setf (sengoku-province-gold province) 1000)
      (let ((result (sengoku-engine-command
                     session 'propose-alliance :target matsudaira :general 2)))
        (should (plist-get result :success))
        (should (sengoku-allied-p game oda matsudaira))
        (should (= (sengoku-province-gold province) 500))
        (should (sengoku-general-used-p game oda 2))))))

(ert-deftest sengoku-engine-player-attack-suspends-for-resolution-choice ()
  (let ((sengoku-random-function (lambda (_upper-bound) 20)))
    (let* ((session (sengoku-test--oda-session))
           (game (sengoku-session-game session))
           (owari (sengoku-session-active-province session))
           (mikawa (sengoku-province-index game "三河"))
           (source (aref (sengoku-game-provinces game) owari))
           (target (aref (sengoku-game-provinces game) mikawa)))
      (setf (sengoku-province-soldiers source) 12000
            (sengoku-province-training source) 90
            (sengoku-province-guns source) 200
            (sengoku-province-soldiers target) 500
            (sengoku-province-training target) 20)
      (let ((result (sengoku-engine-command
                     session 'attack :target mikawa :general 0 :quantity 9000)))
        (should (eq (plist-get result :status) 'battle-choice))
        (should (eq (sengoku-session-phase session) 'battle-choice))
        (should (sengoku-session-pending-battle session))
        (let ((resolved
               (sengoku-engine-resolve-battle-choice session 'automatic)))
          (should (eq (plist-get resolved :status) 'battle-complete))
          (should-not (sengoku-session-pending-battle session))
          (should (eq (sengoku-session-phase session) 'player-direct)))))))

(ert-deftest sengoku-engine-begin-turn-cannot-discard-a-pending-battle ()
  (let ((sengoku-random-function (lambda (_upper-bound) 20)))
    (let* ((session (sengoku-test--oda-session))
           (game (sengoku-session-game session))
           (owari (sengoku-session-active-province session))
           (mikawa (sengoku-province-index game "三河"))
           (source (aref (sengoku-game-provinces game) owari))
           (target (aref (sengoku-game-provinces game) mikawa)))
      (setf (sengoku-province-soldiers source) 12000
            (sengoku-province-soldiers target) 1000)
      (sengoku-engine-command
       session 'attack :target mikawa :general 0 :quantity 9000)
      (let ((pending (sengoku-session-pending-battle session))
            (remaining (sengoku-province-soldiers source)))
        (should-error (sengoku-engine-begin-turn session))
        (should (eq (sengoku-session-pending-battle session) pending))
        (should (= (sengoku-province-soldiers source) remaining))
        (should (eq (sengoku-session-phase session) 'battle-choice))))))

(ert-deftest sengoku-engine-release-later-province-enables-same-month-action ()
  (sengoku-test-with-zero-random
    (let* ((game (sengoku-new-game))
           (oda (sengoku-clan-index game "織田"))
           (owari (sengoku-province-index game "尾張"))
           (mino (sengoku-province-index game "美濃"))
           (target (aref (sengoku-game-provinces game) mino))
           (session (sengoku-new-session game)))
      (setf (sengoku-province-owner target) oda
            (sengoku-province-auto target) 1)
      (sengoku-engine-select-player session oda)
      (should (= (sengoku-session-active-province session) owari))
      (let ((released
             (sengoku-engine-command session 'release :target mino)))
        (should (eq (plist-get released :status) 'ok))
        (should-not (plist-get released :consumed)))
      (sengoku-engine-command session 'wait)
      (let ((next (sengoku-engine-advance session)))
        (should (eq (plist-get next :status) 'player-command))
        (should (= (plist-get next :province) mino))
        (should (= (sengoku-session-active-province session) mino))))))

(ert-deftest sengoku-engine-all-delegated-pause-allows-release ()
  (sengoku-test-with-zero-random
    (let* ((game (sengoku-new-game))
           (oda (sengoku-clan-index game "織田"))
           (owari (sengoku-province-index game "尾張"))
           (province (aref (sengoku-game-provinces game) owari))
           (session (sengoku-new-session game)))
      (setf (sengoku-province-auto province) 1
            (sengoku-game-player game) oda)
      (let ((result (sengoku-engine-begin-turn session)))
        (should (eq (plist-get result :status) 'all-delegated))
        (should (eq (sengoku-session-phase session) 'all-delegated)))
      (let ((released (sengoku-engine-release-delegation session owari)))
        (should (eq (plist-get released :status) 'ok))
        (should (zerop (sengoku-province-auto province)))
        (should (eq (sengoku-session-phase session) 'all-delegated))))))

(ert-deftest sengoku-engine-merchant-offer-is-a-resumable-decision ()
  (sengoku-test-with-zero-random
    (let* ((game (sengoku-new-game))
           (oda (sengoku-clan-index game "織田"))
           (source (sengoku-richest-province game oda))
           (province (aref (sengoku-game-provinces game) source))
           (session (sengoku-new-session game)))
      (setf (sengoku-game-player game) oda
            (sengoku-province-gold province) 10000
            (sengoku-session-phase session) 'month-end
            (sengoku-session-ui-state session) (list :month-stage 'merchant-player))
      (let* ((result (sengoku-engine-advance session))
             (decision (plist-get result :decision))
             (item-index (plist-get decision :item))
             (old-gold (sengoku-province-gold province)))
        (should (eq (plist-get result :status) 'decision))
        (should (eq (plist-get decision :type) 'merchant))
        (let ((price (plist-get (aref (sengoku-game-items game) item-index)
                                :price)))
          (sengoku-engine-resolve-decision session t)
          (should (= (sengoku-item-owner game item-index) oda))
          (should (= (sengoku-province-gold province) (- old-gold price)))
          (should (eq (sengoku-session-phase session) 'month-end)))))))

(ert-deftest sengoku-engine-thirty-six-observer-months-keep-valid-state ()
  (let ((seed 2463534242))
    (let ((sengoku-random-function
           (lambda (upper-bound)
             (setq seed (logand #xffffffff
                                (+ (* seed 1103515245) 12345)))
             (% seed upper-bound)))
          (session (sengoku-new-session)))
      (dotimes (_month 36)
        (let ((result (sengoku-engine-begin-turn session)))
          (should (eq (plist-get result :status) 'observer-month-complete))
          (should-not (sengoku-validate-game
                       (sengoku-session-game session))))
        (should (eq (sengoku-session-phase session) 'observer-idle)))
      (let ((game (sengoku-session-game session)))
        (should (= (sengoku-game-year game) 1563))
        (should (= (sengoku-game-month game) 3))))))

(ert-deftest sengoku-engine-defeat-ends-the-session ()
  (sengoku-test-with-zero-random
    (let* ((game (sengoku-new-game))
           (oda (sengoku-clan-index game "織田"))
           (matsudaira (sengoku-clan-index game "松平"))
           (session (sengoku-new-session game)))
      (setf (sengoku-game-player game) oda
            (sengoku-session-phase session) 'check-before-month)
      (dotimes (province-index (length (sengoku-game-provinces game)))
        (setf (sengoku-province-owner
               (aref (sengoku-game-provinces game) province-index))
              matsudaira))
      (let ((result (sengoku-engine-advance session)))
        (should (eq (plist-get result :status) 'game-over))
        (should (eq (plist-get result :outcome) 'defeat))
        (should (eq (sengoku-session-phase session) 'ended))
        (should (eq (sengoku-session-quit-reason session) 'defeat))))))

(ert-deftest sengoku-engine-victory-ends-the-session ()
  (sengoku-test-with-zero-random
    (let* ((game (sengoku-new-game))
           (oda (sengoku-clan-index game "織田"))
           (session (sengoku-new-session game)))
      (setf (sengoku-game-player game) oda
            (sengoku-session-phase session) 'check-before-month)
      (dotimes (province-index (length (sengoku-game-provinces game)))
        (setf (sengoku-province-owner
               (aref (sengoku-game-provinces game) province-index))
              oda))
      (let ((result (sengoku-engine-advance session)))
        (should (eq (plist-get result :status) 'game-over))
        (should (eq (plist-get result :outcome) 'victory))
        (should (eq (sengoku-session-phase session) 'ended))
        (should (eq (sengoku-session-quit-reason session) 'victory))))))

(ert-deftest sengoku-engine-reports-are-detached-and-stably-ordered ()
  (sengoku-test-with-zero-random
    (let* ((game (sengoku-new-game))
           (oda (sengoku-clan-index game "織田"))
           (generals (sengoku-engine-general-report game oda))
           (clans (sengoku-engine-clan-report game)))
      (should (= (length generals) 8))
      (should (plist-get (car generals) :lord))
      (should (= (plist-get (car generals) :index) 0))
      (should (= (length clans) 32))
      (should (= (plist-get (car clans) :index) 0)))))

(provide 'sengoku-engine-test)

;;; sengoku-engine-test.el ends here
