;;; sengoku-combat-test.el --- Combat tests for Sengoku -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'sengoku-combat)
(require 'sengoku-core-test)

(defun sengoku-test--battle-fixture ()
  "Return deterministic GAME, ODA, OWARI, and MIKAWA values."
  (let* ((game (sengoku-new-game))
         (oda (sengoku-clan-index game "織田"))
         (owari (sengoku-province-index game "尾張"))
         (mikawa (sengoku-province-index game "三河")))
    (list game oda owari mikawa)))

(ert-deftest sengoku-battle-prepare-deducts-committed-resources ()
  (sengoku-test-with-zero-random
    (pcase-let* ((`(,game ,oda ,owari ,mikawa)
                  (sengoku-test--battle-fixture))
                 (source (aref (sengoku-game-provinces game) owari)))
      (setf (sengoku-province-soldiers source) 6000
            (sengoku-province-guns source) 120
            (sengoku-province-cannon source) 5
            (sengoku-province-rice source) 1000)
      (let ((context (sengoku-battle-prepare game oda owari mikawa 3000)))
        (should (= (sengoku-battle-context-guns context) 60))
        (should (= (sengoku-battle-context-cannons context) 2))
        (should (= (sengoku-province-soldiers source) 3000))
        (should (= (sengoku-province-guns source) 60))
        (should (= (sengoku-province-cannon source) 3))
        (should (= (sengoku-province-rice source) 850))))))

(ert-deftest sengoku-undefended-battle-conquers-immediately ()
  (sengoku-test-with-zero-random
    (pcase-let* ((`(,game ,oda ,owari ,mikawa)
                  (sengoku-test--battle-fixture))
                 (target (aref (sengoku-game-provinces game) mikawa)))
      (setf (sengoku-province-soldiers target) 0)
      (let ((context (sengoku-battle-start
                      game oda owari mikawa 1000 'automatic)))
        (should (sengoku-battle-won-p context))
        (should (= (sengoku-province-owner target) oda))
        (should (= (sengoku-province-soldiers target) 1000))
        (should-not (sengoku-validate-resources game))))))

(ert-deftest sengoku-automatic-battle-preserves-valid-resources ()
  (let ((sengoku-random-function (lambda (_upper-bound) 20)))
    (pcase-let* ((`(,game ,oda ,owari ,mikawa)
                  (sengoku-test--battle-fixture))
                 (source (aref (sengoku-game-provinces game) owari))
                 (target (aref (sengoku-game-provinces game) mikawa)))
      (setf (sengoku-province-soldiers source) 12000
            (sengoku-province-training source) 90
            (sengoku-province-guns source) 200
            (sengoku-province-cannon source) 3
            (sengoku-province-soldiers target) 1500
            (sengoku-province-training target) 20
            (sengoku-province-guns target) 0)
      (let ((context (sengoku-battle-start
                      game oda owari mikawa 9000 'automatic)))
        (should (sengoku-battle-finished-p context))
        (should (memq (sengoku-battle-context-result context)
                      '(victory defeat)))
        (should-not (sengoku-validate-resources game))))))

(ert-deftest sengoku-siege-constant-randomness-does-not-hang ()
  (sengoku-test-with-zero-random
    (pcase-let* ((`(,game ,oda ,owari ,mikawa)
                  (sengoku-test--battle-fixture))
                 (context (sengoku-battle-start
                           game oda owari mikawa 3000 'siege
                           :player-side -2))
                 (siege (sengoku-battle-context-siege context))
                 (forests 0))
      (dotimes (row sengoku-data-siege-rows)
        (dotimes (column sengoku-data-siege-columns)
          (when (string= (aref (aref (sengoku-siege-grid siege) row)
                               column)
                         "森")
            (setq forests (1+ forests)))))
      (should (= forests 7))
      (should (memq (sengoku-siege-run-all-ai context)
                    '(fall annihilated destroyed timeout)))
      (sengoku-battle-apply-siege-result game context)
      (should (sengoku-battle-finished-p context))
      (should-not (sengoku-validate-resources game)))))

(ert-deftest sengoku-siege-player-side-must-match-participants ()
  (sengoku-test-with-zero-random
    (pcase-let* ((`(,game ,oda ,owari ,mikawa)
                  (sengoku-test--battle-fixture)))
      (setf (sengoku-game-player game) oda)
      (should-error
       (sengoku-battle-start game oda owari mikawa 2000 'siege
                             :player-side 1)))))

(ert-deftest sengoku-siege-player-snapshot-is-detached ()
  (sengoku-test-with-zero-random
    (pcase-let* ((`(,game ,oda ,owari ,mikawa)
                  (sengoku-test--battle-fixture)))
      (setf (sengoku-game-player game) oda)
      (let* ((context (sengoku-battle-start
                       game oda owari mikawa 3000 'siege :player-side 0)))
        (should (eq (sengoku-siege-advance-to-player context) 'player-turn))
        (let* ((snapshot (sengoku-siege-player-turn-state context))
               (copy (plist-get snapshot :unit))
               (row (sengoku-siege-unit-row copy)))
          (setf (sengoku-siege-unit-row copy) 99)
          (should (= (sengoku-siege-unit-row
                      (plist-get (sengoku-siege-player-turn-state context) :unit))
                     row)))))))

(ert-deftest sengoku-siege-rejects-negative-target-index ()
  (sengoku-test-with-zero-random
    (pcase-let* ((`(,game ,oda ,owari ,mikawa)
                  (sengoku-test--battle-fixture)))
      (setf (sengoku-game-player game) oda)
      (let ((context (sengoku-battle-start
                      game oda owari mikawa 3000 'siege :player-side 0)))
        (sengoku-siege-advance-to-player context)
        ;; The initial unit has no adjacent target; move it beside a defender to
        ;; exercise index validation without depending on a long tactical path.
        (let* ((siege (sengoku-battle-context-siege context))
               (live-unit (aref (sengoku-siege-units siege) 0)))
          (setf (sengoku-siege-unit-row live-unit) 4
                (sengoku-siege-unit-column live-unit) 6)
          (should (sengoku-siege-player-adjacent-targets context))
          (should-error (sengoku-siege-player-attack context -1)))))))

(ert-deftest sengoku-six-all-ai-sieges-keep-ownership-consistent ()
  (dotimes (iteration 6)
    (let ((sengoku-random-function #'random))
      (pcase-let* ((`(,game ,oda ,owari ,mikawa)
                    (sengoku-test--battle-fixture))
                   (source (aref (sengoku-game-provinces game) owari)))
        (setf (sengoku-province-soldiers source) 9000
              (sengoku-province-guns source) 60
              (sengoku-province-training source) 70
              (sengoku-province-cannon source) (mod iteration 3))
        (let* ((context (sengoku-battle-start
                         game oda owari mikawa (+ 4000 (* iteration 1000))
                         'siege :general
                         (aref (aref (sengoku-game-generals game) oda)
                               (mod iteration 8))
                         :player-side -2)))
          (sengoku-siege-run-all-ai context)
          (sengoku-battle-apply-siege-result game context)
          (if (sengoku-battle-won-p context)
              (should (= (sengoku-province-owner
                          (aref (sengoku-game-provinces game) mikawa))
                         oda))
            (should (/= (sengoku-province-owner
                         (aref (sengoku-game-provinces game) mikawa))
                        oda)))
          (should-not (sengoku-validate-resources game)))))))

(provide 'sengoku-combat-test)

;;; sengoku-combat-test.el ends here
