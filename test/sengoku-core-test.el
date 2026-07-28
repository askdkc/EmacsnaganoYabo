;;; sengoku-core-test.el --- Core tests for Sengoku -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'sengoku-save)

(defmacro sengoku-test-with-zero-random (&rest body)
  "Evaluate BODY with deterministic zero-valued game randomness."
  (declare (indent 0) (debug t))
  `(let ((sengoku-random-function (lambda (_upper-bound) 0)))
     ,@body))

(ert-deftest sengoku-data-has-expected-shape ()
  (should (= (length sengoku-data-clans) 32))
  (should (= (length sengoku-data-provinces) 41))
  (should (= (length sengoku-data-retainers) 32))
  (should (= (length sengoku-data-items) 10))
  (should (= (cl-loop for row across sengoku-data-retainers
                      sum (length row))
             224))
  (should-not (sengoku-validate-data)))

(ert-deftest sengoku-new-game-builds-valid-runtime-state ()
  (sengoku-test-with-zero-random
    (let ((game (sengoku-new-game)))
      (should (= (sengoku-game-year game) 1560))
      (should (= (sengoku-game-month game) 3))
      (should (= (sengoku-game-player game) -1))
      (should (= (length (sengoku-game-provinces game)) 41))
      (should (= (length (sengoku-game-generals game)) 32))
      (dotimes (clan-index 32)
        (should (= (length (aref (sengoku-game-generals game) clan-index)) 8))
        (should (= (length (sengoku-unused-generals game clan-index)) 8)))
      (should (= (sengoku-item-owner game 0) 18))
      (should (= (sengoku-item-owner game 1) 18))
      (should (= (sengoku-item-owner game 5) 17))
      (should (= (sengoku-item-owner game 6) 11))
      ;; Base culture plus the initial tea-item bonuses.
      (should (= (sengoku-culture game 18) 47))
      (should (= (sengoku-culture game 17) 28))
      (should (= (sengoku-culture game 11) 32))
      (should-not (sengoku-validate-game game)))))

(ert-deftest sengoku-general-use-and-selection-preserve-roster-order ()
  (sengoku-test-with-zero-random
    (let* ((game (sengoku-new-game))
           (oda (sengoku-clan-index game "織田"))
           (best-war (sengoku-best-general game oda 'war))
           (best-politics (sengoku-best-general game oda 'politics)))
      (should (= best-war 0))
      (should (= best-politics 2))
      (sengoku-use-general game oda best-war)
      (should (sengoku-general-used-p game oda best-war))
      (should (= (sengoku-best-general game oda 'war) 1))
      (sengoku-reset-generals game)
      (should (= (length (sengoku-unused-generals game oda)) 8)))))

(ert-deftest sengoku-alliance-helpers-use-canonical-keys ()
  (sengoku-test-with-zero-random
    (let ((game (sengoku-new-game)))
      (should (equal (sengoku-alliance-key 13 4) "4-13"))
      (sengoku-set-alliance game 13 4 t)
      (should (sengoku-allied-p game 4 13))
      (should (equal (gethash "4-13" (sengoku-game-alliances game)) 1))
      (sengoku-drop-alliances game 4)
      (should-not (sengoku-allied-p game 4 13)))))

(ert-deftest sengoku-conquest-eliminates-clan-and-captures-items ()
  (sengoku-test-with-zero-random
    (let* ((game (sengoku-new-game))
           (attacker (sengoku-clan-index game "織田"))
           (defender (sengoku-clan-index game "松永"))
           (target (car (sengoku-clan-provinces game defender)))
           (lines (sengoku-conquer-apply
                   game attacker target 1500 20 60 nil)))
      (should-not (sengoku-clan-alive-p game defender))
      (should (= (sengoku-province-owner
                  (aref (sengoku-game-provinces game) target))
                 attacker))
      (should (equal (sengoku-clan-state-items
                      (aref (sengoku-game-clan-states game) defender))
                     nil))
      (should (memq 0 (sengoku-clan-state-items
                       (aref (sengoku-game-clan-states game) attacker))))
      (should (memq 1 (sengoku-clan-state-items
                       (aref (sengoku-game-clan-states game) attacker))))
      (should (cl-some (lambda (line) (string-match-p "滅亡" line)) lines))
      (should-not (sengoku-validate-game game)))))

(ert-deftest sengoku-log-keeps-latest-three-hundred-entries ()
  (sengoku-test-with-zero-random
    (let ((game (sengoku-new-game)))
      (dotimes (index 305)
        (sengoku-log game (format "log-%03d" index)))
      (should (= (length (sengoku-game-log game)) 300))
      (should (string-match-p "log-005" (car (sengoku-game-log game))))
      (should (string-match-p "log-304" (car (last (sengoku-game-log game))))))))

(ert-deftest sengoku-save-round-trip-preserves-persistent-state ()
  (sengoku-test-with-zero-random
    (let* ((game (sengoku-new-game))
           (oda (sengoku-clan-index game "織田"))
           (owari (sengoku-province-index game "尾張")))
      (setf (sengoku-game-player game) oda
            (sengoku-game-year game) 1564
            (sengoku-game-month game) 11
            (sengoku-province-gold
             (aref (sengoku-game-provinces game) owari)) 4321
            (sengoku-province-auto
             (aref (sengoku-game-provinces game) owari)) 1)
      (sengoku-set-alliance game oda (sengoku-clan-index game "松平") t)
      (dotimes (index 35)
        (sengoku-log game (format "event-%d" index)))
      (sengoku-use-general game oda 0)
      (let* ((json (sengoku-save-encode-string game))
             (loaded (sengoku-save-decode-string json))
             (loaded-owari (aref (sengoku-game-provinces loaded) owari)))
        (should (= (sengoku-game-player loaded) oda))
        (should (= (sengoku-game-year loaded) 1564))
        (should (= (sengoku-game-month loaded) 11))
        (should (= (sengoku-province-gold loaded-owari) 4321))
        (should (= (sengoku-province-auto loaded-owari) 1))
        (should (= (length (sengoku-game-log loaded)) 30))
        (should (sengoku-allied-p loaded oda
                                  (sengoku-clan-index loaded "松平")))
        (should (= (length (sengoku-unused-generals loaded oda)) 8))
        (should-not (sengoku-validate-game loaded))))))

(ert-deftest sengoku-save-rejects-unsupported-version ()
  (sengoku-test-with-zero-random
    (let* ((object (sengoku-save-encode-object (sengoku-new-game)))
           (version-cell (assq 'ver object)))
      (setcdr version-cell 4)
      (should-error (sengoku-save-decode-object object)
                    :type 'sengoku-save-version-error))))

(ert-deftest sengoku-save-refuses-empty-serializer-output ()
  (sengoku-test-with-zero-random
    (let* ((directory (make-temp-file "sengoku-save-empty-" t))
           (target (expand-file-name "save.json" directory))
           (original "valid-existing-save\n"))
      (unwind-protect
          (progn
            (write-region original nil target nil 'silent)
            (cl-letf (((symbol-function 'json-encode)
                       (lambda (_object) nil)))
              (should-error (sengoku-save-game (sengoku-new-game) target)
                            :type 'sengoku-save-format-error))
            (with-temp-buffer
              (insert-file-contents target)
              (should (string= (buffer-string) original))))
        (delete-directory directory t)))))

(ert-deftest sengoku-save-prefers-configured-file-over-legacy ()
  (let* ((directory (make-temp-file "sengoku-save-test-" t))
         (configured (expand-file-name "configured.json" directory))
         (legacy (expand-file-name "legacy.json" directory)))
    (unwind-protect
        (progn
          (write-region "{}" nil legacy nil 'silent)
          (should (string= (sengoku-save-choose-load-path configured legacy)
                           legacy))
          (write-region "{}" nil configured nil 'silent)
          (should (string= (sengoku-save-choose-load-path configured legacy)
                           configured)))
      (delete-directory directory t))))

(provide 'sengoku-core-test)

;;; sengoku-core-test.el ends here
