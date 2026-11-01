;;; agent-pages-ui-tests.el --- Agent page presentation contracts -*- lexical-binding: t; -*-
(require 'ert)
(require 'tools/agent/manager)
(require 'tools/agent/queue)
(require 'tools/agent/selection)

(ert-deftest agent-pages-c54-usage-renders-reported-metrics ()
  (let* ((source (generate-new-buffer " *usage-source*"))
         (buffer (generate-new-buffer " *usage-page*"))
         (session (list :buffer source :name "Parser review" :agent "codex" :root "/tmp/project/"
                        :usage '(:model "Fixture model" :context-used 144000 :context-size 200000
                                 :input 5200 :output 2800 :cached-read 10000 :cached-write 400
                                 :cost 0 :currency "USD" :turns 3 :compactions 1)))
         (model (etaf-ref (list :sessions (list session) :selected source :scope 'all
                                :query "" :total 1 :attention 0 :view 'usage))))
    (unwind-protect
        (progn
          (etaf-mount buffer (etaf-view (+agent-manager-page :model model :owner buffer))
                      '(:viewport-width 1400 :viewport-height 60))
          (with-current-buffer buffer
            (let ((text (buffer-string)))
              (dolist (value '("Usage" "72%" "Input tokens" "5.2k" "Output tokens"
                               "2.8k" "Cache read" "10.0k" "USD 0.00"))
                (should (string-match-p (regexp-quote value) text))))))
      (kill-buffer buffer) (kill-buffer source))))

(ert-deftest agent-pages-c54-missing-usage-does-not-render-a-zero-meter ()
  (let ((buffer (generate-new-buffer " *unknown-usage*")))
    (unwind-protect
        (progn
          (etaf-mount buffer
                      (etaf-view (+agent-manager-usage-detail
                                  :session '(:name "Waiting for usage"
                                             :usage (:context-used 0 :context-size 0 :input 0 :output 0))))
                      '(:viewport-width 900 :viewport-height 40))
          (with-current-buffer buffer
            (should (string-match-p "not reported" (buffer-string)))
            (should (string-match-p "—" (buffer-string)))
            (should-not (string-match-p "0%" (buffer-string)))))
      (kill-buffer buffer))))

(ert-deftest agent-pages-c54-queue-scrolls-without-dropping-items ()
  (let* ((buffer (generate-new-buffer " *queue-page*"))
         (model (etaf-ref (list :title "Fixture session" :state "Working" :selected 0 :held 2
                               :items (cl-loop for index below 20 collect (format "Follow-up %02d" index))))))
    (unwind-protect
        (progn
          (etaf-mount buffer (etaf-view (+agent-queue-page :model model :owner buffer))
                      '(:viewport-width 900 :viewport-height 24))
          (with-current-buffer buffer
            (should (string-match-p "20 pending" (buffer-string)))
            (should (string-match-p "2 held" (buffer-string)))
            (should (> (etaf-scroll-to (etaf-runtime-for-buffer buffer) 'agent-queue-list 'end) 0))
            (should (= 20 (length (plist-get (etaf-value model) :items))))))
      (kill-buffer buffer))))

(ert-deftest agent-pages-c54-native-context-has-an-empty-state-and-keeps-ids ()
  (with-temp-buffer
    (+agent-selection-review-mode)
    (let ((+agent-selection nil))
      (+agent-selection-review-refresh)
      (should (string-match-p "No context selected" (buffer-string))))
    (let ((+agent-selection '(("/tmp/context.md"))))
      (+agent-selection-review-refresh)
      (goto-char (point-min))
      (while (and (not (eobp)) (not (tabulated-list-get-id))) (forward-line 1))
      (should (equal (tabulated-list-get-id) "/tmp/context.md"))
      (should (string-match-p "1 references" (mapconcat #'identity header-line-format ""))))))

(provide 'agent-pages-ui-tests)
