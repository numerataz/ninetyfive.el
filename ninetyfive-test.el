;;; ninetyfive-test.el --- Tests for ninetyfive.el -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for the file synchronisation protocol.  Run with:
;;
;;   emacs -batch -l ert -l ninetyfive.el -l ninetyfive-test.el \
;;     -f ert-run-tests-batch-and-exit
;;
;; (websocket and async must be on the `load-path'.)
;;
;; Outgoing messages are captured instead of being sent, and every
;; `file-content' / `file-delta' message is applied to a byte-string model
;; of the server's copy of the file exactly as the server applies it
;; (content[:start] + text + content[end:], all offsets in UTF-8 bytes).
;; After every edit the model must equal the buffer's encoded contents.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'ninetyfive)

(defvar ninetyfive-test--messages nil
  "Messages captured from `ninetyfive--send-message', oldest first.")

(defvar ninetyfive-test--model nil
  "Unibyte string modelling the server's copy of the active file.")

(defvar ninetyfive-test--model-path nil
  "Path of the file the server considers active.")

(defun ninetyfive-test--apply (message)
  "Apply MESSAGE to the server model the way the server does."
  (pcase (cdr (assq 'type message))
    ("file-content"
     (setq ninetyfive-test--model-path (cdr (assq 'path message))
           ninetyfive-test--model
           (encode-coding-string (cdr (assq 'text message)) 'utf-8 t)))
    ("file-delta"
     (let ((start (cdr (assq 'start message)))
           (end (cdr (assq 'end message)))
           (text (encode-coding-string (cdr (assq 'text message)) 'utf-8 t)))
       (should ninetyfive-test--model)
       ;; Mirrors the server's PreTokenizedString.replace assertion.
       (should (<= 0 start end (length ninetyfive-test--model)))
       (setq ninetyfive-test--model
             (concat (substring ninetyfive-test--model 0 start)
                     text
                     (substring ninetyfive-test--model end)))))))

(defun ninetyfive-test--capture (message)
  "Record MESSAGE and apply it to the server model."
  (setq ninetyfive-test--messages
        (append ninetyfive-test--messages (list message)))
  (ninetyfive-test--apply message))

(defun ninetyfive-test--buffer-bytes ()
  "Return the whole (widened) current buffer encoded as UTF-8."
  (save-restriction
    (widen)
    (encode-coding-string
     (buffer-substring-no-properties (point-min) (point-max)) 'utf-8 t)))

(defmacro ninetyfive-test--with-session (&rest body)
  "Run BODY with a fake connection that captures outgoing messages."
  (declare (indent 0) (debug t))
  `(let ((ninetyfive--connected t)
         (ninetyfive--last-buffer nil)
         (ninetyfive--completion-text "")
         (ninetyfive--current-request-id nil)
         (ninetyfive-test--messages nil)
         (ninetyfive-test--model nil)
         (ninetyfive-test--model-path nil))
     (cl-letf (((symbol-function 'ninetyfive--send-message)
                #'ninetyfive-test--capture))
       ,@body
       ;; Only the first edit may send the full content; every later
       ;; edit must have gone through as a delta.
       (should (= 1 (cl-count "file-content" ninetyfive-test--messages
                              :key (lambda (m) (cdr (assq 'type m)))
                              :test #'equal))))))

(defmacro ninetyfive-test--with-buffer (initial &rest body)
  "Run BODY in a temp buffer containing INITIAL with `ninetyfive-mode' on."
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (insert ,initial)
     (ninetyfive-mode 1)
     (unwind-protect (progn ,@body)
       (ninetyfive-mode -1))))

(defmacro ninetyfive-test--check (form)
  "Evaluate edit FORM, then assert the server model matches the buffer."
  `(progn
     ,form
     (should (equal (list ',form ninetyfive-test--model)
                    (list ',form (ninetyfive-test--buffer-bytes))))))

(ert-deftest ninetyfive-test-file-deltas-track-buffer ()
  "A series of edits keeps the server's copy identical to the buffer."
  (ninetyfive-test--with-session
    (ninetyfive-test--with-buffer "hello world\nsecond line\n"
      ;; The first edit sends the full content.
      (ninetyfive-test--check (progn (goto-char 6) (insert ",")))
      (should (equal (cdr (assq 'type (car ninetyfive-test--messages)))
                     "file-content"))
      ;; Plain insertion in the middle.
      (ninetyfive-test--check (progn (goto-char 3) (insert "x")))
      ;; Multi-character insertion.
      (ninetyfive-test--check (progn (goto-char 1) (insert "START ")))
      ;; Deletion of one and of several characters.
      (ninetyfive-test--check (delete-region 4 5))
      (ninetyfive-test--check (delete-region 2 7))
      ;; Insertion and deletion at the very start and end of the buffer.
      (ninetyfive-test--check (progn (goto-char (point-min)) (insert "A")))
      (ninetyfive-test--check (progn (goto-char (point-max)) (insert "Z")))
      (ninetyfive-test--check (delete-region (point-min) (1+ (point-min))))
      (ninetyfive-test--check (delete-region (1- (point-max)) (point-max)))
      ;; Multibyte insertion, deletion and replacement.
      (ninetyfive-test--check (progn (goto-char 4) (insert "é€😀")))
      (ninetyfive-test--check (progn (goto-char (point-max)) (insert "日本語")))
      (ninetyfive-test--check (delete-region 5 6))
      (ninetyfive-test--check
       (progn (goto-char (point-min))
              (re-search-forward "日本")
              (replace-match "ascii")))
      (ninetyfive-test--check
       (progn (goto-char (point-min))
              (re-search-forward "second")
              (replace-match "ŝéčöñð")))
      ;; Edits with point after multibyte text.
      (ninetyfive-test--check (progn (goto-char (point-max)) (insert "\nend")))
      (ninetyfive-test--check (delete-region (- (point-max) 4) (- (point-max) 1)))
      ;; Replacement via primitives whose before/after regions differ.
      (ninetyfive-test--check (upcase-region (point-min) (point-max)))
      (ninetyfive-test--check (transpose-regions 1 3 6 9))
      (ninetyfive-test--check (subst-char-in-region (point-min) (point-max) ?É ?Ô))
      ;; Deleting everything and starting over.
      (ninetyfive-test--check (erase-buffer))
      (ninetyfive-test--check (insert "fresh ✓ text"))
      (should (cl-every (lambda (m)
                          (member (cdr (assq 'type m))
                                  '("file-delta" "delta-completion-request")))
                        (cdr ninetyfive-test--messages))))))

(ert-deftest ninetyfive-test-file-deltas-ignore-narrowing ()
  "Offsets are relative to the whole buffer even when it is narrowed."
  (ninetyfive-test--with-session
    (ninetyfive-test--with-buffer "αβγ header\nbody line\nfooter ✓\n"
      (ninetyfive-test--check (progn (goto-char (point-max)) (insert "x")))
      (save-excursion
        (goto-char (point-min))
        (forward-line 1)
        (narrow-to-region (point) (line-end-position))
        (ninetyfive-test--check (progn (goto-char (point-min)) (insert "→")))
        (ninetyfive-test--check (delete-region (+ (point-min) 2) (+ (point-min) 4)))
        (ninetyfive-test--check (progn (goto-char (point-max)) (insert "😀")))
        ;; The completion request position is a byte offset into the
        ;; same (widened) text.
        (let ((req (car (last ninetyfive-test--messages))))
          (should (equal (cdr (assq 'type req)) "delta-completion-request"))
          (should (= (cdr (assq 'pos req))
                     (length (encode-coding-string
                              (save-restriction
                                (widen)
                                (buffer-substring-no-properties 1 (point)))
                              'utf-8 t)))))
        (widen))
      (ninetyfive-test--check (progn (goto-char (point-min)) (insert "top"))))))

(ert-deftest ninetyfive-test-file-content-after-narrowing ()
  "The initial full content is the whole buffer, not the narrowed part."
  (ninetyfive-test--with-session
    (ninetyfive-test--with-buffer "one\ntwo\nthree\n"
      (narrow-to-region 5 8)
      (ninetyfive-test--check (progn (goto-char (point-min)) (insert "2:")))
      (ninetyfive-test--check (progn (goto-char (point-max)) (insert "!"))))))

(ert-deftest ninetyfive-test-accept-completion-sends-deltas ()
  "Accepting a completion keeps the server copy in sync."
  (ninetyfive-test--with-session
    (ninetyfive-test--with-buffer "def foo(): pass\n"
      (ninetyfive-test--check (progn (goto-char 9) (insert "a")))
      (goto-char 10)
      (setq ninetyfive--completion-text "rgs): return 1  # ✓")
      (ninetyfive--show-completion ninetyfive--completion-text)
      (ninetyfive-test--check (ninetyfive--accept-completion)))))

(ert-deftest ninetyfive-test-first-edit-sends-own-buffer ()
  "The first edit in a buffer sends that buffer's content."
  (ninetyfive-test--with-session
    (let ((other (generate-new-buffer " *ninetyfive-other*")))
      (unwind-protect
          (ninetyfive-test--with-buffer "this buffer\n"
            (with-current-buffer other
              (insert "some other buffer\n")
              (ninetyfive-mode 1))
            ;; `other' is now the last buffer; edit this one.
            (should (eq ninetyfive--last-buffer other))
            (ninetyfive-test--check (progn (goto-char (point-max)) (insert "x")))
            (ninetyfive-test--check (progn (goto-char 1) (insert "y"))))
        (kill-buffer other)))))

(provide 'ninetyfive-test)

;;; ninetyfive-test.el ends here
