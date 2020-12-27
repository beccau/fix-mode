;; fix-mode.el --- An emacs mode for viewing and decoding FIX-message logs

;; Copyright 2020 Andreas Beccau
;; 
;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:
;; 
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;; 
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.


;; Usage:
;;
;; In your .emacs, put:
;; (setq fix-mode-fix42-xml ".../FIX42.xml")
;; (setq fix-mode-fix44-xml ".../FIX44.xml")
;; (setq fix-mode-fix50-xml ".../FIX50SP2.xml")
;;
;; Where the XML files can be found eg. at:
;; https://github.com/quickfix-j/quickfixj/tree/master/quickfixj-messages/quickfixj-messages-fix44/src/main/resources
;; https://github.com/quickfix-j/quickfixj/tree/master/quickfixj-messages/quickfixj-messages-fix42/src/main/resources
;; https://github.com/quickfix-j/quickfixj/tree/master/quickfixj-messages/quickfixj-messages-fix50sp2/src/main/resources
;;

(require 'dom)
(require 'subr-x)

;; User vars
(defvar fix-mode-fix42-xml "/FIX42.xml" "XML-file describing FIX42")
(defvar fix-mode-fix44-xml "/FIX44.xml" "XML-file describing FIX44")
(defvar fix-mode-fix50-xml "/FIX50SP2.xml" "XML-file describing FIX50SP2")

(defvar fix-mode-parsed-buffer "*parsed-fix*")


(defconst fix-delim-char "")
(defconst fix-delim-char-2 "|")

(defun fix-read-xml (filepath)
  (with-temp-buffer
    (insert-file-contents filepath)
    (libxml-parse-xml-region (point-min) (point-max))))

(defvar data-dictionary `(("FIX.4.2" . ,(fix-read-xml fix-mode-fix42-xml))
                          ("FIX.4.4" . ,(fix-read-xml fix-mode-fix44-xml))
                          ("FIXT.1.1" . ,(fix-read-xml fix-mode-fix50-xml))))


(defun detect-delimiter (line)
  (cond ((seq-contains-p line (elt fix-delim-char 0)) fix-delim-char)
        ((seq-contains-p line (elt fix-delim-char-2 0)) fix-delim-char-2)))


(defun fix-tokenize-line (line)
  (let ((delim (detect-delimiter line)))
        (if delim (split-string line delim t) ())))


(defun split-pair (pair) (split-string pair "=" t))
(defun tokenize (line) (mapcar 'split-pair (fix-tokenize-line line)))


(defun fix-mode--find-in-dom (dd value attr)
  (dom-search dd (lambda (node)
                   (equal value (dom-attr node attr)))))


(defun fix-mode--enum-name-by-value (dd value)
  (dom-attr (fix-mode--find-in-dom dd value 'enum) 'description))


(defun fix-mode--find-named-tag-value (dd tag value)
  (let* ((field-node (fix-mode--find-in-dom dd tag 'number))
         (tag-name (dom-attr field-node 'name))
         (value-name (if (dom-children field-node)
                         (fix-mode--enum-name-by-value field-node value) ())))
    `(,tag-name . ,value-name)))


(defun fix-mode--format-key-val (fields-dict pair)
  (let* ((raw-tag (car pair))
         (raw-value (nth 1 pair))
         (named-tag-value (fix-mode--find-named-tag-value fields-dict raw-tag raw-value))
         (named-tag (car named-tag-value))
         (named-value (cdr named-tag-value)))
    (format "> %s[%s] = %s[%s]\n"
            (or named-tag "") raw-tag (or named-value "") raw-value)))


(defun fix-mode--decode-message (line)
  (let* ((tokenized (tokenize line))
         (version (nth 1 (car tokenized)))
         (dd (cdr (assoc version data-dictionary)))
         (fields-dict (dom-by-tag dd 'fields)))
    (mapcar (apply-partially 'fix-mode--format-key-val fields-dict) tokenized)))


(defun fix-mode--parse-line ()
  "Parse FIX message"
  (interactive)
  (let ((current-line (thing-at-point 'line t)))
        (get-buffer-create fix-mode-parsed-buffer)
        (set-buffer fix-mode-parsed-buffer)
        (read-only-mode)
        (let ((inhibit-read-only t)
          (line (string-trim-right current-line)))
          (goto-char (point-max))
          (insert (string-join (fix-mode--decode-message line)))
          (insert "----------------------------------------------\n")))
  (display-buffer fix-mode-parsed-buffer))


(defvar fix-mode-map nil "Keymap for `fix-mode'")
(progn
  (setq fix-mode-map (make-sparse-keymap))
  (define-key fix-mode-map (kbd "C-c C-c") 'fix-mode--parse-line))

(setq fix-mode-highlights
      '(("\\([0-9]+\\)=" . (1 font-lock-keyword-face))
        ("=\\([0-9a-zA-Z\-\:\.]+\\)" . (1 font-lock-builtin-face))
        ("=" . font-lock-comment-face)))

(define-derived-mode fix-mode fundamental-mode "FIX"
  "Mode for viewing FIX-log files.
\\{fix-mode-map}"
  (setq font-lock-defaults '(fix-mode-highlights)))

(provide 'fix-mode)

