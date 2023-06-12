;;; format-region.el --- Transform region. -*- lexical-binding: t;
;;
;; Copyright © 2023 Andros Fenollosa
;; Authors: Andros Fenollosa <andros@fenollosa.email>
;; URL: https://github.com/tanrax/format-region.el
;; Version: 1.0.0
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Transform region words to: camelCase, lisp-case, PascalCase and snake_case.

;;; Code:

(defun format-region-to-format (sentence
                                separator
                                is-first-word-capitalized
                                is-all-words-capitalized)
  "Convert SENTENCE to format.
SEPARATOR is the character that will separate the words.
IS-FIRST-WORD-CAPITALIZED is a boolean that indicates
if the first word should be capitalized.
IS-ALL-WORDS-CAPITALIZED is a boolean that indicates
if all words should be capitalized."
  (let* ((words-lower-case (downcase sentence)) ; To lowercase.
         (words (split-string words-lower-case " ")) ; Split sentence into words
					; by spaces.
         (words-case (if is-all-words-capitalized (mapcar #'capitalize words)
                                words)) ; Capitalize first letter of each word.
         (sentence-with-new-separator (mapconcat #'identity words-case
                                separator)) ; Join words with separator.
         (sentence-with-first-word-capitalized (if is-first-word-capitalized
                                sentence-with-new-separator (concat (downcase
                                (substring sentence-with-new-separator 0 1))
                                (substring sentence-with-new-separator 1)))) ; Capitalize first letter of first word.
         )
    sentence-with-first-word-capitalized))


(defmacro format-region-curried (separator
                                 is-first-word-capitalized
                                 is-all-words-capitalized)
  "Curry the function to-format:
SEPARATOR, IS-FIRST-WORD-CAPITALIZED and IS-ALL-WORDS-CAPITALIZED."
  `(lambda (sentence) (format-region-to-format sentence ,separator ,is-first-word-capitalized ,is-all-words-capitalized)))

;; Define functions
(defvar format-region-to-format-camel-case (format-region-curried nil nil t)) ; camelCase
(defvar format-region-to-format-kebab-case (format-region-curried "-" nil nil)) ; kebab-case or lisp-case
(defvar format-region-to-format-pascal-case (format-region-curried nil t t)) ; PascalCase
(defvar format-region-to-format-snake-case (format-region-curried "_" nil nil)) ; snake_case

(defun format-region-selected (fn-format)
  "Format the selected region with FN-FORMAT."
  (interactive)
  (let ((text (buffer-substring-no-properties (region-beginning) (region-end))))
    (delete-region (region-beginning) (region-end))
    (insert (funcall fn-format text))))

;; Interactive functions

(defun format-region-to-camel-case ()
  "Convert the selected text to camelCase."
  (interactive)
  (format-region-selected format-region-to-format-camel-case))

(defun format-region-to-kebab-case ()
  "Convert the selected text to `kebab-case' or lisp-case."
  (interactive)
  (format-region-selected format-region-to-format-kebab-case))

(defun format-region-to-lisp-case ()
  "Convert the selected text to `kebab-case' or lisp-case."
  (interactive)
  (format-region-selected format-region-to-format-kebab-case))

(defun format-region-to-pascal-case ()
  "Convert the selected text to PascalCase."
  (interactive)
  (format-region-selected format-region-to-format-pascal-case))

(defun format-region-to-snake-case ()
  "Convert the selected text to snake_case."
  (interactive)
  (format-region-selected format-region-to-format-snake-case))

;; Tests
(defun format-region-debug ()
  "Check if all functions work correctly. Print OK if so."
  (let (
        (is-camel-case-ok (string= (funcall format-region-to-format-camel-case "hello world") "helloWorld"))
        (is-kebab-case-ok (string= (funcall format-region-to-format-kebab-case "hello world") "hello-world"))
        (is-pascal-case-ok (string= (funcall format-region-to-format-pascal-case "hello world") "HelloWorld"))
        (is-snake-case-ok (string= (funcall format-region-to-format-snake-case "hello world") "hello_world")))
    (if (and is-camel-case-ok is-kebab-case-ok is-pascal-case-ok is-snake-case-ok)
        (message "OK")
      (message "ERROR"))))

(provide 'format-region)

;;; format-region.el ends here
