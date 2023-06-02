;;; transform-texts-to-formats.el --- Convert plain text into different formats: camelCase, kebap-case or lisp-case, PascalCase or snake_case.
;;
;; Copyright © 2023 Andros Fenollosa
;; Authors: Andros Fenollosa <andros@fenollosa.email>
;; URL: https://github.com/tanrax/transform-texts-to-formats.el
;; Version: 1.0.0
;; SPDX-License-Identifier: GNU General Public License v3.0 or later

;;; Commentary:
;; Convert plain text into different formats: camelCase, kebap-case or lisp-case, PascalCase or snake_case.

;;; Code:


(defun to-format (sentence separator is-first-word-capitalized is-all-words-capitalized)
  "Convert SENTENCE to format.
SEPARATOR is the character that will separate the words.
IS-FIRST-WORD-CAPITALIZED is a boolean that indicates if the first word should be capitalized.
IS-ALL-WORDS-CAPITALIZED is a boolean that indicates if all words should be capitalized."
  (let* ((words-lower-case (downcase sentence)) ; To lowercase
	 (words (split-string words-lower-case " ")) ; Split sentence into words by spaces
	 (words-case (if is-all-words-capitalized (mapcar #'capitalize words) words)) ; Capitalize first letter of each word
	 (sentence-with-new-separator (mapconcat 'identity words-case separator)) ; Join words with separator
	 (sentence-with-first-word-capitalized (if is-first-word-capitalized (concat (capitalize (substring sentence-with-new-separator 0 1)) (substring sentence-with-new-separator 1)) sentence-with-new-separator)) ; Capitalize first letter of first word
	 )
    sentence-with-first-word-capitalized))


(defmacro curried-format (separator is-first-word-capitalized is-all-words-capitalized)
  "Curry the function to-format with SEPARATOR, IS-FIRST-WORD-CAPITALIZED and IS-ALL-WORDS-CAPITALIZED."
  `(lambda (sentence) (to-format sentence ,separator ,is-first-word-capitalized ,is-all-words-capitalized)))

;; Define functions
(setq to-camel-case (curried-format nil nil t)) ; camelCase
(setq to-kebab-case (curried-format "-" nil nil)) ; kebab-case
(setq to-pascal-case (curried-format nil t t)) ; PascalCase
(setq to-snake-case (curried-format "_" nil nil)) ; snake_case

;; Interactive functions

(defun format-to-camel-case ()
  "Convert the selected text to camelCase."
  (interactive)
  (let ((text (buffer-substring-no-properties (region-beginning) (region-end))))
    (delete-region (region-beginning) (region-end))
    (insert (funcall to-camel-case text))))

(defun format-to-kebab-case ()
  "Convert the selected text to kebab-case."
  (interactive)
  (let ((text (buffer-substring-no-properties (region-beginning) (region-end))))
    (delete-region (region-beginning) (region-end))
    (insert (funcall to-kebab-case text))))

(defun format-to-pascal-case ()
  "Convert the selected text to PascalCase."
  (interactive)
  (let ((text (buffer-substring-no-properties (region-beginning) (region-end))))
    (delete-region (region-beginning) (region-end))
    (insert (funcall to-pascal-case text))))

;;; transform-texts-to-formats.el ends here
