;;; format-region.el --- Transform region in different formats: camelCase, kebap-case or lisp-case, PascalCase or snake_case.
;;
;; Copyright © 2023 Andros Fenollosa
;; Authors: Andros Fenollosa <andros@fenollosa.email>
;; URL: https://github.com/tanrax/transform-texts-to-formats.el
;; Version: 1.0.0
;; SPDX-License-Identifier: GNU General Public License v3.0 or later

;;; Commentary:
;; Transform region in different formats: camelCase, kebap-case or lisp-case, PascalCase or snake_case.

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
	 (sentence-with-first-word-capitalized (if is-first-word-capitalized sentence-with-new-separator (concat (downcase (substring sentence-with-new-separator 0 1)) (substring sentence-with-new-separator 1)))) ; Capitalize first letter of first word
	 )
    sentence-with-first-word-capitalized))


(defmacro curried-format (separator is-first-word-capitalized is-all-words-capitalized)
  "Curry the function to-format with SEPARATOR, IS-FIRST-WORD-CAPITALIZED and IS-ALL-WORDS-CAPITALIZED."
  `(lambda (sentence) (to-format sentence ,separator ,is-first-word-capitalized ,is-all-words-capitalized)))

;; Define functions
(setq to-camel-case (curried-format nil nil t)) ; camelCase
(setq to-kebab-case (curried-format "-" nil nil)) ; kebab-case or lisp-case
(setq to-pascal-case (curried-format nil t t)) ; PascalCase
(setq to-snake-case (curried-format "_" nil nil)) ; snake_case

(defun format-region (fn-format)
  "Format the selected region with FN-FORMAT."
  (interactive)
  (let ((text (buffer-substring-no-properties (region-beginning) (region-end))))
    (delete-region (region-beginning) (region-end))
    (insert (funcall fn-format text))))

;; Interactive functions

(defun format-to-camel-case-region ()
  "Convert the selected text to camelCase."
  (interactive)
  (format-region to-camel-case))

(defun format-to-kebab-case-region ()
  "Convert the selected text to kebab-case or lisp-case."
  (interactive)
  (format-region to-kebab-case))

(defun format-to-lisp-case-region ()
  "Convert the selected text to kebab-case or lisp-case."
  (interactive)
  (format-region to-kebab-case))

(defun format-to-pascal-case-region ()
  "Convert the selected text to PascalCase."
  (interactive)
  (format-region to-pascal-case))

(defun format-to-snake-case-region ()
  "Convert the selected text to snake_case."
  (interactive)
  (format-region to-snake-case))

;;; transform-texts-to-formats.el ends here
