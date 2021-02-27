;-*- coding:utf-8 -*- 
;;; google-search.el ---                              -*- lexical-binding: t; -*-

;; Copyright (C) 2021  toshiki kawai

;; Author: toshiki kawai <toshiki@aoi-local>
;; Keywords: google search

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Forked from: https://github.com/jd/emacs.d/blob/master/lisp/jd-google.el

;;; Code:

;;;###autoload
(defun google-search (keywords)
  "Form a google query URL and give it to browse-url"
  (interactive
   (list
    (if (use-region-p)
	(buffer-substring (region-beginning) (region-end))
      (read-string "Search Google for: " (thing-at-point 'word)))))
  (browse-url
   (concat "http://www.google.com/search?q="
	   (replace-regexp-in-string
	    "[[:space:]]+"
	    "+"
	    keywords))))

(define-key global-map "\C-c\C-lg" 'google-search)

(provide 'google-search)
;;; google-search.el ends here
