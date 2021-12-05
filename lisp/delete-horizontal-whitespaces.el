;-*- coding:utf-8 -*-
;;; delete-horizontal-whitespaces.el --- Delete horizontal whitespaces and tabs on the right side, and also delete blank lines at the end of the file.  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  toshiki

;; Author: toshiki <toshiki@aoi-local>
;; Keywords: whitespace tab

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(defun delete-horizontal-whitespaces ()
  "Delete all white spaces, tabs on right side and blank lines."
  (interactive "*")
  (progn
    (goto-char (point-min))
    (while (< (point) (point-max))
      ;; --- Delete white spaces and tabs.
      (end-of-line)
      (re-search-backward "[^ \t\r]" nil t)
      (forward-char 1)
      (delete-region (point) (progn (skip-chars-forward " \t") (point)))
      ;; --- Go to the next line.
      (forward-line 1))
    ;; --- Delete the blank line at the end of the file.
    (delete-blank-lines)))

(define-key global-map "\C-c\C-lk" 'delete-horizontal-whitespaces)
(provide 'delete-horizontal-whitespaces)
;;; delete-horizontal-whitespaces.el ends here
