;;-*- coding:utf-8 -*-

;;; coding-utf8.el ---                                    -*- lexical-binding: t; -*-

;; Copyright (C) 2014  toshiki kawai

;; Author: toshiki kawai <toshiki@aoi-local>
;; Keywords: coding utf-8

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

;;

;; val -> pos :: undefine
(defun insert-utf8 ()
  (goto-char (point-min))
  (insert comment-start)
  (insert "-*- coding:utf-8 -*- ")
  (insert comment-end))

(defun insert-string-utf8 ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (if (and (boundp 'comment-start) (char-or-string-p comment-start))
        (insert-utf8)
      (insert "-*- coding:utf-8 -*- "))))

;; keybinding: C-c u
;; Insert an comment of 'coding:utf-8'.
(define-key global-map "\C-cu" 'insert-string-utf8)

(provide 'coding-utf8)
;;; coding-utf8.el ends here
