;-*- coding:utf-8 -*- 
;;; 1row-scroll.el ---                               -*- lexical-binding: t; -*-

;; Copyright (C) 2015  toshiki kawai

;; Author: toshiki kawai <toshiki@aoi-local>
;; Keywords: 

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

;;; Code:
(global-set-key "\C-z"
                '(lambda (arg) (interactive "p") (scroll-up arg)))

(global-set-key "\C-o"
                '(lambda (arg)  (interactive "p") (scroll-down arg)))

(provide '1row-scroll)
;;; 1row-scroll.el ends here
