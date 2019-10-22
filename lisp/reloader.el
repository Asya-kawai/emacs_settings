;; -*- coding:utf-8 -*- 
;;; reloader.el ---                                  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  toshiki kawai

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

;; Forked from https://www.yatex.org/gitbucket/gist/yuuji/424d8293e616d2171aad5ed9ee517e1e.

;;; Code:
;;                                                                              
;; Reload init files (defaults to .emacs, .emacs.el, .emacs.d/init.el)          
;;                                                                              
 
(defvar reloader-default-init-files
  '("~/.emacs" "~/.emacs.el" "~/.emacs.d/init.el")
  "Default target files for reloading.                                          
C-u M-x reloader-reload causes prompt for loading file.")
 
(defun reloader-reload (arg)
  (interactive "P")
  (let ((files (if arg (list (read-file-name "Reload file: "))
                 reloader-default-init-files))
        f)
    (when files
      (setq f (car files))
      (and (stringp f)
           (file-readable-p f)
           (progn
             (message "Reloading %s" f)
             (load-file f)))
      (setq files (cdr files)))))
 
 
(define-key global-map "\C-cr" 'reloader-reload)
(provide 'reloader)
;;; reloader.el ends here
