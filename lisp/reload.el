;-*- coding:utf-8 -*- 
;;; reload.el ---                                    -*- lexical-binding: t; -*-

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

;; 

;;; Code:

;;; reference: https://www.gnu.org/software/emacs/manual/html_node/emacs/Find-Init.html#Find-Init
;;; Order to find Emacs init file.
;;; 1. ~/.emacs
;;; 2. ~/.emacs.el
;;; 3. ~/.emacs.d/init.el
(setq init-files '("~/.emacs" "~/.emacs.el" "~/.emacs.d/init.el"))

(defun load-init-file (list)
  "load init file for Emacs."
  (interactive)
  (when list ;; recursive.
    (let ((f (car list))) ;; get a first element.
      ;; if (f is not nil) and (f is file)
      (when (and f (file-readable-p f))
        (load-file f)
        ;; output message to mini buffer.
        (message (concat "reload " f " complete!")))
      (load-init-file (cdr list)))
    ))

;;(define-key global-map "\C-c\C-r" 'load-init-file)
(define-key global-map "\C-c\C-r"
  '(lambda () (interactive) (load-init-file init-files)))

(provide 'reload)
;;; reload.el ends here
