;;-*- coding:utf-8 -*-

;; Copyright (C) 2014  Toshiki KAWAI

;; Author: Toshiki KAWAI <toshiki@lina>
;; Keywords: coding utf-8

;; val -> pos :: 未実装
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

(define-key global-map "\C-cu" 'insert-string-utf8)

(provide 'coding-utf8)
;;; coding-utf8.el ends here
